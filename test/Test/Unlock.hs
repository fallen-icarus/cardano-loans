{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Test.Unlock
  (
    -- * Scenarios Tested
    -- ** Scenarios that should succeed
    regressionTest1
  , regressionTest2
  , regressionTest3
  , regressionTest4

    -- ** Scenarios that should fail
  , failureTest1
  , failureTest2
  , failureTest3
  , failureTest4
  , failureTest5
  , failureTest6
  , failureTest7
  , failureTest8
  , failureTest9

    -- * Full test function
  , tests
  ) where

import PlutusTx.Prelude (unsafeRatio)
import Plutus.Trace
import Wallet.Emulator.Wallet
import Plutus.Contract.Test as Test
import Test.Tasty

import Test.Internal
import Test.Config
import CardanoLoans

-------------------------------------------------
-- Initialize Reference Scripts
-------------------------------------------------
initializeScripts :: EmulatorTrace ( TxOutRef,TxOutRef )
initializeScripts = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = beaconScript
      , createReferenceScriptAddress = refScriptAddress
      , createReferenceScriptUTxO = 
          ( lovelaceValueOf minUTxOMintRef
          , TxOutDatumInline $ toDatum ()
          )
      }

  void $ waitNSlots 2

  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = loanScript
      , createReferenceScriptAddress = refScriptAddress
      , createReferenceScriptUTxO = 
          ( lovelaceValueOf minUTxOSpendRef
          , TxOutDatumInline $ toDatum ()
          )
      }

  void $ waitNSlots 2

  liftM2 (,) (txOutRefWithValue $ lovelaceValueOf minUTxOMintRef)
             (txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef)

-------------------------------------------------
-- Regression Tests
-------------------------------------------------
-- | Unlock a single expired collateral UTxO.
regressionTest1 :: EmulatorTrace ()
regressionTest1 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  
  (mintRef,spendRef) <- initializeScripts

  -- Create the Ask UTxO.
  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset]
              , mintTokens = [("Ask",1),(assetBeacon,1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2
  
  -- Create the Offer UTxO.
  callEndpoint @"create-transaction" h2 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateOffer lenderCred [asset]
              , mintTokens = [("Offer",1),(assetBeacon,1),(lenderToken,1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum offerDatum
                    , lovelaceValueOf 103_000_000 
                    <> singleton beaconCurrencySymbol "Offer" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol lenderToken 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  startTime <- slotToBeginPOSIXTime def <$> waitNSlots 2

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Ask" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            askDatum
  offer1 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 103_000_000 
            <> singleton beaconCurrencySymbol "Offer" 1
            <> singleton beaconCurrencySymbol lenderToken 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            offerDatum

  let loanIdToken = genLoanId offer1
  let activeDatum = ActiveDatum
        { beaconSym = beaconCurrencySymbol 
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = asset
        , loanPrinciple = 100_000_000
        , rolloverFrequency = Just 1
        , lastCheckpoint = startTime
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , minPayment = 500_000
        , collateralization = [(testToken1, unsafeRatio 1 10_000_000)]
        , collateralIsSwappable = True
        , claimExpiration = startTime + 12000 + 10000
        , loanExpiration = startTime + 12000
        , loanOutstanding = fromInt 100_000_000 .*. (fromInt 1 .+. unsafeRatio 1 10) 
        , loanId = loanIdToken
        }
  
  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateActive borrowerCred [(ask1,offer1)]
              , mintTokens = 
                  [ ("Active",1)
                  , (assetBeacon,-1)
                  , (lenderToken,-1)
                  , (borrowerToken,1)
                  , ("Ask",-1)
                  , ("Offer",-1)
                  , (loanIdToken,2)
                  ]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer AcceptOffer
              , spendFromAddress = loanAddr
              , spendUtxos = 
                  [ ask1
                  , offer1
                  ]
              }
          ]
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum activeDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Active" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol loanIdToken 1
                    <> singleton beaconCurrencySymbol borrowerToken 1
                    <> uncurry singleton testToken1 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = lenderAddr
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,"Accepted") 
                    , lovelaceValueOf 3_000_000
                    <> singleton beaconCurrencySymbol loanIdToken 1
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = refScriptAddress
              , outputUtxos =
                  [ ( Just $ TxOutDatumHash $ toDatum ()
                    , lovelaceValueOf 20_000_000
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval (Just startTime) Nothing
      }

  endTime <- slotToBeginPOSIXTime def <$> waitNSlots 40

  active1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Active" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            <> singleton beaconCurrencySymbol loanIdToken 1
            <> singleton beaconCurrencySymbol borrowerToken 1
            <> uncurry singleton testToken1 10
            )
            activeDatum

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
              , mintTokens = 
                  [ (borrowerToken,-1)
                  , ("Active",-1)
                  , (assetBeacon,-1)
                  , (loanIdToken,-1)
                  ]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer Unlock
              , spendFromAddress = loanAddr
              , spendUtxos = [ active1 ]
              }
          ]
      , outputs = [ ]
      , validityRange = ValidityInterval (Just endTime) Nothing
      }

  where
    borrowerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
    lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2

    borrowerToken = credentialAsToken borrowerCred
    lenderToken = credentialAsToken lenderCred
    
    asset = (adaSymbol,adaToken)
    assetBeacon = genAssetBeaconName asset
    
    askDatum = AskDatum
      { beaconSym = beaconCurrencySymbol
      , borrowerId = borrowerToken
      , loanAsset = asset
      , loanPrinciple = 100_000_000
      , loanTerm = 12000
      , collateral = [testToken1]
      }

    lenderAddr = Address lenderCred Nothing

    offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = asset
        , loanPrinciple = 100_000_000
        , rolloverFrequency = Just 1
        , minPayment = 500_000
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 10_000_000)]
        , claimPeriod = 10000
        , offerDeposit = 3_000_000
        , collateralIsSwappable = True
        }
    
    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | Unlock a single finished Active UTxO even though the claim period has not passed.
regressionTest2 :: EmulatorTrace ()
regressionTest2 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  
  (mintRef,spendRef) <- initializeScripts

  -- Create the Ask UTxO.
  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset]
              , mintTokens = [("Ask",1),(assetBeacon,1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2
  
  -- Create the Offer UTxO.
  callEndpoint @"create-transaction" h2 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateOffer lenderCred [asset]
              , mintTokens = [("Offer",1),(assetBeacon,1),(lenderToken,1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum offerDatum
                    , lovelaceValueOf 103_000_000 
                    <> singleton beaconCurrencySymbol "Offer" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol lenderToken 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  startTime <- slotToBeginPOSIXTime def <$> waitNSlots 2

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Ask" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            askDatum
  offer1 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 103_000_000 
            <> singleton beaconCurrencySymbol "Offer" 1
            <> singleton beaconCurrencySymbol lenderToken 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            offerDatum

  let loanIdToken = genLoanId offer1
  let activeDatum = ActiveDatum
        { beaconSym = beaconCurrencySymbol 
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = asset
        , loanPrinciple = 100_000_000
        , rolloverFrequency = Nothing
        , lastCheckpoint = startTime
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , minPayment = 500_000
        , collateralization = [(testToken1, unsafeRatio 1 10_000_000)]
        , collateralIsSwappable = True
        , claimExpiration = startTime + 12000 + 10000
        , loanExpiration = startTime + 12000
        , loanOutstanding = fromInt 100_000_000 .*. (fromInt 1 .+. unsafeRatio 1 10) 
        , loanId = loanIdToken
        }
  
  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateActive borrowerCred [(ask1,offer1)]
              , mintTokens = 
                  [ ("Active",1)
                  , (assetBeacon,-1)
                  , (lenderToken,-1)
                  , (borrowerToken,1)
                  , ("Ask",-1)
                  , ("Offer",-1)
                  , (loanIdToken,2)
                  ]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer AcceptOffer
              , spendFromAddress = loanAddr
              , spendUtxos = 
                  [ ask1
                  , offer1
                  ]
              }
          ]
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum activeDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Active" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol loanIdToken 1
                    <> singleton beaconCurrencySymbol borrowerToken 1
                    <> uncurry singleton testToken1 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = lenderAddr
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,"Accepted") 
                    , lovelaceValueOf 3_000_000
                    <> singleton beaconCurrencySymbol loanIdToken 1
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = refScriptAddress
              , outputUtxos =
                  [ ( Just $ TxOutDatumHash $ toDatum ()
                    , lovelaceValueOf 20_000_000
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval (Just startTime) Nothing
      }

  void $ waitNSlots 2

  active1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Active" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            <> singleton beaconCurrencySymbol loanIdToken 1
            <> singleton beaconCurrencySymbol borrowerToken 1
            <> uncurry singleton testToken1 10
            )
            activeDatum

  let newActiveDatum = activeDatum
        { loanOutstanding = loanOutstanding activeDatum .-. fromInt 110_000_000
        }

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
              , mintTokens = [(borrowerToken,-1)]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer MakePayment
              , spendFromAddress = loanAddr
              , spendUtxos = [ active1 ]
              }
          ]
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum newActiveDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Active" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol loanIdToken 1
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = lenderAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,loanIdToken)
                    , lovelaceValueOf 110_000_000 
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing (Just $ loanExpiration activeDatum)
      }

  endTime <- slotToBeginPOSIXTime def <$> waitNSlots 2

  finished1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Active" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            <> singleton beaconCurrencySymbol loanIdToken 1
            )
            newActiveDatum

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
              , mintTokens = 
                  [ ("Active",-1)
                  , (assetBeacon,-1)
                  , (loanIdToken,-1)
                  ]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer Unlock
              , spendFromAddress = loanAddr
              , spendUtxos = [ finished1 ]
              }
          ]
      , outputs = [ ]
      , validityRange = ValidityInterval (Just endTime) Nothing
      }

  where
    borrowerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
    lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2

    borrowerToken = credentialAsToken borrowerCred
    lenderToken = credentialAsToken lenderCred
    
    asset = (adaSymbol,adaToken)
    assetBeacon = genAssetBeaconName asset
    
    askDatum = AskDatum
      { beaconSym = beaconCurrencySymbol
      , borrowerId = borrowerToken
      , loanAsset = asset
      , loanPrinciple = 100_000_000
      , loanTerm = 12000
      , collateral = [testToken1]
      }

    lenderAddr = Address lenderCred Nothing

    offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = asset
        , loanPrinciple = 100_000_000
        , rolloverFrequency = Nothing
        , minPayment = 500_000
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 10_000_000)]
        , claimPeriod = 10000
        , offerDeposit = 3_000_000
        , collateralIsSwappable = True
        }
    
    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | Unlock a single invalid ActiveUTxO.
regressionTest3 :: EmulatorTrace ()
regressionTest3 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  
  (mintRef,spendRef) <- initializeScripts

  -- Create the Ask UTxO.
  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset]
              , mintTokens = []
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum
                    , lovelaceValueOf 3_000_000 
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2
  
  startTime <- slotToBeginPOSIXTime def <$> waitNSlots 2

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            )
            askDatum
            
  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
              , mintTokens = [ ]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer Unlock
              , spendFromAddress = loanAddr
              , spendUtxos = [ ask1 ]
              }
          ]
      , outputs = [ ]
      , validityRange = ValidityInterval (Just startTime) Nothing
      }

  where
    borrowerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
    lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2

    borrowerToken = credentialAsToken borrowerCred
    lenderToken = credentialAsToken lenderCred
    
    asset = (adaSymbol,adaToken)
    assetBeacon = genAssetBeaconName asset
    askDatum = ActiveDatum
        { beaconSym = beaconCurrencySymbol 
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = asset
        , loanPrinciple = 100_000_000
        , rolloverFrequency = Just 1
        , lastCheckpoint = 0
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , minPayment = 500_000
        , collateralization = [(testToken1, unsafeRatio 1 10_000_000)]
        , collateralIsSwappable = True
        , claimExpiration = 12000 + 10000
        , loanExpiration = 12000
        , loanOutstanding = fromInt 100_000_000 .*. (fromInt 1 .+. unsafeRatio 1 10) 
        , loanId = "loanIdToken"
        }

    lenderAddr = Address lenderCred Nothing

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | Unlock multiple expired collateral UTxOs in the same transaction.
regressionTest4 :: EmulatorTrace ()
regressionTest4 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  
  (mintRef,spendRef) <- initializeScripts

  -- Create the Ask UTxO.
  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset]
              , mintTokens = [("Ask",2),(assetBeacon,2)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  , ( Just $ TxOutDatumInline $ toDatum askDatum{loanTerm = 22000}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2
  
  -- Create the Offer UTxO.
  callEndpoint @"create-transaction" h2 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateOffer lenderCred [asset]
              , mintTokens = [("Offer",2),(assetBeacon,2),(lenderToken,2)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum offerDatum
                    , lovelaceValueOf 103_000_000 
                    <> singleton beaconCurrencySymbol "Offer" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol lenderToken 1
                    )
                  , ( Just $ TxOutDatumInline $ toDatum offerDatum{loanTerm = 22000}
                    , lovelaceValueOf 103_000_000 
                    <> singleton beaconCurrencySymbol "Offer" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol lenderToken 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  startTime <- slotToBeginPOSIXTime def <$> waitNSlots 2

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Ask" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            askDatum
  offer1 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 103_000_000 
            <> singleton beaconCurrencySymbol "Offer" 1
            <> singleton beaconCurrencySymbol lenderToken 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            offerDatum
  ask2 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Ask" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            askDatum{loanTerm = 22000}
  offer2 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 103_000_000 
            <> singleton beaconCurrencySymbol "Offer" 1
            <> singleton beaconCurrencySymbol lenderToken 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            offerDatum{loanTerm = 22000}

  let loanIdToken1 = genLoanId offer1
  let loanIdToken2 = genLoanId offer2
  let activeDatum1 = ActiveDatum
        { beaconSym = beaconCurrencySymbol 
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = asset
        , loanPrinciple = 100_000_000
        , rolloverFrequency = Nothing
        , lastCheckpoint = startTime
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , minPayment = 500_000
        , collateralization = [(testToken1, unsafeRatio 1 10_000_000)]
        , collateralIsSwappable = True
        , claimExpiration = startTime + 12000 + 10000
        , loanExpiration = startTime + 12000
        , loanOutstanding = fromInt 100_000_000 .*. (fromInt 1 .+. unsafeRatio 1 10) 
        , loanId = loanIdToken1
        }
  let activeDatum2 = ActiveDatum
        { beaconSym = beaconCurrencySymbol 
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = asset
        , loanPrinciple = 100_000_000
        , rolloverFrequency = Nothing
        , lastCheckpoint = startTime
        , loanTerm = 22000
        , loanInterest = unsafeRatio 1 10
        , minPayment = 500_000
        , collateralization = [(testToken1, unsafeRatio 1 10_000_000)]
        , collateralIsSwappable = True
        , claimExpiration = startTime + 22000 + 10000
        , loanExpiration = startTime + 22000
        , loanOutstanding = fromInt 100_000_000 .*. (fromInt 1 .+. unsafeRatio 1 10) 
        , loanId = loanIdToken2
        }
  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateActive borrowerCred 
                  [ (ask1,offer1)
                  , (ask2,offer2)
                  ]
              , mintTokens = 
                  [ ("Active",2)
                  , (assetBeacon,-2)
                  , (lenderToken,-2)
                  , (borrowerToken,2)
                  , ("Ask",-2)
                  , ("Offer",-2)
                  , (loanIdToken1,2)
                  , (loanIdToken2,2)
                  ]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer AcceptOffer
              , spendFromAddress = loanAddr
              , spendUtxos = 
                  [ ask1
                  , offer1
                  , ask2
                  , offer2
                  ]
              }
          ]
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum activeDatum1
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Active" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol loanIdToken1 1
                    <> singleton beaconCurrencySymbol borrowerToken 1
                    <> uncurry singleton testToken1 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum activeDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Active" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol loanIdToken2 1
                    <> singleton beaconCurrencySymbol borrowerToken 1
                    <> uncurry singleton testToken1 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = lenderAddr
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,"Accepted") 
                    , lovelaceValueOf 3_000_000
                    <> singleton beaconCurrencySymbol loanIdToken1 1
                    )
                  , ( Just $ TxOutDatumInline
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,"Accepted") 
                    , lovelaceValueOf 3_000_000
                    <> singleton beaconCurrencySymbol loanIdToken2 1
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = refScriptAddress
              , outputUtxos =
                  [ ( Just $ TxOutDatumHash $ toDatum ()
                    , lovelaceValueOf 20_000_000
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval (Just startTime) Nothing
      }

  void $ waitNSlots 2

  active1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Active" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            <> singleton beaconCurrencySymbol loanIdToken1 1
            <> singleton beaconCurrencySymbol borrowerToken 1
            <> uncurry singleton testToken1 10
            )
            activeDatum1

  active2 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Active" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            <> singleton beaconCurrencySymbol loanIdToken2 1
            <> singleton beaconCurrencySymbol borrowerToken 1
            <> uncurry singleton testToken1 10
            )
            activeDatum2

  let newActiveDatum1 = activeDatum1
        { loanOutstanding = loanOutstanding activeDatum1 .-. fromInt 55_000_000
        }

  let newActiveDatum2 = activeDatum2
        { loanOutstanding = loanOutstanding activeDatum2 .-. fromInt 55_000_000
        }
        
  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
              , mintTokens = [(borrowerToken,0)]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer MakePayment
              , spendFromAddress = loanAddr
              , spendUtxos = [ active1, active2 ]
              }
          ]
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum newActiveDatum1
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Active" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol loanIdToken1 1
                    <> singleton beaconCurrencySymbol borrowerToken 1
                    <> uncurry singleton testToken1 5
                    )
                  , ( Just $ TxOutDatumInline $ toDatum newActiveDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Active" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol loanIdToken2 1
                    <> singleton beaconCurrencySymbol borrowerToken 1
                    <> uncurry singleton testToken1 5
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = lenderAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,loanIdToken1)
                    , lovelaceValueOf 55_000_000 
                    )
                  , ( Just $ TxOutDatumInline 
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,loanIdToken2)
                    , lovelaceValueOf 55_000_000 
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing (Just $ loanExpiration activeDatum1)
      }

  endTime <- slotToBeginPOSIXTime def <$> waitNSlots 40

  post1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Active" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            <> singleton beaconCurrencySymbol loanIdToken1 1
            <> singleton beaconCurrencySymbol borrowerToken 1
            <> uncurry singleton testToken1 5
            )
            newActiveDatum1
  post2 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Active" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            <> singleton beaconCurrencySymbol loanIdToken2 1
            <> singleton beaconCurrencySymbol borrowerToken 1
            <> uncurry singleton testToken1 5
            )
            newActiveDatum2

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
              , mintTokens = 
                  [ (borrowerToken,-2)
                  , ("Active",-2)
                  , (assetBeacon,-2)
                  , (loanIdToken1,-1)
                  , (loanIdToken2,-1)
                  ]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer Unlock
              , spendFromAddress = loanAddr
              , spendUtxos = [ post1,post2 ]
              }
          ]
      , outputs = [ ]
      , validityRange = ValidityInterval (Just endTime) Nothing
      }
      
  where
    borrowerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
    lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2

    borrowerToken = credentialAsToken borrowerCred
    lenderToken = credentialAsToken lenderCred
    
    asset = (adaSymbol,adaToken)
    assetBeacon = genAssetBeaconName asset
    
    askDatum = AskDatum
      { beaconSym = beaconCurrencySymbol
      , borrowerId = borrowerToken
      , loanAsset = asset
      , loanPrinciple = 100_000_000
      , loanTerm = 12000
      , collateral = [testToken1]
      }

    lenderAddr = Address lenderCred Nothing

    offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = asset
        , loanPrinciple = 100_000_000
        , rolloverFrequency = Nothing
        , minPayment = 500_000
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 10_000_000)]
        , claimPeriod = 10000
        , offerDeposit = 3_000_000
        , collateralIsSwappable = True
        }
    
    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-------------------------------------------------
-- Failure Tests
-------------------------------------------------
-- | The input does not have an ActiveDatum.
failureTest1 :: EmulatorTrace ()
failureTest1 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  
  (mintRef,spendRef) <- initializeScripts

  -- Create the Ask UTxO.
  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset]
              , mintTokens = []
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum
                    , lovelaceValueOf 3_000_000 
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2
  
  startTime <- slotToBeginPOSIXTime def <$> waitNSlots 2

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            )
            askDatum
            
  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
              , mintTokens = [ ]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer Unlock
              , spendFromAddress = loanAddr
              , spendUtxos = [ ask1 ]
              }
          ]
      , outputs = [ ]
      , validityRange = ValidityInterval (Just startTime) Nothing
      }

  where
    borrowerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
    lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2

    borrowerToken = credentialAsToken borrowerCred
    lenderToken = credentialAsToken lenderCred
    
    asset = (adaSymbol,adaToken)
    assetBeacon = genAssetBeaconName asset
    askDatum = AskDatum
      { beaconSym = beaconCurrencySymbol
      , borrowerId = credentialAsToken borrowerCred
      , loanAsset = asset
      , loanPrinciple = 100_000_000
      , loanTerm = 12000
      , collateral = [testToken1]
      }

    lenderAddr = Address lenderCred Nothing

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | The borrower did not approve.
failureTest2 :: EmulatorTrace ()
failureTest2 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  
  (mintRef,spendRef) <- initializeScripts

  -- Create the Ask UTxO.
  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset]
              , mintTokens = [("Ask",1),(assetBeacon,1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2
  
  -- Create the Offer UTxO.
  callEndpoint @"create-transaction" h2 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateOffer lenderCred [asset]
              , mintTokens = [("Offer",1),(assetBeacon,1),(lenderToken,1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum offerDatum
                    , lovelaceValueOf 103_000_000 
                    <> singleton beaconCurrencySymbol "Offer" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol lenderToken 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  startTime <- slotToBeginPOSIXTime def <$> waitNSlots 2

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Ask" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            askDatum
  offer1 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 103_000_000 
            <> singleton beaconCurrencySymbol "Offer" 1
            <> singleton beaconCurrencySymbol lenderToken 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            offerDatum

  let loanIdToken = genLoanId offer1
  let activeDatum = ActiveDatum
        { beaconSym = beaconCurrencySymbol 
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = asset
        , loanPrinciple = 100_000_000
        , rolloverFrequency = Just 1
        , lastCheckpoint = startTime
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , minPayment = 500_000
        , collateralization = [(testToken1, unsafeRatio 1 10_000_000)]
        , collateralIsSwappable = True
        , claimExpiration = startTime + 12000 + 10000
        , loanExpiration = startTime + 12000
        , loanOutstanding = fromInt 100_000_000 .*. (fromInt 1 .+. unsafeRatio 1 10) 
        , loanId = loanIdToken
        }
  
  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateActive borrowerCred [(ask1,offer1)]
              , mintTokens = 
                  [ ("Active",1)
                  , (assetBeacon,-1)
                  , (lenderToken,-1)
                  , (borrowerToken,1)
                  , ("Ask",-1)
                  , ("Offer",-1)
                  , (loanIdToken,2)
                  ]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer AcceptOffer
              , spendFromAddress = loanAddr
              , spendUtxos = 
                  [ ask1
                  , offer1
                  ]
              }
          ]
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum activeDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Active" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol loanIdToken 1
                    <> singleton beaconCurrencySymbol borrowerToken 1
                    <> uncurry singleton testToken1 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = lenderAddr
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,"Accepted") 
                    , lovelaceValueOf 3_000_000
                    <> singleton beaconCurrencySymbol loanIdToken 1
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = refScriptAddress
              , outputUtxos =
                  [ ( Just $ TxOutDatumHash $ toDatum ()
                    , lovelaceValueOf 20_000_000
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval (Just startTime) Nothing
      }

  endTime <- slotToBeginPOSIXTime def <$> waitNSlots 40

  active1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Active" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            <> singleton beaconCurrencySymbol loanIdToken 1
            <> singleton beaconCurrencySymbol borrowerToken 1
            <> uncurry singleton testToken1 10
            )
            activeDatum

  callEndpoint @"create-transaction" h2 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
              , mintTokens = 
                  [ (borrowerToken,-1)
                  , ("Active",-1)
                  , (assetBeacon,-1)
                  , (loanIdToken,-1)
                  ]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer Unlock
              , spendFromAddress = loanAddr
              , spendUtxos = [ active1 ]
              }
          ]
      , outputs = [ ]
      , validityRange = ValidityInterval (Just endTime) Nothing
      }

  where
    borrowerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
    lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2

    borrowerToken = credentialAsToken borrowerCred
    lenderToken = credentialAsToken lenderCred
    
    asset = (adaSymbol,adaToken)
    assetBeacon = genAssetBeaconName asset
    
    askDatum = AskDatum
      { beaconSym = beaconCurrencySymbol
      , borrowerId = borrowerToken
      , loanAsset = asset
      , loanPrinciple = 100_000_000
      , loanTerm = 12000
      , collateral = [testToken1]
      }

    lenderAddr = Address lenderCred Nothing

    offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = asset
        , loanPrinciple = 100_000_000
        , rolloverFrequency = Just 1
        , minPayment = 500_000
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 10_000_000)]
        , claimPeriod = 10000
        , offerDeposit = 3_000_000
        , collateralIsSwappable = True
        }
    
    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | The BorrowerID is present in the input but the claim period has not passed.
failureTest3 :: EmulatorTrace ()
failureTest3 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  
  (mintRef,spendRef) <- initializeScripts

  -- Create the Ask UTxO.
  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset]
              , mintTokens = [("Ask",1),(assetBeacon,1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2
  
  -- Create the Offer UTxO.
  callEndpoint @"create-transaction" h2 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateOffer lenderCred [asset]
              , mintTokens = [("Offer",1),(assetBeacon,1),(lenderToken,1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum offerDatum
                    , lovelaceValueOf 103_000_000 
                    <> singleton beaconCurrencySymbol "Offer" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol lenderToken 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  startTime <- slotToBeginPOSIXTime def <$> waitNSlots 2

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Ask" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            askDatum
  offer1 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 103_000_000 
            <> singleton beaconCurrencySymbol "Offer" 1
            <> singleton beaconCurrencySymbol lenderToken 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            offerDatum

  let loanIdToken = genLoanId offer1
  let activeDatum = ActiveDatum
        { beaconSym = beaconCurrencySymbol 
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = asset
        , loanPrinciple = 100_000_000
        , rolloverFrequency = Just 1
        , lastCheckpoint = startTime
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , minPayment = 500_000
        , collateralization = [(testToken1, unsafeRatio 1 10_000_000)]
        , collateralIsSwappable = True
        , claimExpiration = startTime + 12000 + 10000
        , loanExpiration = startTime + 12000
        , loanOutstanding = fromInt 100_000_000 .*. (fromInt 1 .+. unsafeRatio 1 10) 
        , loanId = loanIdToken
        }
  
  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateActive borrowerCred [(ask1,offer1)]
              , mintTokens = 
                  [ ("Active",1)
                  , (assetBeacon,-1)
                  , (lenderToken,-1)
                  , (borrowerToken,1)
                  , ("Ask",-1)
                  , ("Offer",-1)
                  , (loanIdToken,2)
                  ]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer AcceptOffer
              , spendFromAddress = loanAddr
              , spendUtxos = 
                  [ ask1
                  , offer1
                  ]
              }
          ]
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum activeDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Active" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol loanIdToken 1
                    <> singleton beaconCurrencySymbol borrowerToken 1
                    <> uncurry singleton testToken1 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = lenderAddr
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,"Accepted") 
                    , lovelaceValueOf 3_000_000
                    <> singleton beaconCurrencySymbol loanIdToken 1
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = refScriptAddress
              , outputUtxos =
                  [ ( Just $ TxOutDatumHash $ toDatum ()
                    , lovelaceValueOf 20_000_000
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval (Just startTime) Nothing
      }

  endTime <- slotToBeginPOSIXTime def <$> waitNSlots 2

  active1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Active" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            <> singleton beaconCurrencySymbol loanIdToken 1
            <> singleton beaconCurrencySymbol borrowerToken 1
            <> uncurry singleton testToken1 10
            )
            activeDatum

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
              , mintTokens = 
                  [ (borrowerToken,-1)
                  , ("Active",-1)
                  , (assetBeacon,-1)
                  , (loanIdToken,-1)
                  ]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer Unlock
              , spendFromAddress = loanAddr
              , spendUtxos = [ active1 ]
              }
          ]
      , outputs = [ ]
      , validityRange = ValidityInterval (Just endTime) Nothing
      }

  where
    borrowerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
    lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2

    borrowerToken = credentialAsToken borrowerCred
    lenderToken = credentialAsToken lenderCred
    
    asset = (adaSymbol,adaToken)
    assetBeacon = genAssetBeaconName asset
    
    askDatum = AskDatum
      { beaconSym = beaconCurrencySymbol
      , borrowerId = borrowerToken
      , loanAsset = asset
      , loanPrinciple = 100_000_000
      , loanTerm = 12000
      , collateral = [testToken1]
      }

    lenderAddr = Address lenderCred Nothing

    offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = asset
        , loanPrinciple = 100_000_000
        , rolloverFrequency = Just 1
        , minPayment = 500_000
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 10_000_000)]
        , claimPeriod = 10000
        , offerDeposit = 3_000_000
        , collateralIsSwappable = True
        }
    
    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | The invalid-before was not specified.
failureTest4 :: EmulatorTrace ()
failureTest4 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  
  (mintRef,spendRef) <- initializeScripts

  -- Create the Ask UTxO.
  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset]
              , mintTokens = [("Ask",1),(assetBeacon,1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2
  
  -- Create the Offer UTxO.
  callEndpoint @"create-transaction" h2 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateOffer lenderCred [asset]
              , mintTokens = [("Offer",1),(assetBeacon,1),(lenderToken,1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum offerDatum
                    , lovelaceValueOf 103_000_000 
                    <> singleton beaconCurrencySymbol "Offer" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol lenderToken 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  startTime <- slotToBeginPOSIXTime def <$> waitNSlots 2

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Ask" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            askDatum
  offer1 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 103_000_000 
            <> singleton beaconCurrencySymbol "Offer" 1
            <> singleton beaconCurrencySymbol lenderToken 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            offerDatum

  let loanIdToken = genLoanId offer1
  let activeDatum = ActiveDatum
        { beaconSym = beaconCurrencySymbol 
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = asset
        , loanPrinciple = 100_000_000
        , rolloverFrequency = Just 1
        , lastCheckpoint = startTime
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , minPayment = 500_000
        , collateralization = [(testToken1, unsafeRatio 1 10_000_000)]
        , collateralIsSwappable = True
        , claimExpiration = startTime + 12000 + 10000
        , loanExpiration = startTime + 12000
        , loanOutstanding = fromInt 100_000_000 .*. (fromInt 1 .+. unsafeRatio 1 10) 
        , loanId = loanIdToken
        }
  
  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateActive borrowerCred [(ask1,offer1)]
              , mintTokens = 
                  [ ("Active",1)
                  , (assetBeacon,-1)
                  , (lenderToken,-1)
                  , (borrowerToken,1)
                  , ("Ask",-1)
                  , ("Offer",-1)
                  , (loanIdToken,2)
                  ]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer AcceptOffer
              , spendFromAddress = loanAddr
              , spendUtxos = 
                  [ ask1
                  , offer1
                  ]
              }
          ]
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum activeDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Active" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol loanIdToken 1
                    <> singleton beaconCurrencySymbol borrowerToken 1
                    <> uncurry singleton testToken1 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = lenderAddr
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,"Accepted") 
                    , lovelaceValueOf 3_000_000
                    <> singleton beaconCurrencySymbol loanIdToken 1
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = refScriptAddress
              , outputUtxos =
                  [ ( Just $ TxOutDatumHash $ toDatum ()
                    , lovelaceValueOf 20_000_000
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval (Just startTime) Nothing
      }

  endTime <- slotToBeginPOSIXTime def <$> waitNSlots 40

  active1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Active" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            <> singleton beaconCurrencySymbol loanIdToken 1
            <> singleton beaconCurrencySymbol borrowerToken 1
            <> uncurry singleton testToken1 10
            )
            activeDatum

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
              , mintTokens = 
                  [ (borrowerToken,-1)
                  , ("Active",-1)
                  , (assetBeacon,-1)
                  , (loanIdToken,-1)
                  ]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer Unlock
              , spendFromAddress = loanAddr
              , spendUtxos = [ active1 ]
              }
          ]
      , outputs = [ ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  where
    borrowerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
    lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2

    borrowerToken = credentialAsToken borrowerCred
    lenderToken = credentialAsToken lenderCred
    
    asset = (adaSymbol,adaToken)
    assetBeacon = genAssetBeaconName asset
    
    askDatum = AskDatum
      { beaconSym = beaconCurrencySymbol
      , borrowerId = borrowerToken
      , loanAsset = asset
      , loanPrinciple = 100_000_000
      , loanTerm = 12000
      , collateral = [testToken1]
      }

    lenderAddr = Address lenderCred Nothing

    offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = asset
        , loanPrinciple = 100_000_000
        , rolloverFrequency = Just 1
        , minPayment = 500_000
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 10_000_000)]
        , claimPeriod = 10000
        , offerDeposit = 3_000_000
        , collateralIsSwappable = True
        }
    
    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | The input has a BorrowerID but it is not burned.
failureTest5 :: EmulatorTrace ()
failureTest5 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  
  (mintRef,spendRef) <- initializeScripts

  -- Create the Ask UTxO.
  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset]
              , mintTokens = [("Ask",1),(assetBeacon,1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2
  
  -- Create the Offer UTxO.
  callEndpoint @"create-transaction" h2 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateOffer lenderCred [asset]
              , mintTokens = [("Offer",1),(assetBeacon,1),(lenderToken,1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum offerDatum
                    , lovelaceValueOf 103_000_000 
                    <> singleton beaconCurrencySymbol "Offer" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol lenderToken 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  startTime <- slotToBeginPOSIXTime def <$> waitNSlots 2

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Ask" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            askDatum
  offer1 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 103_000_000 
            <> singleton beaconCurrencySymbol "Offer" 1
            <> singleton beaconCurrencySymbol lenderToken 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            offerDatum

  let loanIdToken = genLoanId offer1
  let activeDatum = ActiveDatum
        { beaconSym = beaconCurrencySymbol 
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = asset
        , loanPrinciple = 100_000_000
        , rolloverFrequency = Just 1
        , lastCheckpoint = startTime
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , minPayment = 500_000
        , collateralization = [(testToken1, unsafeRatio 1 10_000_000)]
        , collateralIsSwappable = True
        , claimExpiration = startTime + 12000 + 10000
        , loanExpiration = startTime + 12000
        , loanOutstanding = fromInt 100_000_000 .*. (fromInt 1 .+. unsafeRatio 1 10) 
        , loanId = loanIdToken
        }
  
  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateActive borrowerCred [(ask1,offer1)]
              , mintTokens = 
                  [ ("Active",1)
                  , (assetBeacon,-1)
                  , (lenderToken,-1)
                  , (borrowerToken,1)
                  , ("Ask",-1)
                  , ("Offer",-1)
                  , (loanIdToken,2)
                  ]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer AcceptOffer
              , spendFromAddress = loanAddr
              , spendUtxos = 
                  [ ask1
                  , offer1
                  ]
              }
          ]
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum activeDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Active" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol loanIdToken 1
                    <> singleton beaconCurrencySymbol borrowerToken 1
                    <> uncurry singleton testToken1 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = lenderAddr
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,"Accepted") 
                    , lovelaceValueOf 3_000_000
                    <> singleton beaconCurrencySymbol loanIdToken 1
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = refScriptAddress
              , outputUtxos =
                  [ ( Just $ TxOutDatumHash $ toDatum ()
                    , lovelaceValueOf 20_000_000
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval (Just startTime) Nothing
      }

  endTime <- slotToBeginPOSIXTime def <$> waitNSlots 40

  active1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Active" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            <> singleton beaconCurrencySymbol loanIdToken 1
            <> singleton beaconCurrencySymbol borrowerToken 1
            <> uncurry singleton testToken1 10
            )
            activeDatum

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
              , mintTokens = 
                  [ (borrowerToken,0)
                  , ("Active",-1)
                  , (assetBeacon,-1)
                  , (loanIdToken,-1)
                  ]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer Unlock
              , spendFromAddress = loanAddr
              , spendUtxos = [ active1 ]
              }
          ]
      , outputs = [ ]
      , validityRange = ValidityInterval (Just endTime) Nothing
      }

  where
    borrowerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
    lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2

    borrowerToken = credentialAsToken borrowerCred
    lenderToken = credentialAsToken lenderCred
    
    asset = (adaSymbol,adaToken)
    assetBeacon = genAssetBeaconName asset
    
    askDatum = AskDatum
      { beaconSym = beaconCurrencySymbol
      , borrowerId = borrowerToken
      , loanAsset = asset
      , loanPrinciple = 100_000_000
      , loanTerm = 12000
      , collateral = [testToken1]
      }

    lenderAddr = Address lenderCred Nothing

    offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = asset
        , loanPrinciple = 100_000_000
        , rolloverFrequency = Just 1
        , minPayment = 500_000
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 10_000_000)]
        , claimPeriod = 10000
        , offerDeposit = 3_000_000
        , collateralIsSwappable = True
        }
    
    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | When unlocking multiple expired UTxOs, not all BorrowerIDs burned.
failureTest6 :: EmulatorTrace ()
failureTest6 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  
  (mintRef,spendRef) <- initializeScripts

  -- Create the Ask UTxO.
  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset]
              , mintTokens = [("Ask",2),(assetBeacon,2)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  , ( Just $ TxOutDatumInline $ toDatum askDatum{loanTerm = 22000}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2
  
  -- Create the Offer UTxO.
  callEndpoint @"create-transaction" h2 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateOffer lenderCred [asset]
              , mintTokens = [("Offer",2),(assetBeacon,2),(lenderToken,2)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum offerDatum
                    , lovelaceValueOf 103_000_000 
                    <> singleton beaconCurrencySymbol "Offer" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol lenderToken 1
                    )
                  , ( Just $ TxOutDatumInline $ toDatum offerDatum{loanTerm = 22000}
                    , lovelaceValueOf 103_000_000 
                    <> singleton beaconCurrencySymbol "Offer" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol lenderToken 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  startTime <- slotToBeginPOSIXTime def <$> waitNSlots 2

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Ask" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            askDatum
  offer1 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 103_000_000 
            <> singleton beaconCurrencySymbol "Offer" 1
            <> singleton beaconCurrencySymbol lenderToken 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            offerDatum
  ask2 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Ask" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            askDatum{loanTerm = 22000}
  offer2 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 103_000_000 
            <> singleton beaconCurrencySymbol "Offer" 1
            <> singleton beaconCurrencySymbol lenderToken 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            offerDatum{loanTerm = 22000}

  let loanIdToken1 = genLoanId offer1
  let loanIdToken2 = genLoanId offer2
  let activeDatum1 = ActiveDatum
        { beaconSym = beaconCurrencySymbol 
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = asset
        , loanPrinciple = 100_000_000
        , rolloverFrequency = Nothing
        , lastCheckpoint = startTime
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , minPayment = 500_000
        , collateralization = [(testToken1, unsafeRatio 1 10_000_000)]
        , collateralIsSwappable = True
        , claimExpiration = startTime + 12000 + 10000
        , loanExpiration = startTime + 12000
        , loanOutstanding = fromInt 100_000_000 .*. (fromInt 1 .+. unsafeRatio 1 10) 
        , loanId = loanIdToken1
        }
  let activeDatum2 = ActiveDatum
        { beaconSym = beaconCurrencySymbol 
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = asset
        , loanPrinciple = 100_000_000
        , rolloverFrequency = Nothing
        , lastCheckpoint = startTime
        , loanTerm = 22000
        , loanInterest = unsafeRatio 1 10
        , minPayment = 500_000
        , collateralization = [(testToken1, unsafeRatio 1 10_000_000)]
        , collateralIsSwappable = True
        , claimExpiration = startTime + 22000 + 10000
        , loanExpiration = startTime + 22000
        , loanOutstanding = fromInt 100_000_000 .*. (fromInt 1 .+. unsafeRatio 1 10) 
        , loanId = loanIdToken2
        }
  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateActive borrowerCred 
                  [ (ask1,offer1)
                  , (ask2,offer2)
                  ]
              , mintTokens = 
                  [ ("Active",2)
                  , (assetBeacon,-2)
                  , (lenderToken,-2)
                  , (borrowerToken,2)
                  , ("Ask",-2)
                  , ("Offer",-2)
                  , (loanIdToken1,2)
                  , (loanIdToken2,2)
                  ]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer AcceptOffer
              , spendFromAddress = loanAddr
              , spendUtxos = 
                  [ ask1
                  , offer1
                  , ask2
                  , offer2
                  ]
              }
          ]
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum activeDatum1
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Active" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol loanIdToken1 1
                    <> singleton beaconCurrencySymbol borrowerToken 1
                    <> uncurry singleton testToken1 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum activeDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Active" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol loanIdToken2 1
                    <> singleton beaconCurrencySymbol borrowerToken 1
                    <> uncurry singleton testToken1 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = lenderAddr
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,"Accepted") 
                    , lovelaceValueOf 3_000_000
                    <> singleton beaconCurrencySymbol loanIdToken1 1
                    )
                  , ( Just $ TxOutDatumInline
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,"Accepted") 
                    , lovelaceValueOf 3_000_000
                    <> singleton beaconCurrencySymbol loanIdToken2 1
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = refScriptAddress
              , outputUtxos =
                  [ ( Just $ TxOutDatumHash $ toDatum ()
                    , lovelaceValueOf 20_000_000
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval (Just startTime) Nothing
      }

  void $ waitNSlots 2

  active1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Active" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            <> singleton beaconCurrencySymbol loanIdToken1 1
            <> singleton beaconCurrencySymbol borrowerToken 1
            <> uncurry singleton testToken1 10
            )
            activeDatum1

  active2 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Active" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            <> singleton beaconCurrencySymbol loanIdToken2 1
            <> singleton beaconCurrencySymbol borrowerToken 1
            <> uncurry singleton testToken1 10
            )
            activeDatum2

  let newActiveDatum1 = activeDatum1
        { loanOutstanding = loanOutstanding activeDatum1 .-. fromInt 55_000_000
        }

  let newActiveDatum2 = activeDatum2
        { loanOutstanding = loanOutstanding activeDatum2 .-. fromInt 55_000_000
        }
        
  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
              , mintTokens = [(borrowerToken,0)]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer MakePayment
              , spendFromAddress = loanAddr
              , spendUtxos = [ active1, active2 ]
              }
          ]
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum newActiveDatum1
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Active" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol loanIdToken1 1
                    <> singleton beaconCurrencySymbol borrowerToken 1
                    <> uncurry singleton testToken1 5
                    )
                  , ( Just $ TxOutDatumInline $ toDatum newActiveDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Active" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol loanIdToken2 1
                    <> singleton beaconCurrencySymbol borrowerToken 1
                    <> uncurry singleton testToken1 5
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = lenderAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,loanIdToken1)
                    , lovelaceValueOf 55_000_000 
                    )
                  , ( Just $ TxOutDatumInline 
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,loanIdToken2)
                    , lovelaceValueOf 55_000_000 
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing (Just $ loanExpiration activeDatum1)
      }

  endTime <- slotToBeginPOSIXTime def <$> waitNSlots 40

  post1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Active" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            <> singleton beaconCurrencySymbol loanIdToken1 1
            <> singleton beaconCurrencySymbol borrowerToken 1
            <> uncurry singleton testToken1 5
            )
            newActiveDatum1
  post2 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Active" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            <> singleton beaconCurrencySymbol loanIdToken2 1
            <> singleton beaconCurrencySymbol borrowerToken 1
            <> uncurry singleton testToken1 5
            )
            newActiveDatum2

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
              , mintTokens = 
                  [ (borrowerToken,-1)
                  , ("Active",-2)
                  , (assetBeacon,-2)
                  , (loanIdToken1,-1)
                  , (loanIdToken2,-1)
                  ]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer Unlock
              , spendFromAddress = loanAddr
              , spendUtxos = [ post1,post2 ]
              }
          ]
      , outputs = [ ]
      , validityRange = ValidityInterval (Just endTime) Nothing
      }
      
  where
    borrowerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
    lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2

    borrowerToken = credentialAsToken borrowerCred
    lenderToken = credentialAsToken lenderCred
    
    asset = (adaSymbol,adaToken)
    assetBeacon = genAssetBeaconName asset
    
    askDatum = AskDatum
      { beaconSym = beaconCurrencySymbol
      , borrowerId = borrowerToken
      , loanAsset = asset
      , loanPrinciple = 100_000_000
      , loanTerm = 12000
      , collateral = [testToken1]
      }

    lenderAddr = Address lenderCred Nothing

    offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = asset
        , loanPrinciple = 100_000_000
        , rolloverFrequency = Nothing
        , minPayment = 500_000
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 10_000_000)]
        , claimPeriod = 10000
        , offerDeposit = 3_000_000
        , collateralIsSwappable = True
        }
    
    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | When unlocking multiple expired UTxOs, not all Active beacons burned.
failureTest7 :: EmulatorTrace ()
failureTest7 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  
  (mintRef,spendRef) <- initializeScripts

  -- Create the Ask UTxO.
  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset]
              , mintTokens = [("Ask",2),(assetBeacon,2)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  , ( Just $ TxOutDatumInline $ toDatum askDatum{loanTerm = 22000}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2
  
  -- Create the Offer UTxO.
  callEndpoint @"create-transaction" h2 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateOffer lenderCred [asset]
              , mintTokens = [("Offer",2),(assetBeacon,2),(lenderToken,2)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum offerDatum
                    , lovelaceValueOf 103_000_000 
                    <> singleton beaconCurrencySymbol "Offer" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol lenderToken 1
                    )
                  , ( Just $ TxOutDatumInline $ toDatum offerDatum{loanTerm = 22000}
                    , lovelaceValueOf 103_000_000 
                    <> singleton beaconCurrencySymbol "Offer" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol lenderToken 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  startTime <- slotToBeginPOSIXTime def <$> waitNSlots 2

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Ask" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            askDatum
  offer1 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 103_000_000 
            <> singleton beaconCurrencySymbol "Offer" 1
            <> singleton beaconCurrencySymbol lenderToken 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            offerDatum
  ask2 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Ask" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            askDatum{loanTerm = 22000}
  offer2 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 103_000_000 
            <> singleton beaconCurrencySymbol "Offer" 1
            <> singleton beaconCurrencySymbol lenderToken 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            offerDatum{loanTerm = 22000}

  let loanIdToken1 = genLoanId offer1
  let loanIdToken2 = genLoanId offer2
  let activeDatum1 = ActiveDatum
        { beaconSym = beaconCurrencySymbol 
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = asset
        , loanPrinciple = 100_000_000
        , rolloverFrequency = Nothing
        , lastCheckpoint = startTime
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , minPayment = 500_000
        , collateralization = [(testToken1, unsafeRatio 1 10_000_000)]
        , collateralIsSwappable = True
        , claimExpiration = startTime + 12000 + 10000
        , loanExpiration = startTime + 12000
        , loanOutstanding = fromInt 100_000_000 .*. (fromInt 1 .+. unsafeRatio 1 10) 
        , loanId = loanIdToken1
        }
  let activeDatum2 = ActiveDatum
        { beaconSym = beaconCurrencySymbol 
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = asset
        , loanPrinciple = 100_000_000
        , rolloverFrequency = Nothing
        , lastCheckpoint = startTime
        , loanTerm = 22000
        , loanInterest = unsafeRatio 1 10
        , minPayment = 500_000
        , collateralization = [(testToken1, unsafeRatio 1 10_000_000)]
        , collateralIsSwappable = True
        , claimExpiration = startTime + 22000 + 10000
        , loanExpiration = startTime + 22000
        , loanOutstanding = fromInt 100_000_000 .*. (fromInt 1 .+. unsafeRatio 1 10) 
        , loanId = loanIdToken2
        }
  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateActive borrowerCred 
                  [ (ask1,offer1)
                  , (ask2,offer2)
                  ]
              , mintTokens = 
                  [ ("Active",2)
                  , (assetBeacon,-2)
                  , (lenderToken,-2)
                  , (borrowerToken,2)
                  , ("Ask",-2)
                  , ("Offer",-2)
                  , (loanIdToken1,2)
                  , (loanIdToken2,2)
                  ]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer AcceptOffer
              , spendFromAddress = loanAddr
              , spendUtxos = 
                  [ ask1
                  , offer1
                  , ask2
                  , offer2
                  ]
              }
          ]
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum activeDatum1
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Active" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol loanIdToken1 1
                    <> singleton beaconCurrencySymbol borrowerToken 1
                    <> uncurry singleton testToken1 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum activeDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Active" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol loanIdToken2 1
                    <> singleton beaconCurrencySymbol borrowerToken 1
                    <> uncurry singleton testToken1 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = lenderAddr
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,"Accepted") 
                    , lovelaceValueOf 3_000_000
                    <> singleton beaconCurrencySymbol loanIdToken1 1
                    )
                  , ( Just $ TxOutDatumInline
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,"Accepted") 
                    , lovelaceValueOf 3_000_000
                    <> singleton beaconCurrencySymbol loanIdToken2 1
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = refScriptAddress
              , outputUtxos =
                  [ ( Just $ TxOutDatumHash $ toDatum ()
                    , lovelaceValueOf 20_000_000
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval (Just startTime) Nothing
      }

  void $ waitNSlots 2

  active1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Active" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            <> singleton beaconCurrencySymbol loanIdToken1 1
            <> singleton beaconCurrencySymbol borrowerToken 1
            <> uncurry singleton testToken1 10
            )
            activeDatum1

  active2 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Active" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            <> singleton beaconCurrencySymbol loanIdToken2 1
            <> singleton beaconCurrencySymbol borrowerToken 1
            <> uncurry singleton testToken1 10
            )
            activeDatum2

  let newActiveDatum1 = activeDatum1
        { loanOutstanding = loanOutstanding activeDatum1 .-. fromInt 55_000_000
        }

  let newActiveDatum2 = activeDatum2
        { loanOutstanding = loanOutstanding activeDatum2 .-. fromInt 55_000_000
        }
        
  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
              , mintTokens = [(borrowerToken,0)]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer MakePayment
              , spendFromAddress = loanAddr
              , spendUtxos = [ active1, active2 ]
              }
          ]
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum newActiveDatum1
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Active" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol loanIdToken1 1
                    <> singleton beaconCurrencySymbol borrowerToken 1
                    <> uncurry singleton testToken1 5
                    )
                  , ( Just $ TxOutDatumInline $ toDatum newActiveDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Active" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol loanIdToken2 1
                    <> singleton beaconCurrencySymbol borrowerToken 1
                    <> uncurry singleton testToken1 5
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = lenderAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,loanIdToken1)
                    , lovelaceValueOf 55_000_000 
                    )
                  , ( Just $ TxOutDatumInline 
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,loanIdToken2)
                    , lovelaceValueOf 55_000_000 
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing (Just $ loanExpiration activeDatum1)
      }

  endTime <- slotToBeginPOSIXTime def <$> waitNSlots 40

  post1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Active" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            <> singleton beaconCurrencySymbol loanIdToken1 1
            <> singleton beaconCurrencySymbol borrowerToken 1
            <> uncurry singleton testToken1 5
            )
            newActiveDatum1
  post2 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Active" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            <> singleton beaconCurrencySymbol loanIdToken2 1
            <> singleton beaconCurrencySymbol borrowerToken 1
            <> uncurry singleton testToken1 5
            )
            newActiveDatum2

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
              , mintTokens = 
                  [ (borrowerToken,-2)
                  , ("Active",-1)
                  , (assetBeacon,-2)
                  , (loanIdToken1,-1)
                  , (loanIdToken2,-1)
                  ]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer Unlock
              , spendFromAddress = loanAddr
              , spendUtxos = [ post1,post2 ]
              }
          ]
      , outputs = [ ]
      , validityRange = ValidityInterval (Just endTime) Nothing
      }
      
  where
    borrowerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
    lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2

    borrowerToken = credentialAsToken borrowerCred
    lenderToken = credentialAsToken lenderCred
    
    asset = (adaSymbol,adaToken)
    assetBeacon = genAssetBeaconName asset
    
    askDatum = AskDatum
      { beaconSym = beaconCurrencySymbol
      , borrowerId = borrowerToken
      , loanAsset = asset
      , loanPrinciple = 100_000_000
      , loanTerm = 12000
      , collateral = [testToken1]
      }

    lenderAddr = Address lenderCred Nothing

    offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = asset
        , loanPrinciple = 100_000_000
        , rolloverFrequency = Nothing
        , minPayment = 500_000
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 10_000_000)]
        , claimPeriod = 10000
        , offerDeposit = 3_000_000
        , collateralIsSwappable = True
        }
    
    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | When unlocking multiple expired UTxOs, not all Asset beacons burned.
failureTest8 :: EmulatorTrace ()
failureTest8 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  
  (mintRef,spendRef) <- initializeScripts

  -- Create the Ask UTxO.
  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset]
              , mintTokens = [("Ask",2),(assetBeacon,2)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  , ( Just $ TxOutDatumInline $ toDatum askDatum{loanTerm = 22000}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2
  
  -- Create the Offer UTxO.
  callEndpoint @"create-transaction" h2 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateOffer lenderCred [asset]
              , mintTokens = [("Offer",2),(assetBeacon,2),(lenderToken,2)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum offerDatum
                    , lovelaceValueOf 103_000_000 
                    <> singleton beaconCurrencySymbol "Offer" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol lenderToken 1
                    )
                  , ( Just $ TxOutDatumInline $ toDatum offerDatum{loanTerm = 22000}
                    , lovelaceValueOf 103_000_000 
                    <> singleton beaconCurrencySymbol "Offer" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol lenderToken 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  startTime <- slotToBeginPOSIXTime def <$> waitNSlots 2

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Ask" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            askDatum
  offer1 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 103_000_000 
            <> singleton beaconCurrencySymbol "Offer" 1
            <> singleton beaconCurrencySymbol lenderToken 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            offerDatum
  ask2 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Ask" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            askDatum{loanTerm = 22000}
  offer2 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 103_000_000 
            <> singleton beaconCurrencySymbol "Offer" 1
            <> singleton beaconCurrencySymbol lenderToken 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            offerDatum{loanTerm = 22000}

  let loanIdToken1 = genLoanId offer1
  let loanIdToken2 = genLoanId offer2
  let activeDatum1 = ActiveDatum
        { beaconSym = beaconCurrencySymbol 
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = asset
        , loanPrinciple = 100_000_000
        , rolloverFrequency = Nothing
        , lastCheckpoint = startTime
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , minPayment = 500_000
        , collateralization = [(testToken1, unsafeRatio 1 10_000_000)]
        , collateralIsSwappable = True
        , claimExpiration = startTime + 12000 + 10000
        , loanExpiration = startTime + 12000
        , loanOutstanding = fromInt 100_000_000 .*. (fromInt 1 .+. unsafeRatio 1 10) 
        , loanId = loanIdToken1
        }
  let activeDatum2 = ActiveDatum
        { beaconSym = beaconCurrencySymbol 
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = asset
        , loanPrinciple = 100_000_000
        , rolloverFrequency = Nothing
        , lastCheckpoint = startTime
        , loanTerm = 22000
        , loanInterest = unsafeRatio 1 10
        , minPayment = 500_000
        , collateralization = [(testToken1, unsafeRatio 1 10_000_000)]
        , collateralIsSwappable = True
        , claimExpiration = startTime + 22000 + 10000
        , loanExpiration = startTime + 22000
        , loanOutstanding = fromInt 100_000_000 .*. (fromInt 1 .+. unsafeRatio 1 10) 
        , loanId = loanIdToken2
        }
  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateActive borrowerCred 
                  [ (ask1,offer1)
                  , (ask2,offer2)
                  ]
              , mintTokens = 
                  [ ("Active",2)
                  , (assetBeacon,-2)
                  , (lenderToken,-2)
                  , (borrowerToken,2)
                  , ("Ask",-2)
                  , ("Offer",-2)
                  , (loanIdToken1,2)
                  , (loanIdToken2,2)
                  ]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer AcceptOffer
              , spendFromAddress = loanAddr
              , spendUtxos = 
                  [ ask1
                  , offer1
                  , ask2
                  , offer2
                  ]
              }
          ]
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum activeDatum1
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Active" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol loanIdToken1 1
                    <> singleton beaconCurrencySymbol borrowerToken 1
                    <> uncurry singleton testToken1 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum activeDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Active" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol loanIdToken2 1
                    <> singleton beaconCurrencySymbol borrowerToken 1
                    <> uncurry singleton testToken1 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = lenderAddr
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,"Accepted") 
                    , lovelaceValueOf 3_000_000
                    <> singleton beaconCurrencySymbol loanIdToken1 1
                    )
                  , ( Just $ TxOutDatumInline
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,"Accepted") 
                    , lovelaceValueOf 3_000_000
                    <> singleton beaconCurrencySymbol loanIdToken2 1
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = refScriptAddress
              , outputUtxos =
                  [ ( Just $ TxOutDatumHash $ toDatum ()
                    , lovelaceValueOf 20_000_000
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval (Just startTime) Nothing
      }

  void $ waitNSlots 2

  active1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Active" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            <> singleton beaconCurrencySymbol loanIdToken1 1
            <> singleton beaconCurrencySymbol borrowerToken 1
            <> uncurry singleton testToken1 10
            )
            activeDatum1

  active2 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Active" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            <> singleton beaconCurrencySymbol loanIdToken2 1
            <> singleton beaconCurrencySymbol borrowerToken 1
            <> uncurry singleton testToken1 10
            )
            activeDatum2

  let newActiveDatum1 = activeDatum1
        { loanOutstanding = loanOutstanding activeDatum1 .-. fromInt 55_000_000
        }

  let newActiveDatum2 = activeDatum2
        { loanOutstanding = loanOutstanding activeDatum2 .-. fromInt 55_000_000
        }
        
  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
              , mintTokens = [(borrowerToken,0)]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer MakePayment
              , spendFromAddress = loanAddr
              , spendUtxos = [ active1, active2 ]
              }
          ]
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum newActiveDatum1
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Active" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol loanIdToken1 1
                    <> singleton beaconCurrencySymbol borrowerToken 1
                    <> uncurry singleton testToken1 5
                    )
                  , ( Just $ TxOutDatumInline $ toDatum newActiveDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Active" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol loanIdToken2 1
                    <> singleton beaconCurrencySymbol borrowerToken 1
                    <> uncurry singleton testToken1 5
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = lenderAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,loanIdToken1)
                    , lovelaceValueOf 55_000_000 
                    )
                  , ( Just $ TxOutDatumInline 
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,loanIdToken2)
                    , lovelaceValueOf 55_000_000 
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing (Just $ loanExpiration activeDatum1)
      }

  endTime <- slotToBeginPOSIXTime def <$> waitNSlots 40

  post1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Active" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            <> singleton beaconCurrencySymbol loanIdToken1 1
            <> singleton beaconCurrencySymbol borrowerToken 1
            <> uncurry singleton testToken1 5
            )
            newActiveDatum1
  post2 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Active" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            <> singleton beaconCurrencySymbol loanIdToken2 1
            <> singleton beaconCurrencySymbol borrowerToken 1
            <> uncurry singleton testToken1 5
            )
            newActiveDatum2

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
              , mintTokens = 
                  [ (borrowerToken,-2)
                  , ("Active",-2)
                  , (assetBeacon,-1)
                  , (loanIdToken1,-1)
                  , (loanIdToken2,-1)
                  ]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer Unlock
              , spendFromAddress = loanAddr
              , spendUtxos = [ post1,post2 ]
              }
          ]
      , outputs = [ ]
      , validityRange = ValidityInterval (Just endTime) Nothing
      }
      
  where
    borrowerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
    lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2

    borrowerToken = credentialAsToken borrowerCred
    lenderToken = credentialAsToken lenderCred
    
    asset = (adaSymbol,adaToken)
    assetBeacon = genAssetBeaconName asset
    
    askDatum = AskDatum
      { beaconSym = beaconCurrencySymbol
      , borrowerId = borrowerToken
      , loanAsset = asset
      , loanPrinciple = 100_000_000
      , loanTerm = 12000
      , collateral = [testToken1]
      }

    lenderAddr = Address lenderCred Nothing

    offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = asset
        , loanPrinciple = 100_000_000
        , rolloverFrequency = Nothing
        , minPayment = 500_000
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 10_000_000)]
        , claimPeriod = 10000
        , offerDeposit = 3_000_000
        , collateralIsSwappable = True
        }
    
    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | When unlocking multiple expired collateral UTxOs, not all LoanIDs burned.
failureTest9 :: EmulatorTrace ()
failureTest9 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  
  (mintRef,spendRef) <- initializeScripts

  -- Create the Ask UTxO.
  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset]
              , mintTokens = [("Ask",2),(assetBeacon,2)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  , ( Just $ TxOutDatumInline $ toDatum askDatum{loanTerm = 22000}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2
  
  -- Create the Offer UTxO.
  callEndpoint @"create-transaction" h2 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateOffer lenderCred [asset]
              , mintTokens = [("Offer",2),(assetBeacon,2),(lenderToken,2)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum offerDatum
                    , lovelaceValueOf 103_000_000 
                    <> singleton beaconCurrencySymbol "Offer" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol lenderToken 1
                    )
                  , ( Just $ TxOutDatumInline $ toDatum offerDatum{loanTerm = 22000}
                    , lovelaceValueOf 103_000_000 
                    <> singleton beaconCurrencySymbol "Offer" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol lenderToken 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  startTime <- slotToBeginPOSIXTime def <$> waitNSlots 2

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Ask" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            askDatum
  offer1 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 103_000_000 
            <> singleton beaconCurrencySymbol "Offer" 1
            <> singleton beaconCurrencySymbol lenderToken 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            offerDatum
  ask2 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Ask" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            askDatum{loanTerm = 22000}
  offer2 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 103_000_000 
            <> singleton beaconCurrencySymbol "Offer" 1
            <> singleton beaconCurrencySymbol lenderToken 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            offerDatum{loanTerm = 22000}

  let loanIdToken1 = genLoanId offer1
  let loanIdToken2 = genLoanId offer2
  let activeDatum1 = ActiveDatum
        { beaconSym = beaconCurrencySymbol 
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = asset
        , loanPrinciple = 100_000_000
        , rolloverFrequency = Nothing
        , lastCheckpoint = startTime
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , minPayment = 500_000
        , collateralization = [(testToken1, unsafeRatio 1 10_000_000)]
        , collateralIsSwappable = True
        , claimExpiration = startTime + 12000 + 10000
        , loanExpiration = startTime + 12000
        , loanOutstanding = fromInt 100_000_000 .*. (fromInt 1 .+. unsafeRatio 1 10) 
        , loanId = loanIdToken1
        }
  let activeDatum2 = ActiveDatum
        { beaconSym = beaconCurrencySymbol 
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = asset
        , loanPrinciple = 100_000_000
        , rolloverFrequency = Nothing
        , lastCheckpoint = startTime
        , loanTerm = 22000
        , loanInterest = unsafeRatio 1 10
        , minPayment = 500_000
        , collateralization = [(testToken1, unsafeRatio 1 10_000_000)]
        , collateralIsSwappable = True
        , claimExpiration = startTime + 22000 + 10000
        , loanExpiration = startTime + 22000
        , loanOutstanding = fromInt 100_000_000 .*. (fromInt 1 .+. unsafeRatio 1 10) 
        , loanId = loanIdToken2
        }
  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateActive borrowerCred 
                  [ (ask1,offer1)
                  , (ask2,offer2)
                  ]
              , mintTokens = 
                  [ ("Active",2)
                  , (assetBeacon,-2)
                  , (lenderToken,-2)
                  , (borrowerToken,2)
                  , ("Ask",-2)
                  , ("Offer",-2)
                  , (loanIdToken1,2)
                  , (loanIdToken2,2)
                  ]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer AcceptOffer
              , spendFromAddress = loanAddr
              , spendUtxos = 
                  [ ask1
                  , offer1
                  , ask2
                  , offer2
                  ]
              }
          ]
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum activeDatum1
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Active" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol loanIdToken1 1
                    <> singleton beaconCurrencySymbol borrowerToken 1
                    <> uncurry singleton testToken1 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum activeDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Active" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol loanIdToken2 1
                    <> singleton beaconCurrencySymbol borrowerToken 1
                    <> uncurry singleton testToken1 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = lenderAddr
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,"Accepted") 
                    , lovelaceValueOf 3_000_000
                    <> singleton beaconCurrencySymbol loanIdToken1 1
                    )
                  , ( Just $ TxOutDatumInline
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,"Accepted") 
                    , lovelaceValueOf 3_000_000
                    <> singleton beaconCurrencySymbol loanIdToken2 1
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = refScriptAddress
              , outputUtxos =
                  [ ( Just $ TxOutDatumHash $ toDatum ()
                    , lovelaceValueOf 20_000_000
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval (Just startTime) Nothing
      }

  void $ waitNSlots 2

  active1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Active" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            <> singleton beaconCurrencySymbol loanIdToken1 1
            <> singleton beaconCurrencySymbol borrowerToken 1
            <> uncurry singleton testToken1 10
            )
            activeDatum1

  active2 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Active" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            <> singleton beaconCurrencySymbol loanIdToken2 1
            <> singleton beaconCurrencySymbol borrowerToken 1
            <> uncurry singleton testToken1 10
            )
            activeDatum2

  let newActiveDatum1 = activeDatum1
        { loanOutstanding = loanOutstanding activeDatum1 .-. fromInt 55_000_000
        }

  let newActiveDatum2 = activeDatum2
        { loanOutstanding = loanOutstanding activeDatum2 .-. fromInt 55_000_000
        }
        
  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
              , mintTokens = [(borrowerToken,0)]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer MakePayment
              , spendFromAddress = loanAddr
              , spendUtxos = [ active1, active2 ]
              }
          ]
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum newActiveDatum1
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Active" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol loanIdToken1 1
                    <> singleton beaconCurrencySymbol borrowerToken 1
                    <> uncurry singleton testToken1 5
                    )
                  , ( Just $ TxOutDatumInline $ toDatum newActiveDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Active" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol loanIdToken2 1
                    <> singleton beaconCurrencySymbol borrowerToken 1
                    <> uncurry singleton testToken1 5
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = lenderAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,loanIdToken1)
                    , lovelaceValueOf 55_000_000 
                    )
                  , ( Just $ TxOutDatumInline 
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,loanIdToken2)
                    , lovelaceValueOf 55_000_000 
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing (Just $ loanExpiration activeDatum1)
      }

  endTime <- slotToBeginPOSIXTime def <$> waitNSlots 40

  post1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Active" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            <> singleton beaconCurrencySymbol loanIdToken1 1
            <> singleton beaconCurrencySymbol borrowerToken 1
            <> uncurry singleton testToken1 5
            )
            newActiveDatum1
  post2 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Active" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            <> singleton beaconCurrencySymbol loanIdToken2 1
            <> singleton beaconCurrencySymbol borrowerToken 1
            <> uncurry singleton testToken1 5
            )
            newActiveDatum2

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
              , mintTokens = 
                  [ (borrowerToken,-2)
                  , ("Active",-2)
                  , (assetBeacon,-2)
                  , (loanIdToken2,-1)
                  ]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer Unlock
              , spendFromAddress = loanAddr
              , spendUtxos = [ post1,post2 ]
              }
          ]
      , outputs = [ ]
      , validityRange = ValidityInterval (Just endTime) Nothing
      }
      
  where
    borrowerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
    lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2

    borrowerToken = credentialAsToken borrowerCred
    lenderToken = credentialAsToken lenderCred
    
    asset = (adaSymbol,adaToken)
    assetBeacon = genAssetBeaconName asset
    
    askDatum = AskDatum
      { beaconSym = beaconCurrencySymbol
      , borrowerId = borrowerToken
      , loanAsset = asset
      , loanPrinciple = 100_000_000
      , loanTerm = 12000
      , collateral = [testToken1]
      }

    lenderAddr = Address lenderCred Nothing

    offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = asset
        , loanPrinciple = 100_000_000
        , rolloverFrequency = Nothing
        , minPayment = 500_000
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 10_000_000)]
        , claimPeriod = 10000
        , offerDeposit = 3_000_000
        , collateralIsSwappable = True
        }
    
    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-------------------------------------------------
-- Test Function
-------------------------------------------------
-- | A `TestTree` containing all `Unlock` scenarios.
tests :: TestTree
tests = do
  let opts = defaultCheckOptions & emulatorConfig .~ emConfig
  testGroup "Unlock UTxO(s)"
    [ -- Success Tests (Regression Tests)
      checkPredicateOptions opts "regressionTest1"
        assertNoFailedTransactions regressionTest1
    , checkPredicateOptions opts "regressionTest2"
        assertNoFailedTransactions regressionTest2
    , checkPredicateOptions opts "regressionTest3"
        assertNoFailedTransactions regressionTest3
    , checkPredicateOptions opts "regressionTest4"
        assertNoFailedTransactions regressionTest4

      -- Failure Tests
    , checkPredicateOptions opts "failureTest1"
        (assertEvaluationError "Datum is not an ActiveDatum") failureTest1
    , checkPredicateOptions opts "failureTest2"
        (assertEvaluationError "Borrower did not approve") failureTest2
    , checkPredicateOptions opts "failureTest3"
        (assertEvaluationError "Claim period has not passed") failureTest3
    , checkPredicateOptions opts "failureTest4"
        (assertEvaluationError "invalid-before not specified") failureTest4
    , checkPredicateOptions opts "failureTest5"
        (assertEvaluationError "Not all beacons burned") failureTest5
    , checkPredicateOptions opts "failureTest6"
        (assertEvaluationError "Not all beacons burned") failureTest6
    , checkPredicateOptions opts "failureTest7"
        (assertEvaluationError "Not all beacons burned") failureTest7
    , checkPredicateOptions opts "failureTest8"
        (assertEvaluationError "Not all beacons burned") failureTest8
    , checkPredicateOptions opts "failureTest9"
        (assertEvaluationError "Not all beacons burned") failureTest9

      -- Edge Cases
    ]

testTrace :: IO ()
testTrace = runEmulatorTraceIO' def emConfig failureTest7
