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
    
    -- ** Benchmark Tests
  , benchTest1
  , benchTest2
  , benchTest3
  , benchTest4
  , benchTest5
  , benchTest6

    -- * Full test function
  , tests
  ) where

import PlutusTx.Prelude (unsafeRatio)
import Plutus.Trace
import Wallet.Emulator.Wallet
import Plutus.Contract.Test as Test
import Test.Tasty
import Control.Monad (zipWithM_)
import Data.List (zip4)
import Data.String (fromString)
import Cardano.Node.Emulator.TimeSlot (slotToEndPOSIXTime)

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

    borrowerToken = credentialAsToken False borrowerCred
    lenderToken = credentialAsToken True lenderCred
    
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

    borrowerToken = credentialAsToken False borrowerCred
    lenderToken = credentialAsToken True lenderCred
    
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

    borrowerToken = credentialAsToken False borrowerCred
    lenderToken = credentialAsToken True lenderCred
    
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

    borrowerToken = credentialAsToken False borrowerCred
    lenderToken = credentialAsToken True lenderCred
    
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

    borrowerToken = credentialAsToken False borrowerCred
    lenderToken = credentialAsToken True lenderCred
    
    asset = (adaSymbol,adaToken)
    assetBeacon = genAssetBeaconName asset
    askDatum = AskDatum
      { beaconSym = beaconCurrencySymbol
      , borrowerId = credentialAsToken False borrowerCred
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

    borrowerToken = credentialAsToken False borrowerCred
    lenderToken = credentialAsToken True lenderCred
    
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

    borrowerToken = credentialAsToken False borrowerCred
    lenderToken = credentialAsToken True lenderCred
    
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

    borrowerToken = credentialAsToken False borrowerCred
    lenderToken = credentialAsToken True lenderCred
    
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

    borrowerToken = credentialAsToken False borrowerCred
    lenderToken = credentialAsToken True lenderCred
    
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

    borrowerToken = credentialAsToken False borrowerCred
    lenderToken = credentialAsToken True lenderCred
    
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

    borrowerToken = credentialAsToken False borrowerCred
    lenderToken = credentialAsToken True lenderCred
    
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

    borrowerToken = credentialAsToken False borrowerCred
    lenderToken = credentialAsToken True lenderCred
    
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

    borrowerToken = credentialAsToken False borrowerCred
    lenderToken = credentialAsToken True lenderCred
    
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
-- Bench Tests
-------------------------------------------------
-- | Unlock multiple lost collateral in a single transaction. All UTxOs use one asset for 
-- collateral. Each UTxO is for the same loan asset.
benchTest1 :: Int -> EmulatorTrace ()
benchTest1 numberUnlocked = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  ( mintRef,spendRef ) <- initializeScripts

  let sampleAskOutputs =
        map (\i ->
               ( Just $ TxOutDatumInline 
                      $ toDatum askDatum
               , lovelaceValueOf (3_000_000 + i)
               <> singleton beaconCurrencySymbol "Ask" 1
               <> singleton beaconCurrencySymbol assetBeacon 1
               )  
            )
            [1..20]
  let sampleAskMints =
        TokenMint
          { mintWitness = 
              ( beaconMintingPolicy
              , Just (refScriptAddress, mintRef)
              )
          , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset] 
          , mintTokens = [("Ask",20),(assetBeacon,20)]
          }

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = [ sampleAskMints ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = sampleAskOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }
  
  void $ waitNSlots 2

  let sampleOfferOutputs =
        map (\i ->
               ( Just $ TxOutDatumInline 
                      $ toDatum offerDatum{offerDeposit = 3_000_000 + i}
               , lovelaceValueOf (103_000_000 + i)
               <> singleton beaconCurrencySymbol "Offer" 1
               <> singleton beaconCurrencySymbol assetBeacon 1
               <> singleton beaconCurrencySymbol lenderToken 1
               )  
            )
            [1..20]
  let sampleOfferMints =
        TokenMint
          { mintWitness = 
              ( beaconMintingPolicy
              , Just (refScriptAddress, mintRef)
              )
          , mintRedeemer = toRedeemer $ CreateOffer lenderCred [asset] 
          , mintTokens = 
              [ ("Offer",20)
              , (assetBeacon,20)
              , (lenderToken,20)
              ]
          }

  callEndpoint @"create-transaction" h2 $
    CreateTransactionParams
      { tokens = [ sampleOfferMints ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = sampleOfferOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  targetAsks <- mapM txOutRefWithValue $ map snd sampleAskOutputs
  targetOffers <- mapM txOutRefWithValue $ map snd sampleOfferOutputs

  zipWithM_ 
    (\asks (n,offers) -> do
        startTime <- slotToBeginPOSIXTime def <$> waitNSlots 2

        let loanIds = map genLoanId offers
        let activeDatum = ActiveDatum
              { beaconSym = beaconCurrencySymbol 
              , borrowerId = borrowerToken
              , lenderAddress = lenderAddr
              , loanAsset = asset
              , loanPrinciple = 100_000_000
              , rolloverFrequency = rolloverFrequency offerDatum
              , lastCheckpoint = startTime
              , loanTerm = loanTerm offerDatum
              , loanInterest = unsafeRatio 1 10
              , minPayment = 500_000
              , collateralization = [(testToken1, unsafeRatio 1 10_000_000)]
              , collateralIsSwappable = True
              , claimExpiration = startTime + loanTerm offerDatum + claimPeriod offerDatum
              , loanExpiration = startTime + loanTerm offerDatum
              , loanOutstanding = fromInt 100_000_000 .*. (fromInt 1 .+. unsafeRatio 1 10) 
              , loanId = "" -- This will get replaced.
              }

        let sampleBurn =
              TokenMint
                { mintWitness = 
                    ( beaconMintingPolicy
                    , Just (refScriptAddress, mintRef)
                    )
                , mintRedeemer = 
                    toRedeemer $ CreateActive borrowerCred $ zip asks offers 
                , mintTokens = zip loanIds (repeat 2) <>
                    [ ("Ask",fromIntegral (-groupSize))
                    , (assetBeacon,fromIntegral (-groupSize))
                    , ("Offer",fromIntegral (-groupSize))
                    , ("Active",fromIntegral groupSize)
                    , (lenderToken, fromIntegral (-groupSize))
                    , (borrowerToken, fromIntegral groupSize)
                    ] 
                }

        let sampleCollateral =  
              map (\i ->
                     ( Just $ TxOutDatumInline 
                            $ toDatum activeDatum{loanId = loanIds!!(i-1)}
                     , lovelaceValueOf 3_000_000
                     <> singleton beaconCurrencySymbol "Active" 1
                     <> singleton beaconCurrencySymbol assetBeacon 1
                     <> singleton beaconCurrencySymbol borrowerToken 1
                     <> singleton beaconCurrencySymbol (loanIds!!(i-1)) 1
                     <> uncurry singleton testToken1 10
                     )  
                  )
                  [1..groupSize]

        let samplePayments =  
              map (\i ->
                     ( Just $ TxOutDatumInline 
                            $ toDatum 
                            $ PaymentDatum (beaconCurrencySymbol, "Accepted")
                     , lovelaceValueOf (3_000_000 + fromIntegral (i + groupSize * n))
                     <> singleton beaconCurrencySymbol (loanIds!!(i-1)) 1
                     )  
                  )
                  [1..groupSize]

        callEndpoint @"create-transaction" h1 $
          CreateTransactionParams
            { tokens = [ sampleBurn ]
            , inputs = 
                [ ScriptUtxoInput
                    { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
                    , spendRedeemer = toRedeemer AcceptOffer
                    , spendFromAddress = loanAddr
                    , spendUtxos = asks <> offers
                    }
                ]
            , outputs =
                [ UtxoOutput
                    { toAddress = loanAddr
                    , outputUtxos = sampleCollateral
                    }
                , UtxoOutput
                    { toAddress = lenderAddr
                    , outputUtxos = samplePayments
                    }
                ]
            , validityRange = ValidityInterval (Just startTime) Nothing
            }
      )
      (grouped groupSize targetAsks)
      (zip [0..] $ grouped groupSize targetOffers)

  claimTime <- slotToBeginPOSIXTime def <$> waitNSlots 60

  let loanIds = map genLoanId targetOffers

  targetLoans <- 
    mapM (\v -> txOutRefAndDatumWithAssetAtAddress beaconCurrencySymbol v loanAddr) loanIds
  
  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
              , mintTokens = mconcat
                  [ take numberUnlocked $ zip loanIds (repeat (-1))
                  , [ (borrowerToken,fromIntegral (-numberUnlocked))
                    , ("Active",fromIntegral (-numberUnlocked))
                    , (assetBeacon,fromIntegral (-numberUnlocked))
                    ]
                  ]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer Unlock
              , spendFromAddress = loanAddr
              , spendUtxos = take numberUnlocked $ map fst targetLoans
              }
          ]
      , outputs = [ ]
      , validityRange = ValidityInterval (Just claimTime) Nothing
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

    lenderToken = credentialAsToken True lenderCred

    borrowerToken = credentialAsToken False borrowerCred

    asset = (adaSymbol,adaToken)

    assetBeacon = genAssetBeaconName asset

    lenderAddr = Address lenderCred Nothing

    askDatum = AskDatum
      { beaconSym = beaconCurrencySymbol
      , borrowerId = borrowerToken
      , loanAsset = asset
      , loanPrinciple = 100_000_000
      , loanTerm = 20000
      , collateral = [testToken1]
      }

    offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = Address lenderCred Nothing
        , loanAsset = asset
        , loanPrinciple = 100_000_000
        , rolloverFrequency = Just 4000
        , minPayment = 500_000
        , loanTerm = 20000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 10_000_000)]
        , claimPeriod = 10000
        , offerDeposit = 3_000_000
        , collateralIsSwappable = True
        }
      
    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

    groupSize :: Int
    groupSize = 4

    newAddr = Address (ScriptCredential proxyValidatorHash) (Just $ StakingHash lenderCred)

-- | Unlock multiple lost collateral in a single transaction. All UTxOs use three assets for
-- collateral. Each UTxO is for the same loan asset.
benchTest2 :: Int -> EmulatorTrace ()
benchTest2 numberUnlocked = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  ( mintRef,spendRef ) <- initializeScripts

  let sampleAskOutputs =
        map (\i ->
               ( Just $ TxOutDatumInline 
                      $ toDatum askDatum
               , lovelaceValueOf (3_000_000 + i)
               <> singleton beaconCurrencySymbol "Ask" 1
               <> singleton beaconCurrencySymbol assetBeacon 1
               )  
            )
            [1..20]
  let sampleAskMints =
        TokenMint
          { mintWitness = 
              ( beaconMintingPolicy
              , Just (refScriptAddress, mintRef)
              )
          , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset] 
          , mintTokens = [("Ask",20),(assetBeacon,20)]
          }

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = [ sampleAskMints ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = sampleAskOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }
  
  void $ waitNSlots 2

  let sampleOfferOutputs =
        map (\i ->
               ( Just $ TxOutDatumInline 
                      $ toDatum offerDatum{offerDeposit = 3_000_000 + i}
               , lovelaceValueOf (103_000_000 + i)
               <> singleton beaconCurrencySymbol "Offer" 1
               <> singleton beaconCurrencySymbol assetBeacon 1
               <> singleton beaconCurrencySymbol lenderToken 1
               )  
            )
            [1..20]
  let sampleOfferMints =
        TokenMint
          { mintWitness = 
              ( beaconMintingPolicy
              , Just (refScriptAddress, mintRef)
              )
          , mintRedeemer = toRedeemer $ CreateOffer lenderCred [asset] 
          , mintTokens = 
              [ ("Offer",20)
              , (assetBeacon,20)
              , (lenderToken,20)
              ]
          }

  callEndpoint @"create-transaction" h2 $
    CreateTransactionParams
      { tokens = [ sampleOfferMints ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = sampleOfferOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  targetAsks <- mapM txOutRefWithValue $ map snd sampleAskOutputs
  targetOffers <- mapM txOutRefWithValue $ map snd sampleOfferOutputs

  zipWithM_ 
    (\asks (n,offers) -> do
        startTime <- slotToBeginPOSIXTime def <$> waitNSlots 2

        let loanIds = map genLoanId offers
        let activeDatum = ActiveDatum
              { beaconSym = beaconCurrencySymbol 
              , borrowerId = borrowerToken
              , lenderAddress = lenderAddr
              , loanAsset = asset
              , loanPrinciple = 100_000_000
              , rolloverFrequency = rolloverFrequency offerDatum
              , lastCheckpoint = startTime
              , loanTerm = loanTerm offerDatum
              , loanInterest = unsafeRatio 1 10
              , minPayment = 500_000
              , collateralization = collateralization offerDatum
              , collateralIsSwappable = True
              , claimExpiration = startTime + loanTerm offerDatum + claimPeriod offerDatum
              , loanExpiration = startTime + loanTerm offerDatum
              , loanOutstanding = fromInt 100_000_000 .*. (fromInt 1 .+. unsafeRatio 1 10) 
              , loanId = "" -- This will get replaced.
              }

        let sampleBurn =
              TokenMint
                { mintWitness = 
                    ( beaconMintingPolicy
                    , Just (refScriptAddress, mintRef)
                    )
                , mintRedeemer = 
                    toRedeemer $ CreateActive borrowerCred $ zip asks offers 
                , mintTokens = zip loanIds (repeat 2) <>
                    [ ("Ask",fromIntegral (-groupSize))
                    , (assetBeacon,fromIntegral (-groupSize))
                    , ("Offer",fromIntegral (-groupSize))
                    , ("Active",fromIntegral groupSize)
                    , (lenderToken, fromIntegral (-groupSize))
                    , (borrowerToken, fromIntegral groupSize)
                    ] 
                }

        let sampleCollateral =  
              map (\i ->
                     ( Just $ TxOutDatumInline 
                            $ toDatum activeDatum{loanId = loanIds!!(i-1)}
                     , lovelaceValueOf 4_000_000
                     <> singleton beaconCurrencySymbol "Active" 1
                     <> singleton beaconCurrencySymbol assetBeacon 1
                     <> singleton beaconCurrencySymbol borrowerToken 1
                     <> singleton beaconCurrencySymbol (loanIds!!(i-1)) 1
                     <> uncurry singleton testToken1 3
                     <> uncurry singleton testToken2 3
                     <> uncurry singleton testToken3 4
                     )  
                  )
                  [1..groupSize]

        let samplePayments =  
              map (\i ->
                     ( Just $ TxOutDatumInline 
                            $ toDatum 
                            $ PaymentDatum (beaconCurrencySymbol, "Accepted")
                     , lovelaceValueOf (3_000_000 + fromIntegral (i + groupSize * n))
                     <> singleton beaconCurrencySymbol (loanIds!!(i-1)) 1
                     )  
                  )
                  [1..groupSize]

        callEndpoint @"create-transaction" h1 $
          CreateTransactionParams
            { tokens = [ sampleBurn ]
            , inputs = 
                [ ScriptUtxoInput
                    { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
                    , spendRedeemer = toRedeemer AcceptOffer
                    , spendFromAddress = loanAddr
                    , spendUtxos = asks <> offers
                    }
                ]
            , outputs =
                [ UtxoOutput
                    { toAddress = loanAddr
                    , outputUtxos = sampleCollateral
                    }
                , UtxoOutput
                    { toAddress = lenderAddr
                    , outputUtxos = samplePayments
                    }
                ]
            , validityRange = ValidityInterval (Just startTime) Nothing
            }
      )
      (grouped groupSize targetAsks)
      (zip [0..] $ grouped groupSize targetOffers)

  claimTime <- slotToBeginPOSIXTime def <$> waitNSlots 60

  let loanIds = map genLoanId targetOffers

  targetLoans <- 
    mapM (\v -> txOutRefAndDatumWithAssetAtAddress beaconCurrencySymbol v loanAddr) loanIds
  
  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
              , mintTokens = mconcat
                  [ take numberUnlocked $ zip loanIds (repeat (-1))
                  , [ (borrowerToken,fromIntegral (-numberUnlocked))
                    , ("Active",fromIntegral (-numberUnlocked))
                    , (assetBeacon,fromIntegral (-numberUnlocked))
                    ]
                  ]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer Unlock
              , spendFromAddress = loanAddr
              , spendUtxos = take numberUnlocked $ map fst targetLoans
              }
          ]
      , outputs = [ ]
      , validityRange = ValidityInterval (Just claimTime) Nothing
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

    lenderToken = credentialAsToken True lenderCred

    borrowerToken = credentialAsToken False borrowerCred

    asset = (adaSymbol,adaToken)

    assetBeacon = genAssetBeaconName asset

    lenderAddr = Address lenderCred Nothing

    askDatum = AskDatum
      { beaconSym = beaconCurrencySymbol
      , borrowerId = borrowerToken
      , loanAsset = asset
      , loanPrinciple = 100_000_000
      , loanTerm = 20000
      , collateral = [testToken1,testToken2,testToken3]
      }

    offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = Address lenderCred Nothing
        , loanAsset = asset
        , loanPrinciple = 100_000_000
        , rolloverFrequency = Just 4000
        , minPayment = 500_000
        , loanTerm = 20000
        , loanInterest = unsafeRatio 1 10
        , collateralization = 
            [ (testToken1,unsafeRatio 1 10_000_000)
            , (testToken2,unsafeRatio 1 10_000_000)
            , (testToken3,unsafeRatio 1 10_000_000)
            ]
        , claimPeriod = 10000
        , offerDeposit = 3_000_000
        , collateralIsSwappable = True
        }
      
    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

    groupSize :: Int
    groupSize = 4

    newAddr = Address (ScriptCredential proxyValidatorHash) (Just $ StakingHash lenderCred)

-- | Unlock multiple lost collateral in a single transaction. All UTxOs use one asset for
-- collateral. Each UTxO is for a different loan asset.
benchTest3 :: Int -> EmulatorTrace ()
benchTest3 numberUnlocked = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints


  ( mintRef,spendRef ) <- initializeScripts

  let assets = map (\i -> (fst testToken1, fromString $ "TestToken" <> show @Int i)) [1..20]
  let beacons = map genAssetBeaconName assets

  let sampleAskOutputs =
        zipWith (\a b ->
                   ( Just $ TxOutDatumInline 
                          $ toDatum askDatum{loanAsset = a}
                   , lovelaceValueOf 3_000_000
                   <> singleton beaconCurrencySymbol "Ask" 1
                   <> singleton beaconCurrencySymbol b 1
                   )  
                )
                assets
                beacons
  let sampleAskMints i =
        TokenMint
          { mintWitness = 
              ( beaconMintingPolicy
              , Just (refScriptAddress, mintRef)
              )
          , mintRedeemer = toRedeemer $ CreateAsk borrowerCred $ take i assets
          , mintTokens = ("Ask",fromIntegral i) : take i (zip beacons $ repeat 1)
          }

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = [ sampleAskMints ( 12 :: Int ) ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = take 12 sampleAskOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }
  
  void $ waitNSlots 2

  let sampleOfferOutputs =
        zipWith (\a b ->
                  ( Just $ TxOutDatumInline 
                         $ toDatum offerDatum{loanAsset = a}
                  , lovelaceValueOf 4_000_000 
                  <> singleton beaconCurrencySymbol "Offer" 1
                  <> singleton beaconCurrencySymbol b 1
                  <> singleton beaconCurrencySymbol lenderToken 1
                  <> uncurry singleton a 10
                  )  
                )
                assets
                beacons
  let sampleOfferMints i =
        TokenMint
          { mintWitness = 
              ( beaconMintingPolicy
              , Just (refScriptAddress, mintRef)
              )
          , mintRedeemer = toRedeemer $ CreateOffer lenderCred $ take i assets 
          , mintTokens = 
              [("Offer",fromIntegral i),(lenderToken,fromIntegral i)] 
              <> take i (zip beacons $ repeat 1 )
          }

  callEndpoint @"create-transaction" h2 $
    CreateTransactionParams
      { tokens = [ sampleOfferMints (12 :: Int) ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = take 12 sampleOfferOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }
  
  void $ waitNSlots 2

  targetAsks <- mapM txOutRefWithValue $ take 12 $ map snd sampleAskOutputs
  targetOffers <- mapM txOutRefWithValue $ take 12 $ map snd sampleOfferOutputs
  let pairings = zip4 
        ( grouped groupSize targetAsks ) 
        ( grouped groupSize targetOffers ) 
        ( grouped groupSize assets ) 
        ( grouped groupSize beacons )

  mapM_
    (\(asks,offers,as,bs) -> do
      startTime <- slotToBeginPOSIXTime def <$> waitNSlots 2

      let loanIds = map genLoanId offers
      let activeDatum = ActiveDatum
            { beaconSym = beaconCurrencySymbol 
            , borrowerId = borrowerToken
            , lenderAddress = lenderAddr
            , loanAsset = testToken1
            , loanPrinciple = 10
            , rolloverFrequency = rolloverFrequency offerDatum
            , lastCheckpoint = startTime
            , loanTerm = loanTerm offerDatum
            , loanInterest = unsafeRatio 1 10
            , minPayment = 2
            , collateralization = collateralization offerDatum
            , collateralIsSwappable = True
            , claimExpiration = startTime + loanTerm offerDatum + claimPeriod offerDatum
            , loanExpiration = startTime + loanTerm offerDatum
            , loanOutstanding = fromInt 10 .*. (fromInt 1 .+. unsafeRatio 1 10) 
            , loanId = "" -- This will get replaced.
            }

      let sampleBurn =
            TokenMint
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = 
                  toRedeemer $ CreateActive borrowerCred $ zip asks offers 
              , mintTokens = mconcat 
                  [ zip loanIds (repeat 2)
                  , zip bs (repeat (-1))
                  , [ ("Ask",fromIntegral (-groupSize))
                    , ("Offer",fromIntegral (-groupSize))
                    , ("Active",fromIntegral groupSize)
                    , (lenderToken, fromIntegral (-groupSize))
                    , (borrowerToken, fromIntegral groupSize)
                    ]
                  ]
              }

      let sampleCollateral =  
            map (\i ->
                   ( Just $ TxOutDatumInline 
                          $ toDatum activeDatum{loanAsset = as!!i, loanId = loanIds!!i}
                   , lovelaceValueOf 4_000_000
                   <> singleton beaconCurrencySymbol "Active" 1
                   <> singleton beaconCurrencySymbol (bs!!i) 1
                   <> singleton beaconCurrencySymbol borrowerToken 1
                   <> singleton beaconCurrencySymbol (loanIds!!i) 1
                   <> uncurry singleton testToken15 10
                   )  
                )
                [0..groupSize-1]

      let samplePayments =  
            map (\i ->
                   ( Just $ TxOutDatumInline 
                          $ toDatum 
                          $ PaymentDatum (beaconCurrencySymbol, "Accepted")
                   , lovelaceValueOf 4_000_000
                   <> singleton beaconCurrencySymbol (loanIds!!i) 1
                   )  
                )
                [0..groupSize-1]

      callEndpoint @"create-transaction" h1 $
        CreateTransactionParams
          { tokens = [ sampleBurn ]
          , inputs = 
              [ ScriptUtxoInput
                  { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
                  , spendRedeemer = toRedeemer AcceptOffer
                  , spendFromAddress = loanAddr
                  , spendUtxos = asks <> offers
                  }
              ]
          , outputs =
              [ UtxoOutput
                  { toAddress = loanAddr
                  , outputUtxos = sampleCollateral
                  }
              , UtxoOutput
                  { toAddress = lenderAddr
                  , outputUtxos = samplePayments
                  }
              ]
          , validityRange = ValidityInterval (Just startTime) Nothing
          }
    )
    pairings

  claimTime <- slotToBeginPOSIXTime def <$> waitNSlots 60

  let loanIds = map genLoanId targetOffers

  targetLoans <- 
    mapM (\v -> txOutRefAndDatumWithAssetAtAddress beaconCurrencySymbol v loanAddr) loanIds
  
  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
              , mintTokens = mconcat
                  [ take numberUnlocked $ zip loanIds (repeat (-1))
                  , take numberUnlocked $ zip beacons (repeat (-1))
                  , [ (borrowerToken,fromIntegral (-numberUnlocked))
                    , ("Active",fromIntegral (-numberUnlocked))
                    ]
                  ]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer Unlock
              , spendFromAddress = loanAddr
              , spendUtxos = take numberUnlocked $ map fst targetLoans
              }
          ]
      , outputs = [ ]
      , validityRange = ValidityInterval (Just claimTime) Nothing
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

    lenderToken = credentialAsToken True lenderCred

    borrowerToken = credentialAsToken False borrowerCred

    lenderAddr = Address lenderCred Nothing

    askDatum = AskDatum
      { beaconSym = beaconCurrencySymbol
      , borrowerId = borrowerToken
      , loanAsset = testToken1 -- This will be replaced.
      , loanPrinciple = 10
      , loanTerm = 12000
      , collateral = [testToken15]
      }

    offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = Address lenderCred Nothing
        , loanAsset = testToken1 -- This will be replaced.
        , loanPrinciple = 10
        , rolloverFrequency = Nothing
        , minPayment = 2
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , collateralization = 
            [ (testToken15,unsafeRatio 1 1)
            ]
        , claimPeriod = 10000
        , offerDeposit = 4_000_000
        , collateralIsSwappable = True
        }
      
    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

    groupSize :: Int
    groupSize = 4

-- | Unlock multiple lost collateral in a single transaction. All UTxOs use three assets for
-- collateral. Each UTxO is for a different loan asset.
benchTest4 :: Int -> EmulatorTrace ()
benchTest4 numberUnlocked = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints


  ( mintRef,spendRef ) <- initializeScripts

  let assets = map (\i -> (fst testToken1, fromString $ "TestToken" <> show @Int i)) [1..20]
  let beacons = map genAssetBeaconName assets

  let sampleAskOutputs =
        zipWith (\a b ->
                   ( Just $ TxOutDatumInline 
                          $ toDatum askDatum{loanAsset = a}
                   , lovelaceValueOf 3_000_000
                   <> singleton beaconCurrencySymbol "Ask" 1
                   <> singleton beaconCurrencySymbol b 1
                   )  
                )
                assets
                beacons
  let sampleAskMints i =
        TokenMint
          { mintWitness = 
              ( beaconMintingPolicy
              , Just (refScriptAddress, mintRef)
              )
          , mintRedeemer = toRedeemer $ CreateAsk borrowerCred $ take i assets
          , mintTokens = ("Ask",fromIntegral i) : take i (zip beacons $ repeat 1)
          }

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = [ sampleAskMints ( 12 :: Int ) ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = take 12 sampleAskOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }
  
  void $ waitNSlots 2

  let sampleOfferOutputs =
        zipWith (\a b ->
                  ( Just $ TxOutDatumInline 
                         $ toDatum offerDatum{loanAsset = a}
                  , lovelaceValueOf 4_000_000 
                  <> singleton beaconCurrencySymbol "Offer" 1
                  <> singleton beaconCurrencySymbol b 1
                  <> singleton beaconCurrencySymbol lenderToken 1
                  <> uncurry singleton a 10
                  )  
                )
                assets
                beacons
  let sampleOfferMints i =
        TokenMint
          { mintWitness = 
              ( beaconMintingPolicy
              , Just (refScriptAddress, mintRef)
              )
          , mintRedeemer = toRedeemer $ CreateOffer lenderCred $ take i assets 
          , mintTokens = 
              [("Offer",fromIntegral i),(lenderToken,fromIntegral i)] 
              <> take i (zip beacons $ repeat 1 )
          }

  callEndpoint @"create-transaction" h2 $
    CreateTransactionParams
      { tokens = [ sampleOfferMints (12 :: Int) ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = take 12 sampleOfferOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }
  
  void $ waitNSlots 2

  targetAsks <- mapM txOutRefWithValue $ take 12 $ map snd sampleAskOutputs
  targetOffers <- mapM txOutRefWithValue $ take 12 $ map snd sampleOfferOutputs
  let pairings = zip4 
        ( grouped groupSize targetAsks ) 
        ( grouped groupSize targetOffers ) 
        ( grouped groupSize assets ) 
        ( grouped groupSize beacons )

  mapM_
    (\(asks,offers,as,bs) -> do
      startTime <- slotToBeginPOSIXTime def <$> waitNSlots 2

      let loanIds = map genLoanId offers
      let activeDatum = ActiveDatum
            { beaconSym = beaconCurrencySymbol 
            , borrowerId = borrowerToken
            , lenderAddress = lenderAddr
            , loanAsset = testToken1
            , loanPrinciple = 10
            , rolloverFrequency = rolloverFrequency offerDatum
            , lastCheckpoint = startTime
            , loanTerm = loanTerm offerDatum
            , loanInterest = unsafeRatio 1 10
            , minPayment = 2
            , collateralization = collateralization offerDatum
            , collateralIsSwappable = True
            , claimExpiration = startTime + loanTerm offerDatum + claimPeriod offerDatum
            , loanExpiration = startTime + loanTerm offerDatum
            , loanOutstanding = fromInt 10 .*. (fromInt 1 .+. unsafeRatio 1 10) 
            , loanId = "" -- This will get replaced.
            }

      let sampleBurn =
            TokenMint
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = 
                  toRedeemer $ CreateActive borrowerCred $ zip asks offers 
              , mintTokens = mconcat 
                  [ zip loanIds (repeat 2)
                  , zip bs (repeat (-1))
                  , [ ("Ask",fromIntegral (-groupSize))
                    , ("Offer",fromIntegral (-groupSize))
                    , ("Active",fromIntegral groupSize)
                    , (lenderToken, fromIntegral (-groupSize))
                    , (borrowerToken, fromIntegral groupSize)
                    ]
                  ]
              }

      let sampleCollateral =  
            map (\i ->
                   ( Just $ TxOutDatumInline 
                          $ toDatum activeDatum{loanAsset = as!!i, loanId = loanIds!!i}
                   , lovelaceValueOf 4_000_000
                   <> singleton beaconCurrencySymbol "Active" 1
                   <> singleton beaconCurrencySymbol (bs!!i) 1
                   <> singleton beaconCurrencySymbol borrowerToken 1
                   <> singleton beaconCurrencySymbol (loanIds!!i) 1
                   <> uncurry singleton testToken15 3
                   <> uncurry singleton testToken16 3
                   <> uncurry singleton testToken17 4
                   )  
                )
                [0..groupSize-1]

      let samplePayments =  
            map (\i ->
                   ( Just $ TxOutDatumInline 
                          $ toDatum 
                          $ PaymentDatum (beaconCurrencySymbol, "Accepted")
                   , lovelaceValueOf 4_000_000
                   <> singleton beaconCurrencySymbol (loanIds!!i) 1
                   )  
                )
                [0..groupSize-1]

      callEndpoint @"create-transaction" h1 $
        CreateTransactionParams
          { tokens = [ sampleBurn ]
          , inputs = 
              [ ScriptUtxoInput
                  { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
                  , spendRedeemer = toRedeemer AcceptOffer
                  , spendFromAddress = loanAddr
                  , spendUtxos = asks <> offers
                  }
              ]
          , outputs =
              [ UtxoOutput
                  { toAddress = loanAddr
                  , outputUtxos = sampleCollateral
                  }
              , UtxoOutput
                  { toAddress = lenderAddr
                  , outputUtxos = samplePayments
                  }
              ]
          , validityRange = ValidityInterval (Just startTime) Nothing
          }
    )
    pairings

  claimTime <- slotToBeginPOSIXTime def <$> waitNSlots 30

  let loanIds = map genLoanId targetOffers

  targetLoans <- 
    mapM (\v -> txOutRefAndDatumWithAssetAtAddress beaconCurrencySymbol v loanAddr) loanIds
  
  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
              , mintTokens = mconcat
                  [ take numberUnlocked $ zip loanIds (repeat (-1))
                  , take numberUnlocked $ zip beacons (repeat (-1))
                  , [ (borrowerToken,fromIntegral (-numberUnlocked))
                    , ("Active",fromIntegral (-numberUnlocked))
                    ]
                  ]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer Unlock
              , spendFromAddress = loanAddr
              , spendUtxos = take numberUnlocked $ map fst targetLoans
              }
          ]
      , outputs = [ ]
      , validityRange = ValidityInterval (Just claimTime) Nothing
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

    lenderToken = credentialAsToken True lenderCred

    borrowerToken = credentialAsToken False borrowerCred

    lenderAddr = Address lenderCred Nothing

    askDatum = AskDatum
      { beaconSym = beaconCurrencySymbol
      , borrowerId = borrowerToken
      , loanAsset = testToken1 -- This will be replaced.
      , loanPrinciple = 10
      , loanTerm = 12000
      , collateral = [testToken15,testToken16,testToken17]
      }

    offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = Address lenderCred Nothing
        , loanAsset = testToken1 -- This will be replaced.
        , loanPrinciple = 10
        , rolloverFrequency = Nothing
        , minPayment = 2
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , collateralization = 
            [ (testToken15,unsafeRatio 1 1)
            , (testToken16,unsafeRatio 1 1)
            , (testToken17,unsafeRatio 1 1)
            ]
        , claimPeriod = 10000
        , offerDeposit = 4_000_000
        , collateralIsSwappable = True
        }
      
    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

    groupSize :: Int
    groupSize = 4

-- | Unlock multiple finished loans in a single transaction. All loans are for the same loan 
-- asset. No collateral was present in the finished loan.
benchTest5 :: Int -> EmulatorTrace ()
benchTest5 numberUnlocked = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints


  ( mintRef,spendRef ) <- initializeScripts

  let sampleAskOutputs =
        map (\i ->
               ( Just $ TxOutDatumInline 
                      $ toDatum askDatum
               , lovelaceValueOf (3_000_000 + i)
               <> singleton beaconCurrencySymbol "Ask" 1
               <> singleton beaconCurrencySymbol assetBeacon 1
               )  
            )
            [1..20]
  let sampleAskMints =
        TokenMint
          { mintWitness = 
              ( beaconMintingPolicy
              , Just (refScriptAddress, mintRef)
              )
          , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset] 
          , mintTokens = [("Ask",20),(assetBeacon,20)]
          }

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = [ sampleAskMints ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = sampleAskOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }
  
  void $ waitNSlots 2

  let sampleOfferOutputs =
        map (\i ->
               ( Just $ TxOutDatumInline 
                      $ toDatum offerDatum{offerDeposit = 3_000_000 + i}
               , lovelaceValueOf (103_000_000 + i)
               <> singleton beaconCurrencySymbol "Offer" 1
               <> singleton beaconCurrencySymbol assetBeacon 1
               <> singleton beaconCurrencySymbol lenderToken 1
               )  
            )
            [1..20]
  let sampleOfferMints =
        TokenMint
          { mintWitness = 
              ( beaconMintingPolicy
              , Just (refScriptAddress, mintRef)
              )
          , mintRedeemer = toRedeemer $ CreateOffer lenderCred [asset] 
          , mintTokens = 
              [ ("Offer",20)
              , (assetBeacon,20)
              , (lenderToken,20)
              ]
          }

  callEndpoint @"create-transaction" h2 $
    CreateTransactionParams
      { tokens = [ sampleOfferMints ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = sampleOfferOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  targetAsks <- mapM txOutRefWithValue $ map snd sampleAskOutputs
  targetOffers <- mapM txOutRefWithValue $ map snd sampleOfferOutputs

  zipWithM_ 
    (\asks (n,offers) -> do
        startTime <- slotToBeginPOSIXTime def <$> waitNSlots 2

        let loanIds = map genLoanId offers
        let activeDatum = ActiveDatum
              { beaconSym = beaconCurrencySymbol 
              , borrowerId = borrowerToken
              , lenderAddress = lenderAddr
              , loanAsset = asset
              , loanPrinciple = 100_000_000
              , rolloverFrequency = Nothing
              , lastCheckpoint = startTime
              , loanTerm = loanTerm offerDatum
              , loanInterest = unsafeRatio 1 10
              , minPayment = 500_000
              , collateralization = [(testToken1, unsafeRatio 1 10_000_000)]
              , collateralIsSwappable = True
              , claimExpiration = startTime + loanTerm offerDatum + claimPeriod offerDatum
              , loanExpiration = startTime + loanTerm offerDatum
              , loanOutstanding = fromInt 100_000_000 .*. (fromInt 1 .+. unsafeRatio 1 10) 
              , loanId = "" -- This will get replaced.
              }

        let sampleBurn =
              TokenMint
                { mintWitness = 
                    ( beaconMintingPolicy
                    , Just (refScriptAddress, mintRef)
                    )
                , mintRedeemer = 
                    toRedeemer $ CreateActive borrowerCred $ zip asks offers 
                , mintTokens = zip loanIds (repeat 2) <>
                    [ ("Ask",fromIntegral (-groupSize))
                    , (assetBeacon,fromIntegral (-groupSize))
                    , ("Offer",fromIntegral (-groupSize))
                    , ("Active",fromIntegral groupSize)
                    , (lenderToken, fromIntegral (-groupSize))
                    , (borrowerToken, fromIntegral groupSize)
                    ] 
                }

        let sampleCollateral =  
              map (\i ->
                     ( Just $ TxOutDatumInline 
                            $ toDatum activeDatum{loanId = loanIds!!(i-1)}
                     , lovelaceValueOf 3_000_000
                     <> singleton beaconCurrencySymbol "Active" 1
                     <> singleton beaconCurrencySymbol assetBeacon 1
                     <> singleton beaconCurrencySymbol borrowerToken 1
                     <> singleton beaconCurrencySymbol (loanIds!!(i-1)) 1
                     <> uncurry singleton testToken1 10
                     )  
                  )
                  [1..groupSize]

        let samplePayments =  
              map (\i ->
                     ( Just $ TxOutDatumInline 
                            $ toDatum 
                            $ PaymentDatum (beaconCurrencySymbol, "Accepted")
                     , lovelaceValueOf (3_000_000 + fromIntegral (i + groupSize * n))
                     <> singleton beaconCurrencySymbol (loanIds!!(i-1)) 1
                     )  
                  )
                  [1..groupSize]

        callEndpoint @"create-transaction" h1 $
          CreateTransactionParams
            { tokens = [ sampleBurn ]
            , inputs = 
                [ ScriptUtxoInput
                    { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
                    , spendRedeemer = toRedeemer AcceptOffer
                    , spendFromAddress = loanAddr
                    , spendUtxos = asks <> offers
                    }
                ]
            , outputs =
                [ UtxoOutput
                    { toAddress = loanAddr
                    , outputUtxos = sampleCollateral
                    }
                , UtxoOutput
                    { toAddress = lenderAddr
                    , outputUtxos = samplePayments
                    }
                ]
            , validityRange = ValidityInterval (Just startTime) Nothing
            }
      )
      (grouped groupSize targetAsks)
      (zip [0..] $ grouped groupSize targetOffers)

  void $ waitNSlots 2

  let loanIds = map genLoanId targetOffers

  targetLoans <- 
    mapM (\v -> txOutRefAndDatumWithAssetAtAddress beaconCurrencySymbol v loanAddr) loanIds
  
  mapM_
    (\group -> do
      paymentTime <- (+1000) . slotToEndPOSIXTime def <$> waitNSlots 2
      let newCollateral =
            map (\(_,Just loanDatum) ->
                   ( Just $ TxOutDatumInline 
                          $ toDatum loanDatum{loanOutstanding = fromInt 0}
                   , lovelaceValueOf 3_000_000
                   <> singleton beaconCurrencySymbol "Active" 1
                   <> singleton beaconCurrencySymbol assetBeacon 1
                   <> singleton beaconCurrencySymbol (loanId loanDatum) 1
                   )  
                )
                group

      let newPayment =
            map (\(_,Just loanDatum) ->
                   ( Just $ TxOutDatumInline 
                          $ toDatum 
                          $ PaymentDatum (beaconCurrencySymbol,loanId loanDatum)
                   , lovelaceValueOf 110_000_000
                   )  
                )
                group

      callEndpoint @"create-transaction" h1 $
        CreateTransactionParams
          { tokens = 
            [ TokenMint
                { mintWitness = 
                    ( beaconMintingPolicy
                    , Just (refScriptAddress, mintRef)
                    )
                , mintRedeemer = toRedeemer BurnBeacons
                , mintTokens = [(borrowerToken, fromIntegral (-groupSize))] 
                }
            ]
          , inputs = 
              [ ScriptUtxoInput
                  { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
                  , spendRedeemer = toRedeemer MakePayment
                  , spendFromAddress = loanAddr
                  , spendUtxos = map fst group 
                  }
              ]
          , outputs =
              [ UtxoOutput
                  { toAddress = loanAddr
                  , outputUtxos = newCollateral
                  }
              , UtxoOutput
                  { toAddress = lenderAddr
                  , outputUtxos = newPayment
                  }
              ]
          , validityRange = ValidityInterval Nothing (Just paymentTime)
          }
    )
    (grouped groupSize targetLoans)

  claimTime <- slotToBeginPOSIXTime def <$> waitNSlots 30

  finishedLoans <- txOutRefsAndDatumsAtAddress loanAddr
  
  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
              , mintTokens = mconcat
                  [ take numberUnlocked $ map (\(_,(Just d)) -> (loanId d,-1)) finishedLoans
                  , [ ("Active",fromIntegral (-numberUnlocked))
                    , (assetBeacon, fromIntegral (-numberUnlocked))
                    ]
                  ]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer Unlock
              , spendFromAddress = loanAddr
              , spendUtxos = take numberUnlocked $ map fst finishedLoans
              }
          ]
      , outputs = [ ]
      , validityRange = ValidityInterval (Just claimTime) Nothing
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

    lenderToken = credentialAsToken True lenderCred

    borrowerToken = credentialAsToken False borrowerCred

    asset = (adaSymbol,adaToken)

    assetBeacon = genAssetBeaconName asset

    lenderAddr = Address lenderCred Nothing

    askDatum = AskDatum
      { beaconSym = beaconCurrencySymbol
      , borrowerId = borrowerToken
      , loanAsset = asset
      , loanPrinciple = 100_000_000
      , loanTerm = 20000
      , collateral = [testToken1]
      }

    offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = Address lenderCred Nothing
        , loanAsset = asset
        , loanPrinciple = 100_000_000
        , rolloverFrequency = Nothing
        , minPayment = 500_000
        , loanTerm = 20000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 10_000_000)]
        , claimPeriod = 10000
        , offerDeposit = 3_000_000
        , collateralIsSwappable = True
        }
      
    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

    groupSize :: Int
    groupSize = 4

-- | Unlock multiple finished loans in a single transaction. All loans are for different loan 
-- assets. No collateral was present in the finished loan.
benchTest6 :: Int -> EmulatorTrace ()
benchTest6 numberUnlocked = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints


  ( mintRef,spendRef ) <- initializeScripts

  let assets = map (\i -> (fst testToken1, fromString $ "TestToken" <> show @Int i)) [1..20]
  let beacons = map genAssetBeaconName assets

  let sampleAskOutputs =
        zipWith (\a b ->
                   ( Just $ TxOutDatumInline 
                          $ toDatum askDatum{loanAsset = a}
                   , lovelaceValueOf 3_000_000
                   <> singleton beaconCurrencySymbol "Ask" 1
                   <> singleton beaconCurrencySymbol b 1
                   )  
                )
                assets
                beacons
  let sampleAskMints i =
        TokenMint
          { mintWitness = 
              ( beaconMintingPolicy
              , Just (refScriptAddress, mintRef)
              )
          , mintRedeemer = toRedeemer $ CreateAsk borrowerCred $ take i assets
          , mintTokens = ("Ask",fromIntegral i) : take i (zip beacons $ repeat 1)
          }

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = [ sampleAskMints ( 12 :: Int ) ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = take 12 sampleAskOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }
  
  void $ waitNSlots 2

  let sampleOfferOutputs =
        zipWith (\a b ->
                  ( Just $ TxOutDatumInline 
                         $ toDatum offerDatum{loanAsset = a}
                  , lovelaceValueOf 4_000_000 
                  <> singleton beaconCurrencySymbol "Offer" 1
                  <> singleton beaconCurrencySymbol b 1
                  <> singleton beaconCurrencySymbol lenderToken 1
                  <> uncurry singleton a 10
                  )  
                )
                assets
                beacons
  let sampleOfferMints i =
        TokenMint
          { mintWitness = 
              ( beaconMintingPolicy
              , Just (refScriptAddress, mintRef)
              )
          , mintRedeemer = toRedeemer $ CreateOffer lenderCred $ take i assets 
          , mintTokens = 
              [("Offer",fromIntegral i),(lenderToken,fromIntegral i)] 
              <> take i (zip beacons $ repeat 1 )
          }

  callEndpoint @"create-transaction" h2 $
    CreateTransactionParams
      { tokens = [ sampleOfferMints (12 :: Int) ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = take 12 sampleOfferOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }
  
  void $ waitNSlots 2

  targetAsks <- mapM txOutRefWithValue $ take 12 $ map snd sampleAskOutputs
  targetOffers <- mapM txOutRefWithValue $ take 12 $ map snd sampleOfferOutputs
  let pairings = zip4 
        ( grouped groupSize targetAsks ) 
        ( grouped groupSize targetOffers ) 
        ( grouped groupSize assets ) 
        ( grouped groupSize beacons )

  mapM_
    (\(asks,offers,as,bs) -> do
      startTime <- slotToBeginPOSIXTime def <$> waitNSlots 2

      let loanIds = map genLoanId offers
      let activeDatum = ActiveDatum
            { beaconSym = beaconCurrencySymbol 
            , borrowerId = borrowerToken
            , lenderAddress = lenderAddr
            , loanAsset = testToken1
            , loanPrinciple = 10
            , rolloverFrequency = rolloverFrequency offerDatum
            , lastCheckpoint = startTime
            , loanTerm = loanTerm offerDatum
            , loanInterest = unsafeRatio 1 10
            , minPayment = 2
            , collateralization = collateralization offerDatum
            , collateralIsSwappable = True
            , claimExpiration = startTime + loanTerm offerDatum + claimPeriod offerDatum
            , loanExpiration = startTime + loanTerm offerDatum
            , loanOutstanding = fromInt 10 .*. (fromInt 1 .+. unsafeRatio 1 10) 
            , loanId = "" -- This will get replaced.
            }

      let sampleBurn =
            TokenMint
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = 
                  toRedeemer $ CreateActive borrowerCred $ zip asks offers 
              , mintTokens = mconcat 
                  [ zip loanIds (repeat 2)
                  , zip bs (repeat (-1))
                  , [ ("Ask",fromIntegral (-groupSize))
                    , ("Offer",fromIntegral (-groupSize))
                    , ("Active",fromIntegral groupSize)
                    , (lenderToken, fromIntegral (-groupSize))
                    , (borrowerToken, fromIntegral groupSize)
                    ]
                  ]
              }

      let sampleCollateral =  
            map (\i ->
                   ( Just $ TxOutDatumInline 
                          $ toDatum activeDatum{loanAsset = as!!i, loanId = loanIds!!i}
                   , lovelaceValueOf 4_000_000
                   <> singleton beaconCurrencySymbol "Active" 1
                   <> singleton beaconCurrencySymbol (bs!!i) 1
                   <> singleton beaconCurrencySymbol borrowerToken 1
                   <> singleton beaconCurrencySymbol (loanIds!!i) 1
                   <> uncurry singleton testToken15 10
                   )  
                )
                [0..groupSize-1]

      let samplePayments =  
            map (\i ->
                   ( Just $ TxOutDatumInline 
                          $ toDatum 
                          $ PaymentDatum (beaconCurrencySymbol, "Accepted")
                   , lovelaceValueOf 4_000_000
                   <> singleton beaconCurrencySymbol (loanIds!!i) 1
                   )  
                )
                [0..groupSize-1]

      callEndpoint @"create-transaction" h1 $
        CreateTransactionParams
          { tokens = [ sampleBurn ]
          , inputs = 
              [ ScriptUtxoInput
                  { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
                  , spendRedeemer = toRedeemer AcceptOffer
                  , spendFromAddress = loanAddr
                  , spendUtxos = asks <> offers
                  }
              ]
          , outputs =
              [ UtxoOutput
                  { toAddress = loanAddr
                  , outputUtxos = sampleCollateral
                  }
              , UtxoOutput
                  { toAddress = lenderAddr
                  , outputUtxos = samplePayments
                  }
              ]
          , validityRange = ValidityInterval (Just startTime) Nothing
          }
    )
    pairings

  void $ waitNSlots 2

  let loanIds = map genLoanId targetOffers

  targetLoans <- 
    mapM (\v -> txOutRefAndDatumWithAssetAtAddress beaconCurrencySymbol v loanAddr) loanIds

  mapM_
    (\group -> do
      paymentTime <- (+1000) . slotToEndPOSIXTime def <$> waitNSlots 2
      let newCollateral =
            map (\(_,Just loanDatum) ->
                   ( Just $ TxOutDatumInline 
                          $ toDatum loanDatum{loanOutstanding = fromInt 0}
                   , lovelaceValueOf 3_000_000
                   <> singleton beaconCurrencySymbol "Active" 1
                   <> singleton beaconCurrencySymbol (genAssetBeaconName $ loanAsset loanDatum) 1
                   <> singleton beaconCurrencySymbol (loanId loanDatum) 1
                   )  
                )
                group

      let newPayment =
            map (\(_,Just loanDatum) ->
                   ( Just $ TxOutDatumInline 
                          $ toDatum 
                          $ PaymentDatum (beaconCurrencySymbol,loanId loanDatum)
                   , lovelaceValueOf 2_000_000
                   <> uncurry singleton (loanAsset loanDatum) 11
                   )  
                )
                group

      callEndpoint @"create-transaction" h1 $
        CreateTransactionParams
          { tokens = 
            [ TokenMint
                { mintWitness = 
                    ( beaconMintingPolicy
                    , Just (refScriptAddress, mintRef)
                    )
                , mintRedeemer = toRedeemer BurnBeacons
                , mintTokens = [(borrowerToken, fromIntegral (-groupSize))] 
                }
            ]
          , inputs = 
              [ ScriptUtxoInput
                  { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
                  , spendRedeemer = toRedeemer MakePayment
                  , spendFromAddress = loanAddr
                  , spendUtxos = map fst group 
                  }
              ]
          , outputs =
              [ UtxoOutput
                  { toAddress = loanAddr
                  , outputUtxos = newCollateral
                  }
              , UtxoOutput
                  { toAddress = lenderAddr
                  , outputUtxos = newPayment
                  }
              ]
          , validityRange = ValidityInterval Nothing (Just paymentTime)
          }
    )
    (grouped groupSize targetLoans)

  claimTime <- slotToBeginPOSIXTime def <$> waitNSlots 30

  finishedLoans <- txOutRefsAndDatumsAtAddress loanAddr

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
              , mintTokens =
                  ("Active",fromIntegral (-numberUnlocked)) :
                    concatMap (\(_,(Just d)) -> 
                                [ (loanId d,-1)
                                , (genAssetBeaconName $ loanAsset d,-1)
                                ]
                              ) 
                              (take numberUnlocked finishedLoans)
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer Unlock
              , spendFromAddress = loanAddr
              , spendUtxos = take numberUnlocked $ map fst finishedLoans
              }
          ]
      , outputs = [ ]
      , validityRange = ValidityInterval (Just claimTime) Nothing
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

    lenderToken = credentialAsToken True lenderCred

    borrowerToken = credentialAsToken False borrowerCred

    lenderAddr = Address lenderCred Nothing

    askDatum = AskDatum
      { beaconSym = beaconCurrencySymbol
      , borrowerId = borrowerToken
      , loanAsset = testToken1 -- This will be replaced.
      , loanPrinciple = 10
      , loanTerm = 12000
      , collateral = [testToken15]
      }

    offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = Address lenderCred Nothing
        , loanAsset = testToken1 -- This will be replaced.
        , loanPrinciple = 10
        , rolloverFrequency = Nothing
        , minPayment = 2
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , collateralization = 
            [ (testToken15,unsafeRatio 1 1)
            ]
        , claimPeriod = 10000
        , offerDeposit = 4_000_000
        , collateralIsSwappable = True
        }
      
    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

    groupSize :: Int
    groupSize = 4

benchTrace :: Int -> IO ()
benchTrace = runEmulatorTraceIO' def emConfig . benchTest2

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

      -- Benchmark tests
    , checkPredicateOptions opts "benchTest1"
        assertNoFailedTransactions $ benchTest1 5
    , checkPredicateOptions opts "benchTest2"
        assertNoFailedTransactions $ benchTest2 5
    , checkPredicateOptions opts "benchTest3"
        assertNoFailedTransactions $ benchTest3 4
    , checkPredicateOptions opts "benchTest4"
        assertNoFailedTransactions $ benchTest4 4
    , checkPredicateOptions opts "benchTest5"
        assertNoFailedTransactions $ benchTest5 6
    , checkPredicateOptions opts "benchTest6"
        assertNoFailedTransactions $ benchTest6 5

      -- Performance Increase Tests
    , checkPredicateOptions opts "perfIncreaseTest1"
        (Test.not assertNoFailedTransactions) $ benchTest1 6
    , checkPredicateOptions opts "perfIncreaseTest2"
        (Test.not assertNoFailedTransactions) $ benchTest2 6
    , checkPredicateOptions opts "perfIncreaseTest3"
        (Test.not assertNoFailedTransactions) $ benchTest3 5
    , checkPredicateOptions opts "perfIncreaseTest4"
        (Test.not assertNoFailedTransactions) $ benchTest4 5
    , checkPredicateOptions opts "perfIncreaseTest5"
        (Test.not assertNoFailedTransactions) $ benchTest5 7
    , checkPredicateOptions opts "perfIncreaseTest6"
        (Test.not assertNoFailedTransactions) $ benchTest6 6
    ]

testTrace :: IO ()
testTrace = runEmulatorTraceIO' def emConfig failureTest7
