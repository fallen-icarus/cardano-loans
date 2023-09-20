{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Test.ClaimExpired
  (
    -- * Scenarios Tested
    -- ** Scenarios that should succeed
    regressionTest1
  , regressionTest2

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

    -- ** Edge Case Scenarios
  , edgeCase1
  
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
import Plutus.V2.Ledger.Api (TxId(..))
import Data.String (fromString)
import Control.Monad (zipWithM_)
import Data.List (zip4,zip6)

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
-- | Claim an single expired loan.
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

  void $ waitNSlots 20

  active1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Active" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            <> singleton beaconCurrencySymbol loanIdToken 1
            <> singleton beaconCurrencySymbol borrowerToken 1
            <> uncurry singleton testToken1 10
            )
            activeDatum

  key1 <- txOutRefWithAssetAtAddress beaconCurrencySymbol loanIdToken lenderAddr

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
                  , (loanIdToken,-2)
                  ]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer ClaimExpired
              , spendFromAddress = loanAddr
              , spendUtxos = [ active1 ]
              }
          , PubKeyUtxoInput
             { pubKeyAddress = lenderAddr
             , pubKeyUtxos = [key1]
             }
          ]
      , outputs = [ ]
      , validityRange = ValidityInterval (Just $ loanExpiration activeDatum + 1) Nothing
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

-- | Claim multiple expired loans in the transaction.
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
        , loanId = loanIdToken1
        }
  let activeDatum2 = ActiveDatum
        { beaconSym = beaconCurrencySymbol 
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = asset
        , loanPrinciple = 100_000_000
        , rolloverFrequency = Just 1
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

  endTime <- slotToBeginPOSIXTime def <$> waitNSlots 40

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

  key1 <- txOutRefWithAssetAtAddress beaconCurrencySymbol loanIdToken1 lenderAddr

  key2 <- txOutRefWithAssetAtAddress beaconCurrencySymbol loanIdToken2 lenderAddr

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
                  [ (borrowerToken,-2)
                  , ("Active",-2)
                  , (assetBeacon,-2)
                  , (loanIdToken1,-2)
                  , (loanIdToken2,-2)
                  ]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer ClaimExpired
              , spendFromAddress = loanAddr
              , spendUtxos = [ active1, active2 ]
              }
          , PubKeyUtxoInput
             { pubKeyAddress = lenderAddr
             , pubKeyUtxos = [key1,key2]
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
  
  startTime <- slotToBeginPOSIXTime def <$> waitNSlots 2

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Ask" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            askDatum
            
  void $ waitNSlots 20

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
              , mintTokens = [ ]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer ClaimExpired
              , spendFromAddress = loanAddr
              , spendUtxos = [ ask1 ]
              }
          ]
      , outputs = [ ]
      , validityRange = ValidityInterval (Just $ slotToBeginPOSIXTime def 0) Nothing
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

-- | The input does not have an Active beacon.
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
              , mintTokens = [ ]
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
              , spendUtxos = [ ]
              }
          ]
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum activeDatum
                    , lovelaceValueOf 3_000_000 
                    <> uncurry singleton testToken1 10
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval (Just startTime) Nothing
      }

  void $ waitNSlots 20

  active1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
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
              , mintTokens = [ ]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer ClaimExpired
              , spendFromAddress = loanAddr
              , spendUtxos = [ active1 ]
              }
          ]
      , outputs = [ ]
      , validityRange = ValidityInterval (Just $ loanExpiration activeDatum + 1) Nothing
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

-- | The input does not have a BorrowerID.
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

  void $ waitNSlots 2

  finished <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Active" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            <> singleton beaconCurrencySymbol loanIdToken 1
            )
            newActiveDatum

  key1 <- txOutRefWithAssetAtAddress beaconCurrencySymbol loanIdToken lenderAddr

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
                  [ ("Active",-1)
                  , (assetBeacon,-1)
                  , (loanIdToken,-2)
                  ]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer ClaimExpired
              , spendFromAddress = loanAddr
              , spendUtxos = [ finished ]
              }
          , PubKeyUtxoInput
             { pubKeyAddress = lenderAddr
             , pubKeyUtxos = [key1]
             }
          ]
      , outputs = [ ]
      , validityRange = ValidityInterval (Just $ loanExpiration activeDatum + 1) Nothing
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

-- | Loan is not expired.
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

  key1 <- txOutRefWithAssetAtAddress beaconCurrencySymbol loanIdToken lenderAddr

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
                  , (loanIdToken,-2)
                  ]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer ClaimExpired
              , spendFromAddress = loanAddr
              , spendUtxos = [ active1 ]
              }
          , PubKeyUtxoInput
             { pubKeyAddress = lenderAddr
             , pubKeyUtxos = [key1]
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

-- | The invalid-before wasn't specified.
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

  void $ waitNSlots 20

  active1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Active" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            <> singleton beaconCurrencySymbol loanIdToken 1
            <> singleton beaconCurrencySymbol borrowerToken 1
            <> uncurry singleton testToken1 10
            )
            activeDatum

  key1 <- txOutRefWithAssetAtAddress beaconCurrencySymbol loanIdToken lenderAddr

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
                  , (loanIdToken,-2)
                  ]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer ClaimExpired
              , spendFromAddress = loanAddr
              , spendUtxos = [ active1 ]
              }
          , PubKeyUtxoInput
             { pubKeyAddress = lenderAddr
             , pubKeyUtxos = [key1]
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

-- | Not all Active beacons burned.
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
        , loanId = loanIdToken1
        }
  let activeDatum2 = ActiveDatum
        { beaconSym = beaconCurrencySymbol 
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = asset
        , loanPrinciple = 100_000_000
        , rolloverFrequency = Just 1
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

  endTime <- slotToBeginPOSIXTime def <$> waitNSlots 40

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

  key1 <- txOutRefWithAssetAtAddress beaconCurrencySymbol loanIdToken1 lenderAddr

  key2 <- txOutRefWithAssetAtAddress beaconCurrencySymbol loanIdToken2 lenderAddr

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
                  [ (borrowerToken,-2)
                  , ("Active",-1)
                  , (assetBeacon,-2)
                  , (loanIdToken1,-2)
                  , (loanIdToken2,-2)
                  ]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer ClaimExpired
              , spendFromAddress = loanAddr
              , spendUtxos = [ active1, active2 ]
              }
          , PubKeyUtxoInput
             { pubKeyAddress = lenderAddr
             , pubKeyUtxos = [key1,key2]
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

-- | Not all BorrowerIDs burned.
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
        , loanId = loanIdToken1
        }
  let activeDatum2 = ActiveDatum
        { beaconSym = beaconCurrencySymbol 
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = asset
        , loanPrinciple = 100_000_000
        , rolloverFrequency = Just 1
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

  endTime <- slotToBeginPOSIXTime def <$> waitNSlots 40

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

  key1 <- txOutRefWithAssetAtAddress beaconCurrencySymbol loanIdToken1 lenderAddr

  key2 <- txOutRefWithAssetAtAddress beaconCurrencySymbol loanIdToken2 lenderAddr

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
                  , ("Active",-2)
                  , (assetBeacon,-2)
                  , (loanIdToken1,-2)
                  , (loanIdToken2,-2)
                  ]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer ClaimExpired
              , spendFromAddress = loanAddr
              , spendUtxos = [ active1, active2 ]
              }
          , PubKeyUtxoInput
             { pubKeyAddress = lenderAddr
             , pubKeyUtxos = [key1,key2]
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

-- | Not all LoanIDs burned.
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
        , loanId = loanIdToken1
        }
  let activeDatum2 = ActiveDatum
        { beaconSym = beaconCurrencySymbol 
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = asset
        , loanPrinciple = 100_000_000
        , rolloverFrequency = Just 1
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

  endTime <- slotToBeginPOSIXTime def <$> waitNSlots 40

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

  key1 <- txOutRefWithAssetAtAddress beaconCurrencySymbol loanIdToken1 lenderAddr

  key2 <- txOutRefWithAssetAtAddress beaconCurrencySymbol loanIdToken2 lenderAddr

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
                  [ (borrowerToken,-2)
                  , ("Active",-2)
                  , (assetBeacon,-2)
                  , (loanIdToken1,-1)
                  , (loanIdToken2,-2)
                  ]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer ClaimExpired
              , spendFromAddress = loanAddr
              , spendUtxos = [ active1, active2 ]
              }
          , PubKeyUtxoInput
             { pubKeyAddress = lenderAddr
             , pubKeyUtxos = [key1,key2]
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

-- | Not all Asset beacons burned.
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
        , loanId = loanIdToken1
        }
  let activeDatum2 = ActiveDatum
        { beaconSym = beaconCurrencySymbol 
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = asset
        , loanPrinciple = 100_000_000
        , rolloverFrequency = Just 1
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

  endTime <- slotToBeginPOSIXTime def <$> waitNSlots 40

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

  key1 <- txOutRefWithAssetAtAddress beaconCurrencySymbol loanIdToken1 lenderAddr

  key2 <- txOutRefWithAssetAtAddress beaconCurrencySymbol loanIdToken2 lenderAddr

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
                  [ (borrowerToken,-2)
                  , ("Active",-2)
                  , (assetBeacon,-1)
                  , (loanIdToken1,-2)
                  , (loanIdToken2,-2)
                  ]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer ClaimExpired
              , spendFromAddress = loanAddr
              , spendUtxos = [ active1, active2 ]
              }
          , PubKeyUtxoInput
             { pubKeyAddress = lenderAddr
             , pubKeyUtxos = [key1,key2]
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

-------------------------------------------------
-- Edge Cases
-------------------------------------------------
-- | The Key NFT used was created using the `CreateAsk` redeemer and the output reference for
-- the corresponding Offer UTxO. The Ask UTxO is closed while the Active UTxO is claimed as 
-- expired. Since the `CreateAsk` redeemer names Asset beacons differently than the way LoanIDs
-- are named, the counterfeit LoanID will not be recognized as the Key NFT. This transaction will 
-- fail.
edgeCase1 :: EmulatorTrace ()
edgeCase1 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  
  (mintRef,spendRef) <- initializeScripts

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

  void $ waitNSlots 2

  offer1@(TxOutRef (TxId txHash) idx) <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 103_000_000 
            <> singleton beaconCurrencySymbol "Offer" 1
            <> singleton beaconCurrencySymbol lenderToken 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            offerDatum

  let fakeAsset = (CurrencySymbol txHash, fromString $ show idx)
  let fakeAssetBeacon = genAssetBeaconName fakeAsset
  let fakeAskDatum = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = credentialAsToken False borrowerCred
        , loanAsset = fakeAsset
        , loanPrinciple = 100_000_000
        , loanTerm = 12000
        , collateral = [testToken1]
        }

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
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset,fakeAsset]
              , mintTokens = [("Ask",2),(assetBeacon,1),(fakeAssetBeacon,1)]
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
                  , ( Just $ TxOutDatumInline $ toDatum fakeAskDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol fakeAssetBeacon 1
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
  fakeAsk1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Ask" 1
            <> singleton beaconCurrencySymbol fakeAssetBeacon 1
            )
            fakeAskDatum

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

  void $ waitNSlots 20

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
                  , (fakeAssetBeacon,-1)
                  , ("Ask",-1)
                  ]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer ClaimExpired
              , spendFromAddress = loanAddr
              , spendUtxos = [ active1 ]
              }
          , ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseAsk
              , spendFromAddress = loanAddr
              , spendUtxos = [ fakeAsk1 ]
              }
          ]
      , outputs = [ ]
      , validityRange = ValidityInterval (Just $ loanExpiration activeDatum + 1) Nothing
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

-------------------------------------------------
-- Bench Tests
-------------------------------------------------
-- | Claim multiple expired loans in the same transaction. Each loan uses 
-- one asset for collateral. All loans are for the same loan asset. All loans are from
-- the same borrower.
benchTest1 :: Int -> EmulatorTrace ()
benchTest1 numberClaimed = do
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

  claimTime <- slotToBeginPOSIXTime def <$> waitNSlots 30

  let loanIds = map genLoanId targetOffers

  targetLoans <- 
    mapM (\v -> txOutRefAndDatumWithAssetAtAddress beaconCurrencySymbol v loanAddr) loanIds
  targetKeys <-
    mapM (\v -> txOutRefWithAssetAtAddress beaconCurrencySymbol v lenderAddr) loanIds
  
  callEndpoint @"create-transaction" h2 $
    CreateTransactionParams
      { tokens = 
          [ TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
              , mintTokens = mconcat
                  [ take numberClaimed $ zip loanIds (repeat (-2))
                  , [ (borrowerToken,fromIntegral (-numberClaimed))
                    , ("Active",fromIntegral (-numberClaimed))
                    , (assetBeacon,fromIntegral (-numberClaimed))
                    ]
                  ]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer ClaimExpired
              , spendFromAddress = loanAddr
              , spendUtxos = take numberClaimed $ map fst targetLoans
              }
          , PubKeyUtxoInput
              { pubKeyAddress = lenderAddr
              , pubKeyUtxos = take numberClaimed targetKeys
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

-- | Claim multiple expired loans in the same transaction. Each loan uses 
-- three assets for collateral. All loans are for the same loan asset. All loans are from
-- the same borrower.
benchTest2 :: Int -> EmulatorTrace ()
benchTest2 numberClaimed = do
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

  claimTime <- slotToBeginPOSIXTime def <$> waitNSlots 30

  let loanIds = map genLoanId targetOffers

  targetLoans <- 
    mapM (\v -> txOutRefAndDatumWithAssetAtAddress beaconCurrencySymbol v loanAddr) loanIds
  targetKeys <-
    mapM (\v -> txOutRefWithAssetAtAddress beaconCurrencySymbol v lenderAddr) loanIds
  
  callEndpoint @"create-transaction" h2 $
    CreateTransactionParams
      { tokens = 
          [ TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
              , mintTokens = mconcat
                  [ take numberClaimed $ zip loanIds (repeat (-2))
                  , [ (borrowerToken,fromIntegral (-numberClaimed))
                    , ("Active",fromIntegral (-numberClaimed))
                    , (assetBeacon,fromIntegral (-numberClaimed))
                    ]
                  ]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer ClaimExpired
              , spendFromAddress = loanAddr
              , spendUtxos = take numberClaimed $ map fst targetLoans
              }
          , PubKeyUtxoInput
              { pubKeyAddress = lenderAddr
              , pubKeyUtxos = take numberClaimed targetKeys
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

-- | Claim multiple expired loans for different loan assets. Each loan uses one asset for 
-- collateral. All loans are from the same borrower.
benchTest3 :: Int -> EmulatorTrace ()
benchTest3 numberClaimed = do
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

  claimTime <- slotToBeginPOSIXTime def <$> waitNSlots 30

  let loanIds = map genLoanId targetOffers

  targetLoans <- 
    mapM (\v -> txOutRefAndDatumWithAssetAtAddress beaconCurrencySymbol v loanAddr) loanIds
  targetKeys <-
    mapM (\v -> txOutRefWithAssetAtAddress beaconCurrencySymbol v lenderAddr) loanIds
  
  callEndpoint @"create-transaction" h2 $
    CreateTransactionParams
      { tokens = 
          [ TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
              , mintTokens = mconcat
                  [ take numberClaimed $ zip loanIds (repeat (-2))
                  , take numberClaimed $ zip beacons (repeat (-1))
                  , [ (borrowerToken,fromIntegral (-numberClaimed))
                    , ("Active",fromIntegral (-numberClaimed))
                    ]
                  ]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer ClaimExpired
              , spendFromAddress = loanAddr
              , spendUtxos = take numberClaimed $ map fst targetLoans
              }
          , PubKeyUtxoInput
              { pubKeyAddress = lenderAddr
              , pubKeyUtxos = take numberClaimed targetKeys
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

-- | Claim multiple expired loans for different loan assets. Each loan uses three assets for 
-- collateral, usage was split evenly. All loans are from the same borrower.
benchTest4 :: Int -> EmulatorTrace ()
benchTest4 numberClaimed = do
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
  targetKeys <-
    mapM (\v -> txOutRefWithAssetAtAddress beaconCurrencySymbol v lenderAddr) loanIds
  
  callEndpoint @"create-transaction" h2 $
    CreateTransactionParams
      { tokens = 
          [ TokenMint 
              { mintWitness =
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
              , mintTokens = mconcat
                  [ take numberClaimed $ zip loanIds (repeat (-2))
                  , take numberClaimed $ zip beacons (repeat (-1))
                  , [ (borrowerToken,fromIntegral (-numberClaimed))
                    , ("Active",fromIntegral (-numberClaimed))
                    ]
                  ]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer ClaimExpired
              , spendFromAddress = loanAddr
              , spendUtxos = take numberClaimed $ map fst targetLoans
              }
          , PubKeyUtxoInput
              { pubKeyAddress = lenderAddr
              , pubKeyUtxos = take numberClaimed targetKeys
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

-- | Claim multiple loans for different loan assets and from different borrowers. Each loan
-- uses one asset for collateral.
benchTest5 :: Int -> EmulatorTrace ()
benchTest5 numberClaimed = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  h3 <- activateContractWallet (knownWallet 3) endpoints
  h4 <- activateContractWallet (knownWallet 4) endpoints
  h5 <- activateContractWallet (knownWallet 5) endpoints
  h6 <- activateContractWallet (knownWallet 6) endpoints
  h7 <- activateContractWallet (knownWallet 7) endpoints
  h8 <- activateContractWallet (knownWallet 8) endpoints
  h9 <- activateContractWallet (knownWallet 9) endpoints
  h10 <- activateContractWallet (knownWallet 10) endpoints

  let borrowerHandles = [h2,h3,h4,h5,h6,h7,h8,h9,h10]

  ( mintRef,spendRef ) <- initializeScripts

  let assets = map (\i -> (fst testToken1, fromString $ "TestToken" <> show @Int i)) [1..20]
  let beacons = map genAssetBeaconName assets

  mapM_ 
    (\i -> do
      let asset = assets!!(i-2)
      let assetBeacon = beacons!!(i-2)

      void $ waitNSlots 2
      callEndpoint @"create-transaction" (borrowerHandles!!(i-2)) $
        CreateTransactionParams
          { tokens = 
              [ 
                TokenMint 
                  { mintWitness = 
                      ( beaconMintingPolicy
                      , Just (refScriptAddress, mintRef)
                      )
                  , mintRedeemer = 
                      toRedeemer $ CreateAsk (borrowerCred $ fromIntegral i) [asset]
                  , mintTokens = 
                      [ ("Ask",1)
                      , (assetBeacon,1)
                      ]
                  }
              ]
          , inputs = []
          , outputs =
              [ UtxoOutput
                  { toAddress = loanAddr $ fromIntegral i
                  , outputUtxos = 
                      [ ( Just $ TxOutDatumInline 
                               $ toDatum askDatum{ borrowerId = borrowerToken $ fromIntegral i
                                                 , loanAsset = asset
                                                 }
                        , lovelaceValueOf 3_000_000 
                        <> singleton beaconCurrencySymbol "Ask" 1
                        <> singleton beaconCurrencySymbol assetBeacon 1
                        )
                      ]
                  }
              ]
          , validityRange = ValidityInterval Nothing Nothing
          }
    )
    [2..10]

  mapM_ 
    (\i -> do
      let asset = assets!!(i-2)
      let assetBeacon = beacons!!(i-2)

      void $ waitNSlots 2
      callEndpoint @"create-transaction" h1 $
        CreateTransactionParams
          { tokens = 
              [ 
                TokenMint 
                  { mintWitness = 
                      ( beaconMintingPolicy
                      , Just (refScriptAddress, mintRef)
                      )
                  , mintRedeemer = 
                      toRedeemer $ CreateOffer lenderCred [asset]
                  , mintTokens = 
                      [ ("Offer",1)
                      , (assetBeacon,1)
                      , (lenderToken,1)
                      ]
                  }
              ]
          , inputs = []
          , outputs =
              [ UtxoOutput
                  { toAddress = loanAddr $ fromIntegral i
                  , outputUtxos = 
                      [ ( Just $ TxOutDatumInline 
                               $ toDatum offerDatum{ loanAsset = asset }
                        , lovelaceValueOf 4_000_000 
                        <> singleton beaconCurrencySymbol "Offer" 1
                        <> singleton beaconCurrencySymbol assetBeacon 1
                        <> singleton beaconCurrencySymbol lenderToken 1
                        <> uncurry singleton asset 10
                        )
                      ]
                  }
              ]
          , validityRange = ValidityInterval Nothing Nothing
          }
    )
    [2..10]

  void $ waitNSlots 2

  targetAsks <- 
    mapM 
      (\addr -> txOutRefWithAssetAtAddress beaconCurrencySymbol "Ask" addr) 
      (map loanAddr [2..10])
  targetOffers <- 
    mapM 
      (\addr -> txOutRefWithAssetAtAddress beaconCurrencySymbol "Offer" addr) 
      (map loanAddr [2..10])
  let pairings = zip6 
        targetAsks
        targetOffers 
        assets 
        beacons
        borrowerHandles
        [2..10]

  mapM_
    (\(ask,offer,a,b,h,i) -> do
      startTime <- slotToBeginPOSIXTime def <$> waitNSlots 2

      let loanId' = genLoanId offer
      let activeDatum = ActiveDatum
            { beaconSym = beaconCurrencySymbol 
            , borrowerId = borrowerToken i
            , lenderAddress = lenderAddr
            , loanAsset = a
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
            , loanId = loanId'
            }

      let sampleBurn =
            TokenMint
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = 
                  toRedeemer $ CreateActive (borrowerCred i) $ [(ask,offer)]
              , mintTokens = 
                  [ ("Ask",(-1))
                  , ("Offer",(-1))
                  , ("Active",1)
                  , (lenderToken, (-1))
                  , (borrowerToken i, 1)
                  , (loanId',2)
                  , (b,-1)
                  ]
              }

      let sampleCollateral =  
             ( Just $ TxOutDatumInline 
                    $ toDatum activeDatum
             , lovelaceValueOf 4_000_000
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol b 1
             <> singleton beaconCurrencySymbol (borrowerToken i) 1
             <> singleton beaconCurrencySymbol loanId' 1
             <> uncurry singleton testToken15 10
             )  

      let samplePayments =  
             ( Just $ TxOutDatumInline 
                    $ toDatum 
                    $ PaymentDatum (beaconCurrencySymbol, "Accepted")
             , lovelaceValueOf 4_000_000
             <> singleton beaconCurrencySymbol loanId' 1
             )  

      callEndpoint @"create-transaction" h $
        CreateTransactionParams
          { tokens = [ sampleBurn ]
          , inputs = 
              [ ScriptUtxoInput
                  { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
                  , spendRedeemer = toRedeemer AcceptOffer
                  , spendFromAddress = loanAddr i
                  , spendUtxos = [ask,offer]
                  }
              ]
          , outputs =
              [ UtxoOutput
                  { toAddress = loanAddr i
                  , outputUtxos = [ sampleCollateral ]
                  }
              , UtxoOutput
                  { toAddress = lenderAddr
                  , outputUtxos = [ samplePayments ]
                  }
              ]
          , validityRange = ValidityInterval (Just startTime) Nothing
          }
    )
    pairings

  claimTime <- slotToBeginPOSIXTime def <$> waitNSlots 30

  let loanIds = map genLoanId targetOffers

  targetLoans <- 
    mapM 
      (\(v,addr) -> txOutRefAndDatumWithAssetAtAddress beaconCurrencySymbol v addr) 
      (zip loanIds $ map loanAddr [2..10])
  targetKeys <-
    mapM (\v -> txOutRefWithAssetAtAddress beaconCurrencySymbol v lenderAddr) loanIds

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
                  [ take numberClaimed $ zip loanIds (repeat (-2))
                  , take numberClaimed $ zip beacons (repeat (-1))
                  , take numberClaimed $ map (\i -> (borrowerToken i,-1)) [2..10]
                  , [ ("Active",fromIntegral (-numberClaimed)) ]
                  ]
              } 
          ]
      , inputs = mconcat 
          [ map (\i -> 
                   ScriptUtxoInput
                     { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
                     , spendRedeemer = toRedeemer ClaimExpired
                     , spendFromAddress = loanAddr i
                     , spendUtxos = take numberClaimed $ map fst targetLoans
                     }
                )
                [2..10]
          , [ PubKeyUtxoInput
                { pubKeyAddress = lenderAddr
                , pubKeyUtxos = take numberClaimed targetKeys
                }
            ]
          ]
      , outputs = [ ]
      , validityRange = ValidityInterval (Just claimTime) Nothing
      }

  where
    borrowerCred = PubKeyCredential
                 . unPaymentPubKeyHash 
                 . mockWalletPaymentPubKeyHash 
                 . knownWallet

    lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1

    lenderToken = credentialAsToken True lenderCred

    borrowerToken = credentialAsToken False . borrowerCred

    lenderAddr = Address lenderCred Nothing

    askDatum = AskDatum
      { beaconSym = beaconCurrencySymbol
      , borrowerId = borrowerToken 1 -- This will be replaced.
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
      
    loanAddr = Address (ScriptCredential loanValidatorHash) 
             . Just 
             . StakingHash
             . borrowerCred

    groupSize :: Int
    groupSize = 4

-- | Claim multiple loans for different loan assets and from different borrowers. Each loan
-- uses three assets for collateral, usage was split evenly.
benchTest6 :: Int -> EmulatorTrace ()
benchTest6 numberClaimed = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  h3 <- activateContractWallet (knownWallet 3) endpoints
  h4 <- activateContractWallet (knownWallet 4) endpoints
  h5 <- activateContractWallet (knownWallet 5) endpoints
  h6 <- activateContractWallet (knownWallet 6) endpoints
  h7 <- activateContractWallet (knownWallet 7) endpoints
  h8 <- activateContractWallet (knownWallet 8) endpoints
  h9 <- activateContractWallet (knownWallet 9) endpoints
  h10 <- activateContractWallet (knownWallet 10) endpoints

  let borrowerHandles = [h2,h3,h4,h5,h6,h7,h8,h9,h10]

  ( mintRef,spendRef ) <- initializeScripts

  let assets = map (\i -> (fst testToken1, fromString $ "TestToken" <> show @Int i)) [1..20]
  let beacons = map genAssetBeaconName assets

  mapM_ 
    (\i -> do
      let asset = assets!!(i-2)
      let assetBeacon = beacons!!(i-2)

      void $ waitNSlots 2
      callEndpoint @"create-transaction" (borrowerHandles!!(i-2)) $
        CreateTransactionParams
          { tokens = 
              [ 
                TokenMint 
                  { mintWitness = 
                      ( beaconMintingPolicy
                      , Just (refScriptAddress, mintRef)
                      )
                  , mintRedeemer = 
                      toRedeemer $ CreateAsk (borrowerCred $ fromIntegral i) [asset]
                  , mintTokens = 
                      [ ("Ask",1)
                      , (assetBeacon,1)
                      ]
                  }
              ]
          , inputs = []
          , outputs =
              [ UtxoOutput
                  { toAddress = loanAddr $ fromIntegral i
                  , outputUtxos = 
                      [ ( Just $ TxOutDatumInline 
                               $ toDatum askDatum{ borrowerId = borrowerToken $ fromIntegral i
                                                 , loanAsset = asset
                                                 }
                        , lovelaceValueOf 3_000_000 
                        <> singleton beaconCurrencySymbol "Ask" 1
                        <> singleton beaconCurrencySymbol assetBeacon 1
                        )
                      ]
                  }
              ]
          , validityRange = ValidityInterval Nothing Nothing
          }
    )
    [2..10]

  mapM_ 
    (\i -> do
      let asset = assets!!(i-2)
      let assetBeacon = beacons!!(i-2)

      void $ waitNSlots 2
      callEndpoint @"create-transaction" h1 $
        CreateTransactionParams
          { tokens = 
              [ 
                TokenMint 
                  { mintWitness = 
                      ( beaconMintingPolicy
                      , Just (refScriptAddress, mintRef)
                      )
                  , mintRedeemer = 
                      toRedeemer $ CreateOffer lenderCred [asset]
                  , mintTokens = 
                      [ ("Offer",1)
                      , (assetBeacon,1)
                      , (lenderToken,1)
                      ]
                  }
              ]
          , inputs = []
          , outputs =
              [ UtxoOutput
                  { toAddress = loanAddr $ fromIntegral i
                  , outputUtxos = 
                      [ ( Just $ TxOutDatumInline 
                               $ toDatum offerDatum{ loanAsset = asset }
                        , lovelaceValueOf 4_000_000 
                        <> singleton beaconCurrencySymbol "Offer" 1
                        <> singleton beaconCurrencySymbol assetBeacon 1
                        <> singleton beaconCurrencySymbol lenderToken 1
                        <> uncurry singleton asset 10
                        )
                      ]
                  }
              ]
          , validityRange = ValidityInterval Nothing Nothing
          }
    )
    [2..10]

  void $ waitNSlots 2

  targetAsks <- 
    mapM 
      (\addr -> txOutRefWithAssetAtAddress beaconCurrencySymbol "Ask" addr) 
      (map loanAddr [2..10])
  targetOffers <- 
    mapM 
      (\addr -> txOutRefWithAssetAtAddress beaconCurrencySymbol "Offer" addr) 
      (map loanAddr [2..10])
  let pairings = zip6 
        targetAsks
        targetOffers 
        assets 
        beacons
        borrowerHandles
        [2..10]

  mapM_
    (\(ask,offer,a,b,h,i) -> do
      startTime <- slotToBeginPOSIXTime def <$> waitNSlots 2

      let loanId' = genLoanId offer
      let activeDatum = ActiveDatum
            { beaconSym = beaconCurrencySymbol 
            , borrowerId = borrowerToken i
            , lenderAddress = lenderAddr
            , loanAsset = a
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
            , loanId = loanId'
            }

      let sampleBurn =
            TokenMint
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = 
                  toRedeemer $ CreateActive (borrowerCred i) $ [(ask,offer)]
              , mintTokens = 
                  [ ("Ask",(-1))
                  , ("Offer",(-1))
                  , ("Active",1)
                  , (lenderToken, (-1))
                  , (borrowerToken i, 1)
                  , (loanId',2)
                  , (b,-1)
                  ]
              }

      let sampleCollateral =  
             ( Just $ TxOutDatumInline 
                    $ toDatum activeDatum
             , lovelaceValueOf 4_000_000
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol b 1
             <> singleton beaconCurrencySymbol (borrowerToken i) 1
             <> singleton beaconCurrencySymbol loanId' 1
             <> uncurry singleton testToken15 3
             <> uncurry singleton testToken16 3
             <> uncurry singleton testToken17 4
             )  

      let samplePayments =  
             ( Just $ TxOutDatumInline 
                    $ toDatum 
                    $ PaymentDatum (beaconCurrencySymbol, "Accepted")
             , lovelaceValueOf 4_000_000
             <> singleton beaconCurrencySymbol loanId' 1
             )  

      callEndpoint @"create-transaction" h $
        CreateTransactionParams
          { tokens = [ sampleBurn ]
          , inputs = 
              [ ScriptUtxoInput
                  { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
                  , spendRedeemer = toRedeemer AcceptOffer
                  , spendFromAddress = loanAddr i
                  , spendUtxos = [ask,offer]
                  }
              ]
          , outputs =
              [ UtxoOutput
                  { toAddress = loanAddr i
                  , outputUtxos = [ sampleCollateral ]
                  }
              , UtxoOutput
                  { toAddress = lenderAddr
                  , outputUtxos = [ samplePayments ]
                  }
              ]
          , validityRange = ValidityInterval (Just startTime) Nothing
          }
    )
    pairings

  claimTime <- slotToBeginPOSIXTime def <$> waitNSlots 30

  let loanIds = map genLoanId targetOffers

  targetLoans <- 
    mapM 
      (\(v,addr) -> txOutRefAndDatumWithAssetAtAddress beaconCurrencySymbol v addr) 
      (zip loanIds $ map loanAddr [2..10])
  targetKeys <-
    mapM (\v -> txOutRefWithAssetAtAddress beaconCurrencySymbol v lenderAddr) loanIds

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
                  [ take numberClaimed $ zip loanIds (repeat (-2))
                  , take numberClaimed $ zip beacons (repeat (-1))
                  , take numberClaimed $ map (\i -> (borrowerToken i,-1)) [2..10]
                  , [ ("Active",fromIntegral (-numberClaimed)) ]
                  ]
              } 
          ]
      , inputs = mconcat 
          [ map (\i -> 
                   ScriptUtxoInput
                     { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
                     , spendRedeemer = toRedeemer ClaimExpired
                     , spendFromAddress = loanAddr i
                     , spendUtxos = take numberClaimed $ map fst targetLoans
                     }
                )
                [2..10]
          , [ PubKeyUtxoInput
                { pubKeyAddress = lenderAddr
                , pubKeyUtxos = take numberClaimed targetKeys
                }
            ]
          ]
      , outputs = [ ]
      , validityRange = ValidityInterval (Just claimTime) Nothing
      }

  where
    borrowerCred = PubKeyCredential
                 . unPaymentPubKeyHash 
                 . mockWalletPaymentPubKeyHash 
                 . knownWallet

    lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1

    lenderToken = credentialAsToken True lenderCred

    borrowerToken = credentialAsToken False . borrowerCred

    lenderAddr = Address lenderCred Nothing

    askDatum = AskDatum
      { beaconSym = beaconCurrencySymbol
      , borrowerId = borrowerToken 1 -- This will be replaced.
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
      
    loanAddr = Address (ScriptCredential loanValidatorHash) 
             . Just 
             . StakingHash
             . borrowerCred

    groupSize :: Int
    groupSize = 4

benchTrace :: Int -> IO ()
benchTrace = runEmulatorTraceIO' def emConfig . benchTest6

-------------------------------------------------
-- Test Function
-------------------------------------------------
-- | A `TestTree` containing all `ClaimExpired` scenarios.
tests :: TestTree
tests = do
  let opts = defaultCheckOptions & emulatorConfig .~ emConfig
  testGroup "Claim Expired Loan(s)"
    [ -- Success Tests (Regression Tests)
      checkPredicateOptions opts "regressionTest1"
        assertNoFailedTransactions regressionTest1
    , checkPredicateOptions opts "regressionTest2"
        assertNoFailedTransactions regressionTest2

      -- Failure Tests
    , checkPredicateOptions opts "failureTest1"
        (assertEvaluationError "Datum is not an ActiveDatum") failureTest1
    , checkPredicateOptions opts "failureTest2"
        (assertEvaluationError "Input missing Active beacon") failureTest2
    , checkPredicateOptions opts "failureTest3"
        (assertEvaluationError "Input missing BorrowerID") failureTest3
    , checkPredicateOptions opts "failureTest4"
        (assertEvaluationError "Loan is not expired") failureTest4
    , checkPredicateOptions opts "failureTest5"
        (assertEvaluationError "invalid-before not specified") failureTest5
    , checkPredicateOptions opts "failureTest6"
        (assertEvaluationError "Not all beacons burned") failureTest6
    , checkPredicateOptions opts "failureTest7"
        (assertEvaluationError "Not all beacons burned") failureTest7
    , checkPredicateOptions opts "failureTest8"
        (assertEvaluationError "Not all beacons burned") failureTest8
    , checkPredicateOptions opts "failureTest9"
        (assertEvaluationError "Not all beacons burned") failureTest9

      -- Edge Cases
    , checkPredicateOptions opts "edgeCase1"
        (assertEvaluationError "Key NFT not found") edgeCase1

      -- Benchmarks
    , checkPredicateOptions opts "benchTest1"
        assertNoFailedTransactions $ benchTest1 4
    , checkPredicateOptions opts "benchTest2"
        assertNoFailedTransactions $ benchTest2 4
    , checkPredicateOptions opts "benchTest3"
        assertNoFailedTransactions $ benchTest3 4
    , checkPredicateOptions opts "benchTest4"
        assertNoFailedTransactions $ benchTest4 4
    , checkPredicateOptions opts "benchTest5"
        assertNoFailedTransactions $ benchTest5 4
    , checkPredicateOptions opts "benchTest6"
        assertNoFailedTransactions $ benchTest6 3
    ]

testTrace :: IO ()
testTrace = runEmulatorTraceIO' def emConfig edgeCase1
