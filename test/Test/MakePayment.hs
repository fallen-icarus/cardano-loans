{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Test.MakePayment
  (
    -- * Scenarios Tested
    -- ** Scenarios that should succeed
    regressionTest1
  , regressionTest2
  , regressionTest3
  , regressionTest4
  , regressionTest5
  , regressionTest6
  , regressionTest7
  , regressionTest8
  , regressionTest9
  , regressionTest10
  , regressionTest11
  , regressionTest12
  , regressionTest13
  , regressionTest14
  , regressionTest15

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
  , failureTest10
  , failureTest11
  , failureTest12
  , failureTest13
  , failureTest14
  , failureTest15
  , failureTest16
  , failureTest17
  , failureTest18
  , failureTest19
  , failureTest20
  , failureTest21
  , failureTest22
  , failureTest23
  , failureTest24
  , failureTest25
  , failureTest26
  , failureTest27
  , failureTest28
  , failureTest29
  , failureTest30
  , failureTest31
  , failureTest32
  , failureTest33
  , failureTest34
  , failureTest35
  , failureTest36
  , failureTest37
  , failureTest38
  , failureTest39
  , failureTest40
  , failureTest41
  , failureTest42
  , failureTest43
  , failureTest44
  , failureTest45
  , failureTest46
  , failureTest47
  , failureTest48
  , failureTest49
  , failureTest50

    -- ** Edge Case Scenarios
  , edgeCase1

    -- * Benchmark Tests
  , benchTest1
  , benchTest2
  , benchTest3
  , benchTest4

    -- * Full test function
  , tests
  ) where

import PlutusTx.Prelude (unsafeRatio)
import Plutus.Trace
import Wallet.Emulator.Wallet
import Plutus.Contract.Test as Test
import Test.Tasty
import Data.Maybe (fromMaybe)
import Control.Monad (zipWithM_)
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
-- | Make a full payment on a single loan.
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

-- | Make a partial payment on a single loan.
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
        { loanOutstanding = loanOutstanding activeDatum .-. fromInt 55_000_000
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
                           $ PaymentDatum (beaconCurrencySymbol,loanIdToken)
                    , lovelaceValueOf 55_000_000 
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing (Just $ loanExpiration activeDatum)
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

-- | Make a full payment on multiple loans from the same lender.
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
        { loanOutstanding = loanOutstanding activeDatum1 .-. fromInt 110_000_000
        }

  let newActiveDatum2 = activeDatum2
        { loanOutstanding = loanOutstanding activeDatum2 .-. fromInt 110_000_000
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
              , mintTokens = [(borrowerToken,-2)]
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
                    )
                  , ( Just $ TxOutDatumInline $ toDatum newActiveDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Active" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol loanIdToken2 1
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = lenderAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,loanIdToken1)
                    , lovelaceValueOf 110_000_000 
                    )
                  , ( Just $ TxOutDatumInline 
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,loanIdToken2)
                    , lovelaceValueOf 110_000_000 
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing (Just $ loanExpiration activeDatum1)
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

-- | Make a partial payment on multiple loans from the same lender.
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

-- | Mix full and partial payments on multiple loans from the same lender.
regressionTest5 :: EmulatorTrace ()
regressionTest5 = do
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
        { loanOutstanding = loanOutstanding activeDatum1 .-. fromInt 110_000_000
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
              , mintTokens = [(borrowerToken,-1)]
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
                    , lovelaceValueOf 110_000_000 
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

-- | Make full payments on multiple loans from different lenders.
regressionTest6 :: EmulatorTrace ()
regressionTest6 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  h3 <- activateContractWallet (knownWallet 3) endpoints
  
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
              , mintRedeemer = toRedeemer $ CreateOffer lenderCred1 [asset]
              , mintTokens = [("Offer",1),(assetBeacon,1),(lenderToken1,1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum offerDatum1
                    , lovelaceValueOf 103_000_000 
                    <> singleton beaconCurrencySymbol "Offer" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol lenderToken1 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  -- Create the Offer UTxO.
  callEndpoint @"create-transaction" h3 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateOffer lenderCred2 [asset]
              , mintTokens = [("Offer",1),(assetBeacon,1),(lenderToken2,1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum offerDatum2
                    , lovelaceValueOf 103_000_000 
                    <> singleton beaconCurrencySymbol "Offer" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol lenderToken2 1
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
            <> singleton beaconCurrencySymbol lenderToken1 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            offerDatum1
  ask2 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Ask" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            askDatum{loanTerm = 22000}
  offer2 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 103_000_000 
            <> singleton beaconCurrencySymbol "Offer" 1
            <> singleton beaconCurrencySymbol lenderToken2 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            offerDatum2

  let loanIdToken1 = genLoanId offer1
  let loanIdToken2 = genLoanId offer2
  let activeDatum1 = ActiveDatum
        { beaconSym = beaconCurrencySymbol 
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr1
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
        , lenderAddress = lenderAddr2
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
                  , (lenderToken1,-1)
                  , (lenderToken2,-1)
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
              { toAddress = lenderAddr1
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,"Accepted") 
                    , lovelaceValueOf 3_000_000
                    <> singleton beaconCurrencySymbol loanIdToken1 1
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = lenderAddr2
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline
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
        { loanOutstanding = loanOutstanding activeDatum1 .-. fromInt 110_000_000
        }

  let newActiveDatum2 = activeDatum2
        { loanOutstanding = loanOutstanding activeDatum2 .-. fromInt 110_000_000
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
              , mintTokens = [(borrowerToken,-2)]
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
                    )
                  , ( Just $ TxOutDatumInline $ toDatum newActiveDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Active" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol loanIdToken2 1
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = lenderAddr1
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,loanIdToken1)
                    , lovelaceValueOf 110_000_000 
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = lenderAddr2
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,loanIdToken2)
                    , lovelaceValueOf 110_000_000 
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing (Just $ loanExpiration activeDatum1)
      }

  where
    borrowerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
    lenderCred1 = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
    lenderCred2 = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 3

    
    borrowerToken = credentialAsToken borrowerCred
    lenderToken1 = credentialAsToken lenderCred1
    lenderToken2 = credentialAsToken lenderCred2
    
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

    lenderAddr1 = Address lenderCred1 Nothing
    lenderAddr2 = Address lenderCred2 Nothing

    offerDatum1 = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken1
        , lenderAddress = lenderAddr1
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
    offerDatum2 = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken2
        , lenderAddress = lenderAddr2
        , loanAsset = asset
        , loanPrinciple = 100_000_000
        , rolloverFrequency = Nothing
        , minPayment = 500_000
        , loanTerm = 22000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 10_000_000)]
        , claimPeriod = 10000
        , offerDeposit = 3_000_000
        , collateralIsSwappable = True
        }
    
    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | Make partial payments on multiple loans from different lenders.
regressionTest7 :: EmulatorTrace ()
regressionTest7 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  h3 <- activateContractWallet (knownWallet 3) endpoints
  
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
              , mintRedeemer = toRedeemer $ CreateOffer lenderCred1 [asset]
              , mintTokens = [("Offer",1),(assetBeacon,1),(lenderToken1,1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum offerDatum1
                    , lovelaceValueOf 103_000_000 
                    <> singleton beaconCurrencySymbol "Offer" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol lenderToken1 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  -- Create the Offer UTxO.
  callEndpoint @"create-transaction" h3 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateOffer lenderCred2 [asset]
              , mintTokens = [("Offer",1),(assetBeacon,1),(lenderToken2,1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum offerDatum2
                    , lovelaceValueOf 103_000_000 
                    <> singleton beaconCurrencySymbol "Offer" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol lenderToken2 1
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
            <> singleton beaconCurrencySymbol lenderToken1 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            offerDatum1
  ask2 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Ask" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            askDatum{loanTerm = 22000}
  offer2 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 103_000_000 
            <> singleton beaconCurrencySymbol "Offer" 1
            <> singleton beaconCurrencySymbol lenderToken2 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            offerDatum2

  let loanIdToken1 = genLoanId offer1
  let loanIdToken2 = genLoanId offer2
  let activeDatum1 = ActiveDatum
        { beaconSym = beaconCurrencySymbol 
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr1
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
        , lenderAddress = lenderAddr2
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
                  , (lenderToken1,-1)
                  , (lenderToken2,-1)
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
              { toAddress = lenderAddr1
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,"Accepted") 
                    , lovelaceValueOf 3_000_000
                    <> singleton beaconCurrencySymbol loanIdToken1 1
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = lenderAddr2
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline
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
              { toAddress = lenderAddr1
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,loanIdToken1)
                    , lovelaceValueOf 55_000_000 
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = lenderAddr2
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,loanIdToken2)
                    , lovelaceValueOf 55_000_000 
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing (Just $ loanExpiration activeDatum1)
      }

  where
    borrowerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
    lenderCred1 = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
    lenderCred2 = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 3

    
    borrowerToken = credentialAsToken borrowerCred
    lenderToken1 = credentialAsToken lenderCred1
    lenderToken2 = credentialAsToken lenderCred2
    
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

    lenderAddr1 = Address lenderCred1 Nothing
    lenderAddr2 = Address lenderCred2 Nothing

    offerDatum1 = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken1
        , lenderAddress = lenderAddr1
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
    offerDatum2 = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken2
        , lenderAddress = lenderAddr2
        , loanAsset = asset
        , loanPrinciple = 100_000_000
        , rolloverFrequency = Nothing
        , minPayment = 500_000
        , loanTerm = 22000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 10_000_000)]
        , claimPeriod = 10000
        , offerDeposit = 3_000_000
        , collateralIsSwappable = True
        }
    
    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | Mix partial and full payments on multiple loans from different lenders.
regressionTest8 :: EmulatorTrace ()
regressionTest8 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  h3 <- activateContractWallet (knownWallet 3) endpoints
  
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
              , mintRedeemer = toRedeemer $ CreateOffer lenderCred1 [asset]
              , mintTokens = [("Offer",1),(assetBeacon,1),(lenderToken1,1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum offerDatum1
                    , lovelaceValueOf 103_000_000 
                    <> singleton beaconCurrencySymbol "Offer" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol lenderToken1 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  -- Create the Offer UTxO.
  callEndpoint @"create-transaction" h3 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateOffer lenderCred2 [asset]
              , mintTokens = [("Offer",1),(assetBeacon,1),(lenderToken2,1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum offerDatum2
                    , lovelaceValueOf 103_000_000 
                    <> singleton beaconCurrencySymbol "Offer" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol lenderToken2 1
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
            <> singleton beaconCurrencySymbol lenderToken1 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            offerDatum1
  ask2 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Ask" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            askDatum{loanTerm = 22000}
  offer2 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 103_000_000 
            <> singleton beaconCurrencySymbol "Offer" 1
            <> singleton beaconCurrencySymbol lenderToken2 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            offerDatum2

  let loanIdToken1 = genLoanId offer1
  let loanIdToken2 = genLoanId offer2
  let activeDatum1 = ActiveDatum
        { beaconSym = beaconCurrencySymbol 
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr1
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
        , lenderAddress = lenderAddr2
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
                  , (lenderToken1,-1)
                  , (lenderToken2,-1)
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
              { toAddress = lenderAddr1
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,"Accepted") 
                    , lovelaceValueOf 3_000_000
                    <> singleton beaconCurrencySymbol loanIdToken1 1
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = lenderAddr2
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline
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
        { loanOutstanding = loanOutstanding activeDatum1 .-. fromInt 110_000_000
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
              , mintTokens = [(borrowerToken,-1)]
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
              { toAddress = lenderAddr1
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,loanIdToken1)
                    , lovelaceValueOf 110_000_000 
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = lenderAddr2
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,loanIdToken2)
                    , lovelaceValueOf 55_000_000 
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing (Just $ loanExpiration activeDatum1)
      }

  where
    borrowerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
    lenderCred1 = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
    lenderCred2 = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 3

    
    borrowerToken = credentialAsToken borrowerCred
    lenderToken1 = credentialAsToken lenderCred1
    lenderToken2 = credentialAsToken lenderCred2
    
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

    lenderAddr1 = Address lenderCred1 Nothing
    lenderAddr2 = Address lenderCred2 Nothing

    offerDatum1 = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken1
        , lenderAddress = lenderAddr1
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
    offerDatum2 = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken2
        , lenderAddress = lenderAddr2
        , loanAsset = asset
        , loanPrinciple = 100_000_000
        , rolloverFrequency = Nothing
        , minPayment = 500_000
        , loanTerm = 22000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 10_000_000)]
        , claimPeriod = 10000
        , offerDeposit = 3_000_000
        , collateralIsSwappable = True
        }
    
    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | Make a partial payment on a single loan that is using multiple collateral assets. The
-- collateral is not swapped out. None of the collateral had their relative prices set
-- to zero.
regressionTest9 :: EmulatorTrace ()
regressionTest9 = do
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
        , collateralization = 
            [ (testToken1, unsafeRatio 1 10_000_000)
            , (testToken2, unsafeRatio 1 50_000_000)
            ]
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
                    , lovelaceValueOf 4_000_000 
                    <> singleton beaconCurrencySymbol "Active" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol loanIdToken 1
                    <> singleton beaconCurrencySymbol borrowerToken 1
                    <> uncurry singleton testToken1 5
                    <> uncurry singleton testToken2 1
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
            (lovelaceValueOf 4_000_000 
            <> singleton beaconCurrencySymbol "Active" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            <> singleton beaconCurrencySymbol loanIdToken 1
            <> singleton beaconCurrencySymbol borrowerToken 1
            <> uncurry singleton testToken1 5
            <> uncurry singleton testToken2 1
            )
            activeDatum

  let newActiveDatum = activeDatum
        { loanOutstanding = loanOutstanding activeDatum .-. fromInt 55_000_000
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
              , spendUtxos = [ active1 ]
              }
          ]
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum newActiveDatum
                    , lovelaceValueOf 4_000_000 
                    <> singleton beaconCurrencySymbol "Active" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol loanIdToken 1
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
                           $ PaymentDatum (beaconCurrencySymbol,loanIdToken)
                    , lovelaceValueOf 55_000_000 
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing (Just $ loanExpiration activeDatum)
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
      , collateral = [testToken1,testToken2]
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
        , collateralization = 
            [ (testToken1,unsafeRatio 1 10_000_000)
            , (testToken2,unsafeRatio 1 50_000_000)
            ]
        , claimPeriod = 10000
        , offerDeposit = 3_000_000
        , collateralIsSwappable = True
        }
    
    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | Make a partial payment on a single loan that is using multiple collateral assets. The
-- collateral is not swapped out. One of the collateral assets used had their relative price 
-- set to zero.
regressionTest10 :: EmulatorTrace ()
regressionTest10 = do
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
        , collateralization = 
            [ (testToken1, unsafeRatio 0 10_000_000)
            , (testToken2, unsafeRatio 1 50_000_000)
            ]
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
                    , lovelaceValueOf 4_000_000 
                    <> singleton beaconCurrencySymbol "Active" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol loanIdToken 1
                    <> singleton beaconCurrencySymbol borrowerToken 1
                    <> uncurry singleton testToken1 5
                    <> uncurry singleton testToken2 2
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
            (lovelaceValueOf 4_000_000 
            <> singleton beaconCurrencySymbol "Active" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            <> singleton beaconCurrencySymbol loanIdToken 1
            <> singleton beaconCurrencySymbol borrowerToken 1
            <> uncurry singleton testToken1 5
            <> uncurry singleton testToken2 2
            )
            activeDatum

  let newActiveDatum = activeDatum
        { loanOutstanding = loanOutstanding activeDatum .-. fromInt 55_000_000
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
              , spendUtxos = [ active1 ]
              }
          ]
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum newActiveDatum
                    , lovelaceValueOf 4_000_000 
                    <> singleton beaconCurrencySymbol "Active" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol loanIdToken 1
                    <> singleton beaconCurrencySymbol borrowerToken 1
                    <> uncurry singleton testToken2 1
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = lenderAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,loanIdToken)
                    , lovelaceValueOf 55_000_000 
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing (Just $ loanExpiration activeDatum)
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
      , collateral = [testToken1,testToken2]
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
        , collateralization = 
            [ (testToken1,unsafeRatio 0 10_000_000)
            , (testToken2,unsafeRatio 1 50_000_000)
            ]
        , claimPeriod = 10000
        , offerDeposit = 3_000_000
        , collateralIsSwappable = True
        }
    
    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | Make a partial payment on a single loan that is using multiple collateral assets. The
-- collateral is swapped out. None of the collateral had their relative prices set
-- to zero.
regressionTest11 :: EmulatorTrace ()
regressionTest11 = do
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
        , collateralization = 
            [ (testToken1, unsafeRatio 1 10_000_000)
            , (testToken2, unsafeRatio 1 50_000_000)
            , (testToken3, unsafeRatio 1 50_000_000)
            ]
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
                    , lovelaceValueOf 4_000_000 
                    <> singleton beaconCurrencySymbol "Active" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol loanIdToken 1
                    <> singleton beaconCurrencySymbol borrowerToken 1
                    <> uncurry singleton testToken1 5
                    <> uncurry singleton testToken2 1
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
            (lovelaceValueOf 4_000_000 
            <> singleton beaconCurrencySymbol "Active" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            <> singleton beaconCurrencySymbol loanIdToken 1
            <> singleton beaconCurrencySymbol borrowerToken 1
            <> uncurry singleton testToken1 5
            <> uncurry singleton testToken2 1
            )
            activeDatum

  let newActiveDatum = activeDatum
        { loanOutstanding = loanOutstanding activeDatum .-. fromInt 55_000_000
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
              , spendUtxos = [ active1 ]
              }
          ]
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum newActiveDatum
                    , lovelaceValueOf 4_000_000 
                    <> singleton beaconCurrencySymbol "Active" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol loanIdToken 1
                    <> singleton beaconCurrencySymbol borrowerToken 1
                    <> uncurry singleton testToken3 1
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = lenderAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,loanIdToken)
                    , lovelaceValueOf 55_000_000 
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing (Just $ loanExpiration activeDatum)
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
      , collateral = [testToken1,testToken2,testToken3]
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
        , collateralization = 
            [ (testToken1,unsafeRatio 1 10_000_000)
            , (testToken2,unsafeRatio 1 50_000_000)
            , (testToken3,unsafeRatio 1 50_000_000)
            ]
        , claimPeriod = 10000
        , offerDeposit = 3_000_000
        , collateralIsSwappable = True
        }
    
    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | Repay the remaining balance when it is less than the minPayment.
regressionTest12 :: EmulatorTrace ()
regressionTest12 = do
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
        , minPayment = 500_000_000
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
        , minPayment = 500_000_000
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 10_000_000)]
        , claimPeriod = 10000
        , offerDeposit = 3_000_000
        , collateralIsSwappable = True
        }
    
    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | Make a partial payment on a single loan and mint an unrelated token in the same
-- transaction.
regressionTest13 :: EmulatorTrace ()
regressionTest13 = do
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
        { loanOutstanding = loanOutstanding activeDatum .-. fromInt 55_000_000
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
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing 
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("other",1)]
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
                           $ PaymentDatum (beaconCurrencySymbol,loanIdToken)
                    , lovelaceValueOf 55_000_000 
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing (Just $ loanExpiration activeDatum)
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

-- | Make a partial payment on multiple loans from the same lender. Mint an unrelated
-- token in the same transaction.
regressionTest14 :: EmulatorTrace ()
regressionTest14 = do
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
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing 
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("other",1)]
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

-- | Make a payment after making a rollover. The amount paid is the full balance prior to the
-- rollover. Since the interest accrued during the rollover, it is not longer the full
-- balance.
regressionTest15 :: EmulatorTrace ()
regressionTest15 = do
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
        , rolloverFrequency = Just 4000
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

  void $ waitNSlots 4

  active1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Active" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            <> singleton beaconCurrencySymbol loanIdToken 1
            <> singleton beaconCurrencySymbol borrowerToken 1
            <> uncurry singleton testToken1 10
            )
            activeDatum

  let rolloverDatum = activeDatum
        { lastCheckpoint = 
            lastCheckpoint activeDatum + fromMaybe 0 (rolloverFrequency activeDatum)
        , loanOutstanding = 
            loanOutstanding activeDatum .*. (fromInt 1 .+. loanInterest activeDatum)
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
              , mintTokens = []
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer Rollover
              , spendFromAddress = loanAddr
              , spendUtxos = [ active1 ]
              }
          ]
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum rolloverDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Active" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol loanIdToken 1
                    <> singleton beaconCurrencySymbol borrowerToken 1
                    <> uncurry singleton testToken1 10
                    )
                  ]
              }
          ]
      , validityRange = 
          ValidityInterval 
            Nothing 
            (Just $ loanExpiration rolloverDatum)
      }

  void $ waitNSlots 2

  active1' <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Active" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            <> singleton beaconCurrencySymbol loanIdToken 1
            <> singleton beaconCurrencySymbol borrowerToken 1
            <> uncurry singleton testToken1 10
            )
            rolloverDatum

  let newActiveDatum = rolloverDatum
        { loanOutstanding = loanOutstanding rolloverDatum .-. fromInt 110_000_000
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
              , spendUtxos = [ active1' ]
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
                    <> singleton beaconCurrencySymbol borrowerToken 1
                    <> uncurry singleton testToken1 1
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
      , validityRange = 
          ValidityInterval 
            Nothing 
            (Just $ lastCheckpoint rolloverDatum + fromMaybe 0 (rolloverFrequency rolloverDatum))
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
        , rolloverFrequency = Just 4000
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
-- | The loan is expired.
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
      , validityRange = ValidityInterval Nothing (Just $ slotToBeginPOSIXTime def 50)
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

-- | The next rollover is required.
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

-- | New collateral datum beaconSym changed.
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
        , beaconSym = adaSymbol
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

-- | New collateral datum borrowerId changed.
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
        , borrowerId = ""
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

-- | New collateral datum lender address changed.
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
        , lenderAddress = loanAddr
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

-- | New collateral datum loan asset changed.
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
        , loanAsset = testToken1
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

-- | New collateral datum loan principle changed.
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
        , loanPrinciple = 0
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

-- | New collateral datum rolloverFrequency changed.
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
        , rolloverFrequency = Just 100
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

-- | New collateral datum lastCheckpoint changed.
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
        , lastCheckpoint = 1000000
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

-- | New collateral datum loanTerm changed.
failureTest10 :: EmulatorTrace ()
failureTest10 = do
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
        , loanTerm = 0
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

-- | New collateral datum loanInterest changed.
failureTest11 :: EmulatorTrace ()
failureTest11 = do
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
        , loanInterest = unsafeRatio 1 1
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

-- | New collateral datum minPayment changed.
failureTest12 :: EmulatorTrace ()
failureTest12 = do
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
        , minPayment = 1_000_000
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

-- | New collateral datum collateralization changed.
failureTest13 :: EmulatorTrace ()
failureTest13 = do
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
        , collateralization = []
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

-- | New collateral datum collateralIsSwappable changed.
failureTest14 :: EmulatorTrace ()
failureTest14 = do
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
        , collateralIsSwappable = False
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

-- | New collateral datum claimExpiration changed.
failureTest15 :: EmulatorTrace ()
failureTest15 = do
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
        , claimExpiration = 0
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

-- | New collateral datum loanExpiration changed.
failureTest16 :: EmulatorTrace ()
failureTest16 = do
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
        , loanExpiration = 0
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

-- | New collateral datum loanId changed.
failureTest17 :: EmulatorTrace ()
failureTest17 = do
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
        , loanId = ""
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

-- | New collateral outstandingBalance does not match payment.
failureTest18 :: EmulatorTrace ()
failureTest18 = do
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
        { loanOutstanding = loanOutstanding activeDatum .-. fromInt 111_000_000
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

-- | BorrowerID withdrawn instead of burned when making a single full payment.
failureTest19 :: EmulatorTrace ()
failureTest19 = do
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
              , mintTokens = [(borrowerToken,0)]
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

-- | Active beacon withdrawn when making a single full payment.
failureTest20 :: EmulatorTrace ()
failureTest20 = do
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

-- | Asset beacon withdrawn when making a single full payment.
failureTest21 :: EmulatorTrace ()
failureTest21 = do
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

-- | LoanID witdrawn when making a single full payment.
failureTest22 :: EmulatorTrace ()
failureTest22 = do
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

-- | LoanID burned when making a full payment.
failureTest23 :: EmulatorTrace ()
failureTest23 = do
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
              , mintTokens = [(borrowerToken,-1),(loanIdToken,-1)]
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

-- | BorrowerID withdrawn when making a single partial payment.
failureTest24 :: EmulatorTrace ()
failureTest24 = do
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
        { loanOutstanding = loanOutstanding activeDatum .-. fromInt 55_000_000
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
                    <> uncurry singleton testToken1 5
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = lenderAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,loanIdToken)
                    , lovelaceValueOf 55_000_000 
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing (Just $ loanExpiration activeDatum)
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

-- | BorrowerID burned when making a single partial payment.
failureTest25 :: EmulatorTrace ()
failureTest25 = do
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
        { loanOutstanding = loanOutstanding activeDatum .-. fromInt 55_000_000
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
                    <> uncurry singleton testToken1 5
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = lenderAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,loanIdToken)
                    , lovelaceValueOf 55_000_000 
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing (Just $ loanExpiration activeDatum)
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

-- | The BorrowerID is still present in the collateral output when making a full payment.
failureTest26 :: EmulatorTrace ()
failureTest26 = do
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
              , mintTokens = [(borrowerToken,0)]
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
                    <> singleton beaconCurrencySymbol borrowerToken 1
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

-- | Mint an unrelated token when burning the BorrowerID from a fully paid loan.
failureTest27 :: EmulatorTrace ()
failureTest27 = do
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
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing 
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("other",1)]
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

-- | Make less than the minimum payment when making a partial payment.
failureTest28 :: EmulatorTrace ()
failureTest28 = do
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
        { loanOutstanding = loanOutstanding activeDatum .-. fromInt 100_000
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
                           $ PaymentDatum (beaconCurrencySymbol,loanIdToken)
                    , lovelaceValueOf 100_000 
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing (Just $ loanExpiration activeDatum)
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

-- | Too much collateral taken when making a partial payment.
failureTest29 :: EmulatorTrace ()
failureTest29 = do
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
        { loanOutstanding = loanOutstanding activeDatum .-. fromInt 55_000_000
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
                    <> singleton beaconCurrencySymbol borrowerToken 1
                    <> uncurry singleton testToken1 2
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = lenderAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,loanIdToken)
                    , lovelaceValueOf 55_000_000 
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing (Just $ loanExpiration activeDatum)
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

-- | The Active beacon is withdrawn when making a partial payment.
failureTest30 :: EmulatorTrace ()
failureTest30 = do
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
        { loanOutstanding = loanOutstanding activeDatum .-. fromInt 55_000_000
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
              , spendUtxos = [ active1 ]
              }
          ]
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum newActiveDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol loanIdToken 1
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
                           $ PaymentDatum (beaconCurrencySymbol,loanIdToken)
                    , lovelaceValueOf 55_000_000 
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing (Just $ loanExpiration activeDatum)
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

-- | The Asset beacon is withdrawn when making a partial payment.
failureTest31 :: EmulatorTrace ()
failureTest31 = do
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
        { loanOutstanding = loanOutstanding activeDatum .-. fromInt 55_000_000
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
                    <> singleton beaconCurrencySymbol loanIdToken 1
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
                           $ PaymentDatum (beaconCurrencySymbol,loanIdToken)
                    , lovelaceValueOf 55_000_000 
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing (Just $ loanExpiration activeDatum)
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

-- | The LoanID is withdrawn when making a partial payment.
failureTest32 :: EmulatorTrace ()
failureTest32 = do
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
        { loanOutstanding = loanOutstanding activeDatum .-. fromInt 55_000_000
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
                           $ PaymentDatum (beaconCurrencySymbol,loanIdToken)
                    , lovelaceValueOf 55_000_000 
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing (Just $ loanExpiration activeDatum)
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

-- | LoanID burned when making a partial payment.
failureTest33 :: EmulatorTrace ()
failureTest33 = do
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
        { loanOutstanding = loanOutstanding activeDatum .-. fromInt 55_000_000
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
              , mintTokens = [(loanIdToken,-1)]
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
                           $ PaymentDatum (beaconCurrencySymbol,loanIdToken)
                    , lovelaceValueOf 55_000_000 
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing (Just $ loanExpiration activeDatum)
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

-- | Collateral output sent to wrong address.
failureTest34 :: EmulatorTrace ()
failureTest34 = do
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
              { toAddress = refScriptAddress
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

-- | Make the payment to the wrong lender address.
failureTest35 :: EmulatorTrace ()
failureTest35 = do
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
              { toAddress = refScriptAddress
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

-- | Borrower did not approve.
failureTest36 :: EmulatorTrace ()
failureTest36 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  h3 <- activateContractWallet (knownWallet 3) endpoints
  
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

  callEndpoint @"create-transaction" h3 $
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

-- | The input has an ActiveDatum but is missing the beacons.
failureTest37 :: EmulatorTrace ()
failureTest37 = do
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
  
  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            )
            askDatum

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = [ ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer MakePayment
              , spendFromAddress = loanAddr
              , spendUtxos = [ ask1 ]
              }
          ]
      , outputs = [ ]
      , validityRange = ValidityInterval Nothing (Just $ loanExpiration askDatum)
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
    
    newAddr = Address (ScriptCredential proxyValidatorHash) (Just $ StakingHash lenderCred)

-- | The input is a finished loan.
failureTest38 :: EmulatorTrace ()
failureTest38 = do
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

  active1' <- txOutRefWithValueAndDatum 
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
              , mintTokens = [(borrowerToken,0)]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer MakePayment
              , spendFromAddress = loanAddr
              , spendUtxos = [ active1' ]
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

-- | The collateral output datum is not an inline datum.
failureTest39 :: EmulatorTrace ()
failureTest39 = do
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
                  [ ( Just $ TxOutDatumHash $ toDatum newActiveDatum
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

-- | The payment datum is not an inline datum.
failureTest40 :: EmulatorTrace ()
failureTest40 = do
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
                  [ ( Just $ TxOutDatumHash
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,loanIdToken)
                    , lovelaceValueOf 110_000_000 
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing (Just $ loanExpiration activeDatum)
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

-- | Mix up the LoanIDs in the collateral outputs when making multiple payments.
failureTest41 :: EmulatorTrace ()
failureTest41 = do
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
        { loanOutstanding = loanOutstanding activeDatum1 .-. fromInt 110_000_000
        }

  let newActiveDatum2 = activeDatum2
        { loanOutstanding = loanOutstanding activeDatum2 .-. fromInt 110_000_000
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
              , mintTokens = [(borrowerToken,-2)]
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
                    <> singleton beaconCurrencySymbol loanIdToken2 1
                    )
                  , ( Just $ TxOutDatumInline $ toDatum newActiveDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Active" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol loanIdToken1 1
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = lenderAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,loanIdToken1)
                    , lovelaceValueOf 110_000_000 
                    )
                  , ( Just $ TxOutDatumInline 
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,loanIdToken2)
                    , lovelaceValueOf 110_000_000 
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing (Just $ loanExpiration activeDatum1)
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

-- | The first collateral output is incorrect when making multiple payments. This test and
-- `failureTest43` are to explicitly test that the order of collateral outputs does not
-- impact the transaction's validity.
failureTest42 :: EmulatorTrace ()
failureTest42 = do
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
        { loanOutstanding = loanOutstanding activeDatum1 .-. fromInt 110_000_000
        }

  let newActiveDatum2 = activeDatum2
        { loanOutstanding = loanOutstanding activeDatum2 .-. fromInt 110_000_000
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
              , mintTokens = [(borrowerToken,-2)]
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
                    <> singleton beaconCurrencySymbol loanIdToken1 1
                    )
                  , ( Just $ TxOutDatumInline $ toDatum newActiveDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Active" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol loanIdToken2 1
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = lenderAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,loanIdToken1)
                    , lovelaceValueOf 110_000_000 
                    )
                  , ( Just $ TxOutDatumInline 
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,loanIdToken2)
                    , lovelaceValueOf 110_000_000 
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing (Just $ loanExpiration activeDatum1)
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

-- | The second collateral output is incorrect when making multiple payments. This test and
-- `failureTest42` are to explicitly test that the order of collateral outputs does not
-- impact the transaction's validity.
failureTest43 :: EmulatorTrace ()
failureTest43 = do
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
        { loanOutstanding = loanOutstanding activeDatum1 .-. fromInt 110_000_000
        }

  let newActiveDatum2 = activeDatum2
        { loanOutstanding = loanOutstanding activeDatum2 .-. fromInt 110_000_000
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
              , mintTokens = [(borrowerToken,-2)]
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
                    )
                  , ( Just $ TxOutDatumInline $ toDatum newActiveDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Active" 1
                    <> singleton beaconCurrencySymbol loanIdToken2 1
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = lenderAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,loanIdToken1)
                    , lovelaceValueOf 110_000_000 
                    )
                  , ( Just $ TxOutDatumInline 
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,loanIdToken2)
                    , lovelaceValueOf 110_000_000 
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing (Just $ loanExpiration activeDatum1)
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

-- | The first payment output is incorrect when making multiple payments. This test and
-- `failureTest45` are to explicitly test that the order of payment outputs does not
-- impact the transaction's validity.
failureTest44 :: EmulatorTrace ()
failureTest44 = do
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
        { loanOutstanding = loanOutstanding activeDatum1 .-. fromInt 110_000_000
        }

  let newActiveDatum2 = activeDatum2
        { loanOutstanding = loanOutstanding activeDatum2 .-. fromInt 110_000_000
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
              , mintTokens = [(borrowerToken,-2)]
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
                    )
                  , ( Just $ TxOutDatumInline $ toDatum newActiveDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Active" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol loanIdToken2 1
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = lenderAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,"loanIdToken1")
                    , lovelaceValueOf 110_000_000 
                    )
                  , ( Just $ TxOutDatumInline 
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,loanIdToken2)
                    , lovelaceValueOf 110_000_000 
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing (Just $ loanExpiration activeDatum1)
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

-- | The second payment output is incorrect when making multiple payments. This test and
-- `failureTest44` are to explicitly test that the order of payment outputs does not
-- impact the transaction's validity.
failureTest45 :: EmulatorTrace ()
failureTest45 = do
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
        { loanOutstanding = loanOutstanding activeDatum1 .-. fromInt 110_000_000
        }

  let newActiveDatum2 = activeDatum2
        { loanOutstanding = loanOutstanding activeDatum2 .-. fromInt 110_000_000
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
              , mintTokens = [(borrowerToken,-2)]
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
                    )
                  , ( Just $ TxOutDatumInline $ toDatum newActiveDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Active" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol loanIdToken2 1
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = lenderAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,loanIdToken1)
                    , lovelaceValueOf 110_000_000 
                    )
                  , ( Just $ TxOutDatumInline 
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,"loanIdToken2")
                    , lovelaceValueOf 110_000_000 
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing (Just $ loanExpiration activeDatum1)
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

-- | Move the BorrowerID from a finished loan to another active loan UTxO when mixing
-- partial and full payments.
failureTest46 :: EmulatorTrace ()
failureTest46 = do
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
        { loanOutstanding = loanOutstanding activeDatum1 .-. fromInt 110_000_000
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
                    )
                  , ( Just $ TxOutDatumInline $ toDatum newActiveDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Active" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol loanIdToken2 1
                    <> singleton beaconCurrencySymbol borrowerToken 2
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
                    , lovelaceValueOf 110_000_000 
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

-- | Swap out the collateral even though the lender did not agree to it.
failureTest47 :: EmulatorTrace ()
failureTest47 = do
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
        , collateralization = 
            [ (testToken1, unsafeRatio 1 10_000_000)
            , (testToken2, unsafeRatio 1 50_000_000)
            , (testToken3, unsafeRatio 1 50_000_000)
            ]
        , collateralIsSwappable = False
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
                    , lovelaceValueOf 4_000_000 
                    <> singleton beaconCurrencySymbol "Active" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol loanIdToken 1
                    <> singleton beaconCurrencySymbol borrowerToken 1
                    <> uncurry singleton testToken1 5
                    <> uncurry singleton testToken2 1
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
            (lovelaceValueOf 4_000_000 
            <> singleton beaconCurrencySymbol "Active" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            <> singleton beaconCurrencySymbol loanIdToken 1
            <> singleton beaconCurrencySymbol borrowerToken 1
            <> uncurry singleton testToken1 5
            <> uncurry singleton testToken2 1
            )
            activeDatum

  let newActiveDatum = activeDatum
        { loanOutstanding = loanOutstanding activeDatum .-. fromInt 55_000_000
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
              , spendUtxos = [ active1 ]
              }
          ]
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum newActiveDatum
                    , lovelaceValueOf 4_000_000 
                    <> singleton beaconCurrencySymbol "Active" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol loanIdToken 1
                    <> singleton beaconCurrencySymbol borrowerToken 1
                    <> uncurry singleton testToken3 1
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = lenderAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline 
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,loanIdToken)
                    , lovelaceValueOf 55_000_000 
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing (Just $ loanExpiration activeDatum)
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
      , collateral = [testToken1,testToken2,testToken3]
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
        , collateralization = 
            [ (testToken1,unsafeRatio 1 10_000_000)
            , (testToken2,unsafeRatio 1 50_000_000)
            , (testToken3,unsafeRatio 1 50_000_000)
            ]
        , claimPeriod = 10000
        , offerDeposit = 3_000_000
        , collateralIsSwappable = False
        }
    
    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | The payment datum is not the right type.
failureTest48 :: EmulatorTrace ()
failureTest48 = do
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
                           $ toDatum ()
                    , lovelaceValueOf 110_000_000 
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing (Just $ loanExpiration activeDatum)
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

-- | The collateral datum is not an ActiveDatum, it is one of the other phase datums.
failureTest49 :: EmulatorTrace ()
failureTest49 = do
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
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum
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

-- | The collateral datum is not an ActiveDatum. It is an entirely different kind of datum.
failureTest50 :: EmulatorTrace ()
failureTest50 = do
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
                  [ ( Just $ TxOutDatumInline $ toDatum ()
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
-- Edge Cases
-------------------------------------------------
-- | Accept a loan and make a partial payment on another loan in the same transaction. This
-- will fail because the `CreateActive` redeemer does not allow any other inputs from the
-- loan address than those necessary for the acceptance.
edgeCase1 :: EmulatorTrace ()
edgeCase1 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  h3 <- activateContractWallet (knownWallet 3) endpoints
  
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
              , mintRedeemer = toRedeemer $ CreateOffer lenderCred1 [asset]
              , mintTokens = [("Offer",1),(assetBeacon,1),(lenderToken1,1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum offerDatum1
                    , lovelaceValueOf 103_000_000 
                    <> singleton beaconCurrencySymbol "Offer" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol lenderToken1 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  -- Create the Offer UTxO.
  callEndpoint @"create-transaction" h3 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateOffer lenderCred2 [asset]
              , mintTokens = [("Offer",1),(assetBeacon,1),(lenderToken2,1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum offerDatum2
                    , lovelaceValueOf 103_000_000 
                    <> singleton beaconCurrencySymbol "Offer" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol lenderToken2 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  startTime1 <- slotToBeginPOSIXTime def <$> waitNSlots 2
  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Ask" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            askDatum
  offer1 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 103_000_000 
            <> singleton beaconCurrencySymbol "Offer" 1
            <> singleton beaconCurrencySymbol lenderToken1 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            offerDatum1
  ask2 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Ask" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            askDatum{loanTerm = 22000}
  offer2 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 103_000_000 
            <> singleton beaconCurrencySymbol "Offer" 1
            <> singleton beaconCurrencySymbol lenderToken2 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            offerDatum2

  let loanIdToken1 = genLoanId offer1
  let loanIdToken2 = genLoanId offer2
  let activeDatum1 = ActiveDatum
        { beaconSym = beaconCurrencySymbol 
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr1
        , loanAsset = asset
        , loanPrinciple = 100_000_000
        , rolloverFrequency = Nothing
        , lastCheckpoint = startTime1
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , minPayment = 500_000
        , collateralization = [(testToken1, unsafeRatio 1 10_000_000)]
        , collateralIsSwappable = True
        , claimExpiration = startTime1 + 12000 + 10000
        , loanExpiration = startTime1 + 12000
        , loanOutstanding = fromInt 100_000_000 .*. (fromInt 1 .+. unsafeRatio 1 10) 
        , loanId = loanIdToken1
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
                  ]
              , mintTokens = 
                  [ ("Active",1)
                  , (assetBeacon,-1)
                  , (lenderToken1,-1)
                  , (borrowerToken,1)
                  , ("Ask",-1)
                  , ("Offer",-1)
                  , (loanIdToken1,2)
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
                  [ ( Just $ TxOutDatumInline $ toDatum activeDatum1
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Active" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol loanIdToken1 1
                    <> singleton beaconCurrencySymbol borrowerToken 1
                    <> uncurry singleton testToken1 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = lenderAddr1
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,"Accepted") 
                    , lovelaceValueOf 3_000_000
                    <> singleton beaconCurrencySymbol loanIdToken1 1
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
      , validityRange = ValidityInterval (Just startTime1) Nothing
      }

  startTime2 <- slotToBeginPOSIXTime def <$> waitNSlots 2

  active1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Active" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            <> singleton beaconCurrencySymbol loanIdToken1 1
            <> singleton beaconCurrencySymbol borrowerToken 1
            <> uncurry singleton testToken1 10
            )
            activeDatum1

  let newActiveDatum1 = activeDatum1
        { loanOutstanding = loanOutstanding activeDatum1 .-. fromInt 55_000_000
        }

  let activeDatum2 = ActiveDatum
        { beaconSym = beaconCurrencySymbol 
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr2
        , loanAsset = asset
        , loanPrinciple = 100_000_000
        , rolloverFrequency = Nothing
        , lastCheckpoint = startTime2
        , loanTerm = 22000
        , loanInterest = unsafeRatio 1 10
        , minPayment = 500_000
        , collateralization = [(testToken1, unsafeRatio 1 10_000_000)]
        , collateralIsSwappable = True
        , claimExpiration = startTime2 + 22000 + 10000
        , loanExpiration = startTime2 + 22000
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
                  [ (ask2,offer2)
                  ]
              , mintTokens = 
                  [ ("Active",1)
                  , (assetBeacon,-1)
                  , (lenderToken2,-1)
                  , (borrowerToken,1)
                  , ("Ask",-1)
                  , ("Offer",-1)
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
                  [ ask2
                  , offer2
                  ]
              }
          , ScriptUtxoInput
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
                  [ ( Just $ TxOutDatumInline $ toDatum activeDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Active" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol loanIdToken2 1
                    <> singleton beaconCurrencySymbol borrowerToken 1
                    <> uncurry singleton testToken1 10
                    )
                  , ( Just $ TxOutDatumInline $ toDatum newActiveDatum1
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Active" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol loanIdToken1 1
                    <> singleton beaconCurrencySymbol borrowerToken 1
                    <> uncurry singleton testToken1 5
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = lenderAddr2
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,"Accepted") 
                    , lovelaceValueOf 3_000_000
                    <> singleton beaconCurrencySymbol loanIdToken2 1
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = lenderAddr1
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,loanIdToken1) 
                    , lovelaceValueOf 55_000_000
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
      , validityRange = ValidityInterval (Just startTime2) (Just $ loanExpiration activeDatum1)
      }
      
  where
    borrowerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
    lenderCred1 = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
    lenderCred2 = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 3

    
    borrowerToken = credentialAsToken borrowerCred
    lenderToken1 = credentialAsToken lenderCred1
    lenderToken2 = credentialAsToken lenderCred2
    
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

    lenderAddr1 = Address lenderCred1 Nothing
    lenderAddr2 = Address lenderCred2 Nothing

    offerDatum1 = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken1
        , lenderAddress = lenderAddr1
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
    offerDatum2 = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken2
        , lenderAddress = lenderAddr2
        , loanAsset = asset
        , loanPrinciple = 100_000_000
        , rolloverFrequency = Nothing
        , minPayment = 500_000
        , loanTerm = 22000
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
-- | Make full payments on multiple loans in the same transaction. Each loan uses one
-- asset for collateral.
benchTest1 :: Int -> EmulatorTrace ()
benchTest1 numberPaid = do
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

  paymentTime <- (+1000) . slotToEndPOSIXTime def <$> waitNSlots 2

  let loanIds = map genLoanId targetOffers

  targetLoans <- 
    mapM (\v -> txOutRefAndDatumWithAssetAtAddress beaconCurrencySymbol v loanAddr) loanIds
  
  let newCollateral =
        map (\(_,Just activeDatum) ->
               ( Just $ TxOutDatumInline 
                      $ toDatum activeDatum{loanOutstanding = fromInt 0}
               , lovelaceValueOf 3_000_000
               <> singleton beaconCurrencySymbol "Active" 1
               <> singleton beaconCurrencySymbol assetBeacon 1
               <> singleton beaconCurrencySymbol (loanId activeDatum) 1
               )  
            )
            targetLoans

  let newPayment =
        map (\(_,Just activeDatum) ->
               ( Just $ TxOutDatumInline 
                      $ toDatum 
                      $ PaymentDatum (beaconCurrencySymbol,loanId activeDatum)
               , lovelaceValueOf 110_000_000
               )  
            )
            targetLoans

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
        [ TokenMint
            { mintWitness = 
                ( beaconMintingPolicy
                , Just (refScriptAddress, mintRef)
                )
            , mintRedeemer = toRedeemer BurnBeacons
            , mintTokens = [(borrowerToken, fromIntegral (-numberPaid))] 
            }
        ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer MakePayment
              , spendFromAddress = loanAddr
              , spendUtxos = take numberPaid $ map fst targetLoans
              }
          ]
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = take numberPaid newCollateral
              }
          , UtxoOutput
              { toAddress = lenderAddr
              , outputUtxos = take numberPaid newPayment
              }
          ]
      , validityRange = ValidityInterval Nothing (Just paymentTime)
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

    lenderToken = credentialAsToken lenderCred

    borrowerToken = credentialAsToken borrowerCred

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

-- | Make partial payments on multiple loans in the same transaction. Each loan uses one
-- asset for collateral. Half of each loan was paid off and half of the collateral was
-- reclaimed.
benchTest2 :: Int -> EmulatorTrace ()
benchTest2 numberPaid = do
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

  paymentTime <- (+1000) . slotToEndPOSIXTime def <$> waitNSlots 2

  let loanIds = map genLoanId targetOffers

  targetLoans <- 
    mapM (\v -> txOutRefAndDatumWithAssetAtAddress beaconCurrencySymbol v loanAddr) loanIds
  
  let newCollateral =
        map (\(_,Just activeDatum) ->
               ( Just $ TxOutDatumInline 
                      $ toDatum activeDatum{loanOutstanding = fromInt 55_000_000}
               , lovelaceValueOf 3_000_000
               <> singleton beaconCurrencySymbol "Active" 1
               <> singleton beaconCurrencySymbol assetBeacon 1
               <> singleton beaconCurrencySymbol (loanId activeDatum) 1
               <> singleton beaconCurrencySymbol borrowerToken 1
               <> uncurry singleton testToken1 5
               )  
            )
            targetLoans

  let newPayment =
        map (\(_,Just activeDatum) ->
               ( Just $ TxOutDatumInline 
                      $ toDatum 
                      $ PaymentDatum (beaconCurrencySymbol,loanId activeDatum)
               , lovelaceValueOf 55_000_000
               )  
            )
            targetLoans

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
        [ TokenMint
            { mintWitness = 
                ( beaconMintingPolicy
                , Just (refScriptAddress, mintRef)
                )
            , mintRedeemer = toRedeemer BurnBeacons
            , mintTokens = [(borrowerToken, 0)] 
            }
        ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer MakePayment
              , spendFromAddress = loanAddr
              , spendUtxos = take numberPaid $ map fst targetLoans
              }
          ]
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = take numberPaid newCollateral
              }
          , UtxoOutput
              { toAddress = lenderAddr
              , outputUtxos = take numberPaid newPayment
              }
          ]
      , validityRange = ValidityInterval Nothing (Just paymentTime)
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

    lenderToken = credentialAsToken lenderCred

    borrowerToken = credentialAsToken borrowerCred

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

-- | Make partial payments on multiple loans in the same transaction. Each loan uses three
-- assets for collateral. Half of each loan was paid off and half of the collateral was
-- reclaimed.
benchTest3 :: Int -> EmulatorTrace ()
benchTest3 numberPaid = do
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

  paymentTime <- (+1000) . slotToEndPOSIXTime def <$> waitNSlots 2

  let loanIds = map genLoanId targetOffers

  targetLoans <- 
    mapM (\v -> txOutRefAndDatumWithAssetAtAddress beaconCurrencySymbol v loanAddr) loanIds

  let newCollateral =
        map (\(_,Just activeDatum) ->
               ( Just $ TxOutDatumInline 
                      $ toDatum activeDatum{loanOutstanding = fromInt 55_000_000}
               , lovelaceValueOf 4_000_000
               <> singleton beaconCurrencySymbol "Active" 1
               <> singleton beaconCurrencySymbol assetBeacon 1
               <> singleton beaconCurrencySymbol (loanId activeDatum) 1
               <> singleton beaconCurrencySymbol borrowerToken 1
               <> uncurry singleton testToken1 2
               <> uncurry singleton testToken2 2
               <> uncurry singleton testToken3 1
               )  
            )
            targetLoans

  let newPayment =
        map (\(_,Just activeDatum) ->
               ( Just $ TxOutDatumInline 
                      $ toDatum 
                      $ PaymentDatum (beaconCurrencySymbol,loanId activeDatum)
               , lovelaceValueOf 55_000_000
               )  
            )
            targetLoans

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
        [ TokenMint
            { mintWitness = 
                ( beaconMintingPolicy
                , Just (refScriptAddress, mintRef)
                )
            , mintRedeemer = toRedeemer BurnBeacons
            , mintTokens = [(borrowerToken, 0)] 
            }
        ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer MakePayment
              , spendFromAddress = loanAddr
              , spendUtxos = take numberPaid $ map fst targetLoans
              }
          ]
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = take numberPaid newCollateral
              }
          , UtxoOutput
              { toAddress = lenderAddr
              , outputUtxos = take numberPaid newPayment
              }
          ]
      , validityRange = ValidityInterval Nothing (Just paymentTime)
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

    lenderToken = credentialAsToken lenderCred

    borrowerToken = credentialAsToken borrowerCred

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
        , rolloverFrequency = Nothing
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

-- | Make full payments on multiple loans in the same transaction. Each loan uses three
-- assets for collateral. 
benchTest4 :: Int -> EmulatorTrace ()
benchTest4 numberPaid = do
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

  paymentTime <- (+1000) . slotToEndPOSIXTime def <$> waitNSlots 2

  let loanIds = map genLoanId targetOffers

  targetLoans <- 
    mapM (\v -> txOutRefAndDatumWithAssetAtAddress beaconCurrencySymbol v loanAddr) loanIds

  let newCollateral =
        map (\(_,Just activeDatum) ->
               ( Just $ TxOutDatumInline 
                      $ toDatum activeDatum{loanOutstanding = fromInt 0}
               , lovelaceValueOf 4_000_000
               <> singleton beaconCurrencySymbol "Active" 1
               <> singleton beaconCurrencySymbol assetBeacon 1
               <> singleton beaconCurrencySymbol (loanId activeDatum) 1
               )  
            )
            targetLoans

  let newPayment =
        map (\(_,Just activeDatum) ->
               ( Just $ TxOutDatumInline 
                      $ toDatum 
                      $ PaymentDatum (beaconCurrencySymbol,loanId activeDatum)
               , lovelaceValueOf 110_000_000
               )  
            )
            targetLoans

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
        [ TokenMint
            { mintWitness = 
                ( beaconMintingPolicy
                , Just (refScriptAddress, mintRef)
                )
            , mintRedeemer = toRedeemer BurnBeacons
            , mintTokens = [(borrowerToken, fromIntegral (-numberPaid))] 
            }
        ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer MakePayment
              , spendFromAddress = loanAddr
              , spendUtxos = take numberPaid $ map fst targetLoans
              }
          ]
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = take numberPaid newCollateral
              }
          , UtxoOutput
              { toAddress = lenderAddr
              , outputUtxos = take numberPaid newPayment
              }
          ]
      , validityRange = ValidityInterval Nothing (Just paymentTime)
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

    lenderToken = credentialAsToken lenderCred

    borrowerToken = credentialAsToken borrowerCred

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
        , rolloverFrequency = Nothing
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

benchTrace :: Int -> IO ()
benchTrace = runEmulatorTraceIO' def emConfig . benchTest3

-------------------------------------------------
-- Test Function
-------------------------------------------------
-- | A `TestTree` containing all `MakePayment` scenarios.
tests :: TestTree
tests = do
  let opts = defaultCheckOptions & emulatorConfig .~ emConfig
  testGroup "Make Payment(s)"
    [ -- Success Tests (Regression Tests)
      checkPredicateOptions opts "regressionTest1"
        assertNoFailedTransactions regressionTest1
    , checkPredicateOptions opts "regressionTest2"
        assertNoFailedTransactions regressionTest2
    , checkPredicateOptions opts "regressionTest3"
        assertNoFailedTransactions regressionTest3
    , checkPredicateOptions opts "regressionTest4"
        assertNoFailedTransactions regressionTest4
    , checkPredicateOptions opts "regressionTest5"
        assertNoFailedTransactions regressionTest5
    , checkPredicateOptions opts "regressionTest6"
        assertNoFailedTransactions regressionTest6
    , checkPredicateOptions opts "regressionTest7"
        assertNoFailedTransactions regressionTest7
    , checkPredicateOptions opts "regressionTest8"
        assertNoFailedTransactions regressionTest8
    , checkPredicateOptions opts "regressionTest9"
        assertNoFailedTransactions regressionTest9
    , checkPredicateOptions opts "regressionTest10"
        assertNoFailedTransactions regressionTest10
    , checkPredicateOptions opts "regressionTest11"
        assertNoFailedTransactions regressionTest11
    , checkPredicateOptions opts "regressionTest12"
        assertNoFailedTransactions regressionTest12
    , checkPredicateOptions opts "regressionTest13"
        assertNoFailedTransactions regressionTest13
    , checkPredicateOptions opts "regressionTest14"
        assertNoFailedTransactions regressionTest14
    , checkPredicateOptions opts "regressionTest15"
        assertNoFailedTransactions regressionTest15

      -- Failure Tests
    , checkPredicateOptions opts "failureTest1"
        (assertEvaluationError "Loan is expired") failureTest1
    , checkPredicateOptions opts "failureTest2"
        (assertEvaluationError "Next rollover is required") failureTest2
    , checkPredicateOptions opts "failureTest3"
        (assertEvaluationError "Collateral output has wrong datum") failureTest3
    , checkPredicateOptions opts "failureTest4"
        (assertEvaluationError "Collateral output has wrong datum") failureTest4
    , checkPredicateOptions opts "failureTest5"
        (assertEvaluationError "Collateral output has wrong datum") failureTest5
    , checkPredicateOptions opts "failureTest6"
        (assertEvaluationError "Collateral output has wrong datum") failureTest6
    , checkPredicateOptions opts "failureTest7"
        (assertEvaluationError "Collateral output has wrong datum") failureTest7
    , checkPredicateOptions opts "failureTest8"
        (assertEvaluationError "Collateral output has wrong datum") failureTest8
    , checkPredicateOptions opts "failureTest9"
        (assertEvaluationError "Collateral output has wrong datum") failureTest9
    , checkPredicateOptions opts "failureTest10"
        (assertEvaluationError "Collateral output has wrong datum") failureTest10
    , checkPredicateOptions opts "failureTest11"
        (assertEvaluationError "Collateral output has wrong datum") failureTest11
    , checkPredicateOptions opts "failureTest12"
        (assertEvaluationError "Collateral output has wrong datum") failureTest12
    , checkPredicateOptions opts "failureTest13"
        (assertEvaluationError "Collateral output has wrong datum") failureTest13
    , checkPredicateOptions opts "failureTest14"
        (assertEvaluationError "Collateral output has wrong datum") failureTest14
    , checkPredicateOptions opts "failureTest15"
        (assertEvaluationError "Collateral output has wrong datum") failureTest15
    , checkPredicateOptions opts "failureTest16"
        (assertEvaluationError "Collateral output has wrong datum") failureTest16
    , checkPredicateOptions opts "failureTest17"
        (assertEvaluationError "Collateral output has wrong datum") failureTest17
    , checkPredicateOptions opts "failureTest18"
        (assertEvaluationError "Collateral output has wrong datum") failureTest18
    , checkPredicateOptions opts "failureTest19"
        (assertEvaluationError "BorrowerID not stored with a valid Active UTxO") failureTest19
    , checkPredicateOptions opts "failureTest20"
        (assertEvaluationError "Collateral output must have 1 Active beacon") failureTest20
    , checkPredicateOptions opts "failureTest21"
        (assertEvaluationError "Collateral output must have 1 Asset beacon") failureTest21
    , checkPredicateOptions opts "failureTest22"
        (assertEvaluationError "LoanID sent to wrong address") failureTest22
    , checkPredicateOptions opts "failureTest23"
        (assertEvaluationError "Collateral output not found") failureTest23
    , checkPredicateOptions opts "failureTest24"
        (assertEvaluationError "BorrowerID not stored with a valid Active UTxO") failureTest24
    , checkPredicateOptions opts "failureTest25"
        (assertEvaluationError "Collateral output must have one BorrowerID") failureTest25
    , checkPredicateOptions opts "failureTest26"
        (assertEvaluationError "BorrowerID from fully paid loan must be burned") failureTest26
    , checkPredicateOptions opts "failureTest27"
        (assertEvaluationError "BorrowerIDs must be burned in isolation") failureTest27
    , checkPredicateOptions opts "failureTest28"
        (assertEvaluationError "Minimum payment not met") failureTest28
    , checkPredicateOptions opts "failureTest29"
        (assertEvaluationError "Too much collateral taken") failureTest29
    , checkPredicateOptions opts "failureTest30"
        (assertEvaluationError "Collateral output must have 1 Active beacon") failureTest30
    , checkPredicateOptions opts "failureTest31"
        (assertEvaluationError "Collateral output must have 1 Asset beacon") failureTest31
    , checkPredicateOptions opts "failureTest32"
        (assertEvaluationError "LoanID sent to wrong address") failureTest32
    , checkPredicateOptions opts "failureTest33"
        (assertEvaluationError "Collateral output not found") failureTest33
    , checkPredicateOptions opts "failureTest34"
        (assertEvaluationError "LoanID sent to wrong address") failureTest34
    , checkPredicateOptions opts "failureTest35"
        (assertEvaluationError "Collateral output has wrong datum") failureTest35
    , checkPredicateOptions opts "failureTest36"
        (assertEvaluationError "Borrower did not approve") failureTest36
    , checkPredicateOptions opts "failureTest37"
        (assertEvaluationError "Input missing Active beacon") failureTest37
    , checkPredicateOptions opts "failureTest38"
        (assertEvaluationError "Input missing BorrowerID") failureTest38
    , checkPredicateOptions opts "failureTest39"
        (assertEvaluationError "All datums must be inline datums") failureTest39
    , checkPredicateOptions opts "failureTest40"
        (assertEvaluationError "All datums must be inline datums") failureTest40
    , checkPredicateOptions opts "failureTest41"
        (assertEvaluationError "Collateral output has wrong datum") failureTest41
    , checkPredicateOptions opts "failureTest42"
        (assertEvaluationError "Collateral output must have 1 Asset beacon") failureTest42
    , checkPredicateOptions opts "failureTest43"
        (assertEvaluationError "Collateral output must have 1 Asset beacon") failureTest43
    , checkPredicateOptions opts "failureTest44"
        (assertEvaluationError "Collateral output has wrong datum") failureTest44
    , checkPredicateOptions opts "failureTest45"
        (assertEvaluationError "Collateral output has wrong datum") failureTest45
    , checkPredicateOptions opts "failureTest46"
        (assertEvaluationError "1 BorrowerID per loan") failureTest46
    , checkPredicateOptions opts "failureTest47"
        (assertEvaluationError "Collateral cannot be deposited/swapped") failureTest47
    , checkPredicateOptions opts "failureTest48"
        (Test.not assertNoFailedTransactions) failureTest48
    , checkPredicateOptions opts "failureTest49"
        (assertEvaluationError "Collateral output has wrong datum") failureTest49
    , checkPredicateOptions opts "failureTest50"
        (Test.not assertNoFailedTransactions) failureTest50

      -- Edge Cases
    , checkPredicateOptions opts "edgeCase1"
        (Test.not assertNoFailedTransactions) edgeCase1

      -- Benchmarks
    , checkPredicateOptions opts "benchTest1"
        assertNoFailedTransactions $ benchTest1 7
    , checkPredicateOptions opts "benchTest2"
        assertNoFailedTransactions $ benchTest2 6
    , checkPredicateOptions opts "benchTest3"
        assertNoFailedTransactions $ benchTest3 5
    , checkPredicateOptions opts "benchTest4"
        assertNoFailedTransactions $ benchTest4 6
    ]

testTrace :: IO ()
testTrace = runEmulatorTraceIO' def emConfig regressionTest2

