{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-ambiguous-fields #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | The emulator is currently unable to test staking validators. However, the logic of the 
-- staking execution for the script is identical to the minting policy execution so this
-- is not an obstacle. Once the emulator is able to test staking validators, this module will
-- be finished. For now, this module uses the minting execution just to get the script
-- to be executed by the emulator.
module Test.UpdateOffer
  ( 
    -- * Scenarios Tested
    -- ** Scenarios that should succeed
    regressionTest1
  , regressionTest2
  , regressionTest3

    -- ** Scenarios that should fail
    
    -- ** Benchmark Tests
  -- , benchTest1
  -- , benchTest2
  -- , benchTest3
    
    -- * Full TestTree
  , tests
  ) where

import qualified Ledger.Value.CardanoAPI as LV
import qualified Ledger.Address as LA
import qualified PlutusLedgerApi.V2 as PV2
import qualified Ledger.CardanoWallet as Mock 
import Test.Tasty (TestTree,testGroup)
import Data.String (fromString)
import Control.Monad (forM_,replicateM_)

import CardanoLoans

import Test.Prelude

-------------------------------------------------
-- Regression Tests
-------------------------------------------------
-- | Convert a single valid Offer UTxO to one that uses a different loan asset. 
regressionTest1 :: MonadEmulator m => m ()
regressionTest1 = do
  let -- Borrower Info
      borrowerWallet = Mock.knownMockWallet 1
      -- borrowerPersonalAddr = Mock.mockWalletAddress borrowerWallet
      -- borrowerPayPrivKey = Mock.paymentPrivateKey borrowerWallet
      borrowerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash borrowerWallet
      borrowerCred = PV2.PubKeyCredential borrowerPubKey
      loanAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash loanScript) 
                    (Just $ PV2.StakingHash borrowerCred)

      -- Lender Info
      lenderWallet = Mock.knownMockWallet 1
      lenderPersonalAddr = Mock.mockWalletAddress lenderWallet
      lenderPayPrivKey = Mock.paymentPrivateKey lenderWallet
      lenderPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash lenderWallet
      lenderCred = PV2.PubKeyCredential lenderPubKey
      lenderBeacon = genLenderID lenderCred
      lenderAddr = 
        PV2.Address (PV2.ScriptCredential $ scriptHash proxyScript) 
                    (Just $ PV2.StakingHash lenderCred)

      -- Loan Info
      loanAsset1 = Asset (testTokenSymbol,"TestToken2")
      loanAsset2 = Asset ("","")
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      loanBeacon1 = genLoanAssetBeaconName loanAsset1
      loanBeacon2 = genLoanAssetBeaconName loanAsset2
      loanDatum1 = unsafeCreateOfferDatum $ NewOfferInfo
        { lenderId = lenderCred
        , lenderAddress = lenderAddr
        , loanAsset = loanAsset1
        , loanPrinciple = 10
        , rolloverFrequency = Nothing
        , loanTerm = 3600
        , loanInterest = Fraction (1,10)
        , minPayment = 0
        , collateralization = [(collateral1,Fraction(1,1))]
        , collateralIsSwappable = False
        , claimPeriod = 3600
        , offerDeposit = 4_000_000
        , offerExpiration = Nothing
        }
      loanDatum2 = unsafeCreateOfferDatum $ NewOfferInfo
        { lenderId = lenderCred
        , lenderAddress = lenderAddr
        , loanAsset = loanAsset2
        , loanPrinciple = 10_000_000
        , rolloverFrequency = Nothing
        , loanTerm = 3600
        , loanInterest = Fraction (1,10)
        , minPayment = 0
        , collateralization = [(collateral1,Fraction(1,1))]
        , collateralIsSwappable = False
        , claimPeriod = 3600
        , offerDeposit = 4_000_000
        , offerExpiration = Nothing
        }

  -- Initialize scenario
  (negotiationRef,_,loanRef,_) <- initializeReferenceScripts 
  mintTestTokens lenderWallet 10_000_000 [("TestToken2",1000)]

  -- Create the Offer UTxO.
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",1)
                  , (unAssetBeacon loanBeacon1,1)
                  , (unLenderID lenderBeacon,1)
                  ]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateOffer lenderCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (unAssetBeacon loanBeacon1) 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (unLenderID lenderBeacon) 1
                  , uncurry PV2.singleton (unAsset loanAsset1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

  offerRef <- 
    txOutRefWithValue $ 
      utxoValue 4_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (unAssetBeacon loanBeacon1) 1
        , PV2.singleton negotiationBeaconCurrencySymbol (unLenderID lenderBeacon) 1
        , uncurry PV2.singleton (unAsset loanAsset1) 10
        ]

  -- Try to convert the Offer UTxO.
  void $ transact lenderPersonalAddr [loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(unAssetBeacon loanBeacon1,-1),(unAssetBeacon loanBeacon2,1)]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateOffer lenderCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , inputs =
          [ Input
              { inputId = offerRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer CloseOrUpdateOffer)
              }
          ]
      , outputs =
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (unAssetBeacon loanBeacon2) 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (unLenderID lenderBeacon) 1
                  , uncurry PV2.singleton (unAsset loanAsset2) 10_000_000
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum2
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef,loanRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

-- | Convert multiple valid Offer UTxOs to ones that uses different loan assets. The offers
-- start and end with the same loan asset.
regressionTest2 :: MonadEmulator m => m ()
regressionTest2 = do
  let -- Borrower Info
      borrowerWallet = Mock.knownMockWallet 1
      -- borrowerPersonalAddr = Mock.mockWalletAddress borrowerWallet
      -- borrowerPayPrivKey = Mock.paymentPrivateKey borrowerWallet
      borrowerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash borrowerWallet
      borrowerCred = PV2.PubKeyCredential borrowerPubKey
      loanAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash loanScript) 
                    (Just $ PV2.StakingHash borrowerCred)

      -- Lender Info
      lenderWallet = Mock.knownMockWallet 1
      lenderPersonalAddr = Mock.mockWalletAddress lenderWallet
      lenderPayPrivKey = Mock.paymentPrivateKey lenderWallet
      lenderPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash lenderWallet
      lenderCred = PV2.PubKeyCredential lenderPubKey
      lenderBeacon = genLenderID lenderCred
      lenderAddr = 
        PV2.Address (PV2.ScriptCredential $ scriptHash proxyScript) 
                    (Just $ PV2.StakingHash lenderCred)

      -- Loan Info
      loanAsset1 = Asset (testTokenSymbol,"TestToken2")
      loanAsset2 = Asset ("","")
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      loanBeacon1 = genLoanAssetBeaconName loanAsset1
      loanBeacon2 = genLoanAssetBeaconName loanAsset2
      loanDatum1 = unsafeCreateOfferDatum $ NewOfferInfo
        { lenderId = lenderCred
        , lenderAddress = lenderAddr
        , loanAsset = loanAsset1
        , loanPrinciple = 10
        , rolloverFrequency = Nothing
        , loanTerm = 3600
        , loanInterest = Fraction (1,10)
        , minPayment = 0
        , collateralization = [(collateral1,Fraction(1,1))]
        , collateralIsSwappable = False
        , claimPeriod = 3600
        , offerDeposit = 4_000_000
        , offerExpiration = Nothing
        }
      loanDatum2 = unsafeCreateOfferDatum $ NewOfferInfo
        { lenderId = lenderCred
        , lenderAddress = lenderAddr
        , loanAsset = loanAsset2
        , loanPrinciple = 10_000_000
        , rolloverFrequency = Nothing
        , loanTerm = 3600
        , loanInterest = Fraction (1,10)
        , minPayment = 0
        , collateralization = [(collateral1,Fraction(1,1))]
        , collateralIsSwappable = False
        , claimPeriod = 3600
        , offerDeposit = 4_000_000
        , offerExpiration = Nothing
        }

  -- Initialize scenario
  (negotiationRef,_,loanRef,_) <- initializeReferenceScripts 
  mintTestTokens lenderWallet 10_000_000 [("TestToken2",1000)]

  -- Create the Offer UTxO.
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",2)
                  , (unAssetBeacon loanBeacon1,2)
                  , (unLenderID lenderBeacon,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateOffer lenderCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (unAssetBeacon loanBeacon1) 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (unLenderID lenderBeacon) 1
                  , uncurry PV2.singleton (unAsset loanAsset1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (unAssetBeacon loanBeacon1) 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (unLenderID lenderBeacon) 1
                  , uncurry PV2.singleton (unAsset loanAsset1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

  offerUTxOs <- txOutRefsAndDatumsAtAddress @OfferDatum loanAddress

  -- Try to convert the Offer UTxO.
  void $ transact lenderPersonalAddr [loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(unAssetBeacon loanBeacon1,-2),(unAssetBeacon loanBeacon2,2)]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateOffer lenderCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , inputs = flip map offerUTxOs $ \(offerRef,_) ->
          Input
            { inputId = offerRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer CloseOrUpdateOffer)
            }
      , outputs =
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (unAssetBeacon loanBeacon2) 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (unLenderID lenderBeacon) 1
                  , uncurry PV2.singleton (unAsset loanAsset2) 10_000_000
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum2
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (unAssetBeacon loanBeacon2) 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (unLenderID lenderBeacon) 1
                  , uncurry PV2.singleton (unAsset loanAsset2) 10_000_000
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum2
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef,loanRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

-- | Update the terms for a single valid Offer UTxO. The loan asset does not change.
regressionTest3 :: MonadEmulator m => m ()
regressionTest3 = do
  let -- Borrower Info
      borrowerWallet = Mock.knownMockWallet 1
      -- borrowerPersonalAddr = Mock.mockWalletAddress borrowerWallet
      -- borrowerPayPrivKey = Mock.paymentPrivateKey borrowerWallet
      borrowerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash borrowerWallet
      borrowerCred = PV2.PubKeyCredential borrowerPubKey
      loanAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash loanScript) 
                    (Just $ PV2.StakingHash borrowerCred)

      -- Lender Info
      lenderWallet = Mock.knownMockWallet 1
      lenderPersonalAddr = Mock.mockWalletAddress lenderWallet
      lenderPayPrivKey = Mock.paymentPrivateKey lenderWallet
      lenderPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash lenderWallet
      lenderCred = PV2.PubKeyCredential lenderPubKey
      lenderBeacon = genLenderID lenderCred
      lenderAddr = 
        PV2.Address (PV2.ScriptCredential $ scriptHash proxyScript) 
                    (Just $ PV2.StakingHash lenderCred)

      -- Loan Info
      loanAsset1 = Asset (testTokenSymbol,"TestToken2")
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      loanBeacon1 = genLoanAssetBeaconName loanAsset1
      loanDatum1 = unsafeCreateOfferDatum $ NewOfferInfo
        { lenderId = lenderCred
        , lenderAddress = lenderAddr
        , loanAsset = loanAsset1
        , loanPrinciple = 10
        , rolloverFrequency = Nothing
        , loanTerm = 3600
        , loanInterest = Fraction (1,10)
        , minPayment = 0
        , collateralization = [(collateral1,Fraction(1,1))]
        , collateralIsSwappable = False
        , claimPeriod = 3600
        , offerDeposit = 4_000_000
        , offerExpiration = Nothing
        }

  -- Initialize scenario
  (negotiationRef,_,loanRef,_) <- initializeReferenceScripts 
  mintTestTokens lenderWallet 10_000_000 [("TestToken2",1000)]

  -- Create the Offer UTxO.
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",1)
                  , (unAssetBeacon loanBeacon1,1)
                  , (unLenderID lenderBeacon,1)
                  ]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateOffer lenderCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (unAssetBeacon loanBeacon1) 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (unLenderID lenderBeacon) 1
                  , uncurry PV2.singleton (unAsset loanAsset1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

  offerRef <- 
    txOutRefWithValue $ 
      utxoValue 4_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (unAssetBeacon loanBeacon1) 1
        , PV2.singleton negotiationBeaconCurrencySymbol (unLenderID lenderBeacon) 1
        , uncurry PV2.singleton (unAsset loanAsset1) 10
        ]

  -- Try to update the Offer UTxO.
  void $ transact lenderPersonalAddr [loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",1)
                  , (unAssetBeacon loanBeacon1,1)
                  , (unLenderID lenderBeacon,1)
                  ]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateOffer lenderCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , inputs =
          [ Input
              { inputId = offerRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer CloseOrUpdateOffer)
              }
          ]
      , outputs =
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (unAssetBeacon loanBeacon1) 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (unLenderID lenderBeacon) 1
                  , uncurry PV2.singleton (unAsset loanAsset1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (unAssetBeacon loanBeacon1) 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (unLenderID lenderBeacon) 1
                  , uncurry PV2.singleton (unAsset loanAsset1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum @OfferDatum loanDatum1{loanTerm = 10}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef,loanRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

-------------------------------------------------
-- Benchmark Tests
-------------------------------------------------
-- | Convert multiple valid Offer UTxOs to ones that uses different loan assets. The offers
-- start and end with the same loan asset.
benchTest1 :: MonadEmulator m => Int -> m ()
benchTest1 number = do
  let -- Borrower Info
      borrowerWallet = Mock.knownMockWallet 1
      -- borrowerPersonalAddr = Mock.mockWalletAddress borrowerWallet
      -- borrowerPayPrivKey = Mock.paymentPrivateKey borrowerWallet
      borrowerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash borrowerWallet
      borrowerCred = PV2.PubKeyCredential borrowerPubKey
      loanAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash loanScript) 
                    (Just $ PV2.StakingHash borrowerCred)

      -- Lender Info
      lenderWallet = Mock.knownMockWallet 1
      lenderPersonalAddr = Mock.mockWalletAddress lenderWallet
      lenderPayPrivKey = Mock.paymentPrivateKey lenderWallet
      lenderPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash lenderWallet
      lenderCred = PV2.PubKeyCredential lenderPubKey
      lenderBeacon = genLenderID lenderCred
      lenderAddr = 
        PV2.Address (PV2.ScriptCredential $ scriptHash proxyScript) 
                    (Just $ PV2.StakingHash lenderCred)

      -- Loan Info
      loanAsset1 = Asset (testTokenSymbol,"TestToken2")
      loanAsset2 = Asset ("","")
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      loanBeacon1 = genLoanAssetBeaconName loanAsset1
      loanBeacon2 = genLoanAssetBeaconName loanAsset2
      loanDatum1 = unsafeCreateOfferDatum $ NewOfferInfo
        { lenderId = lenderCred
        , lenderAddress = lenderAddr
        , loanAsset = loanAsset1
        , loanPrinciple = 10
        , rolloverFrequency = Nothing
        , loanTerm = 3600
        , loanInterest = Fraction (1,10)
        , minPayment = 0
        , collateralization = [(collateral1,Fraction(1,1))]
        , collateralIsSwappable = False
        , claimPeriod = 3600
        , offerDeposit = 4_000_000
        , offerExpiration = Nothing
        }
      loanDatum2 = unsafeCreateOfferDatum $ NewOfferInfo
        { lenderId = lenderCred
        , lenderAddress = lenderAddr
        , loanAsset = loanAsset2
        , loanPrinciple = 10_000_000
        , rolloverFrequency = Nothing
        , loanTerm = 3600
        , loanInterest = Fraction (1,10)
        , minPayment = 0
        , collateralization = [(collateral1,Fraction(1,1))]
        , collateralIsSwappable = False
        , claimPeriod = 3600
        , offerDeposit = 4_000_000
        , offerExpiration = Nothing
        }

  -- Initialize scenario
  (negotiationRef,_,loanRef,_) <- initializeReferenceScripts 
  mintTestTokens lenderWallet 10_000_000 [("TestToken2",1000)]

  -- Create the Offer UTxO.
  replicateM_ 2 $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",20)
                  , (unAssetBeacon loanBeacon1,20)
                  , (unLenderID lenderBeacon,20)
                  ]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateOffer lenderCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs = replicate 20
          Output
            { outputAddress = loanAddress
            , outputValue = utxoValue 4_000_000 $ mconcat
                [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
                , PV2.singleton negotiationBeaconCurrencySymbol (unAssetBeacon loanBeacon1) 1
                , PV2.singleton negotiationBeaconCurrencySymbol (unLenderID lenderBeacon) 1
                , uncurry PV2.singleton (unAsset loanAsset1) 10
                ]
            , outputDatum = OutputDatum $ toDatum loanDatum1
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

  offerUTxOs <- take number <$> txOutRefsAndDatumsAtAddress @OfferDatum loanAddress

  -- Try to convert the Offer UTxOs.
  void $ transact lenderPersonalAddr [loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens = flip map offerUTxOs $ const
          TokenMint
            { mintTokens = [(unAssetBeacon loanBeacon1,-1),(unAssetBeacon loanBeacon2,1)]
            , mintRedeemer = toRedeemer $ CreateCloseOrUpdateOffer lenderCred
            , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
            , mintReference = Just negotiationRef
            }
      , inputs = flip map offerUTxOs $ \(offerRef,_) ->
          Input
            { inputId = offerRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer CloseOrUpdateOffer)
            }
      , outputs = replicate number
          Output
            { outputAddress = loanAddress
            , outputValue = utxoValue 4_000_000 $ mconcat
                [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
                , PV2.singleton negotiationBeaconCurrencySymbol (unAssetBeacon loanBeacon2) 1
                , PV2.singleton negotiationBeaconCurrencySymbol (unLenderID lenderBeacon) 1
                , uncurry PV2.singleton (unAsset loanAsset2) 10_000_000
                ]
            , outputDatum = OutputDatum $ toDatum loanDatum2
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [negotiationRef,loanRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

-- | Convert multiple valid Offer UTxOs. All loans are for different loan assets and converted to
-- new Offer UTxOs that also have different loan assets. 
benchTest2 :: MonadEmulator m => Int -> m ()
benchTest2 number = do
  let -- Borrower Info
      borrowerWallet = Mock.knownMockWallet 1
      -- borrowerPersonalAddr = Mock.mockWalletAddress borrowerWallet
      -- borrowerPayPrivKey = Mock.paymentPrivateKey borrowerWallet
      borrowerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash borrowerWallet
      borrowerCred = PV2.PubKeyCredential borrowerPubKey
      loanAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash loanScript) 
                    (Just $ PV2.StakingHash borrowerCred)

      -- Lender Info
      lenderWallet = Mock.knownMockWallet 1
      lenderPersonalAddr = Mock.mockWalletAddress lenderWallet
      lenderPayPrivKey = Mock.paymentPrivateKey lenderWallet
      lenderPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash lenderWallet
      lenderCred = PV2.PubKeyCredential lenderPubKey
      lenderBeacon = genLenderID lenderCred
      lenderAddr = 
        PV2.Address (PV2.ScriptCredential $ scriptHash proxyScript) 
                    (Just $ PV2.StakingHash lenderCred)

      -- Other Info
      assetNames = map (\i -> fromString $ "TestToken" <> show @Int i) [1..60]
      
      -- Loan Info
      loanAssets = map (\name -> Asset (testTokenSymbol,name)) assetNames
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      pairs = zip loanAssets $ repeat collateral1
      datums = 
        flip map pairs $ \(loan,col) -> 
          unsafeCreateOfferDatum $ NewOfferInfo
            { lenderId = lenderCred
            , lenderAddress = lenderAddr
            , loanAsset = loan
            , loanPrinciple = 10
            , rolloverFrequency = Nothing
            , loanTerm = 3600
            , loanInterest = Fraction (1,10)
            , minPayment = 0
            , collateralization = [(col,Fraction(1,1))]
            , collateralIsSwappable = False
            , claimPeriod = 3600
            , offerDeposit = 4_000_000
            , offerExpiration = Nothing
            }
      
      beforeDatums = take 30 datums
      afterDatums = drop 30 datums

      sampleOutputs ds = flip map ds $ \datum@OfferDatum{..} ->
        Output
          { outputAddress = loanAddress
          , outputValue = utxoValue (LV.Lovelace offerDeposit) $ mconcat $
              [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
              , PV2.singleton negotiationBeaconCurrencySymbol (unAssetBeacon assetBeacon) 1
              , PV2.singleton negotiationBeaconCurrencySymbol (unLenderID lenderId) 1
              , uncurry PV2.singleton (unAsset loanAsset) loanPrinciple
              ]
          , outputDatum = OutputDatum $ toDatum datum
          , outputReferenceScript = toReferenceScript Nothing
          }

  -- Initialize scenario
  (negotiationRef,_,loanRef,_) <- initializeReferenceScripts 
  mintTestTokens lenderWallet 100_000_000 $ zip assetNames $ repeat 1000

  -- Create the Offer UTxO.
  forM_ (grouped 20 beforeDatums) $ \ds -> 
    transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
      emptyTxParams
        { tokens =
            [ TokenMint
                { mintTokens = flip concatMap ds $ \OfferDatum{assetBeacon,lenderId} ->
                    [ ("Offer",1)
                    , (unAssetBeacon assetBeacon,1)
                    , (unLenderID lenderId,1)
                    ]
                , mintRedeemer = toRedeemer $ CreateCloseOrUpdateOffer lenderCred
                , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
                , mintReference = Just negotiationRef
                }
            ]
        , outputs = sampleOutputs ds
        , referenceInputs = [negotiationRef]
        , extraKeyWitnesses = [lenderPubKey]
        }

  offerUTxOs <- take number <$> txOutRefsAndDatumsAtAddress @OfferDatum loanAddress

  let sampleBurns = 
        zipWith 
          (\(_,Just OfferDatum{assetBeacon=beforeBeacon}) OfferDatum{assetBeacon=afterBeacon} ->
            TokenMint 
              { mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateOffer lenderCred
              , mintTokens = 
                  [ 
                    (unAssetBeacon beforeBeacon,-1)
                  , (unAssetBeacon afterBeacon,1)
                  ]
              , mintReference = Just negotiationRef
              })
          offerUTxOs
          afterDatums

  -- Try to convert the Offer UTxOs.
  void $ transact lenderPersonalAddr [loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens = sampleBurns
      , inputs = flip map offerUTxOs $ \(offerRef,_) ->
          Input
            { inputId = offerRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer CloseOrUpdateOffer)
            }
      , outputs = sampleOutputs $ take number afterDatums
      , referenceInputs = [negotiationRef,loanRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

-------------------------------------------------
-- Test Function
-------------------------------------------------
-- | A `TestTree` containing all Offer update scenarios.
tests :: TestTree
tests =
  testGroup "Update Offer(s)"
    [ -- Success Tests (Regression Tests)
      mustSucceed "regressionTest1" regressionTest1
    , mustSucceed "regressionTest2" regressionTest2
    , mustSucceed "regressionTest3" regressionTest3

      -- Benchmark Tests
    , mustSucceed "benchTest1" $ benchTest1 18
    , mustSucceed "benchTest2" $ benchTest2 18

      -- Performance Increase Tests
    , mustExceedTxLimits "perfIncreaseTest1" $ benchTest1 19
    , mustExceedTxLimits "perfIncreaseTest2" $ benchTest2 19
    ]
