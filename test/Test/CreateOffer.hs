{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.CreateOffer
  ( 
    -- * Scenarios Tested
    -- ** Scenarios that should succeed
    regressionTest1
  , regressionTest2
  , regressionTest3
  , regressionTest4
  , regressionTest5

    -- ** Scenarios that should fail
    
    -- ** Benchmark Tests
  , benchTest1
  , benchTest2
    
    -- * Full TestTree
  , tests
  ) where

import qualified Ledger.Value.CardanoAPI as LV
import qualified Ledger.Address as LA
import qualified PlutusLedgerApi.V2 as PV2
import qualified Ledger.CardanoWallet as Mock 
import Test.Tasty (TestTree,testGroup)
import Data.String (fromString)

import CardanoLoans

import Test.Prelude

-------------------------------------------------
-- Regression Tests
-------------------------------------------------
-- | Create a single valid Offer UTxO. The loan asset is ADA and one native asset is used as
-- collateral.
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
      loanAsset = Asset (adaSymbol,adaToken)
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { lenderId = lenderCred
        , lenderAddress = lenderAddr
        , loanAsset = loanAsset
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
  (negotiationRef,_,_,_) <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",1)
                  , (unAssetBeacon loanBeacon,1)
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (unAssetBeacon loanBeacon) 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (unLenderID lenderBeacon) 1
                  , uncurry PV2.singleton (unAsset loanAsset) 10_000_000
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

-- | Create a single valid Offer UTxO. The loan asset is a native asset and one native asset is 
-- used as collateral.
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
      loanAsset = Asset (testTokenSymbol,"TestToken2")
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { lenderId = lenderCred
        , lenderAddress = lenderAddr
        , loanAsset = loanAsset
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
  (negotiationRef,_,_,_) <- initializeReferenceScripts 
  mintTestTokens lenderWallet 100_000_000 [("TestToken2",1000)]

  -- Try to create the Offer UTxO.
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",1)
                  , (unAssetBeacon loanBeacon,1)
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (unAssetBeacon loanBeacon) 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (unLenderID lenderBeacon) 1
                  , uncurry PV2.singleton (unAsset loanAsset) 10
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

-- | Create multiple valid Offer UTxOs. The terms are the same. The loan asset is ADA and one 
-- native asset is used as collateral.
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
      loanAsset = Asset (adaSymbol,adaToken)
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { lenderId = lenderCred
        , lenderAddress = lenderAddr
        , loanAsset = loanAsset
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
  (negotiationRef,_,_,_) <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",2)
                  , (unAssetBeacon loanBeacon,2)
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (unAssetBeacon loanBeacon) 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (unLenderID lenderBeacon) 1
                  , uncurry PV2.singleton (unAsset loanAsset) 10_000_000
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (unAssetBeacon loanBeacon) 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (unLenderID lenderBeacon) 1
                  , uncurry PV2.singleton (unAsset loanAsset) 10_000_000
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

-- | Create multiple valid Offer UTxOs. The terms are the same. The loan asset is a native asset and 
-- one native asset is used as collateral.
regressionTest4 :: MonadEmulator m => m ()
regressionTest4 = do
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
      loanAsset = Asset (testTokenSymbol,"TestToken2")
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { lenderId = lenderCred
        , lenderAddress = lenderAddr
        , loanAsset = loanAsset
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
  (negotiationRef,_,_,_) <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000),("TestToken2",1000)]

  -- Try to create the Offer UTxO.
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",2)
                  , (unAssetBeacon loanBeacon,2)
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (unAssetBeacon loanBeacon) 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (unLenderID lenderBeacon) 1
                  , uncurry PV2.singleton (unAsset loanAsset) 10
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (unAssetBeacon loanBeacon) 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (unLenderID lenderBeacon) 1
                  , uncurry PV2.singleton (unAsset loanAsset) 10
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

-- | Create multiple valid Offer UTxOs. The terms are different. One loan uses ada as the loan asset
-- while the other loan uses a native asset as the loan asset.
regressionTest5 :: MonadEmulator m => m ()
regressionTest5 = do
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
  (negotiationRef,_,_,_) <- initializeReferenceScripts 
  mintTestTokens lenderWallet 100_000_000 [("TestToken2",1000)]

  -- Try to create the Offer UTxO.
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",2)
                  , (unAssetBeacon loanBeacon1,1)
                  , (unAssetBeacon loanBeacon2,1)
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (unAssetBeacon loanBeacon2) 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (unLenderID lenderBeacon) 1
                  , uncurry PV2.singleton (unAsset loanAsset2) 10_000_000
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum2
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

-------------------------------------------------
-- Benchmark Tests
-------------------------------------------------
-- | Create multiple valid Offer UTxO. The terms are the same. The loan asset is ADA and one 
-- native asset is used as collateral.
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
      loanAsset = Asset (adaSymbol,adaToken)
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { lenderId = lenderCred
        , lenderAddress = lenderAddr
        , loanAsset = loanAsset
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
  (negotiationRef,_,_,_) <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",fromIntegral number)
                  , (unAssetBeacon loanBeacon,fromIntegral number)
                  , (unLenderID lenderBeacon,fromIntegral number)
                  ]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateOffer lenderCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs = replicate number
          Output
            { outputAddress = loanAddress
            , outputValue = utxoValue 4_000_000 $ mconcat
                [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
                , PV2.singleton negotiationBeaconCurrencySymbol (unAssetBeacon loanBeacon) 1
                , PV2.singleton negotiationBeaconCurrencySymbol (unLenderID lenderBeacon) 1
                , uncurry PV2.singleton (unAsset loanAsset) 10_000_000
                ]
            , outputDatum = OutputDatum $ toDatum loanDatum
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

-- | Create multiple valid Offer UTxO. The terms are different. Each loan uses a different loan
-- asset.
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
      assetNames = map (\i -> fromString $ "TestToken" <> show @Int i) [2..80]
      
      -- Loan Info
      loanAssets = map (\name -> Asset (testTokenSymbol,name)) assetNames
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      pairs = zip loanAssets $ repeat collateral1
      datums = take number $
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

      sampleOutputs = flip map datums $ \datum@OfferDatum{..} ->
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
  (negotiationRef,_,_,_) <- initializeReferenceScripts 
  mintTestTokens lenderWallet 100_000_000 $ zip assetNames $ repeat 1000

  -- Try to create the Offer UTxO.
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap datums $ \OfferDatum{..} ->
                    [ ("Offer",1)
                    , (unAssetBeacon assetBeacon,1)
                    , (unLenderID lenderBeacon,1)
                    ]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateOffer lenderCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs = sampleOutputs
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

-------------------------------------------------
-- Test Function
-------------------------------------------------
-- | A `TestTree` containing all Offer creation scenarios.
tests :: TestTree
tests =
  testGroup "Create Offer(s)"
    [ -- Success Tests (Regression Tests)
      mustSucceed "regressionTest1" regressionTest1
    , mustSucceed "regressionTest2" regressionTest2
    , mustSucceed "regressionTest3" regressionTest3
    , mustSucceed "regressionTest4" regressionTest4
    , mustSucceed "regressionTest5" regressionTest5

      -- Benchmark Tests
    , mustSucceed "benchTest1" $ benchTest1 33
    , mustSucceed "benchTest2" $ benchTest2 24

      -- Performance Increase Tests
    , mustExceedTxLimits "perfIncreaseTest1" $ benchTest1 34
    , mustExceedTxLimits "perfIncreaseTest2" $ benchTest2 25
    ]
