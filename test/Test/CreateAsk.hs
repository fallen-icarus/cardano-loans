{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.CreateAsk
  ( 
    -- * Scenarios Tested
    -- ** Scenarios that should succeed
    regressionTest1
  , regressionTest2
  , regressionTest3
  , regressionTest4
  , regressionTest5
  , regressionTest6

    -- ** Scenarios that should fail
    
    -- ** Benchmark Tests
  , benchTest1
  , benchTest2
  , benchTest3
  , benchTest4
    
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
-- | Create a single valid Ask UTxO. The loan asset is ADA and one native asset is used as
-- collateral.
regressionTest1 :: MonadEmulator m => m ()
regressionTest1 = do
  let -- Borrower Info
      borrowerWallet = Mock.knownMockWallet 1
      borrowerPersonalAddr = Mock.mockWalletAddress borrowerWallet
      borrowerPayPrivKey = Mock.paymentPrivateKey borrowerWallet
      borrowerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash borrowerWallet
      borrowerCred = PV2.PubKeyCredential borrowerPubKey
      loanAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash loanScript) 
                    (Just $ PV2.StakingHash borrowerCred)

      -- Loan Info
      loanAsset = Asset (adaSymbol,adaToken)
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateAskDatum $ NewAskInfo
        { borrowerId = borrowerCred
        , loanAsset = loanAsset
        , loanPrinciple = 10_000_000
        , loanTerm = 3600
        , collateral = [collateral1]
        }

  -- Initialize scenario
  (negotiationRef,_,_,_) <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Try to create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(unAssetBeacon loanBeacon,1)]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (unAssetBeacon loanBeacon) 1
                  , uncurry PV2.singleton (unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [borrowerPubKey]
      }

-- | Create multiple valid Ask UTxOs. All loans are for the same terms. One native asset is used
-- as collateral for each loan.
regressionTest2 :: MonadEmulator m => m ()
regressionTest2 = do
  let -- Borrower Info
      borrowerWallet = Mock.knownMockWallet 1
      borrowerPersonalAddr = Mock.mockWalletAddress borrowerWallet
      borrowerPayPrivKey = Mock.paymentPrivateKey borrowerWallet
      borrowerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash borrowerWallet
      borrowerCred = PV2.PubKeyCredential borrowerPubKey
      loanAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash loanScript) 
                    (Just $ PV2.StakingHash borrowerCred)

      -- Loan Info
      loanAsset = Asset (adaSymbol,adaToken)
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateAskDatum $ NewAskInfo
        { borrowerId = borrowerCred
        , loanAsset = loanAsset
        , loanPrinciple = 10_000_000
        , loanTerm = 3600
        , collateral = [collateral1]
        }

  -- Initialize scenario
  (negotiationRef,_,_,_) <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Try to create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",2),(unAssetBeacon loanBeacon,2)]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (unAssetBeacon loanBeacon) 1
                  , uncurry PV2.singleton (unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (unAssetBeacon loanBeacon) 1
                  , uncurry PV2.singleton (unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [borrowerPubKey]
      }

-- | Create a single valid Ask UTxO. The loan asset is ADA and three native assets are used as
-- collateral.
regressionTest3 :: MonadEmulator m => m ()
regressionTest3 = do
  let -- Borrower Info
      borrowerWallet = Mock.knownMockWallet 1
      borrowerPersonalAddr = Mock.mockWalletAddress borrowerWallet
      borrowerPayPrivKey = Mock.paymentPrivateKey borrowerWallet
      borrowerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash borrowerWallet
      borrowerCred = PV2.PubKeyCredential borrowerPubKey
      loanAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash loanScript) 
                    (Just $ PV2.StakingHash borrowerCred)

      -- Loan Info
      loanAsset = Asset (adaSymbol,adaToken)
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      collateral3 = Asset (testTokenSymbol,"TestToken3")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateAskDatum $ NewAskInfo
        { borrowerId = borrowerCred
        , loanAsset = loanAsset
        , loanPrinciple = 10_000_000
        , loanTerm = 3600
        , collateral = [collateral1,collateral2,collateral3]
        }

  -- Initialize scenario
  (negotiationRef,_,_,_) <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 
    [ ("TestToken1",1000)
    , ("TestToken2",1000)
    , ("TestToken3",1000)
    ]

  -- Try to create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(unAssetBeacon loanBeacon,1)]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (unAssetBeacon loanBeacon) 1
                  , uncurry PV2.singleton (unAsset collateral1) 1
                  , uncurry PV2.singleton (unAsset collateral2) 1
                  , uncurry PV2.singleton (unAsset collateral3) 1
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [borrowerPubKey]
      }

-- | Create multiple valid Ask UTxOs. All loans are for the same terms. Three native assets are used
-- as collateral for each loan.
regressionTest4 :: MonadEmulator m => m ()
regressionTest4 = do
  let -- Borrower Info
      borrowerWallet = Mock.knownMockWallet 1
      borrowerPersonalAddr = Mock.mockWalletAddress borrowerWallet
      borrowerPayPrivKey = Mock.paymentPrivateKey borrowerWallet
      borrowerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash borrowerWallet
      borrowerCred = PV2.PubKeyCredential borrowerPubKey
      loanAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash loanScript) 
                    (Just $ PV2.StakingHash borrowerCred)

      -- Loan Info
      loanAsset = Asset (adaSymbol,adaToken)
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      collateral3 = Asset (testTokenSymbol,"TestToken3")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateAskDatum $ NewAskInfo
        { borrowerId = borrowerCred
        , loanAsset = loanAsset
        , loanPrinciple = 10_000_000
        , loanTerm = 3600
        , collateral = [collateral1,collateral2,collateral3]
        }

  -- Initialize scenario
  (negotiationRef,_,_,_) <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 
    [ ("TestToken1",1000)
    , ("TestToken2",1000)
    , ("TestToken3",1000)
    ]

  -- Try to create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",2),(unAssetBeacon loanBeacon,2)]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (unAssetBeacon loanBeacon) 1
                  , uncurry PV2.singleton (unAsset collateral1) 1
                  , uncurry PV2.singleton (unAsset collateral2) 1
                  , uncurry PV2.singleton (unAsset collateral3) 1
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (unAssetBeacon loanBeacon) 1
                  , uncurry PV2.singleton (unAsset collateral1) 1
                  , uncurry PV2.singleton (unAsset collateral2) 1
                  , uncurry PV2.singleton (unAsset collateral3) 1
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [borrowerPubKey]
      }

-- | Create multiple valid Ask UTxOs. All loans are for different loan assets. Each loan uses
-- the same single native asset for collateral.
regressionTest5 :: MonadEmulator m => m ()
regressionTest5 = do
  let -- Borrower Info
      borrowerWallet = Mock.knownMockWallet 1
      borrowerPersonalAddr = Mock.mockWalletAddress borrowerWallet
      borrowerPayPrivKey = Mock.paymentPrivateKey borrowerWallet
      borrowerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash borrowerWallet
      borrowerCred = PV2.PubKeyCredential borrowerPubKey
      loanAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash loanScript) 
                    (Just $ PV2.StakingHash borrowerCred)

      -- Loan Info
      loanAsset1 = Asset (testTokenSymbol,"TestToken10")
      loanAsset2 = Asset (testTokenSymbol,"TestToken11")
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      loanBeacon1 = genLoanAssetBeaconName loanAsset1
      loanBeacon2 = genLoanAssetBeaconName loanAsset2
      loanDatum1 = unsafeCreateAskDatum $ NewAskInfo
        { borrowerId = borrowerCred
        , loanAsset = loanAsset1
        , loanPrinciple = 10
        , loanTerm = 3600
        , collateral = [collateral1]
        }
      loanDatum2 = unsafeCreateAskDatum $ NewAskInfo
        { borrowerId = borrowerCred
        , loanAsset = loanAsset2
        , loanPrinciple = 10
        , loanTerm = 3600
        , collateral = [collateral1]
        }

  -- Initialize scenario
  (negotiationRef,_,_,_) <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Try to create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Ask",2)
                  , (unAssetBeacon loanBeacon1,1)
                  , (unAssetBeacon loanBeacon2,1)
                  ]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (unAssetBeacon loanBeacon1) 1
                  , uncurry PV2.singleton (unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (unAssetBeacon loanBeacon2) 1
                  , uncurry PV2.singleton (unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum2
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [borrowerPubKey]
      }

-- | Create multiple valid Ask UTxOs. All loans are for different loan assets. Each loan uses
-- the same three native assets for collateral.
regressionTest6 :: MonadEmulator m => m ()
regressionTest6 = do
  let -- Borrower Info
      borrowerWallet = Mock.knownMockWallet 1
      borrowerPersonalAddr = Mock.mockWalletAddress borrowerWallet
      borrowerPayPrivKey = Mock.paymentPrivateKey borrowerWallet
      borrowerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash borrowerWallet
      borrowerCred = PV2.PubKeyCredential borrowerPubKey
      loanAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash loanScript) 
                    (Just $ PV2.StakingHash borrowerCred)

      -- Loan Info
      loanAsset1 = Asset (testTokenSymbol,"TestToken10")
      loanAsset2 = Asset (testTokenSymbol,"TestToken11")
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      collateral3 = Asset (testTokenSymbol,"TestToken3")
      loanBeacon1 = genLoanAssetBeaconName loanAsset1
      loanBeacon2 = genLoanAssetBeaconName loanAsset2
      loanDatum1 = unsafeCreateAskDatum $ NewAskInfo
        { borrowerId = borrowerCred
        , loanAsset = loanAsset1
        , loanPrinciple = 10
        , loanTerm = 3600
        , collateral = [collateral1,collateral2,collateral3]
        }
      loanDatum2 = unsafeCreateAskDatum $ NewAskInfo
        { borrowerId = borrowerCred
        , loanAsset = loanAsset2
        , loanPrinciple = 10
        , loanTerm = 3600
        , collateral = [collateral1,collateral2,collateral3]
        }

  -- Initialize scenario
  (negotiationRef,_,_,_) <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 
    [ ("TestToken1",1000)
    , ("TestToken2",1000)
    , ("TestToken3",1000)
    ]

  -- Try to create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Ask",2)
                  , (unAssetBeacon loanBeacon1,1)
                  , (unAssetBeacon loanBeacon2,1)
                  ]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (unAssetBeacon loanBeacon1) 1
                  , uncurry PV2.singleton (unAsset collateral1) 1
                  , uncurry PV2.singleton (unAsset collateral2) 1
                  , uncurry PV2.singleton (unAsset collateral3) 1
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (unAssetBeacon loanBeacon2) 1
                  , uncurry PV2.singleton (unAsset collateral1) 1
                  , uncurry PV2.singleton (unAsset collateral2) 1
                  , uncurry PV2.singleton (unAsset collateral3) 1
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum2
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [borrowerPubKey]
      }

-------------------------------------------------
-- Benchmark Tests
-------------------------------------------------
-- | Create multiple Ask UTxOs for the same loan terms. The loan asset is ada and there is one
-- native token used as collateral.
benchTest1 :: MonadEmulator m => Int -> m ()
benchTest1 numberCreated = do
  let -- Borrower Info
      borrowerWallet = Mock.knownMockWallet 1
      borrowerPersonalAddr = Mock.mockWalletAddress borrowerWallet
      borrowerPayPrivKey = Mock.paymentPrivateKey borrowerWallet
      borrowerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash borrowerWallet
      borrowerCred = PV2.PubKeyCredential borrowerPubKey
      loanAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash loanScript) 
                    (Just $ PV2.StakingHash borrowerCred)

      -- Loan Info
      loanAsset = Asset (adaSymbol,adaToken)
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateAskDatum $ NewAskInfo
        { borrowerId = borrowerCred
        , loanAsset = loanAsset
        , loanPrinciple = 10_000_000
        , loanTerm = 3600
        , collateral = [collateral1]
        }

  -- Initialize scenario
  (negotiationRef,_,_,_) <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Try to create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Ask",fromIntegral numberCreated)
                  , (unAssetBeacon loanBeacon,fromIntegral numberCreated)
                  ]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs = replicate numberCreated $ 
          Output
            { outputAddress = loanAddress
            , outputValue = utxoValue 3_000_000 $ mconcat
                [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                , PV2.singleton negotiationBeaconCurrencySymbol (unAssetBeacon loanBeacon) 1
                , uncurry PV2.singleton (unAsset collateral1) 1
                ]
            , outputDatum = OutputDatum $ toDatum loanDatum
            , outputReferenceScript = toReferenceScript Nothing
            }
          
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [borrowerPubKey]
      }

-- | Create multiple Ask UTxOs for the same loan terms. The loan asset is ada and there are three
-- native tokens used as collateral.
benchTest2 :: MonadEmulator m => Int -> m ()
benchTest2 numberCreated = do
  let -- Borrower Info
      borrowerWallet = Mock.knownMockWallet 1
      borrowerPersonalAddr = Mock.mockWalletAddress borrowerWallet
      borrowerPayPrivKey = Mock.paymentPrivateKey borrowerWallet
      borrowerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash borrowerWallet
      borrowerCred = PV2.PubKeyCredential borrowerPubKey
      loanAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash loanScript) 
                    (Just $ PV2.StakingHash borrowerCred)

      -- Loan Info
      loanAsset = Asset (adaSymbol,adaToken)
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      collateral3 = Asset (testTokenSymbol,"TestToken3")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateAskDatum $ NewAskInfo
        { borrowerId = borrowerCred
        , loanAsset = loanAsset
        , loanPrinciple = 10_000_000
        , loanTerm = 3600
        , collateral = [collateral1,collateral2,collateral3]
        }

  -- Initialize scenario
  (negotiationRef,_,_,_) <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 
    [ ("TestToken1",1000)
    , ("TestToken2",1000)
    , ("TestToken3",1000)
    ]

  -- Try to create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Ask",fromIntegral numberCreated)
                  , (unAssetBeacon loanBeacon,fromIntegral numberCreated)
                  ]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs = replicate numberCreated $ 
          Output
            { outputAddress = loanAddress
            , outputValue = utxoValue 3_000_000 $ mconcat
                [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                , PV2.singleton negotiationBeaconCurrencySymbol (unAssetBeacon loanBeacon) 1
                , uncurry PV2.singleton (unAsset collateral1) 1
                , uncurry PV2.singleton (unAsset collateral2) 1
                , uncurry PV2.singleton (unAsset collateral3) 1
                ]
            , outputDatum = OutputDatum $ toDatum loanDatum
            , outputReferenceScript = toReferenceScript Nothing
            }
          
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [borrowerPubKey]
      }

-- | Create multiple valid Ask UTxOs. All loans are for different loan assets. Each loan uses
-- a different single native asset for collateral.
benchTest3 :: MonadEmulator m => Int -> m ()
benchTest3 numberCreated = do
  let -- Borrower Info
      borrowerWallet = Mock.knownMockWallet 1
      borrowerPersonalAddr = Mock.mockWalletAddress borrowerWallet
      borrowerPayPrivKey = Mock.paymentPrivateKey borrowerWallet
      borrowerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash borrowerWallet
      borrowerCred = PV2.PubKeyCredential borrowerPubKey
      loanAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash loanScript) 
                    (Just $ PV2.StakingHash borrowerCred)

      -- Other Info
      assetNames = map (\i -> fromString $ "TestToken" <> show @Int i) [1..80]
      
      -- Loan Info
      loanAssets = map (\name -> Asset (testTokenSymbol,name)) $ drop 40 assetNames
      collateralAssets = map (\name -> Asset (testTokenSymbol,name)) $ take 40 assetNames
      pairs = zip loanAssets collateralAssets
      datums = take numberCreated $
        flip map pairs $ \(loan,col) -> 
          unsafeCreateAskDatum $ NewAskInfo
            { borrowerId = borrowerCred
            , loanAsset = loan
            , loanPrinciple = 10
            , loanTerm = 3600
            , collateral = [col]
            }

      sampleOutputs = flip map datums $ \datum@AskDatum{..} ->
        Output
          { outputAddress = loanAddress
          , outputValue = utxoValue 3_000_000 $ mconcat $
              [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
              , PV2.singleton negotiationBeaconCurrencySymbol (unAssetBeacon assetBeacon) 1
              ] <> map (\(Asset x) -> uncurry PV2.singleton x 1) (unCollateral collateral)
          , outputDatum = OutputDatum $ toDatum datum
          , outputReferenceScript = toReferenceScript Nothing
          }

  -- Initialize scenario
  (negotiationRef,_,_,_) <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 100_000_000 $ zip assetNames $ repeat 1000

  -- Try to create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap datums $ \AskDatum{..} ->
                  [ (unAssetBeacon assetBeacon,1)
                  , ("Ask",1)
                  ]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs = sampleOutputs
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [borrowerPubKey]
      }

-- | Create multiple valid Ask UTxOs. All loans are for different loan assets. Each loan uses
-- different three native asset for collateral.
benchTest4 :: MonadEmulator m => Int -> m ()
benchTest4 numberCreated = do
  let -- Borrower Info
      borrowerWallet = Mock.knownMockWallet 1
      borrowerPersonalAddr = Mock.mockWalletAddress borrowerWallet
      borrowerPayPrivKey = Mock.paymentPrivateKey borrowerWallet
      borrowerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash borrowerWallet
      borrowerCred = PV2.PubKeyCredential borrowerPubKey
      loanAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash loanScript) 
                    (Just $ PV2.StakingHash borrowerCred)

      -- Other Info
      assetNames = map (\i -> fromString $ "TestToken" <> show @Int i) [1..160]
      
      -- Loan Info
      loanAssets = map (\name -> Asset (testTokenSymbol,name)) $ drop 120 assetNames
      collateralAssets = grouped 3 $ 
        map (\name -> Asset (testTokenSymbol,name)) $ take 120 assetNames
      pairs = zip loanAssets collateralAssets
      datums = take numberCreated $
        flip map pairs $ \(loan,col) -> 
          unsafeCreateAskDatum $ NewAskInfo
            { borrowerId = borrowerCred
            , loanAsset = loan
            , loanPrinciple = 10
            , loanTerm = 3600
            , collateral = col
            }

      sampleOutputs = flip map datums $ \datum@AskDatum{..} ->
        Output
          { outputAddress = loanAddress
          , outputValue = utxoValue 3_000_000 $ mconcat $
              [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
              , PV2.singleton negotiationBeaconCurrencySymbol (unAssetBeacon assetBeacon) 1
              ] <> map (\(Asset x) -> uncurry PV2.singleton x 1) (unCollateral collateral)
          , outputDatum = OutputDatum $ toDatum datum
          , outputReferenceScript = toReferenceScript Nothing
          }

  -- Initialize scenario
  (negotiationRef,_,_,_) <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 100_000_000 $ zip assetNames $ repeat 100

  -- Try to create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap datums $ \AskDatum{..} ->
                  [ (unAssetBeacon assetBeacon,1)
                  , ("Ask",1)
                  ]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs = sampleOutputs
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [borrowerPubKey]
      }

-------------------------------------------------
-- Test Function
-------------------------------------------------
-- | A `TestTree` containing all Ask creation scenarios.
tests :: TestTree
tests =
  testGroup "Create Ask(s)"
    [ -- Success Tests (Regression Tests)
      mustSucceed "regressionTest1" regressionTest1
    , mustSucceed "regressionTest2" regressionTest2
    , mustSucceed "regressionTest3" regressionTest3
    , mustSucceed "regressionTest4" regressionTest4
    , mustSucceed "regressionTest5" regressionTest5
    , mustSucceed "regressionTest6" regressionTest6

      -- Benchmark Tests
    , mustSucceed "benchTest1" $ benchTest1 42
    , mustSucceed "benchTest2" $ benchTest2 32
    , mustSucceed "benchTest3" $ benchTest3 32
    , mustSucceed "benchTest4" $ benchTest4 24

      -- Performance Increase Tests
    , mustExceedTxLimits "perfIncreaseTest1" $ benchTest1 43
    , mustExceedTxLimits "perfIncreaseTest2" $ benchTest2 33
    , mustExceedTxLimits "perfIncreaseTest3" $ benchTest3 33
    , mustExceedTxLimits "perfIncreaseTest4" $ benchTest4 25
    ]
