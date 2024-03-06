{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | The emulator is currently unable to test staking validators. However, the logic of the 
-- staking execution for the script is identical to the minting policy execution so this
-- is not an obstacle. Once the emulator is able to test staking validators, this module will
-- be finished. For now, this module uses the minting execution just to get the script
-- to be executed by the emulator.
module Test.UpdateAsk
  ( 
    -- * Scenarios Tested
    -- ** Scenarios that should succeed
    regressionTest1
  , regressionTest2
  , regressionTest3

    -- ** Scenarios that should fail
    
    -- ** Benchmark Tests
  , benchTest1
  , benchTest2
  , benchTest3
    
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
-- | Convert a single valid Ask UTxO to one that uses a different loan asset. 
-- One native asset is used as collateral both before and after the conversion.
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
      loanAsset1 = Asset (adaSymbol,adaToken)
      loanAsset2 = Asset (testTokenSymbol,"TestToken10")
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      loanBeacon1 = genLoanAssetBeaconName loanAsset1
      loanBeacon2 = genLoanAssetBeaconName loanAsset2
      loanDatum1 = unsafeCreateAskDatum $ NewAskInfo
        { borrowerId = borrowerCred
        , loanAsset = loanAsset1
        , loanPrinciple = 10_000_000
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
  (negotiationRef,_,loanRef,_) <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(unAssetBeacon loanBeacon1,1)]
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
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [borrowerPubKey]
      }

  askRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (unAssetBeacon loanBeacon1) 1
        , uncurry PV2.singleton (unAsset collateral1) 1
        ]

  -- Try to convert the Ask UTxO.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(unAssetBeacon loanBeacon1,-1),(unAssetBeacon loanBeacon2,1)]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , inputs =
          [ Input
              { inputId = askRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer CloseOrUpdateAsk)
              }
          ]
      , outputs =
          [ Output
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
      , referenceInputs = [negotiationRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      }

-- | Convert multiple valid Ask UTxO to one that uses a different loan asset. 
-- One native asset is used as collateral both before and after the conversion.
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
      loanAsset1 = Asset (adaSymbol,adaToken)
      loanAsset2 = Asset (testTokenSymbol,"TestToken10")
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      loanBeacon1 = genLoanAssetBeaconName loanAsset1
      loanBeacon2 = genLoanAssetBeaconName loanAsset2
      loanDatum1 = unsafeCreateAskDatum $ NewAskInfo
        { borrowerId = borrowerCred
        , loanAsset = loanAsset1
        , loanPrinciple = 10_000_000
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
  (negotiationRef,_,loanRef,_) <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",2),(unAssetBeacon loanBeacon1,2)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (unAssetBeacon loanBeacon1) 1
                  , uncurry PV2.singleton (unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [borrowerPubKey]
      }

  askUTxOs <- txOutRefsAndDatumsAtAddress @AskDatum loanAddress

  -- Try to convert the Ask UTxO.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(unAssetBeacon loanBeacon1,-2),(unAssetBeacon loanBeacon2,2)]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , inputs = flip map askUTxOs $ \(askRef,_) ->
          Input
            { inputId = askRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer CloseOrUpdateAsk)
            }
      , outputs =
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (unAssetBeacon loanBeacon2) 1
                  , uncurry PV2.singleton (unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum2
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
      , referenceInputs = [negotiationRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      }

-- | Update the terms for a single valid Ask UTxO. The loan asset does not change.
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
      loanAsset1 = Asset (adaSymbol,adaToken)
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      loanBeacon1 = genLoanAssetBeaconName loanAsset1
      loanDatum1 = unsafeCreateAskDatum $ NewAskInfo
        { borrowerId = borrowerCred
        , loanAsset = loanAsset1
        , loanPrinciple = 10_000_000
        , loanTerm = 3600
        , collateral = [collateral1]
        }
      loanDatum2 = unsafeCreateAskDatum $ NewAskInfo
        { borrowerId = borrowerCred
        , loanAsset = loanAsset1
        , loanPrinciple = 10_000_000
        , loanTerm = 4600
        , collateral = [collateral1]
        }

  -- Initialize scenario
  (negotiationRef,_,loanRef,_) <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(unAssetBeacon loanBeacon1,1)]
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
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [borrowerPubKey]
      }

  askUTxOs <- txOutRefsAndDatumsAtAddress @AskDatum loanAddress

  -- Try to update the Ask UTxO.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(unAssetBeacon loanBeacon1,1)]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , inputs = flip map askUTxOs $ \(askRef,_) ->
          Input
            { inputId = askRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer CloseOrUpdateAsk)
            }
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (unAssetBeacon loanBeacon1) 1
                  , uncurry PV2.singleton (unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum2
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      }

-------------------------------------------------
-- Benchmark Tests
-------------------------------------------------
-- | Convert multiple valid Ask UTxO to one that uses a different loan asset. All asks undergo
-- the same conversion. One native asset is used as collateral both before and after the 
-- conversion.
benchTest1 :: MonadEmulator m => Int -> m ()
benchTest1 number = do
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
      loanAsset1 = Asset (adaSymbol,adaToken)
      loanAsset2 = Asset (testTokenSymbol,"TestToken10")
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      loanBeacon1 = genLoanAssetBeaconName loanAsset1
      loanBeacon2 = genLoanAssetBeaconName loanAsset2
      loanDatum1 = unsafeCreateAskDatum $ NewAskInfo
        { borrowerId = borrowerCred
        , loanAsset = loanAsset1
        , loanPrinciple = 10_000_000
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
  (negotiationRef,_,loanRef,_) <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",20),(unAssetBeacon loanBeacon1,20)]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs = replicate 20 $
          Output
            { outputAddress = loanAddress
            , outputValue = utxoValue 3_000_000 $ mconcat
                [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                , PV2.singleton negotiationBeaconCurrencySymbol (unAssetBeacon loanBeacon1) 1
                , uncurry PV2.singleton (unAsset collateral1) 1
                ]
            , outputDatum = OutputDatum $ toDatum loanDatum1
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [borrowerPubKey]
      }

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",20),(unAssetBeacon loanBeacon1,20)]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs = replicate 20 $
          Output
            { outputAddress = loanAddress
            , outputValue = utxoValue 3_000_000 $ mconcat
                [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                , PV2.singleton negotiationBeaconCurrencySymbol (unAssetBeacon loanBeacon1) 1
                , uncurry PV2.singleton (unAsset collateral1) 1
                ]
            , outputDatum = OutputDatum $ toDatum loanDatum1
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [borrowerPubKey]
      }

  askUTxOs <- take number <$> txOutRefsAndDatumsAtAddress @AskDatum loanAddress

  -- Try to update the Ask UTxO.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = flip map askUTxOs $ const
          TokenMint
            { mintTokens = [(unAssetBeacon loanBeacon1,-1),(unAssetBeacon loanBeacon2,1)]
            , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred
            , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
            , mintReference = Just negotiationRef
            }
      , inputs = flip map askUTxOs $ \(askRef,_) ->
          Input
            { inputId = askRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer CloseOrUpdateAsk)
            }
      , outputs = replicate number $
          Output
            { outputAddress = loanAddress
            , outputValue = utxoValue 3_000_000 $ mconcat
                [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                , PV2.singleton negotiationBeaconCurrencySymbol (unAssetBeacon loanBeacon2) 1
                , uncurry PV2.singleton (unAsset collateral1) 1
                ]
            , outputDatum = OutputDatum $ toDatum loanDatum2
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [negotiationRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      }

-- | Convert multiple valid Ask UTxOs. All loans are for different loan assets and converted to
-- new Ask UTxOs that also have different loan assets. Each loan uses the a different single native 
-- asset for collateral.
benchTest2 :: MonadEmulator m => Int -> m ()
benchTest2 number = do
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
      assetNames = map (\i -> fromString $ "TestToken" <> show @Int i) [1..120]
      
      -- Loan Info
      loanAssets = map (\name -> Asset (testTokenSymbol,name)) $ drop 60 assetNames
      collateralAssets = map (\name -> Asset (testTokenSymbol,name)) $ take 60 assetNames
      pairs = zip loanAssets collateralAssets
      datums = 
        flip map pairs $ \(loan,col) -> 
          unsafeCreateAskDatum $ NewAskInfo
            { borrowerId = borrowerCred
            , loanAsset = loan
            , loanPrinciple = 10
            , loanTerm = 3600
            , collateral = [col]
            }
      
      beforeDatums = take 30 datums
      afterDatums = drop 30 datums

      sampleOutputs ds = flip map ds $ \datum@AskDatum{..} ->
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
  (negotiationRef,_,loanRef,_) <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 100_000_000 $ zip assetNames $ repeat 1000

  -- Create the Ask UTxO.
  forM_ (grouped 20 beforeDatums) $ \ds -> 
    transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
      emptyTxParams
        { tokens =
            [ TokenMint
                { mintTokens = flip concatMap ds $ \AskDatum{..} ->
                    [ (unAssetBeacon assetBeacon,1)
                    , ("Ask",1)
                    ]
                , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred
                , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
                , mintReference = Just negotiationRef
                }
            ]
        , outputs = sampleOutputs ds
        , referenceInputs = [negotiationRef]
        , extraKeyWitnesses = [borrowerPubKey]
        }

  askUTxOs <- take number <$> txOutRefsAndDatumsAtAddress @AskDatum loanAddress

  let sampleBurns = 
        zipWith (\(_,Just AskDatum{assetBeacon=beforeBeacon}) AskDatum{assetBeacon=afterBeacon} ->
                  TokenMint 
                    { mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
                    , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred
                    , mintTokens = 
                        [ 
                          (unAssetBeacon beforeBeacon,-1)
                        , (unAssetBeacon afterBeacon,1)
                        ]
                    , mintReference = Just negotiationRef
                    })
                askUTxOs
                afterDatums

  -- Try to convert the Ask UTxOs.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = sampleBurns
      , inputs = flip map askUTxOs $ \(askRef,_) ->
          Input
            { inputId = askRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer CloseOrUpdateAsk)
            }
      , outputs = sampleOutputs $ take number afterDatums
      , referenceInputs = [negotiationRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      }

-- | Update the terms for a multiple valid Ask UTxO. The loan asset does not change.
benchTest3 :: MonadEmulator m => Int -> m ()
benchTest3 number = do
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
      loanAsset1 = Asset (adaSymbol,adaToken)
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      loanBeacon1 = genLoanAssetBeaconName loanAsset1
      loanDatum1 = unsafeCreateAskDatum $ NewAskInfo
        { borrowerId = borrowerCred
        , loanAsset = loanAsset1
        , loanPrinciple = 10_000_000
        , loanTerm = 3600
        , collateral = [collateral1]
        }
      loanDatum2 = unsafeCreateAskDatum $ NewAskInfo
        { borrowerId = borrowerCred
        , loanAsset = loanAsset1
        , loanPrinciple = 10_000_000
        , loanTerm = 4600
        , collateral = [collateral1]
        }

  -- Initialize scenario
  (negotiationRef,_,loanRef,_) <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  replicateM_ 3 $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",20),(unAssetBeacon loanBeacon1,20)]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs = replicate 20 $
          Output
            { outputAddress = loanAddress
            , outputValue = utxoValue 3_000_000 $ mconcat
                [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                , PV2.singleton negotiationBeaconCurrencySymbol (unAssetBeacon loanBeacon1) 1
                , uncurry PV2.singleton (unAsset collateral1) 1
                ]
            , outputDatum = OutputDatum $ toDatum loanDatum1
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [borrowerPubKey]
      }

  askUTxOs <- take number <$> txOutRefsAndDatumsAtAddress @AskDatum loanAddress

  -- Try to update the Ask UTxO.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(unAssetBeacon loanBeacon1,1)]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , inputs = flip map askUTxOs $ \(askRef,_) ->
          Input
            { inputId = askRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer CloseOrUpdateAsk)
            }
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
          ] <> replicate number
            Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (unAssetBeacon loanBeacon1) 1
                  , uncurry PV2.singleton (unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum2
              , outputReferenceScript = toReferenceScript Nothing
              }
      , referenceInputs = [negotiationRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      }

-------------------------------------------------
-- Test Function
-------------------------------------------------
-- | A `TestTree` containing all Ask close scenarios.
tests :: TestTree
tests =
  testGroup "Close Ask(s)"
    [ -- Success Tests (Regression Tests)
      mustSucceed "regressionTest1" regressionTest1
    , mustSucceed "regressionTest2" regressionTest2
    , mustSucceed "regressionTest3" regressionTest3

      -- Benchmark Tests
    , mustSucceed "benchTest1" $ benchTest1 25
    , mustSucceed "benchTest2" $ benchTest2 22
    , mustSucceed "benchTest3" $ benchTest3 24

      -- Performance Increase Tests
    , mustExceedTxLimits "perfIncreaseTest1" $ benchTest1 26
    , mustExceedTxLimits "perfIncreaseTest2" $ benchTest2 23
    , mustExceedTxLimits "perfIncreaseTest3" $ benchTest3 25
    ]
