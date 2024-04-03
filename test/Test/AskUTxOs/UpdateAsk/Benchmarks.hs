{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.AskUTxOs.UpdateAsk.Benchmarks where

import qualified Ledger.Address as LA
import qualified PlutusLedgerApi.V2 as PV2
import qualified Ledger.CardanoWallet as Mock 
import Test.Tasty (TestTree)
import Data.String (fromString)
import Control.Monad (forM_)

import CardanoLoans

import Test.Prelude

-------------------------------------------------
-- Benchmark Tests
-------------------------------------------------
-- | Convert multiple valid Ask UTxO to ones that use a different loan asset. All asks undergo
-- the same conversion. Three native assets are used as collateral both before and after the 
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      collateral3 = Asset (testTokenSymbol,"TestToken3")
      loanBeacon1 = genLoanAssetBeaconName loanAsset1
      loanBeacon2 = genLoanAssetBeaconName loanAsset2
      loanDatum1 = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred
        , _loanAsset = loanAsset1
        , _loanPrinciple = 10_000_000
        , _loanTerm = 3600
        , _collateral = [collateral1,collateral2,collateral3]
        }
      loanDatum2 = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred
        , _loanAsset = loanAsset2
        , _loanPrinciple = 10
        , _loanTerm = 3600
        , _collateral = [collateral1,collateral2,collateral3]
        }

  -- Initialize scenario
  References{negotiationRef,loanRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 
    [ ("TestToken1",1000)
    , ("TestToken2",1000)
    , ("TestToken3",1000)
    ]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",20),(_unAssetBeacon loanBeacon1,20)]
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
                , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon1) 1
                , uncurry PV2.singleton (_unAsset collateral1) 1
                , uncurry PV2.singleton (_unAsset collateral2) 1
                , uncurry PV2.singleton (_unAsset collateral3) 1
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
              { mintTokens = [("Ask",20),(_unAssetBeacon loanBeacon1,20)]
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
                , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon1) 1
                , uncurry PV2.singleton (_unAsset collateral1) 1
                , uncurry PV2.singleton (_unAsset collateral2) 1
                , uncurry PV2.singleton (_unAsset collateral3) 1
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
            { mintTokens = [(_unAssetBeacon loanBeacon1,-1),(_unAssetBeacon loanBeacon2,1)]
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
                , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon2) 1
                , uncurry PV2.singleton (_unAsset collateral1) 1
                , uncurry PV2.singleton (_unAsset collateral2) 1
                , uncurry PV2.singleton (_unAsset collateral3) 1
                ]
            , outputDatum = OutputDatum $ toDatum loanDatum2
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [negotiationRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      }

-- | Convert multiple valid Ask UTxOs. All Ask UTxOs start with unique loan assets and collateral
-- and are converted to Ask UTxOs with unique loan assets and collateral. Each Ask uses
-- one native asset for collateral.
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
            { _borrowerId = borrowerCred
            , _loanAsset = loan
            , _loanPrinciple = 10
            , _loanTerm = 3600
            , _collateral = [col]
            }
      
      beforeDatums = take 30 datums
      afterDatums = drop 30 datums

      sampleOutputs ds = flip map ds $ \datum@AskDatum{..} ->
        Output
          { outputAddress = loanAddress
          , outputValue = utxoValue 3_000_000 $ mconcat $
              [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
              , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
              ] <> map (\(Asset x) -> uncurry PV2.singleton x 1) (_unCollateral _collateral)
          , outputDatum = OutputDatum $ toDatum datum
          , outputReferenceScript = toReferenceScript Nothing
          }

  -- Initialize scenario
  References{negotiationRef,loanRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 100_000_000 $ zip assetNames $ repeat 100

  -- Create the Ask UTxO.
  forM_ (grouped 20 beforeDatums) $ \ds -> 
    transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
      emptyTxParams
        { tokens =
            [ TokenMint
                { mintTokens = flip concatMap ds $ \AskDatum{..} ->
                    [ (_unAssetBeacon _assetBeacon,1)
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
        zipWith (\(_,Just AskDatum{_assetBeacon=beforeBeacon}) AskDatum{_assetBeacon=afterBeacon} ->
                  TokenMint 
                    { mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
                    , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred
                    , mintTokens = 
                        [ 
                          (_unAssetBeacon beforeBeacon,-1)
                        , (_unAssetBeacon afterBeacon,1)
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

-- | Convert multiple valid Ask UTxOs. All Ask UTxOs start with unique loan assets and collateral
-- and are converted to Ask UTxOs with unique loan assets and collateral. Each Ask uses
-- three native assets for collateral.
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

      -- Other Info
      assetNames = map (\i -> fromString $ "TestToken" <> show @Int i) [1..240]
      
      -- Loan Info
      loanAssets = map (\name -> Asset (testTokenSymbol,name)) $ drop 180 assetNames
      collateralAssets = grouped 3 $
        map (\name -> Asset (testTokenSymbol,name)) $ take 180 assetNames
      pairs = zip loanAssets collateralAssets
      datums = 
        flip map pairs $ \(loan,col) -> 
          unsafeCreateAskDatum $ NewAskInfo
            { _borrowerId = borrowerCred
            , _loanAsset = loan
            , _loanPrinciple = 10
            , _loanTerm = 3600
            , _collateral = col
            }
      
      beforeDatums = take 30 datums
      afterDatums = drop 30 datums

      sampleOutputs ds = flip map ds $ \datum@AskDatum{..} ->
        Output
          { outputAddress = loanAddress
          , outputValue = utxoValue 3_000_000 $ mconcat $
              [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
              , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
              ] <> map (\(Asset x) -> uncurry PV2.singleton x 1) (_unCollateral _collateral)
          , outputDatum = OutputDatum $ toDatum datum
          , outputReferenceScript = toReferenceScript Nothing
          }

  -- Initialize scenario
  References{negotiationRef,loanRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 100_000_000 $ zip assetNames $ repeat 100

  -- Create the Ask UTxO.
  forM_ (grouped 20 beforeDatums) $ \ds -> 
    transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
      emptyTxParams
        { tokens =
            [ TokenMint
                { mintTokens = flip concatMap ds $ \AskDatum{..} ->
                    [ (_unAssetBeacon _assetBeacon,1)
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
        zipWith (\(_,Just AskDatum{_assetBeacon=beforeBeacon}) AskDatum{_assetBeacon=afterBeacon} ->
                  TokenMint 
                    { mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
                    , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred
                    , mintTokens = 
                        [ 
                          (_unAssetBeacon beforeBeacon,-1)
                        , (_unAssetBeacon afterBeacon,1)
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

-------------------------------------------------
-- TestTree
-------------------------------------------------
-- | A `TestTree` containing all benchmark scenarios for updating Ask UTxOs.
tests :: [TestTree]
tests =
  [ mustSucceed "benchTest1" $ benchTest1 21
  , mustSucceed "benchTest2" $ benchTest2 23
  , mustSucceed "benchTest3" $ benchTest3 13

  , mustExceedTxLimits "perfIncreaseTest1" $ benchTest1 22
  , mustExceedTxLimits "perfIncreaseTest2" $ benchTest2 24
  , mustExceedTxLimits "perfIncreaseTest3" $ benchTest3 14
  ]
