{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.AskUTxOs.CloseAsk.Benchmarks where

import qualified Ledger.Address as LA
import qualified PlutusLedgerApi.V2 as PV2
import qualified Ledger.CardanoWallet as Mock 
import Test.Tasty (TestTree)
import Data.String (fromString)

import CardanoLoans

import Test.Prelude

-------------------------------------------------
-- Benchmark Tests
-------------------------------------------------
-- | Close multiple valid Ask UTxOs. All loans are for different loan assets. Each loan uses
-- a different single native asset for collateral.
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

      -- Other Info
      assetNames = map (\i -> fromString $ "TestToken" <> show @Int i) [1..80]
      
      -- Loan Info
      loanAssets = map (\name -> Asset (testTokenSymbol,name)) $ drop 40 assetNames
      collateralAssets = map (\name -> Asset (testTokenSymbol,name)) $ take 40 assetNames
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

      sampleOutputs = flip map datums $ \datum@AskDatum{..} ->
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
  mintTestTokens borrowerWallet 100_000_000 $ zip assetNames $ repeat 1000

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap (take 20 datums) $ \AskDatum{..} ->
                  [ (_unAssetBeacon _assetBeacon,1)
                  , ("Ask",1)
                  ]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs = take 20 sampleOutputs
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [borrowerPubKey]
      }

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap (take 20 $ drop 20 datums) $ \AskDatum{..} ->
                  [ (_unAssetBeacon _assetBeacon,1)
                  , ("Ask",1)
                  ]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs = take 20 $ drop 20 sampleOutputs
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [borrowerPubKey]
      }

  askUTxOs <- take number <$> txOutRefsAndDatumsAtAddress @AskDatum loanAddress

  -- Try to close the Ask UTxOs.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = flip map askUTxOs $ \(_,Just AskDatum{_assetBeacon}) ->
         TokenMint
            { mintTokens = [("Ask",-1),(_unAssetBeacon _assetBeacon,-1)]
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
      , referenceInputs = [negotiationRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      }

-- | Close multiple valid Ask UTxOs. All loans are for different loan assets. Each loan uses
-- different three native asset for collateral.
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
      assetNames = map (\i -> fromString $ "TestToken" <> show @Int i) [1..160]
      
      -- Loan Info
      loanAssets = map (\name -> Asset (testTokenSymbol,name)) $ drop 120 assetNames
      collateralAssets = grouped 3 $ 
        map (\name -> Asset (testTokenSymbol,name)) $ take 120 assetNames
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

      sampleOutputs = flip map datums $ \datum@AskDatum{..} ->
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
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap (take 20 datums) $ \AskDatum{..} ->
                  [ (_unAssetBeacon _assetBeacon,1)
                  , ("Ask",1)
                  ]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs = take 20 sampleOutputs
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [borrowerPubKey]
      }

  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = flip concatMap (take 20 $ drop 20 datums) $ \AskDatum{..} ->
                  [ (_unAssetBeacon _assetBeacon,1)
                  , ("Ask",1)
                  ]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs = take 20 $ drop 20 sampleOutputs
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [borrowerPubKey]
      }

  askUTxOs <- take number <$> txOutRefsAndDatumsAtAddress @AskDatum loanAddress

  -- Try to close the Ask UTxOs.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = flip map askUTxOs $ \(_,Just AskDatum{_assetBeacon}) ->
         TokenMint
            { mintTokens = [("Ask",-1),(_unAssetBeacon _assetBeacon,-1)]
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
      , referenceInputs = [negotiationRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      }

-------------------------------------------------
-- TestTree
-------------------------------------------------
-- | A `TestTree` containing all benchmark scenarios for closing Ask UTxOs.
tests :: [TestTree]
tests =
  [ mustSucceed "benchTest1" $ benchTest1 32
  , mustSucceed "benchTest2" $ benchTest2 32

  , mustExceedTxLimits "perfIncreaseTest1" $ benchTest1 33
  , mustExceedTxLimits "perfIncreaseTest2" $ benchTest2 33
  ]
