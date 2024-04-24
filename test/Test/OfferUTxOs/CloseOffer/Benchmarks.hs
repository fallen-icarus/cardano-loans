{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.OfferUTxOs.CloseOffer.Benchmarks where

import qualified Ledger.Value.CardanoAPI as LV
import qualified Ledger.Address as LA
import qualified PlutusLedgerApi.V2 as PV2
import qualified Ledger.CardanoWallet as Mock 
import Test.Tasty (TestTree)
import Data.String (fromString)
import Control.Monad (forM_,replicateM_)

import CardanoLoans

import Test.Prelude

-------------------------------------------------
-- Benchmark Tests
-------------------------------------------------
-- | Close multiple valid Offer UTxOs. The loans use the same loan asset.
benchTest1 :: MonadEmulator m => Int -> m ()
benchTest1 number = do
  let -- Borrower Info
      borrowerWallet = Mock.knownMockWallet 1
      borrowerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash borrowerWallet
      borrowerCred = PV2.PubKeyCredential borrowerPubKey
      loanAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash loanScript) 
                    (Just $ PV2.StakingHash borrowerCred)

      -- Lender Info
      lenderWallet = Mock.knownMockWallet 2
      lenderPersonalAddr = Mock.mockWalletAddress lenderWallet
      lenderPayPrivKey = Mock.paymentPrivateKey lenderWallet
      lenderPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash lenderWallet
      lenderCred = PV2.PubKeyCredential lenderPubKey
      lenderBeacon = genLenderId lenderCred
      lenderAddr = 
        PV2.Address (PV2.ScriptCredential $ scriptHash proxyScript) 
                    (Just $ PV2.StakingHash lenderCred)

      -- Loan Info
      loanAsset = Asset (adaSymbol,adaToken)
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Nothing
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 0
        , _penalty = NoPenalty
        , _collateralization = [(collateral1,Fraction(1,1))]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef,loanRef} <- initializeReferenceScripts 
  mintTestTokens lenderWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Offer UTxOs.
  replicateM_ 2 $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",20)
                  , (_unAssetBeacon loanBeacon,20)
                  , (_unLenderId lenderBeacon,20)
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
                , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
                , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
                ]
            , outputDatum = OutputDatum $ toDatum loanDatum
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

  offerUTxOs <- take number <$> txOutRefsAndDatumsAtAddress @OfferDatum loanAddress

  -- Try to close the Offer UTxOs.
  void $ transact lenderPersonalAddr [loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens = flip map offerUTxOs $ const
         TokenMint
            { mintTokens = 
                [ ("Offer",-1)
                , (_unAssetBeacon loanBeacon,-1)
                , (_unLenderId lenderBeacon,-1)
                ]
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
      , referenceInputs = [negotiationRef,loanRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

-- | Close multiple valid Offer UTxOs. The offers use different loan assets.
benchTest2 :: MonadEmulator m => Int -> m ()
benchTest2 number = do
  let -- Borrower Info
      borrowerWallet = Mock.knownMockWallet 1
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
      lenderBeacon = genLenderId lenderCred
      lenderAddr = 
        PV2.Address (PV2.ScriptCredential $ scriptHash proxyScript) 
                    (Just $ PV2.StakingHash lenderCred)

      -- Other Info
      assetNames = map (\i -> fromString $ "TestToken" <> show @Int i) [2..80]
      
      -- Loan Info
      loanAssets = map (\name -> Asset (testTokenSymbol,name)) assetNames
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      pairs = zip loanAssets $ repeat collateral1
      datums = 
        flip map pairs $ \(loan,col) -> 
          unsafeCreateOfferDatum $ NewOfferInfo
            { _lenderId = lenderCred
            , _lenderAddress = lenderAddr
            , _loanAsset = loan
            , _loanPrinciple = 10
            , _compoundFrequency = Nothing
            , _loanTerm = 3600
            , _loanInterest = Fraction (1,10)
            , _minPayment = 0
            , _penalty = NoPenalty
            , _collateralization = [(col,Fraction(1,1))]
            , _collateralIsSwappable = False
            , _claimPeriod = 3600
            , _offerDeposit = 4_000_000
            , _offerExpiration = Nothing
            }

      sampleOutputs ds = flip map ds $ \datum@OfferDatum{..} ->
        Output
          { outputAddress = loanAddress
          , outputValue = utxoValue (LV.Lovelace _offerDeposit) $ mconcat $
              [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
              , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
              , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId _lenderId) 1
              , uncurry PV2.singleton (_unAsset _loanAsset) _loanPrinciple
              ]
          , outputDatum = OutputDatum $ toDatum datum
          , outputReferenceScript = toReferenceScript Nothing
          }

  -- Initialize scenario
  References{negotiationRef,loanRef} <- initializeReferenceScripts 
  mintTestTokens lenderWallet 100_000_000 $ zip assetNames $ repeat 1000

  -- Create the Offer UTxO.
  forM_ (grouped 20 datums) $ \ds -> 
    transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
      emptyTxParams
        { tokens =
            [ TokenMint
                { mintTokens = flip concatMap ds $ \OfferDatum{_assetBeacon,_lenderId} ->
                    [ ("Offer",1)
                    , (_unAssetBeacon _assetBeacon,1)
                    , (_unLenderId _lenderId,1)
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

  -- Try to close the Offer UTxOs.
  void $ transact lenderPersonalAddr [loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens = flip map offerUTxOs $ \(_,Just OfferDatum{_assetBeacon,_lenderId}) ->
         TokenMint
            { mintTokens = 
                [ ("Offer",-1)
                , (_unAssetBeacon _assetBeacon,-1)
                , (_unLenderId _lenderId,-1)
                ]
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
      , referenceInputs = [negotiationRef,loanRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

-------------------------------------------------
-- TestTree
-------------------------------------------------
-- | A `TestTree` containing all benchmark scenarios for closing Offer UTxOs.
tests :: [TestTree]
tests =
  [ mustSucceed "benchTest1" $ benchTest1 30
  , mustSucceed "benchTest2" $ benchTest2 30

  , mustExceedTxLimits "perfIncreaseTest1" $ benchTest1 31
  , mustExceedTxLimits "perfIncreaseTest2" $ benchTest2 31
  ]
