{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.OfferUTxOs.UpdateOffer.Benchmarks where

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
-- | Convert multiple valid Offer UTxOs to ones that uses different loan assets. The offers
-- start and end with the same loan asset.
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
      loanAsset1 = Asset (testTokenSymbol,"TestToken2")
      loanAsset2 = Asset ("","")
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      loanBeacon1 = genLoanAssetBeaconName loanAsset1
      loanBeacon2 = genLoanAssetBeaconName loanAsset2
      loanDatum1 = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset1
        , _loanPrincipal = 10
        , _epochDuration = Nothing
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _compoundingInterest = True
        , _minPayment = 0
        , _penalty = NoPenalty
        , _maxConsecutiveMisses = Nothing
        , _collateralization = [(collateral1,Fraction(1,1))]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }
      loanDatum2 = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset2
        , _loanPrincipal = 10_000_000
        , _epochDuration = Nothing
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _compoundingInterest = True
        , _minPayment = 0
        , _penalty = NoPenalty
        , _maxConsecutiveMisses = Nothing
        , _collateralization = [(collateral1,Fraction(1,1))]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef,loanRef} <- initializeReferenceScripts 
  mintTestTokens lenderWallet 10_000_000 [("TestToken2",1000)]

  -- Create the Offer UTxO.
  replicateM_ 2 $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",20)
                  , (_unAssetBeacon loanBeacon1,20)
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
                , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon1) 1
                , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
                , uncurry PV2.singleton (_unAsset loanAsset1) 10
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
            { mintTokens = [(_unAssetBeacon loanBeacon1,-1),(_unAssetBeacon loanBeacon2,1)]
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
                , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon2) 1
                , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
                , uncurry PV2.singleton (_unAsset loanAsset2) 10_000_000
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
      lenderBeacon = genLenderId lenderCred
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
            { _lenderId = lenderCred
            , _lenderAddress = lenderAddr
            , _loanAsset = loan
            , _loanPrincipal = 10
            , _epochDuration = Nothing
            , _loanTerm = 3600
            , _loanInterest = Fraction (1,10)
            , _compoundingInterest = True
            , _minPayment = 0
            , _penalty = NoPenalty
            , _maxConsecutiveMisses = Nothing
            , _collateralization = [(col,Fraction(1,1))]
            , _collateralIsSwappable = False
            , _claimPeriod = 3600
            , _offerDeposit = 4_000_000
            , _offerExpiration = Nothing
            }
      
      beforeDatums = take 30 datums
      afterDatums = drop 30 datums

      sampleOutputs ds = flip map ds $ \datum@OfferDatum{..} ->
        Output
          { outputAddress = loanAddress
          , outputValue = utxoValue (LV.Lovelace _offerDeposit) $ mconcat $
              [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
              , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
              , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId _lenderId) 1
              , uncurry PV2.singleton (_unAsset _loanAsset) _loanPrincipal
              ]
          , outputDatum = OutputDatum $ toDatum datum
          , outputReferenceScript = toReferenceScript Nothing
          }

  -- Initialize scenario
  References{negotiationRef,loanRef} <- initializeReferenceScripts 
  mintTestTokens lenderWallet 100_000_000 $ zip assetNames $ repeat 1000

  -- Create the Offer UTxO.
  forM_ (grouped 20 beforeDatums) $ \ds -> 
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

  let sampleBurns = 
        zipWith 
          (\(_,Just OfferDatum{_assetBeacon=beforeBeacon}) OfferDatum{_assetBeacon=afterBeacon} ->
            TokenMint 
              { mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateOffer lenderCred
              , mintTokens = 
                  [ 
                    (_unAssetBeacon beforeBeacon,-1)
                  , (_unAssetBeacon afterBeacon,1)
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
-- TestTree
-------------------------------------------------
-- | A `TestTree` containing all benchmark scenarios for updating Offer UTxOs.
tests :: [TestTree]
tests =
  [ mustSucceed "benchTest1" $ benchTest1 19
  , mustSucceed "benchTest2" $ benchTest2 18

  , mustExceedTxLimits "perfIncreaseTest1" $ benchTest1 20
  , mustExceedTxLimits "perfIncreaseTest2" $ benchTest2 19
  ]
