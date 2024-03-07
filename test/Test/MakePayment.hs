{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.MakePayment
  ( 
    -- * Scenarios Tested
    -- ** Scenarios that should succeed
    regressionTest1

    -- ** Scenarios that should fail
    
    -- ** Benchmark Tests
    
    -- * Full TestTree
  , tests
  ) where

import qualified Ledger.Value.CardanoAPI as LV
import qualified Ledger.Address as LA
import qualified PlutusLedgerApi.V2 as PV2
import qualified Ledger.CardanoWallet as Mock 
import Test.Tasty (TestTree,testGroup)
import Data.String (fromString)
import Control.Monad (forM_)

import CardanoLoans

import Test.Prelude

-------------------------------------------------
-- Regression Tests
-------------------------------------------------
-- | Make a partial payment on a single loan. The loan asset is ada and the loan uses one native
-- asset as collateral.
regressionTest1 :: MonadEmulator m => m ()
regressionTest1 = do
  let -- Borrower Info
      borrowerWallet = Mock.knownMockWallet 1
      borrowerPersonalAddr = Mock.mockWalletAddress borrowerWallet
      borrowerPayPrivKey = Mock.paymentPrivateKey borrowerWallet
      borrowerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash borrowerWallet
      borrowerCred = PV2.PubKeyCredential borrowerPubKey
      borrowerBeacon = genBorrowerID borrowerCred
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
      askDatum = unsafeCreateAskDatum $ NewAskInfo
        { borrowerId = borrowerCred
        , loanAsset = loanAsset
        , loanPrinciple = 10_000_000
        , loanTerm = 3600
        , collateral = [collateral1]
        }
      offerDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { lenderId = lenderCred
        , lenderAddress = lenderAddr
        , loanAsset = loanAsset
        , loanPrinciple = 10_000_000
        , rolloverFrequency = Nothing
        , loanTerm = 3600
        , loanInterest = Fraction (1,10)
        , minPayment = 0
        , collateralization = [(collateral1,Fraction(1,1_000_000))]
        , collateralIsSwappable = False
        , claimPeriod = 3600
        , offerDeposit = 4_000_000
        , offerExpiration = Nothing
        }

  -- Initialize scenario
  (negotiationRef,activeRef,loanRef,_,paymentObserverRef) <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
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
              , outputDatum = OutputDatum $ toDatum askDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [borrowerPubKey]
      }

  -- Create the Offer UTxO.
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
              , outputDatum = OutputDatum $ toDatum offerDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

  startSlot <- currentSlot

  askRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (unAssetBeacon loanBeacon) 1
        , uncurry PV2.singleton (unAsset collateral1) 1
        ]

  offerRef <-
    txOutRefWithValue $ 
      utxoValue 4_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (unAssetBeacon loanBeacon) 1
        , PV2.singleton negotiationBeaconCurrencySymbol (unLenderID lenderBeacon) 1
        , uncurry PV2.singleton (unAsset loanAsset) 10_000_000
        ]

  let activeDatum = 
        createAcceptanceDatum borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      loanIdBeacon = genLoanId offerRef

  -- Accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (unAssetBeacon loanBeacon,-2)
                  , (unLenderID lenderBeacon,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (unBorrowerID borrowerBeacon,1)
                  , (unAssetBeacon loanBeacon,1)
                  , (unLoanID loanIdBeacon,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive borrowerCred negotiationBeaconCurrencySymbol
              , mintPolicy = toVersionedMintingPolicy activeBeaconScript
              , mintReference = Just activeRef
              }
          ]
      , inputs = 
          [ Input
              { inputId = offerRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer AcceptOffer)
              }
          , Input
              { inputId = askRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer AcceptOffer)
              }
          ]
      , outputs =
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (unBorrowerID borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (unLoanID loanIdBeacon) 1
                  , uncurry PV2.singleton (unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (unLoanID loanIdBeacon) 1 ]
              , outputDatum = 
                  OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,"Accepted")
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just startSlot
          , validityRangeUpperBound = Nothing
          }
      }

  activeUTxOs <-
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      loanAddress 
      (activeBeaconCurrencySymbol,"Active")

  let samplePayments acs = flip concatMap acs $ 
        \(offerRef,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (unAssetBeacon assetBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (unBorrowerID borrowerId) 1
                  , PV2.singleton activeBeaconCurrencySymbol (unLoanID loanId) 1
                  , uncurry PV2.singleton (unAsset collateral1) 6
                  ]
              , outputDatum = OutputDatum $ toDatum $ createPostPaymentActiveDatum 5_000_000 ad
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 5_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,unLoanID loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("dummy",1)]
              , mintRedeemer = toRedeemer ObservePayment
              , mintPolicy = toVersionedMintingPolicy paymentObserverScript
              , mintReference = Just paymentObserverRef
              }
          ]
      , inputs = flip map activeUTxOs $ \(activeRef,_) ->
          Input
            { inputId = activeRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just startSlot
          , validityRangeUpperBound = Nothing
          }
      }

-------------------------------------------------
-- Benchmark Tests
-------------------------------------------------
-- | Make partial payments on multiple Active loans.
benchTest1 :: MonadEmulator m => Int -> m ()
benchTest1 number = do
  let -- Borrower Info
      borrowerWallet = Mock.knownMockWallet 1
      borrowerPersonalAddr = Mock.mockWalletAddress borrowerWallet
      borrowerPayPrivKey = Mock.paymentPrivateKey borrowerWallet
      borrowerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash borrowerWallet
      borrowerCred = PV2.PubKeyCredential borrowerPubKey
      borrowerBeacon = genBorrowerID borrowerCred
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
      askDatum = unsafeCreateAskDatum $ NewAskInfo
        { borrowerId = borrowerCred
        , loanAsset = loanAsset
        , loanPrinciple = 10_000_000
        , loanTerm = 3600
        , collateral = [collateral1]
        }
      offerDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { lenderId = lenderCred
        , lenderAddress = lenderAddr
        , loanAsset = loanAsset
        , loanPrinciple = 10_000_000
        , rolloverFrequency = Nothing
        , loanTerm = 10000
        , loanInterest = Fraction (1,10)
        , minPayment = 0
        , collateralization = [(collateral1,Fraction(1,1_000_000))]
        , collateralIsSwappable = False
        , claimPeriod = 3600
        , offerDeposit = 4_000_000
        , offerExpiration = Nothing
        }

  -- Initialize scenario
  (negotiationRef,activeRef,loanRef,_,paymentObserverRef) <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 900_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",20),(unAssetBeacon loanBeacon,20)]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs = replicate 20
          Output
            { outputAddress = loanAddress
            , outputValue = utxoValue 3_000_000 $ mconcat
                [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                , PV2.singleton negotiationBeaconCurrencySymbol (unAssetBeacon loanBeacon) 1
                , uncurry PV2.singleton (unAsset collateral1) 1
                ]
            , outputDatum = OutputDatum $ toDatum askDatum
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [borrowerPubKey]
      }

  -- Create the Offer UTxO.
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",20)
                  , (unAssetBeacon loanBeacon,20)
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
                , PV2.singleton negotiationBeaconCurrencySymbol (unAssetBeacon loanBeacon) 1
                , PV2.singleton negotiationBeaconCurrencySymbol (unLenderID lenderBeacon) 1
                , uncurry PV2.singleton (unAsset loanAsset) 10_000_000
                ]
            , outputDatum = OutputDatum $ toDatum offerDatum
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

  askUTxOs <-
    txOutRefsAndDatumsAtAddressWithBeacon @AskDatum 
      loanAddress 
      (negotiationBeaconCurrencySymbol,"Ask")

  offerUTxOs <- 
    txOutRefsAndDatumsAtAddressWithBeacon @OfferDatum
      loanAddress 
      (negotiationBeaconCurrencySymbol,"Offer")

  let sampleBurns os as = concat $ (flip . flip zipWith) os as $
        \(offerRef,Just OfferDatum{assetBeacon=oLoanBeacon,lenderId}) 
         (_,Just AskDatum{assetBeacon=aLoanBeacon}) ->
            [ TokenMint
                { mintTokens = 
                    [ ("Offer",-1)
                    , ("Ask",-1)
                    , (unAssetBeacon oLoanBeacon,-1)
                    , (unAssetBeacon aLoanBeacon,-1)
                    , (unLenderID lenderId,-1)
                    ]
                , mintRedeemer = toRedeemer BurnNegotiationBeacons
                , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
                , mintReference = Just negotiationRef
                }
            , TokenMint
                { mintTokens = 
                    [ ("Active",1)
                    , (unBorrowerID borrowerBeacon,1)
                    , (unAssetBeacon oLoanBeacon,1)
                    , (unLoanID $ genLoanId offerRef,2)
                    ]
                , mintRedeemer = toRedeemer $ 
                    CreateActive borrowerCred negotiationBeaconCurrencySymbol
                , mintPolicy = toVersionedMintingPolicy activeBeaconScript
                , mintReference = Just activeRef
                }
            ]

      sampleOutputs start os = flip concatMap os $ 
        \(offerRef,Just od@OfferDatum{assetBeacon}) ->
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (unAssetBeacon assetBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (unBorrowerID borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (unLoanID $ genLoanId offerRef) 1
                  , uncurry PV2.singleton (unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum $ 
                  createAcceptanceDatum borrowerCred offerRef (slotToPosixTime start) od
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (unLoanID $ genLoanId offerRef) 1 ]
              , outputDatum = 
                  OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,"Accepted")
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

      sampleInputs os as = concat $
        [ flip map os $ \(offerRef,_) ->
            Input
              { inputId = offerRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer AcceptOffer)
              }
        , flip map as $ \(askRef,_) ->
            Input
              { inputId = askRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer AcceptOffer)
              }
        ]
      
  -- Accept the offers.
  forM_ (zip (grouped 5 offerUTxOs) (grouped 5 askUTxOs)) $ \(offers,asks) -> do
      startSlot <- currentSlot

      transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
        emptyTxParams
          { tokens = sampleBurns offers asks
          , inputs = 
              concat $ (flip . flip zipWith) offers asks $ \(offerRef,_) (askRef,_) ->
                [ Input
                    { inputId = offerRef
                    , inputWitness = 
                        SpendWithPlutusReference loanRef InlineDatum (toRedeemer AcceptOffer)
                    }
                , Input
                    { inputId = askRef
                    , inputWitness = 
                        SpendWithPlutusReference loanRef InlineDatum (toRedeemer AcceptOffer)
                    }
                ]
          , outputs = sampleOutputs startSlot offers
          , referenceInputs = [negotiationRef,activeRef,loanRef]
          , extraKeyWitnesses = [borrowerPubKey]
          , validityRange = ValidityRange
              { validityRangeLowerBound = Just startSlot
              , validityRangeUpperBound = Nothing
              }
          }

  activeUTxOs <- take number <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      loanAddress 
      (activeBeaconCurrencySymbol,"Active")

  let samplePayments acs = flip concatMap acs $ 
        \(offerRef,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (unAssetBeacon assetBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (unBorrowerID borrowerId) 1
                  , PV2.singleton activeBeaconCurrencySymbol (unLoanID loanId) 1
                  , uncurry PV2.singleton (unAsset collateral1) 6
                  ]
              , outputDatum = OutputDatum $ toDatum $ createPostPaymentActiveDatum 5_000_000 ad
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 5_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,unLoanID loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  paymentTime <- currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("dummy",1)]
              , mintRedeemer = toRedeemer ObservePayment
              , mintPolicy = toVersionedMintingPolicy paymentObserverScript
              , mintReference = Just paymentObserverRef
              }
          ]
      , inputs = flip map activeUTxOs $ \(activeRef,_) ->
          Input
            { inputId = activeRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just paymentTime
          , validityRangeUpperBound = Nothing
          }
      }

-------------------------------------------------
-- Test Function
-------------------------------------------------
-- | A `TestTree` containing all payment scenarios.
tests :: TestTree
tests =
  testGroup "Accept Offer(s)"
    [ -- Success Tests (Regression Tests)
      mustSucceed "regressionTest1" regressionTest1
    -- , mustSucceed "regressionTest2" regressionTest2
    -- , mustSucceed "regressionTest3" regressionTest3
    -- , mustSucceed "regressionTest4" regressionTest4
    -- , mustSucceed "regressionTest5" regressionTest5
    -- , mustSucceed "regressionTest6" regressionTest6

      -- Benchmark Tests
    , mustSucceed "benchTest1" $ benchTest1 8
    -- , mustSucceed "benchTest2" $ benchTest2 32
    -- , mustSucceed "benchTest3" $ benchTest3 32
    -- , mustSucceed "benchTest4" $ benchTest4 24

      -- Performance Increase Tests
    , mustExceedTxLimits "perfIncreaseTest1" $ benchTest1 9
    -- , mustExceedTxLimits "perfIncreaseTest2" $ benchTest2 33
    -- , mustExceedTxLimits "perfIncreaseTest3" $ benchTest3 33
    -- , mustExceedTxLimits "perfIncreaseTest4" $ benchTest4 25
    ]
