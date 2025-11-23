{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.ActiveUTxOs.ClaimDefaultedCollateral.Benchmarks where

import qualified Ledger.Address as LA
import qualified PlutusLedgerApi.V2 as PV2
import qualified Ledger.CardanoWallet as Mock 
import Test.Tasty (TestTree)
import Control.Monad (forM,forM_)
import Data.String (fromString)

import CardanoLoans

import Test.Prelude

-------------------------------------------------
-- Benchmark Tests
-------------------------------------------------
-- | Claim the collateral for multiple expired loans. All loans use one asset as collateral. The key NFTs
-- are in separate UTxOs. All loans come from the same borrower and use the same loan asset.
benchTest1 :: MonadEmulator m => Int -> m ()
benchTest1 number = do
  let -- Borrower Info
      borrowerWallet = Mock.knownMockWallet 1
      borrowerPersonalAddr = Mock.mockWalletAddress borrowerWallet
      borrowerPayPrivKey = Mock.paymentPrivateKey borrowerWallet
      borrowerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash borrowerWallet
      borrowerCred = PV2.PubKeyCredential borrowerPubKey
      borrowerBeacon = genBorrowerId borrowerCred
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
      newLenderAddr = PV2.Address lenderCred Nothing

      -- Loan Info
      loanAsset = Asset (adaSymbol,adaToken)
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      loanBeacon = genLoanAssetBeaconName loanAsset
      askDatum = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred
        , _loanAsset = loanAsset
        , _loanPrincipal = 10_000_000
        , _loanTerm = 3600
        , _collateral = [collateral1]
        }
      offerDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrincipal = 10_000_000
        , _epochDuration = Nothing
        , _loanTerm = 10000
        , _loanInterest = Fraction (1,10)
        , _compoundingInterest = True
        , _minPayment = 0
        , _penalty = NoPenalty
        , _maxConsecutiveMisses = Nothing
        , _collateralization = [(collateral1,Fraction(1,1_000_000))]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef,activeRef,loanRef,addressUpdateObserverRef,proxyRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 900_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",20),(_unAssetBeacon loanBeacon,20)]
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
                , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                , uncurry PV2.singleton (_unAsset collateral1) 1
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
        \(offerRef,Just OfferDatum{_assetBeacon=oLoanBeacon,_lenderId}) 
         (_,Just AskDatum{_assetBeacon=aLoanBeacon}) ->
            [ TokenMint
                { mintTokens = 
                    [ ("Offer",-1)
                    , ("Ask",-1)
                    , (_unAssetBeacon oLoanBeacon,-1)
                    , (_unAssetBeacon aLoanBeacon,-1)
                    , (_unLenderId _lenderId,-1)
                    ]
                , mintRedeemer = toRedeemer BurnNegotiationBeacons
                , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
                , mintReference = Just negotiationRef
                }
            , TokenMint
                { mintTokens = 
                    [ ("Active",1)
                    , (_unBorrowerId borrowerBeacon,1)
                    , (_unAssetBeacon oLoanBeacon,1)
                    , (_unLoanId $ genLoanId offerRef,2)
                    ]
                , mintRedeemer = toRedeemer $ 
                    CreateActive negotiationBeaconCurrencySymbol
                , mintPolicy = toVersionedMintingPolicy activeBeaconScript
                , mintReference = Just activeRef
                }
            ]

      sampleOutputs start os = flip concatMap os $ 
        \(offerRef,Just od@OfferDatum{_assetBeacon}) ->
          let activeDatum@ActiveDatum{_loanId,_lenderAddress} = 
                createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime start) offerDatum
          in [ Output
                { outputAddress = loanAddress
                , outputValue = utxoValue 4_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unLoanId $ genLoanId offerRef) 1
                    , uncurry PV2.singleton (_unAsset collateral1) 10
                    ]
                , outputDatum = OutputDatum $ toDatum $ 
                    createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime start) od
                , outputReferenceScript = toReferenceScript Nothing
                }
             , Output
                { outputAddress = toCardanoApiAddress lenderAddr
                , outputValue = utxoValue 4_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1 ]
                , outputDatum = 
                    OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
                , outputReferenceScript = toReferenceScript Nothing
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

  keyUTxOs <- fmap concat $ forM activeUTxOs $ \(_,Just ad@ActiveDatum{_loanId}) ->
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      (toCardanoApiAddress lenderAddr) 
      (activeBeaconCurrencySymbol,_unLoanId _loanId)

  let sampleKeyBurns acs = flip map acs $
        \(_,Just ad@ActiveDatum{..}) ->
          TokenMint
            { mintTokens = 
                [ ("Active",-1)
                , (_unBorrowerId _borrowerId,-1)
                , (_unAssetBeacon _assetBeacon,-1)
                , (_unLoanId _loanId,-2)
                ]
            , mintRedeemer = toRedeemer BurnKeyAndClaimDefaulted
            , mintPolicy = toVersionedMintingPolicy activeBeaconScript
            , mintReference = Just activeRef
            }

  currentSlot >>= awaitTime . slotToPosixTime . (+3600)
  claimSlot <- currentSlot

  -- Try to claim the collateral.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens = sampleKeyBurns activeUTxOs
      , inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ SpendWithKeyNFT)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , referenceInputs = [activeRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just claimSlot
          , validityRangeUpperBound = Nothing
          }
      }

-- | Claim the collateral for multiple expired loans. All loans use one asset as collateral. The key NFTs
-- are in one UTxO. All loans come from the same borrower and use the same loan asset.
benchTest2 :: MonadEmulator m => Int -> m ()
benchTest2 number = do
  let -- Borrower Info
      borrowerWallet = Mock.knownMockWallet 1
      borrowerPersonalAddr = Mock.mockWalletAddress borrowerWallet
      borrowerPayPrivKey = Mock.paymentPrivateKey borrowerWallet
      borrowerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash borrowerWallet
      borrowerCred = PV2.PubKeyCredential borrowerPubKey
      borrowerBeacon = genBorrowerId borrowerCred
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
      newLenderAddr = PV2.Address lenderCred Nothing

      -- Loan Info
      loanAsset = Asset (adaSymbol,adaToken)
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      loanBeacon = genLoanAssetBeaconName loanAsset
      askDatum = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred
        , _loanAsset = loanAsset
        , _loanPrincipal = 10_000_000
        , _loanTerm = 3600
        , _collateral = [collateral1]
        }
      offerDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrincipal = 10_000_000
        , _epochDuration = Nothing
        , _loanTerm = 10000
        , _loanInterest = Fraction (1,10)
        , _compoundingInterest = True
        , _minPayment = 0
        , _penalty = NoPenalty
        , _maxConsecutiveMisses = Nothing
        , _collateralization = [(collateral1,Fraction(1,1_000_000))]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef,activeRef,loanRef,addressUpdateObserverRef,proxyRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 900_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",20),(_unAssetBeacon loanBeacon,20)]
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
                , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                , uncurry PV2.singleton (_unAsset collateral1) 1
                ]
            , outputDatum = OutputDatum $ toDatum askDatum
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
              { mintTokens = [("Ask",20),(_unAssetBeacon loanBeacon,20)]
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
                , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                , uncurry PV2.singleton (_unAsset collateral1) 1
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
            , outputDatum = OutputDatum $ toDatum offerDatum
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

  -- Create the Offer UTxO.
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
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
        \(offerRef,Just OfferDatum{_assetBeacon=oLoanBeacon,_lenderId}) 
         (_,Just AskDatum{_assetBeacon=aLoanBeacon}) ->
            [ TokenMint
                { mintTokens = 
                    [ ("Offer",-1)
                    , ("Ask",-1)
                    , (_unAssetBeacon oLoanBeacon,-1)
                    , (_unAssetBeacon aLoanBeacon,-1)
                    , (_unLenderId _lenderId,-1)
                    ]
                , mintRedeemer = toRedeemer BurnNegotiationBeacons
                , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
                , mintReference = Just negotiationRef
                }
            , TokenMint
                { mintTokens = 
                    [ ("Active",1)
                    , (_unBorrowerId borrowerBeacon,1)
                    , (_unAssetBeacon oLoanBeacon,1)
                    , (_unLoanId $ genLoanId offerRef,2)
                    ]
                , mintRedeemer = toRedeemer $ 
                    CreateActive negotiationBeaconCurrencySymbol
                , mintPolicy = toVersionedMintingPolicy activeBeaconScript
                , mintReference = Just activeRef
                }
            ]

      sampleOutputs start os = flip concatMap os $ 
        \(offerRef,Just od@OfferDatum{_assetBeacon}) ->
          let activeDatum@ActiveDatum{_loanId,_lenderAddress} = 
                createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime start) offerDatum
          in [ Output
                { outputAddress = loanAddress
                , outputValue = utxoValue 4_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unLoanId $ genLoanId offerRef) 1
                    , uncurry PV2.singleton (_unAsset collateral1) 10
                    ]
                , outputDatum = OutputDatum $ toDatum $ 
                    createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime start) od
                , outputReferenceScript = toReferenceScript Nothing
                }
             , Output
                { outputAddress = toCardanoApiAddress lenderAddr
                , outputValue = utxoValue 4_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1 ]
                , outputDatum = 
                    OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
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

  keyUTxOs <- fmap concat $ forM activeUTxOs $ \(_,Just ad@ActiveDatum{_loanId}) ->
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      (toCardanoApiAddress lenderAddr) 
      (activeBeaconCurrencySymbol,_unLoanId _loanId)

  -- Consolidate the key NFTs.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip map keyUTxOs $ \(keyRef,_) ->
          Input
            { inputId = keyRef
            , inputWitness = 
                SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
            }
      , outputs = 
          [ 
            Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 5_000_000 $ mconcat $
                  flip map activeUTxOs $ \(_,Just ActiveDatum{_loanId}) ->
                    PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
              , outputDatum = OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,"")
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Nothing
          }
      }

  newKeyUTxO <- fmap head $ fmap concat $ forM activeUTxOs $ \(_,Just ad@ActiveDatum{_loanId}) ->
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      (toCardanoApiAddress lenderAddr) 
      (activeBeaconCurrencySymbol,_unLoanId _loanId)

  let sampleKeyBurns acs = flip map acs $
        \(_,Just ad@ActiveDatum{..}) ->
          TokenMint
            { mintTokens = 
                [ ("Active",-1)
                , (_unBorrowerId _borrowerId,-1)
                , (_unAssetBeacon _assetBeacon,-1)
                , (_unLoanId _loanId,-2)
                ]
            , mintRedeemer = toRedeemer BurnKeyAndClaimDefaulted
            , mintPolicy = toVersionedMintingPolicy activeBeaconScript
            , mintReference = Just activeRef
            }

  currentSlot >>= awaitTime . slotToPosixTime . (+3600)
  claimSlot <- currentSlot

  -- Try to claim the collateral.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens = sampleKeyBurns activeUTxOs
      , inputs = mconcat
          [ flip map activeUTxOs $ \(activeUtxoRef,_) ->
              Input
                { inputId = activeUtxoRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum $ toRedeemer $ 
                      SpendWithKeyNFT
                }
          , [ Input
                { inputId = fst newKeyUTxO
                , inputWitness = 
                    SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
                }
            ]
          ]
      , referenceInputs = [activeRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just claimSlot
          , validityRangeUpperBound = Nothing
          }
      }

-- | Claim the collateral for multiple expired loans. All loans use one asset as collateral. The key NFTs
-- are in one UTxO. All loans come from the same borrower, but use different loan assets.
benchTest3 :: MonadEmulator m => Int -> m ()
benchTest3 number = do
  let -- Borrower Info
      borrowerWallet = Mock.knownMockWallet 1
      borrowerPersonalAddr = Mock.mockWalletAddress borrowerWallet
      borrowerPayPrivKey = Mock.paymentPrivateKey borrowerWallet
      borrowerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash borrowerWallet
      borrowerCred = PV2.PubKeyCredential borrowerPubKey
      borrowerBeacon = genBorrowerId borrowerCred
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
      newLenderAddr = PV2.Address lenderCred Nothing

      -- Loan Info
      loanAssets = map (\i -> Asset (testTokenSymbol, fromString $ "TestToken" <> show @Int i)) [1..30]
      collateral1 = Asset (testTokenSymbol,"TestToken31")
      askDatums = flip map loanAssets $ \loanAsset -> unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred
        , _loanAsset = loanAsset
        , _loanPrincipal = 10
        , _loanTerm = 3600
        , _collateral = [collateral1]
        }
      offerDatums = flip map loanAssets $ \loanAsset -> unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrincipal = 10
        , _epochDuration = Nothing
        , _loanTerm = 10000
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
  References{negotiationRef,activeRef,loanRef,addressUpdateObserverRef,proxyRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 900_000_000 [("TestToken31",1000)]
  mintTestTokens lenderWallet 900_000_000 $ map ((,1000) . snd . _unAsset) loanAssets

  -- Create the Ask UTxO.
  forM_ askDatums $ \askDatum@AskDatum{..} -> 
    transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey]
      emptyTxParams
        { tokens =
            [ TokenMint
                { mintTokens = [("Ask",1),(_unAssetBeacon _assetBeacon,1)]
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
                    , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                    , uncurry PV2.singleton (_unAsset collateral1) 1
                    ]
                , outputDatum = OutputDatum $ toDatum askDatum
                , outputReferenceScript = toReferenceScript Nothing
                }
            ]
        , referenceInputs = [negotiationRef]
        , extraKeyWitnesses = [borrowerPubKey]
        }

  -- Create the Offer UTxO.
  forM_ offerDatums $ \offerDatum@OfferDatum{..} ->
    transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey]
      emptyTxParams
        { tokens =
            [ TokenMint
                { mintTokens = 
                    [ ("Offer",1)
                    , (_unAssetBeacon _assetBeacon,1)
                    , (_unLenderId lenderBeacon,1)
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
                    , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                    , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
                    , uncurry PV2.singleton (_unAsset _loanAsset) _loanPrincipal
                    ]
                , outputDatum = OutputDatum $ toDatum offerDatum
                , outputReferenceScript = toReferenceScript Nothing
                }
            ]
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
        \(offerRef,Just OfferDatum{_assetBeacon=oLoanBeacon,_lenderId}) 
         (_,Just AskDatum{_assetBeacon=aLoanBeacon}) ->
            [ TokenMint
                { mintTokens = 
                    [ ("Offer",-1)
                    , ("Ask",-1)
                    , (_unAssetBeacon oLoanBeacon,-1)
                    , (_unAssetBeacon aLoanBeacon,-1)
                    , (_unLenderId _lenderId,-1)
                    ]
                , mintRedeemer = toRedeemer BurnNegotiationBeacons
                , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
                , mintReference = Just negotiationRef
                }
            , TokenMint
                { mintTokens = 
                    [ ("Active",1)
                    , (_unBorrowerId borrowerBeacon,1)
                    , (_unAssetBeacon oLoanBeacon,1)
                    , (_unLoanId $ genLoanId offerRef,2)
                    ]
                , mintRedeemer = toRedeemer $ 
                    CreateActive negotiationBeaconCurrencySymbol
                , mintPolicy = toVersionedMintingPolicy activeBeaconScript
                , mintReference = Just activeRef
                }
            ]

      sampleOutputs start os = flip concatMap os $ 
        \(offerRef,Just od@OfferDatum{_assetBeacon}) ->
          let activeDatum@ActiveDatum{_loanId,_lenderAddress} = 
                createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime start) od
          in [ Output
                { outputAddress = loanAddress
                , outputValue = utxoValue 4_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unLoanId $ genLoanId offerRef) 1
                    , uncurry PV2.singleton (_unAsset collateral1) 10
                    ]
                , outputDatum = OutputDatum $ toDatum $ 
                    createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime start) od
                , outputReferenceScript = toReferenceScript Nothing
                }
             , Output
                { outputAddress = toCardanoApiAddress lenderAddr
                , outputValue = utxoValue 4_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1 ]
                , outputDatum = 
                    OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
                , outputReferenceScript = toReferenceScript Nothing
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

  keyUTxOs <- fmap concat $ forM activeUTxOs $ \(_,Just ad@ActiveDatum{_loanId}) ->
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      (toCardanoApiAddress lenderAddr) 
      (activeBeaconCurrencySymbol,_unLoanId _loanId)

  -- Consolidate the key NFTs.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip map keyUTxOs $ \(keyRef,_) ->
          Input
            { inputId = keyRef
            , inputWitness = 
                SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
            }
      , outputs = 
          [ 
            Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 5_000_000 $ mconcat $
                  flip map activeUTxOs $ \(_,Just ActiveDatum{_loanId}) ->
                    PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
              , outputDatum = OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,"")
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Nothing
          }
      }

  newKeyUTxO <- fmap head $ fmap concat $ forM activeUTxOs $ \(_,Just ad@ActiveDatum{_loanId}) ->
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      (toCardanoApiAddress lenderAddr) 
      (activeBeaconCurrencySymbol,_unLoanId _loanId)

  let sampleKeyBurns acs = flip map acs $
        \(_,Just ad@ActiveDatum{..}) ->
          TokenMint
            { mintTokens = 
                [ ("Active",-1)
                , (_unBorrowerId _borrowerId,-1)
                , (_unAssetBeacon _assetBeacon,-1)
                , (_unLoanId _loanId,-2)
                ]
            , mintRedeemer = toRedeemer BurnKeyAndClaimDefaulted
            , mintPolicy = toVersionedMintingPolicy activeBeaconScript
            , mintReference = Just activeRef
            }

  currentSlot >>= awaitTime . slotToPosixTime . (+3600)
  claimSlot <- currentSlot

  -- Try to claim the collateral.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens = sampleKeyBurns activeUTxOs
      , inputs = mconcat
          [ flip map activeUTxOs $ \(activeUtxoRef,_) ->
              Input
                { inputId = activeUtxoRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum $ toRedeemer $ 
                      SpendWithKeyNFT
                }
          , [ Input
                { inputId = fst newKeyUTxO
                , inputWitness = 
                    SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
                }
            ]
          ]
      , referenceInputs = [activeRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just claimSlot
          , validityRangeUpperBound = Nothing
          }
      }

-- | Claim the collateral for multiple expired loans. All loans use one asset as collateral. The key NFTs
-- are in separate UTxOs. All loans come from the same borrower, but use different loan assets.
benchTest4 :: MonadEmulator m => Int -> m ()
benchTest4 number = do
  let -- Borrower Info
      borrowerWallet = Mock.knownMockWallet 1
      borrowerPersonalAddr = Mock.mockWalletAddress borrowerWallet
      borrowerPayPrivKey = Mock.paymentPrivateKey borrowerWallet
      borrowerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash borrowerWallet
      borrowerCred = PV2.PubKeyCredential borrowerPubKey
      borrowerBeacon = genBorrowerId borrowerCred
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
      newLenderAddr = PV2.Address lenderCred Nothing

      -- Loan Info
      loanAssets = map (\i -> Asset (testTokenSymbol, fromString $ "TestToken" <> show @Int i)) [1..30]
      collateral1 = Asset (testTokenSymbol,"TestToken31")
      askDatums = flip map loanAssets $ \loanAsset -> unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred
        , _loanAsset = loanAsset
        , _loanPrincipal = 10
        , _loanTerm = 3600
        , _collateral = [collateral1]
        }
      offerDatums = flip map loanAssets $ \loanAsset -> unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrincipal = 10
        , _epochDuration = Nothing
        , _loanTerm = 10000
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
  References{negotiationRef,activeRef,loanRef,addressUpdateObserverRef,proxyRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 900_000_000 [("TestToken31",1000)]
  mintTestTokens lenderWallet 900_000_000 $ map ((,1000) . snd . _unAsset) loanAssets

  -- Create the Ask UTxO.
  forM_ askDatums $ \askDatum@AskDatum{..} -> 
    transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey]
      emptyTxParams
        { tokens =
            [ TokenMint
                { mintTokens = [("Ask",1),(_unAssetBeacon _assetBeacon,1)]
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
                    , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                    , uncurry PV2.singleton (_unAsset collateral1) 1
                    ]
                , outputDatum = OutputDatum $ toDatum askDatum
                , outputReferenceScript = toReferenceScript Nothing
                }
            ]
        , referenceInputs = [negotiationRef]
        , extraKeyWitnesses = [borrowerPubKey]
        }

  -- Create the Offer UTxO.
  forM_ offerDatums $ \offerDatum@OfferDatum{..} ->
    transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey]
      emptyTxParams
        { tokens =
            [ TokenMint
                { mintTokens = 
                    [ ("Offer",1)
                    , (_unAssetBeacon _assetBeacon,1)
                    , (_unLenderId lenderBeacon,1)
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
                    , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                    , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
                    , uncurry PV2.singleton (_unAsset _loanAsset) _loanPrincipal
                    ]
                , outputDatum = OutputDatum $ toDatum offerDatum
                , outputReferenceScript = toReferenceScript Nothing
                }
            ]
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
        \(offerRef,Just OfferDatum{_assetBeacon=oLoanBeacon,_lenderId}) 
         (_,Just AskDatum{_assetBeacon=aLoanBeacon}) ->
            [ TokenMint
                { mintTokens = 
                    [ ("Offer",-1)
                    , ("Ask",-1)
                    , (_unAssetBeacon oLoanBeacon,-1)
                    , (_unAssetBeacon aLoanBeacon,-1)
                    , (_unLenderId _lenderId,-1)
                    ]
                , mintRedeemer = toRedeemer BurnNegotiationBeacons
                , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
                , mintReference = Just negotiationRef
                }
            , TokenMint
                { mintTokens = 
                    [ ("Active",1)
                    , (_unBorrowerId borrowerBeacon,1)
                    , (_unAssetBeacon oLoanBeacon,1)
                    , (_unLoanId $ genLoanId offerRef,2)
                    ]
                , mintRedeemer = toRedeemer $ 
                    CreateActive negotiationBeaconCurrencySymbol
                , mintPolicy = toVersionedMintingPolicy activeBeaconScript
                , mintReference = Just activeRef
                }
            ]

      sampleOutputs start os = flip concatMap os $ 
        \(offerRef,Just od@OfferDatum{_assetBeacon}) ->
          let activeDatum@ActiveDatum{_loanId,_lenderAddress} = 
                createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime start) od
          in [ Output
                { outputAddress = loanAddress
                , outputValue = utxoValue 4_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unLoanId $ genLoanId offerRef) 1
                    , uncurry PV2.singleton (_unAsset collateral1) 10
                    ]
                , outputDatum = OutputDatum $ toDatum $ 
                    createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime start) od
                , outputReferenceScript = toReferenceScript Nothing
                }
             , Output
                { outputAddress = toCardanoApiAddress lenderAddr
                , outputValue = utxoValue 4_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1 ]
                , outputDatum = 
                    OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
                , outputReferenceScript = toReferenceScript Nothing
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

  keyUTxOs <- fmap concat $ forM activeUTxOs $ \(_,Just ad@ActiveDatum{_loanId}) ->
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      (toCardanoApiAddress lenderAddr) 
      (activeBeaconCurrencySymbol,_unLoanId _loanId)

  let sampleKeyBurns acs = flip map acs $
        \(_,Just ad@ActiveDatum{..}) ->
          TokenMint
            { mintTokens = 
                [ ("Active",-1)
                , (_unBorrowerId _borrowerId,-1)
                , (_unAssetBeacon _assetBeacon,-1)
                , (_unLoanId _loanId,-2)
                ]
            , mintRedeemer = toRedeemer BurnKeyAndClaimDefaulted
            , mintPolicy = toVersionedMintingPolicy activeBeaconScript
            , mintReference = Just activeRef
            }

  currentSlot >>= awaitTime . slotToPosixTime . (+3600)
  claimSlot <- currentSlot

  -- Try to claim the collateral.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens = sampleKeyBurns activeUTxOs
      , inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ SpendWithKeyNFT)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , referenceInputs = [activeRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just claimSlot
          , validityRangeUpperBound = Nothing
          }
      }

-- | Claim the collateral for multiple expired loans. All loans use one asset as collateral. The key NFTs
-- are in separate UTxOs. All loans come from different borrowers, and use different loan assets.
benchTest5 :: MonadEmulator m => Int -> m ()
benchTest5 number = do
  References{..} <- initializeReferenceScripts 

  let -- Lender Info
      lenderWallet = Mock.knownMockWallet 1
      lenderPersonalAddr = Mock.mockWalletAddress lenderWallet
      lenderPayPrivKey = Mock.paymentPrivateKey lenderWallet
      lenderPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash lenderWallet
      lenderCred = PV2.PubKeyCredential lenderPubKey
      lenderBeacon = genLenderId lenderCred
      lenderAddr = 
        PV2.Address (PV2.ScriptCredential $ scriptHash proxyScript) 
                    (Just $ PV2.StakingHash lenderCred)
      newLenderAddr = PV2.Address lenderCred Nothing

      -- Loan Info
      loanAssets = map (\i -> Asset (testTokenSymbol, fromString $ "TestToken" <> show @Int i)) [1..10]
      collateral1 = Asset (testTokenSymbol,"TestToken11")
      offerDatums = flip map loanAssets $ \loanAsset -> unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrincipal = 10
        , _epochDuration = Nothing
        , _loanTerm = 10000
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

  borrowerInfo <- forM (zip loanAssets [1..10]) $ \(loanAsset,i) -> do
      let -- Borrower Info
          borrowerWallet = Mock.knownMockWallet i
          borrowerPersonalAddr = Mock.mockWalletAddress borrowerWallet
          borrowerPayPrivKey = Mock.paymentPrivateKey borrowerWallet
          borrowerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash borrowerWallet
          borrowerCred = PV2.PubKeyCredential borrowerPubKey
          borrowerBeacon = genBorrowerId borrowerCred
          loanAddress = toCardanoApiAddress $
            PV2.Address (PV2.ScriptCredential $ scriptHash loanScript) 
                        (Just $ PV2.StakingHash borrowerCred)

          askDatum@AskDatum{..} = unsafeCreateAskDatum $ NewAskInfo
            { _borrowerId = borrowerCred
            , _loanAsset = loanAsset
            , _loanPrincipal = 10
            , _loanTerm = 3600
            , _collateral = [collateral1]
            }

      mintTestTokens borrowerWallet 900_000_000 [("TestToken11",1000)]

      -- Create the Ask UTxO.
      void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
        emptyTxParams
          { tokens =
              [ TokenMint
                  { mintTokens = [("Ask",1),(_unAssetBeacon _assetBeacon,1)]
                  , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred
                  , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
                  , mintReference = Just negotiationRef
                  }
              ]
          , outputs = replicate 1
              Output
                { outputAddress = loanAddress
                , outputValue = utxoValue 3_000_000 $ mconcat
                    [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                    , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                    , uncurry PV2.singleton (_unAsset collateral1) 1
                    ]
                , outputDatum = OutputDatum $ toDatum askDatum
                , outputReferenceScript = toReferenceScript Nothing
                }
          , referenceInputs = [negotiationRef]
          , extraKeyWitnesses = [borrowerPubKey]
          }

      return (borrowerPersonalAddr,borrowerPayPrivKey,borrowerPubKey,loanAddress,borrowerCred)

  -- Create the Offer UTxO.
  mintTestTokens lenderWallet 900_000_000 $ map ((,1000) . snd . _unAsset) loanAssets

  forM_ (zip offerDatums borrowerInfo) $ \(offerDatum@OfferDatum{..},(_,_,_,loanAddress,_)) ->
    transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey]
      emptyTxParams
        { tokens =
            [ TokenMint
                { mintTokens = 
                    [ ("Offer",1)
                    , (_unAssetBeacon _assetBeacon,1)
                    , (_unLenderId lenderBeacon,1)
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
                    , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                    , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
                    , uncurry PV2.singleton (_unAsset _loanAsset) _loanPrincipal
                    ]
                , outputDatum = OutputDatum $ toDatum offerDatum
                , outputReferenceScript = toReferenceScript Nothing
                }
            ]
        , referenceInputs = [negotiationRef]
        , extraKeyWitnesses = [lenderPubKey]
        }

  forM_ borrowerInfo $ 
    \(borrowerPersonalAddr,borrowerPayPrivKey,borrowerPubKey,loanAddress,borrowerCred) -> do
      askUTxOs <-
        txOutRefsAndDatumsAtAddressWithBeacon @AskDatum 
          loanAddress 
          (negotiationBeaconCurrencySymbol,"Ask")

      offerUTxOs <- 
        txOutRefsAndDatumsAtAddressWithBeacon @OfferDatum
          loanAddress 
          (negotiationBeaconCurrencySymbol,"Offer")

      let sampleBurns os as = concat $ (flip . flip zipWith) os as $
            \(offerRef,Just OfferDatum{_assetBeacon=oLoanBeacon,_lenderId}) 
             (_,Just AskDatum{_assetBeacon=aLoanBeacon}) ->
                [ TokenMint
                    { mintTokens = 
                        [ ("Offer",-1)
                        , ("Ask",-1)
                        , (_unAssetBeacon oLoanBeacon,-1)
                        , (_unAssetBeacon aLoanBeacon,-1)
                        , (_unLenderId _lenderId,-1)
                        ]
                    , mintRedeemer = toRedeemer BurnNegotiationBeacons
                    , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
                    , mintReference = Just negotiationRef
                    }
                , TokenMint
                    { mintTokens = 
                        [ ("Active",1)
                        , (_unBorrowerId $ genBorrowerId borrowerCred,1)
                        , (_unAssetBeacon oLoanBeacon,1)
                        , (_unLoanId $ genLoanId offerRef,2)
                        ]
                    , mintRedeemer = toRedeemer $ 
                        CreateActive negotiationBeaconCurrencySymbol
                    , mintPolicy = toVersionedMintingPolicy activeBeaconScript
                    , mintReference = Just activeRef
                    }
                ]

          sampleOutputs start os = flip concatMap os $ 
            \(offerRef,Just od@OfferDatum{_assetBeacon}) ->
              let activeDatum@ActiveDatum{_loanId,_lenderAddress,_borrowerId} = 
                    createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime start) od
              in [ Output
                    { outputAddress = loanAddress
                    , outputValue = utxoValue 4_000_000 $ mconcat
                        [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                        , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                        , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                        , PV2.singleton activeBeaconCurrencySymbol (_unLoanId $ genLoanId offerRef) 1
                        , uncurry PV2.singleton (_unAsset collateral1) 10
                        ]
                    , outputDatum = OutputDatum $ toDatum $ 
                        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime start) od
                    , outputReferenceScript = toReferenceScript Nothing
                    }
                 , Output
                    { outputAddress = toCardanoApiAddress lenderAddr
                    , outputValue = utxoValue 4_000_000 $ mconcat
                        [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1 ]
                    , outputDatum = 
                        OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
                    , outputReferenceScript = toReferenceScript Nothing
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

  activeUTxOs <- fmap (take number . concat) $
    forM borrowerInfo $ \(_,_,_,loanAddress,_) -> 
      txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
        loanAddress 
        (activeBeaconCurrencySymbol,"Active")

  keyUTxOs <- fmap concat $ forM activeUTxOs $ \(_,Just ad@ActiveDatum{_loanId}) ->
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      (toCardanoApiAddress lenderAddr) 
      (activeBeaconCurrencySymbol,_unLoanId _loanId)

  let sampleKeyBurns acs = flip map acs $
        \(_,Just ad@ActiveDatum{..}) ->
          TokenMint
            { mintTokens = 
                [ ("Active",-1)
                , (_unBorrowerId _borrowerId,-1)
                , (_unAssetBeacon _assetBeacon,-1)
                , (_unLoanId _loanId,-2)
                ]
            , mintRedeemer = toRedeemer BurnKeyAndClaimDefaulted
            , mintPolicy = toVersionedMintingPolicy activeBeaconScript
            , mintReference = Just activeRef
            }

  currentSlot >>= awaitTime . slotToPosixTime . (+3600)
  claimSlot <- currentSlot

  -- Try to claim the collateral.
  void $ 
    transact 
      lenderPersonalAddr 
      ([toCardanoApiAddress lenderAddr,refScriptAddress] <> map (\(_,_,_,addr,_) -> addr) borrowerInfo)
      [lenderPayPrivKey]
      emptyTxParams
        { tokens = sampleKeyBurns activeUTxOs
        , inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
            [ Input
                { inputId = activeUtxoRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ SpendWithKeyNFT)
                }
            , Input
                { inputId = keyRef
                , inputWitness = 
                    SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
                }
            ]
        , referenceInputs = [activeRef,loanRef,proxyRef]
        , extraKeyWitnesses = [lenderPubKey]
        , validityRange = ValidityRange
            { validityRangeLowerBound = Just claimSlot
            , validityRangeUpperBound = Nothing
            }
        }

-- | Claim the collateral for multiple expired loans. All loans use three assets as collateral. The key NFTs
-- are in separate UTxOs. All loans come from different borrowers, and use different loan assets.
benchTest6 :: MonadEmulator m => Int -> m ()
benchTest6 number = do
  References{..} <- initializeReferenceScripts 

  let -- Lender Info
      lenderWallet = Mock.knownMockWallet 1
      lenderPersonalAddr = Mock.mockWalletAddress lenderWallet
      lenderPayPrivKey = Mock.paymentPrivateKey lenderWallet
      lenderPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash lenderWallet
      lenderCred = PV2.PubKeyCredential lenderPubKey
      lenderBeacon = genLenderId lenderCred
      lenderAddr = 
        PV2.Address (PV2.ScriptCredential $ scriptHash proxyScript) 
                    (Just $ PV2.StakingHash lenderCred)
      newLenderAddr = PV2.Address lenderCred Nothing

      -- Loan Info
      loanAssets = map (\i -> Asset (testTokenSymbol, fromString $ "TestToken" <> show @Int i)) [1..10]
      collateral1 = Asset (testTokenSymbol,"TestToken11")
      collateral2 = Asset (testTokenSymbol,"TestToken12")
      collateral3 = Asset (testTokenSymbol,"TestToken13")
      offerDatums = flip map loanAssets $ \loanAsset -> unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrincipal = 10
        , _epochDuration = Nothing
        , _loanTerm = 10000
        , _loanInterest = Fraction (1,10)
        , _compoundingInterest = True
        , _minPayment = 0
        , _penalty = NoPenalty
        , _maxConsecutiveMisses = Nothing
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            , (collateral3,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  borrowerInfo <- forM (zip loanAssets [1..10]) $ \(loanAsset,i) -> do
      let -- Borrower Info
          borrowerWallet = Mock.knownMockWallet i
          borrowerPersonalAddr = Mock.mockWalletAddress borrowerWallet
          borrowerPayPrivKey = Mock.paymentPrivateKey borrowerWallet
          borrowerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash borrowerWallet
          borrowerCred = PV2.PubKeyCredential borrowerPubKey
          borrowerBeacon = genBorrowerId borrowerCred
          loanAddress = toCardanoApiAddress $
            PV2.Address (PV2.ScriptCredential $ scriptHash loanScript) 
                        (Just $ PV2.StakingHash borrowerCred)

          askDatum@AskDatum{..} = unsafeCreateAskDatum $ NewAskInfo
            { _borrowerId = borrowerCred
            , _loanAsset = loanAsset
            , _loanPrincipal = 10
            , _loanTerm = 3600
            , _collateral = [collateral1,collateral2,collateral3]
            }

      mintTestTokens borrowerWallet 900_000_000 
        [ ("TestToken11",1000)
        , ("TestToken12",1000)
        , ("TestToken13",1000)
        ]

      -- Create the Ask UTxO.
      void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
        emptyTxParams
          { tokens =
              [ TokenMint
                  { mintTokens = [("Ask",1),(_unAssetBeacon _assetBeacon,1)]
                  , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred
                  , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
                  , mintReference = Just negotiationRef
                  }
              ]
          , outputs = replicate 1
              Output
                { outputAddress = loanAddress
                , outputValue = utxoValue 4_000_000 $ mconcat
                    [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                    , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                    , uncurry PV2.singleton (_unAsset collateral1) 1
                    , uncurry PV2.singleton (_unAsset collateral2) 1
                    , uncurry PV2.singleton (_unAsset collateral3) 1
                    ]
                , outputDatum = OutputDatum $ toDatum askDatum
                , outputReferenceScript = toReferenceScript Nothing
                }
          , referenceInputs = [negotiationRef]
          , extraKeyWitnesses = [borrowerPubKey]
          }

      return (borrowerPersonalAddr,borrowerPayPrivKey,borrowerPubKey,loanAddress,borrowerCred)

  -- Create the Offer UTxO.
  mintTestTokens lenderWallet 900_000_000 $ map ((,1000) . snd . _unAsset) loanAssets

  forM_ (zip offerDatums borrowerInfo) $ \(offerDatum@OfferDatum{..},(_,_,_,loanAddress,_)) ->
    transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey]
      emptyTxParams
        { tokens =
            [ TokenMint
                { mintTokens = 
                    [ ("Offer",1)
                    , (_unAssetBeacon _assetBeacon,1)
                    , (_unLenderId lenderBeacon,1)
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
                    , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                    , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
                    , uncurry PV2.singleton (_unAsset _loanAsset) _loanPrincipal
                    ]
                , outputDatum = OutputDatum $ toDatum offerDatum
                , outputReferenceScript = toReferenceScript Nothing
                }
            ]
        , referenceInputs = [negotiationRef]
        , extraKeyWitnesses = [lenderPubKey]
        }

  forM_ borrowerInfo $ 
    \(borrowerPersonalAddr,borrowerPayPrivKey,borrowerPubKey,loanAddress,borrowerCred) -> do
      askUTxOs <-
        txOutRefsAndDatumsAtAddressWithBeacon @AskDatum 
          loanAddress 
          (negotiationBeaconCurrencySymbol,"Ask")

      offerUTxOs <- 
        txOutRefsAndDatumsAtAddressWithBeacon @OfferDatum
          loanAddress 
          (negotiationBeaconCurrencySymbol,"Offer")

      let sampleBurns os as = concat $ (flip . flip zipWith) os as $
            \(offerRef,Just OfferDatum{_assetBeacon=oLoanBeacon,_lenderId}) 
             (_,Just AskDatum{_assetBeacon=aLoanBeacon}) ->
                [ TokenMint
                    { mintTokens = 
                        [ ("Offer",-1)
                        , ("Ask",-1)
                        , (_unAssetBeacon oLoanBeacon,-1)
                        , (_unAssetBeacon aLoanBeacon,-1)
                        , (_unLenderId _lenderId,-1)
                        ]
                    , mintRedeemer = toRedeemer BurnNegotiationBeacons
                    , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
                    , mintReference = Just negotiationRef
                    }
                , TokenMint
                    { mintTokens = 
                        [ ("Active",1)
                        , (_unBorrowerId $ genBorrowerId borrowerCred,1)
                        , (_unAssetBeacon oLoanBeacon,1)
                        , (_unLoanId $ genLoanId offerRef,2)
                        ]
                    , mintRedeemer = toRedeemer $ 
                        CreateActive negotiationBeaconCurrencySymbol
                    , mintPolicy = toVersionedMintingPolicy activeBeaconScript
                    , mintReference = Just activeRef
                    }
                ]

          sampleOutputs start os = flip concatMap os $ 
            \(offerRef,Just od@OfferDatum{_assetBeacon}) ->
              let activeDatum@ActiveDatum{_loanId,_lenderAddress,_borrowerId} = 
                    createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime start) od
              in [ Output
                    { outputAddress = loanAddress
                    , outputValue = utxoValue 5_000_000 $ mconcat
                        [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                        , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                        , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                        , PV2.singleton activeBeaconCurrencySymbol (_unLoanId $ genLoanId offerRef) 1
                        , uncurry PV2.singleton (_unAsset collateral1) 3
                        , uncurry PV2.singleton (_unAsset collateral2) 3
                        , uncurry PV2.singleton (_unAsset collateral3) 4
                        ]
                    , outputDatum = OutputDatum $ toDatum $ 
                        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime start) od
                    , outputReferenceScript = toReferenceScript Nothing
                    }
                 , Output
                    { outputAddress = toCardanoApiAddress lenderAddr
                    , outputValue = utxoValue 4_000_000 $ mconcat
                        [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1 ]
                    , outputDatum = 
                        OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
                    , outputReferenceScript = toReferenceScript Nothing
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

  activeUTxOs <- fmap (take number . concat) $
    forM borrowerInfo $ \(_,_,_,loanAddress,_) -> 
      txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
        loanAddress 
        (activeBeaconCurrencySymbol,"Active")

  keyUTxOs <- fmap concat $ forM activeUTxOs $ \(_,Just ad@ActiveDatum{_loanId}) ->
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      (toCardanoApiAddress lenderAddr) 
      (activeBeaconCurrencySymbol,_unLoanId _loanId)

  let sampleKeyBurns acs = flip map acs $
        \(_,Just ad@ActiveDatum{..}) ->
          TokenMint
            { mintTokens = 
                [ ("Active",-1)
                , (_unBorrowerId _borrowerId,-1)
                , (_unAssetBeacon _assetBeacon,-1)
                , (_unLoanId _loanId,-2)
                ]
            , mintRedeemer = toRedeemer BurnKeyAndClaimDefaulted
            , mintPolicy = toVersionedMintingPolicy activeBeaconScript
            , mintReference = Just activeRef
            }

  currentSlot >>= awaitTime . slotToPosixTime . (+3600)
  claimSlot <- currentSlot

  -- Try to claim the collateral.
  void $ 
    transact 
      lenderPersonalAddr 
      ([toCardanoApiAddress lenderAddr,refScriptAddress] <> map (\(_,_,_,addr,_) -> addr) borrowerInfo)
      [lenderPayPrivKey]
      emptyTxParams
        { tokens = sampleKeyBurns activeUTxOs
        , inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
            [ Input
                { inputId = activeUtxoRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ SpendWithKeyNFT)
                }
            , Input
                { inputId = keyRef
                , inputWitness = 
                    SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
                }
            ]
        , referenceInputs = [activeRef,loanRef,proxyRef]
        , extraKeyWitnesses = [lenderPubKey]
        , validityRange = ValidityRange
            { validityRangeLowerBound = Just claimSlot
            , validityRangeUpperBound = Nothing
            }
        }

-------------------------------------------------
-- TestTree
-------------------------------------------------
-- | A `TestTree` containing all benchmark scenarios for claiming expired collateral.
tests :: [TestTree]
tests =
  [ mustSucceed "benchTest1" $ benchTest1 15
  , mustSucceed "benchTest2" $ benchTest2 19
  , mustSucceed "benchTest3" $ benchTest3 17
  , mustSucceed "benchTest4" $ benchTest4 15
  , mustSucceed "benchTest5" $ benchTest5 10
  , mustSucceed "benchTest6" $ benchTest6 10

  , mustExceedTxLimits "perfIncreaseTest1" $ benchTest1 16
  , mustExceedTxLimits "perfIncreaseTest2" $ benchTest2 20
  , mustExceedTxLimits "perfIncreaseTest3" $ benchTest3 18
  , mustExceedTxLimits "perfIncreaseTest4" $ benchTest4 16
    -- It is hard to test loans from different borrowers since the emulator only has ten keys.
  ]
