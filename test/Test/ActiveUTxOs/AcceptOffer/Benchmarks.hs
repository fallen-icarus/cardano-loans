{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.ActiveUTxOs.AcceptOffer.Benchmarks where

import qualified Ledger.Value.CardanoAPI as LV
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
-- | Accept multiple loan offers all for the exact same terms and from the same lender. One asset is
-- used as collateral.
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
      lenderWallet = Mock.knownMockWallet 1
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
      askDatum = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _loanTerm = 3600
        , _collateral = [collateral1]
        }
      offerDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Nothing
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 0
        , _penalty = NoPenalty
        , _collateralization = [(collateral1,Fraction(1,1_000_000))]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef,activeRef,loanRef} <- initializeReferenceScripts 
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

  startSlot <- currentSlot

  askUTxOs <- take number <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @AskDatum 
      loanAddress 
      (negotiationBeaconCurrencySymbol,"Ask")

  offerUTxOs <- take number <$> 
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

      sampleOutputs os = flip concatMap os $ 
        \(offerRef,Just od@OfferDatum{_assetBeacon}) ->
          let activeDatum@ActiveDatum{_loanId,_lenderAddress} = 
                createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
          in [ Output
                { outputAddress = loanAddress
                , outputValue = utxoValue 4_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unLoanId $ genLoanId offerRef) 1
                    , uncurry PV2.singleton (_unAsset collateral1) 10
                    ]
                , outputDatum = OutputDatum $ toDatum activeDatum
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

  -- Try to accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = sampleBurns offerUTxOs askUTxOs
      , inputs = 
          concat $ (flip . flip zipWith) offerUTxOs askUTxOs $ \(offerRef,_) (askRef,_) ->
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
      , outputs = sampleOutputs offerUTxOs
      , referenceInputs = [negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just startSlot
          , validityRangeUpperBound = Nothing
          }
      }

-- | Accept multiple loan offers all for the exact same terms and from the same lender. Three assets are
-- used as collateral.
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
      lenderWallet = Mock.knownMockWallet 1
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      collateral3 = Asset (testTokenSymbol,"TestToken3")
      loanBeacon = genLoanAssetBeaconName loanAsset
      askDatum = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _loanTerm = 3600
        , _collateral = [collateral1,collateral2,collateral3]
        }
      offerDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Nothing
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 0
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1_000_000))
            , (collateral2,Fraction(1,1_000_000))
            , (collateral3,Fraction(1,1_000_000))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef,activeRef,loanRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 900_000_000 
    [ ("TestToken1",1000)
    , ("TestToken2",1000)
    , ("TestToken3",1000)
    ]

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
                , uncurry PV2.singleton (_unAsset collateral2) 1
                , uncurry PV2.singleton (_unAsset collateral3) 1
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

  startSlot <- currentSlot

  askUTxOs <- take number <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @AskDatum 
      loanAddress 
      (negotiationBeaconCurrencySymbol,"Ask")

  offerUTxOs <- take number <$> 
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

      sampleOutputs os = flip concatMap os $ 
        \(offerRef,Just od@OfferDatum{_assetBeacon}) ->
          let activeDatum@ActiveDatum{_loanId,_lenderAddress} = 
                createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
          in [ Output
                { outputAddress = loanAddress
                , outputValue = utxoValue 5_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unLoanId $ genLoanId offerRef) 1
                    , uncurry PV2.singleton (_unAsset collateral1) 3
                    , uncurry PV2.singleton (_unAsset collateral2) 5
                    , uncurry PV2.singleton (_unAsset collateral3) 2
                    ]
                , outputDatum = OutputDatum $ toDatum activeDatum
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

  -- Try to accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = sampleBurns offerUTxOs askUTxOs
      , inputs = 
          concat $ (flip . flip zipWith) offerUTxOs askUTxOs $ \(offerRef,_) (askRef,_) ->
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
      , outputs = sampleOutputs offerUTxOs
      , referenceInputs = [negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just startSlot
          , validityRangeUpperBound = Nothing
          }
      }

-- | Accept multiple loan offers from different lenders. Three assets are used as collateral for
-- each loan. All loans use the same loan asset and the same collateral.
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

      -- Loan Info
      loanAsset = Asset (adaSymbol,adaToken)
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      collateral3 = Asset (testTokenSymbol,"TestToken3")
      loanBeacon = genLoanAssetBeaconName loanAsset
      askDatum = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _loanTerm = 3600
        , _collateral = [collateral1,collateral2,collateral3]
        }

  -- Initialize scenario
  References{negotiationRef,activeRef,loanRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 900_000_000 
    [ ("TestToken1",1000)
    , ("TestToken2",1000)
    , ("TestToken3",1000)
    ]

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
                , uncurry PV2.singleton (_unAsset collateral2) 1
                , uncurry PV2.singleton (_unAsset collateral3) 1
                ]
            , outputDatum = OutputDatum $ toDatum askDatum
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [borrowerPubKey]
      }

  -- Create the Offer UTxO.
  forM_ [2..10] $ \i -> do
      let lenderWallet = Mock.knownMockWallet i
          lenderPersonalAddr = Mock.mockWalletAddress lenderWallet
          lenderPayPrivKey = Mock.paymentPrivateKey lenderWallet
          lenderPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash lenderWallet
          lenderCred = PV2.PubKeyCredential lenderPubKey
          lenderBeacon = genLenderId lenderCred
          lenderAddr = 
            PV2.Address (PV2.ScriptCredential $ scriptHash proxyScript) 
                        (Just $ PV2.StakingHash lenderCred)

          offerDatum = unsafeCreateOfferDatum $ NewOfferInfo
            { _lenderId = lenderCred
            , _lenderAddress = lenderAddr
            , _loanAsset = loanAsset
            , _loanPrinciple = 10_000_000
            , _compoundFrequency = Nothing
            , _loanTerm = 3600
            , _loanInterest = Fraction (1,10)
            , _minPayment = 0
            , _penalty = NoPenalty
            , _collateralization = 
                [ (collateral1,Fraction(1,1_000_000))
                , (collateral2,Fraction(1,1_000_000))
                , (collateral3,Fraction(1,1_000_000))
                ]
            , _collateralIsSwappable = False
            , _claimPeriod = 3600
            , _offerDeposit = 4_000_000
            , _offerExpiration = Nothing
            }

      transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
        emptyTxParams
          { tokens =
              [ TokenMint
                  { mintTokens = 
                      [ ("Offer",1)
                      , (_unAssetBeacon loanBeacon,1)
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
                      , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                      , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
                      , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
                      ]
                  , outputDatum = OutputDatum $ toDatum offerDatum
                  , outputReferenceScript = toReferenceScript Nothing
                  }
              ]
          , referenceInputs = [negotiationRef]
          , extraKeyWitnesses = [lenderPubKey]
          }

  startSlot <- currentSlot

  askUTxOs <- take number <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @AskDatum 
      loanAddress 
      (negotiationBeaconCurrencySymbol,"Ask")

  offerUTxOs <- take number <$> 
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

      sampleOutputs os = flip concatMap os $ 
        \(offerRef,Just od@OfferDatum{_assetBeacon}) ->
          let activeDatum@ActiveDatum{_loanId,_lenderAddress} = 
                createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) od
          in [ Output
                { outputAddress = loanAddress
                , outputValue = utxoValue 5_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unLoanId $ genLoanId offerRef) 1
                    , uncurry PV2.singleton (_unAsset collateral1) 3
                    , uncurry PV2.singleton (_unAsset collateral2) 5
                    , uncurry PV2.singleton (_unAsset collateral3) 2
                    ]
                , outputDatum = OutputDatum $ toDatum activeDatum
                , outputReferenceScript = toReferenceScript Nothing
                }
             , Output
                { outputAddress = toCardanoApiAddress _lenderAddress
                , outputValue = utxoValue 4_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1 ]
                , outputDatum = 
                    OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
                , outputReferenceScript = toReferenceScript Nothing
                }
             ]

  -- Try to accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = sampleBurns offerUTxOs askUTxOs
      , inputs = 
          concat $ (flip . flip zipWith) offerUTxOs askUTxOs $ \(offerRef,_) (askRef,_) ->
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
      , outputs = sampleOutputs offerUTxOs
      , referenceInputs = [negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just startSlot
          , validityRangeUpperBound = Nothing
          }
      }

-- | Accept multiple loan offers from different lenders. Three assets are used as collateral for
-- each loan. All loans use different loan asset but the same collateral.
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

      -- Loan Info
      loanAssets = 
        map (\i -> Asset $ (testTokenSymbol,) $ fromString $ "TestToken" <> show @Int i) [4..20]
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      collateral3 = Asset (testTokenSymbol,"TestToken3")
      askDatums = flip map loanAssets $ \loanAsset -> unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred
        , _loanAsset = loanAsset
        , _loanPrinciple = 10
        , _loanTerm = 3600
        , _collateral = [collateral1,collateral2,collateral3]
        }

  -- Initialize scenario
  References{negotiationRef,activeRef,loanRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 900_000_000 
    [ ("TestToken1",100)
    , ("TestToken2",100)
    , ("TestToken3",100)
    ]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = flip map askDatums $ \AskDatum{_assetBeacon} ->
          TokenMint
            { mintTokens = [("Ask",1),(_unAssetBeacon _assetBeacon,1)]
            , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred
            , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
            , mintReference = Just negotiationRef
            }
      , outputs = flip map askDatums $ \askDatum@AskDatum{_assetBeacon} ->
          Output
            { outputAddress = loanAddress
            , outputValue = utxoValue 3_000_000 $ mconcat
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

  -- Create the Offer UTxO.
  forM_ (zip [2..10] askDatums) $ \(i,AskDatum{_loanAsset,_assetBeacon}) -> do
      let lenderWallet = Mock.knownMockWallet i
          lenderPersonalAddr = Mock.mockWalletAddress lenderWallet
          lenderPayPrivKey = Mock.paymentPrivateKey lenderWallet
          lenderPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash lenderWallet
          lenderCred = PV2.PubKeyCredential lenderPubKey
          lenderBeacon = genLenderId lenderCred
          lenderAddr = 
            PV2.Address (PV2.ScriptCredential $ scriptHash proxyScript) 
                        (Just $ PV2.StakingHash lenderCred)

          offerDatum = unsafeCreateOfferDatum $ NewOfferInfo
            { _lenderId = lenderCred
            , _lenderAddress = lenderAddr
            , _loanAsset = _loanAsset
            , _loanPrinciple = 10
            , _compoundFrequency = Nothing
            , _loanTerm = 3600
            , _loanInterest = Fraction (1,10)
            , _minPayment = 0
            , _penalty = NoPenalty
            , _collateralization = 
                [ (collateral1,Fraction(1,1_000_000))
                , (collateral2,Fraction(1,1_000_000))
                , (collateral3,Fraction(1,1_000_000))
                ]
            , _collateralIsSwappable = False
            , _claimPeriod = 3600
            , _offerDeposit = 5_000_000
            , _offerExpiration = Nothing
            }

      mintTestTokens lenderWallet 900_000_000 [(snd $ _unAsset _loanAsset,10)]

      transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
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
                  , outputValue = utxoValue 5_000_000 $ mconcat
                      [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
                      , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                      , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
                      , uncurry PV2.singleton (_unAsset _loanAsset) 10
                      ]
                  , outputDatum = OutputDatum $ toDatum offerDatum
                  , outputReferenceScript = toReferenceScript Nothing
                  }
              ]
          , referenceInputs = [negotiationRef]
          , extraKeyWitnesses = [lenderPubKey]
          }

  startSlot <- currentSlot

  askUTxOs <- take number <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @AskDatum 
      loanAddress 
      (negotiationBeaconCurrencySymbol,"Ask")

  offerUTxOs <- take number <$> 
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

      sampleOutputs os = flip concatMap os $ 
        \(offerRef,Just od@OfferDatum{_assetBeacon,_offerDeposit}) ->
          let activeDatum@ActiveDatum{_loanId,_lenderAddress} = 
                createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) od
          in [ Output
                { outputAddress = loanAddress
                , outputValue = utxoValue 5_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unLoanId $ genLoanId offerRef) 1
                    , uncurry PV2.singleton (_unAsset collateral1) 3
                    , uncurry PV2.singleton (_unAsset collateral2) 5
                    , uncurry PV2.singleton (_unAsset collateral3) 2
                    ]
                , outputDatum = OutputDatum $ toDatum activeDatum
                , outputReferenceScript = toReferenceScript Nothing
                }
             , Output
                { outputAddress = toCardanoApiAddress _lenderAddress
                , outputValue = utxoValue (LV.Lovelace _offerDeposit) $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1 ]
                , outputDatum = 
                    OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
                , outputReferenceScript = toReferenceScript Nothing
                }
             ]

  -- Try to accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = sampleBurns offerUTxOs askUTxOs
      , inputs = 
          concat $ (flip . flip zipWith) offerUTxOs askUTxOs $ \(offerRef,_) (askRef,_) ->
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
      , outputs = sampleOutputs offerUTxOs
      , referenceInputs = [negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just startSlot
          , validityRangeUpperBound = Nothing
          }
      }

-------------------------------------------------
-- TestTree
-------------------------------------------------
-- | A `TestTree` containing all benchmark scenarios for accepting Offer UTxOs.
tests :: [TestTree]
tests =
  [ mustSucceed "benchTest1" $ benchTest1 9
  , mustSucceed "benchTest2" $ benchTest2 9
  , mustSucceed "benchTest3" $ benchTest3 8
  , mustSucceed "benchTest4" $ benchTest4 8

  , mustExceedTxLimits "perfIncreaseTest1" $ benchTest1 10
  , mustExceedTxLimits "perfIncreaseTest2" $ benchTest2 10
  , mustExceedTxLimits "perfIncreaseTest3" $ benchTest3 9
  , mustExceedTxLimits "perfIncreaseTest4" $ benchTest4 9
  ]
