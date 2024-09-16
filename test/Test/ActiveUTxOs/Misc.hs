{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.ActiveUTxOs.Misc where

import qualified Ledger.Address as LA
import qualified PlutusLedgerApi.V2 as PV2
import qualified Ledger.CardanoWallet as Mock 
import Test.Tasty (TestTree,testGroup)
import Control.Monad (forM)

import CardanoLoans

import Test.Prelude

-------------------------------------------------
-- Miscelleneous Regression Tests
-------------------------------------------------
-- | Make a partial payment in the same transaction where an offer is accepted. The payment outputs
-- are last.
regressionTest1 :: MonadEmulator m => m ()
regressionTest1 = do
  let borrowerWallet1 = Mock.knownMockWallet 1
      borrowerPersonalAddr1 = Mock.mockWalletAddress borrowerWallet1
      borrowerPayPrivKey1 = Mock.paymentPrivateKey borrowerWallet1
      borrowerPubKey1 = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash borrowerWallet1
      borrowerCred1 = PV2.PubKeyCredential borrowerPubKey1
      borrowerBeacon1 = genBorrowerId borrowerCred1
      loanAddress1 = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash loanScript) (Just $ PV2.StakingHash borrowerCred1)

      lenderWallet1 = Mock.knownMockWallet 2
      lenderPersonalAddr1 = Mock.mockWalletAddress lenderWallet1
      lenderPayPrivKey1 = Mock.paymentPrivateKey lenderWallet1
      lenderPubKey1 = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash lenderWallet1
      lenderCred1 = PV2.PubKeyCredential lenderPubKey1
      lenderBeacon1 = genLenderId lenderCred1
      lenderAddr1 = 
        PV2.Address (PV2.ScriptCredential $ scriptHash proxyScript) (Just $ PV2.StakingHash lenderCred1)

      -- Loan Info
      loanAsset = Asset (adaSymbol,adaToken)
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      loanBeacon = genLoanAssetBeaconName loanAsset
      askDatum = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred1
        , _loanAsset = loanAsset
        , _loanPrincipal = 10_000_000
        , _loanTerm = 3600
        , _collateral = [collateral1]
        }
      offerDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred1
        , _lenderAddress = lenderAddr1
        , _loanAsset = loanAsset
        , _loanPrincipal = 10_000_000
        , _epochDuration = Nothing
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _compoundingInterest = True
        , _minPayment = 0
        , _penalty = NoPenalty
        , _collateralization = [(collateral1,Fraction(1,1_000_000))]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet1 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr1 [refScriptAddress] [borrowerPayPrivKey1] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",2),(_unAssetBeacon loanBeacon,2)]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred1
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum askDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum askDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [borrowerPubKey1]
      }

  -- Create the Offer UTxO.
  void $ transact lenderPersonalAddr1 [refScriptAddress] [lenderPayPrivKey1] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",2)
                  , (_unAssetBeacon loanBeacon,2)
                  , (_unLenderId lenderBeacon1,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateOffer lenderCred1
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon1) 1
                  , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
                  ]
              , outputDatum = OutputDatum $ toDatum offerDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon1) 1
                  , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
                  ]
              , outputDatum = OutputDatum $ toDatum offerDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey1]
      }

  startSlot <- currentSlot

  askRefs <- take 1 <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @AskDatum 
      loanAddress1
      (negotiationBeaconCurrencySymbol,"Ask")

  offerRefs <- take 1 <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @OfferDatum 
      loanAddress1
      (negotiationBeaconCurrencySymbol,"Offer")

  let [activeDatum1] = flip map offerRefs $ \(offerRef,_) ->
        createAcceptanceDatumFromOffer borrowerCred1 offerRef (slotToPosixTime startSlot) offerDatum
      [loanIdBeacon1] = map (genLoanId . fst) offerRefs

  -- Try to accept the offer.
  void $ transact borrowerPersonalAddr1 [loanAddress1,refScriptAddress] [borrowerPayPrivKey1] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon1,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon1,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon1,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
              , mintPolicy = toVersionedMintingPolicy activeBeaconScript
              , mintReference = Just activeRef
              }
          ]
      , inputs = concat
          [ flip map offerRefs $ \(offerRef,_) -> 
              Input
                { inputId = offerRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum (toRedeemer AcceptOffer)
                }
          , flip map askRefs $ \(askRef,_) ->
              Input 
                { inputId = askRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum (toRedeemer AcceptOffer)
                }
          ]
      , outputs =
          [ Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon1) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon1) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr1
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon1) 1 ]
              , outputDatum = OutputDatum $ toDatum $ 
                  PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon1)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey1]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just startSlot
          , validityRangeUpperBound = Nothing
          }
      }

  newStartSlot <- currentSlot

  otherAskRefs <- take 1 <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @AskDatum 
      loanAddress1
      (negotiationBeaconCurrencySymbol,"Ask")

  otherOfferRefs <- take 1 <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @OfferDatum 
      loanAddress1
      (negotiationBeaconCurrencySymbol,"Offer")

  let [activeDatum2] = flip map otherOfferRefs $ \(offerRef,_) ->
        createAcceptanceDatumFromOffer borrowerCred1 offerRef (slotToPosixTime newStartSlot) offerDatum
      [loanIdBeacon2] = map (genLoanId . fst) otherOfferRefs

  activeUTxOs <-
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      loanAddress1
      (activeBeaconCurrencySymbol,"Active")

  let samplePayments acs = flip concatMap acs $ 
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 6
                  ]
              , outputDatum = OutputDatum $ toDatum $ createPostPaymentActiveDatum 5_000_000 ad
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr1
              , outputValue = utxoValue 5_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  -- Try to accept the offer when making another payment.
  void $ transact borrowerPersonalAddr1 [loanAddress1,refScriptAddress] [borrowerPayPrivKey1] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon1,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon1,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon2,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
              , mintPolicy = toVersionedMintingPolicy activeBeaconScript
              , mintReference = Just activeRef
              }
          ]
      , inputs = concat
          [ flip map otherOfferRefs $ \(offerRef,_) -> 
              Input
                { inputId = offerRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum (toRedeemer AcceptOffer)
                }
          , flip map otherAskRefs $ \(askRef,_) ->
              Input 
                { inputId = askRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum (toRedeemer AcceptOffer)
                }
          , flip map activeUTxOs $ \(activeUtxoRef,_) ->
              Input
                { inputId = activeUtxoRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
                }
          ]
      , outputs = mconcat
          [ [ Output
                { outputAddress = loanAddress1
                , outputValue = utxoValue 4_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon1) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon2) 1
                    , uncurry PV2.singleton (_unAsset collateral1) 10
                    ]
                , outputDatum = OutputDatum $ toDatum activeDatum2
                , outputReferenceScript = toReferenceScript Nothing
                }
            , Output
                { outputAddress = toCardanoApiAddress lenderAddr1
                , outputValue = utxoValue 4_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon2) 1 ]
                , outputDatum = OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon2)
                , outputReferenceScript = toReferenceScript Nothing
                }
            ]
          , samplePayments activeUTxOs
          ]
      , referenceInputs = [negotiationRef,activeRef,loanRef,paymentObserverRef]
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , extraKeyWitnesses = [borrowerPubKey1]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just newStartSlot
          , validityRangeUpperBound = Just $ newStartSlot + 1
          }
      }

-- | Make a partial payment in the same transaction where an offer is accepted. The payment outputs
-- are first.
regressionTest2 :: MonadEmulator m => m ()
regressionTest2 = do
  let borrowerWallet1 = Mock.knownMockWallet 1
      borrowerPersonalAddr1 = Mock.mockWalletAddress borrowerWallet1
      borrowerPayPrivKey1 = Mock.paymentPrivateKey borrowerWallet1
      borrowerPubKey1 = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash borrowerWallet1
      borrowerCred1 = PV2.PubKeyCredential borrowerPubKey1
      borrowerBeacon1 = genBorrowerId borrowerCred1
      loanAddress1 = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash loanScript) (Just $ PV2.StakingHash borrowerCred1)

      lenderWallet1 = Mock.knownMockWallet 2
      lenderPersonalAddr1 = Mock.mockWalletAddress lenderWallet1
      lenderPayPrivKey1 = Mock.paymentPrivateKey lenderWallet1
      lenderPubKey1 = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash lenderWallet1
      lenderCred1 = PV2.PubKeyCredential lenderPubKey1
      lenderBeacon1 = genLenderId lenderCred1
      lenderAddr1 = 
        PV2.Address (PV2.ScriptCredential $ scriptHash proxyScript) (Just $ PV2.StakingHash lenderCred1)

      -- Loan Info
      loanAsset = Asset (adaSymbol,adaToken)
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      loanBeacon = genLoanAssetBeaconName loanAsset
      askDatum = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred1
        , _loanAsset = loanAsset
        , _loanPrincipal = 10_000_000
        , _loanTerm = 3600
        , _collateral = [collateral1]
        }
      offerDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred1
        , _lenderAddress = lenderAddr1
        , _loanAsset = loanAsset
        , _loanPrincipal = 10_000_000
        , _epochDuration = Nothing
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _compoundingInterest = True
        , _minPayment = 0
        , _penalty = NoPenalty
        , _collateralization = [(collateral1,Fraction(1,1_000_000))]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet1 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr1 [refScriptAddress] [borrowerPayPrivKey1] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",2),(_unAssetBeacon loanBeacon,2)]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred1
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum askDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum askDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [borrowerPubKey1]
      }

  -- Create the Offer UTxO.
  void $ transact lenderPersonalAddr1 [refScriptAddress] [lenderPayPrivKey1] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",2)
                  , (_unAssetBeacon loanBeacon,2)
                  , (_unLenderId lenderBeacon1,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateOffer lenderCred1
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon1) 1
                  , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
                  ]
              , outputDatum = OutputDatum $ toDatum offerDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon1) 1
                  , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
                  ]
              , outputDatum = OutputDatum $ toDatum offerDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey1]
      }

  startSlot <- currentSlot

  askRefs <- take 1 <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @AskDatum 
      loanAddress1
      (negotiationBeaconCurrencySymbol,"Ask")

  offerRefs <- take 1 <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @OfferDatum 
      loanAddress1
      (negotiationBeaconCurrencySymbol,"Offer")

  let [activeDatum1] = flip map offerRefs $ \(offerRef,_) ->
        createAcceptanceDatumFromOffer borrowerCred1 offerRef (slotToPosixTime startSlot) offerDatum
      [loanIdBeacon1] = map (genLoanId . fst) offerRefs

  -- Try to accept the offer.
  void $ transact borrowerPersonalAddr1 [loanAddress1,refScriptAddress] [borrowerPayPrivKey1] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon1,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon1,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon1,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
              , mintPolicy = toVersionedMintingPolicy activeBeaconScript
              , mintReference = Just activeRef
              }
          ]
      , inputs = concat
          [ flip map offerRefs $ \(offerRef,_) -> 
              Input
                { inputId = offerRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum (toRedeemer AcceptOffer)
                }
          , flip map askRefs $ \(askRef,_) ->
              Input 
                { inputId = askRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum (toRedeemer AcceptOffer)
                }
          ]
      , outputs =
          [ Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon1) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon1) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr1
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon1) 1 ]
              , outputDatum = OutputDatum $ toDatum $ 
                  PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon1)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey1]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just startSlot
          , validityRangeUpperBound = Nothing
          }
      }

  newStartSlot <- currentSlot

  otherAskRefs <- take 1 <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @AskDatum 
      loanAddress1
      (negotiationBeaconCurrencySymbol,"Ask")

  otherOfferRefs <- take 1 <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @OfferDatum 
      loanAddress1
      (negotiationBeaconCurrencySymbol,"Offer")

  let [activeDatum2] = flip map otherOfferRefs $ \(offerRef,_) ->
        createAcceptanceDatumFromOffer borrowerCred1 offerRef (slotToPosixTime newStartSlot) offerDatum
      [loanIdBeacon2] = map (genLoanId . fst) otherOfferRefs

  activeUTxOs <-
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      loanAddress1
      (activeBeaconCurrencySymbol,"Active")

  let samplePayments acs = flip concatMap acs $ 
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 6
                  ]
              , outputDatum = OutputDatum $ toDatum $ createPostPaymentActiveDatum 5_000_000 ad
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr1
              , outputValue = utxoValue 5_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  -- Try to accept the offer when making another payment.
  void $ transact borrowerPersonalAddr1 [loanAddress1,refScriptAddress] [borrowerPayPrivKey1] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon1,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon1,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon2,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
              , mintPolicy = toVersionedMintingPolicy activeBeaconScript
              , mintReference = Just activeRef
              }
          ]
      , inputs = concat
          [ flip map otherOfferRefs $ \(offerRef,_) -> 
              Input
                { inputId = offerRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum (toRedeemer AcceptOffer)
                }
          , flip map otherAskRefs $ \(askRef,_) ->
              Input 
                { inputId = askRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum (toRedeemer AcceptOffer)
                }
          , flip map activeUTxOs $ \(activeUtxoRef,_) ->
              Input
                { inputId = activeUtxoRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
                }
          ]
      , outputs = mconcat
          [ samplePayments activeUTxOs
          , [ Output
                { outputAddress = loanAddress1
                , outputValue = utxoValue 4_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon1) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon2) 1
                    , uncurry PV2.singleton (_unAsset collateral1) 10
                    ]
                , outputDatum = OutputDatum $ toDatum activeDatum2
                , outputReferenceScript = toReferenceScript Nothing
                }
            , Output
                { outputAddress = toCardanoApiAddress lenderAddr1
                , outputValue = utxoValue 4_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon2) 1 ]
                , outputDatum = OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon2)
                , outputReferenceScript = toReferenceScript Nothing
                }
            ]
          ]
      , referenceInputs = [negotiationRef,activeRef,loanRef,paymentObserverRef]
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , extraKeyWitnesses = [borrowerPubKey1]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just newStartSlot
          , validityRangeUpperBound = Just $ newStartSlot + 1
          }
      }

-- | Apply interest to a loan in the same transaction where an offer is accepted. The interest outputs
-- are last.
regressionTest3 :: MonadEmulator m => m ()
regressionTest3 = do
  let borrowerWallet1 = Mock.knownMockWallet 1
      borrowerPersonalAddr1 = Mock.mockWalletAddress borrowerWallet1
      borrowerPayPrivKey1 = Mock.paymentPrivateKey borrowerWallet1
      borrowerPubKey1 = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash borrowerWallet1
      borrowerCred1 = PV2.PubKeyCredential borrowerPubKey1
      borrowerBeacon1 = genBorrowerId borrowerCred1
      loanAddress1 = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash loanScript) (Just $ PV2.StakingHash borrowerCred1)

      lenderWallet1 = Mock.knownMockWallet 2
      lenderPersonalAddr1 = Mock.mockWalletAddress lenderWallet1
      lenderPayPrivKey1 = Mock.paymentPrivateKey lenderWallet1
      lenderPubKey1 = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash lenderWallet1
      lenderCred1 = PV2.PubKeyCredential lenderPubKey1
      lenderBeacon1 = genLenderId lenderCred1
      lenderAddr1 = 
        PV2.Address (PV2.ScriptCredential $ scriptHash proxyScript) (Just $ PV2.StakingHash lenderCred1)

      -- Loan Info
      loanAsset = Asset (adaSymbol,adaToken)
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      loanBeacon = genLoanAssetBeaconName loanAsset
      askDatum = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred1
        , _loanAsset = loanAsset
        , _loanPrincipal = 10_000_000
        , _loanTerm = 3600
        , _collateral = [collateral1]
        }
      offerDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred1
        , _lenderAddress = lenderAddr1
        , _loanAsset = loanAsset
        , _loanPrincipal = 10_000_000
        , _epochDuration = Nothing
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _compoundingInterest = True
        , _minPayment = 0
        , _penalty = NoPenalty
        , _collateralization = [(collateral1,Fraction(1,1_000_000))]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet1 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr1 [refScriptAddress] [borrowerPayPrivKey1] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",2),(_unAssetBeacon loanBeacon,2)]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred1
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum askDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum askDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [borrowerPubKey1]
      }

  -- Create the Offer UTxO.
  void $ transact lenderPersonalAddr1 [refScriptAddress] [lenderPayPrivKey1] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",2)
                  , (_unAssetBeacon loanBeacon,2)
                  , (_unLenderId lenderBeacon1,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateOffer lenderCred1
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon1) 1
                  , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
                  ]
              , outputDatum = OutputDatum $ toDatum offerDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon1) 1
                  , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
                  ]
              , outputDatum = OutputDatum $ toDatum offerDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey1]
      }

  startSlot <- currentSlot

  askRefs <- take 1 <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @AskDatum 
      loanAddress1
      (negotiationBeaconCurrencySymbol,"Ask")

  offerRefs <- take 1 <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @OfferDatum 
      loanAddress1
      (negotiationBeaconCurrencySymbol,"Offer")

  let [activeDatum1] = flip map offerRefs $ \(offerRef,_) ->
        createAcceptanceDatumFromOffer borrowerCred1 offerRef (slotToPosixTime startSlot) offerDatum
      [loanIdBeacon1] = map (genLoanId . fst) offerRefs

  -- Try to accept the offer.
  void $ transact borrowerPersonalAddr1 [loanAddress1,refScriptAddress] [borrowerPayPrivKey1] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon1,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon1,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon1,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
              , mintPolicy = toVersionedMintingPolicy activeBeaconScript
              , mintReference = Just activeRef
              }
          ]
      , inputs = concat
          [ flip map offerRefs $ \(offerRef,_) -> 
              Input
                { inputId = offerRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum (toRedeemer AcceptOffer)
                }
          , flip map askRefs $ \(askRef,_) ->
              Input 
                { inputId = askRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum (toRedeemer AcceptOffer)
                }
          ]
      , outputs =
          [ Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon1) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon1) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr1
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon1) 1 ]
              , outputDatum = OutputDatum $ toDatum $ 
                  PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon1)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey1]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just startSlot
          , validityRangeUpperBound = Nothing
          }
      }

  newStartSlot <- currentSlot

  otherAskRefs <- take 1 <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @AskDatum 
      loanAddress1
      (negotiationBeaconCurrencySymbol,"Ask")

  otherOfferRefs <- take 1 <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @OfferDatum 
      loanAddress1
      (negotiationBeaconCurrencySymbol,"Offer")

  let [activeDatum2] = flip map otherOfferRefs $ \(offerRef,_) ->
        createAcceptanceDatumFromOffer borrowerCred1 offerRef (slotToPosixTime newStartSlot) offerDatum
      [loanIdBeacon2] = map (genLoanId . fst) otherOfferRefs

  activeUTxOs <-
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      loanAddress1
      (activeBeaconCurrencySymbol,"Active")

  let sampleRollovers acs = flip map acs $ 
        \(_,Just ad@ActiveDatum{..}) ->
          Output
            { outputAddress = loanAddress1
            , outputValue = utxoValue 4_000_000 $ mconcat
                [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                , uncurry PV2.singleton (_unAsset collateral1) 10
                ]
            , outputDatum = OutputDatum $ toDatum $ createPostInterestActiveDatum 1 ad
            , outputReferenceScript = toReferenceScript Nothing
            }

  -- Try to accept the offer when applying interest to another loan.
  void $ transact borrowerPersonalAddr1 [loanAddress1,refScriptAddress] [borrowerPayPrivKey1] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon1,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon1,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon2,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
              , mintPolicy = toVersionedMintingPolicy activeBeaconScript
              , mintReference = Just activeRef
              }
          ]
      , inputs = concat
          [ flip map otherOfferRefs $ \(offerRef,_) -> 
              Input
                { inputId = offerRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum (toRedeemer AcceptOffer)
                }
          , flip map otherAskRefs $ \(askRef,_) ->
              Input 
                { inputId = askRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum (toRedeemer AcceptOffer)
                }
          , flip map activeUTxOs $ \(activeUtxoRef,_) ->
              Input
                { inputId = activeUtxoRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ ApplyInterest 0 1)
                }
          ]
      , outputs = mconcat
          [ [ Output
                { outputAddress = loanAddress1
                , outputValue = utxoValue 4_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon1) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon2) 1
                    , uncurry PV2.singleton (_unAsset collateral1) 10
                    ]
                , outputDatum = OutputDatum $ toDatum activeDatum2
                , outputReferenceScript = toReferenceScript Nothing
                }
            , Output
                { outputAddress = toCardanoApiAddress lenderAddr1
                , outputValue = utxoValue 4_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon2) 1 ]
                , outputDatum = OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon2)
                , outputReferenceScript = toReferenceScript Nothing
                }
            ]
          , sampleRollovers activeUTxOs
          ]
      , referenceInputs = [negotiationRef,activeRef,loanRef,interestObserverRef]
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash interestObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference interestObserverRef $ 
                    toRedeemer ObserveInterest
              }
          ]
      , extraKeyWitnesses = [borrowerPubKey1]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just newStartSlot
          , validityRangeUpperBound = Just $ newStartSlot + 1
          }
      }

-- | Apply interest to a loan in the same transaction where an offer is accepted. The interest outputs
-- are first.
regressionTest4 :: MonadEmulator m => m ()
regressionTest4 = do
  let borrowerWallet1 = Mock.knownMockWallet 1
      borrowerPersonalAddr1 = Mock.mockWalletAddress borrowerWallet1
      borrowerPayPrivKey1 = Mock.paymentPrivateKey borrowerWallet1
      borrowerPubKey1 = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash borrowerWallet1
      borrowerCred1 = PV2.PubKeyCredential borrowerPubKey1
      borrowerBeacon1 = genBorrowerId borrowerCred1
      loanAddress1 = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash loanScript) (Just $ PV2.StakingHash borrowerCred1)

      lenderWallet1 = Mock.knownMockWallet 2
      lenderPersonalAddr1 = Mock.mockWalletAddress lenderWallet1
      lenderPayPrivKey1 = Mock.paymentPrivateKey lenderWallet1
      lenderPubKey1 = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash lenderWallet1
      lenderCred1 = PV2.PubKeyCredential lenderPubKey1
      lenderBeacon1 = genLenderId lenderCred1
      lenderAddr1 = 
        PV2.Address (PV2.ScriptCredential $ scriptHash proxyScript) (Just $ PV2.StakingHash lenderCred1)

      -- Loan Info
      loanAsset = Asset (adaSymbol,adaToken)
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      loanBeacon = genLoanAssetBeaconName loanAsset
      askDatum = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred1
        , _loanAsset = loanAsset
        , _loanPrincipal = 10_000_000
        , _loanTerm = 3600
        , _collateral = [collateral1]
        }
      offerDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred1
        , _lenderAddress = lenderAddr1
        , _loanAsset = loanAsset
        , _loanPrincipal = 10_000_000
        , _epochDuration = Nothing
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _compoundingInterest = True
        , _minPayment = 0
        , _penalty = NoPenalty
        , _collateralization = [(collateral1,Fraction(1,1_000_000))]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet1 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr1 [refScriptAddress] [borrowerPayPrivKey1] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",2),(_unAssetBeacon loanBeacon,2)]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred1
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum askDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum askDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [borrowerPubKey1]
      }

  -- Create the Offer UTxO.
  void $ transact lenderPersonalAddr1 [refScriptAddress] [lenderPayPrivKey1] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",2)
                  , (_unAssetBeacon loanBeacon,2)
                  , (_unLenderId lenderBeacon1,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateOffer lenderCred1
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon1) 1
                  , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
                  ]
              , outputDatum = OutputDatum $ toDatum offerDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon1) 1
                  , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
                  ]
              , outputDatum = OutputDatum $ toDatum offerDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey1]
      }

  startSlot <- currentSlot

  askRefs <- take 1 <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @AskDatum 
      loanAddress1
      (negotiationBeaconCurrencySymbol,"Ask")

  offerRefs <- take 1 <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @OfferDatum 
      loanAddress1
      (negotiationBeaconCurrencySymbol,"Offer")

  let [activeDatum1] = flip map offerRefs $ \(offerRef,_) ->
        createAcceptanceDatumFromOffer borrowerCred1 offerRef (slotToPosixTime startSlot) offerDatum
      [loanIdBeacon1] = map (genLoanId . fst) offerRefs

  -- Try to accept the offer.
  void $ transact borrowerPersonalAddr1 [loanAddress1,refScriptAddress] [borrowerPayPrivKey1] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon1,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon1,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon1,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
              , mintPolicy = toVersionedMintingPolicy activeBeaconScript
              , mintReference = Just activeRef
              }
          ]
      , inputs = concat
          [ flip map offerRefs $ \(offerRef,_) -> 
              Input
                { inputId = offerRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum (toRedeemer AcceptOffer)
                }
          , flip map askRefs $ \(askRef,_) ->
              Input 
                { inputId = askRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum (toRedeemer AcceptOffer)
                }
          ]
      , outputs =
          [ Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon1) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon1) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr1
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon1) 1 ]
              , outputDatum = OutputDatum $ toDatum $ 
                  PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon1)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey1]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just startSlot
          , validityRangeUpperBound = Nothing
          }
      }

  newStartSlot <- currentSlot

  otherAskRefs <- take 1 <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @AskDatum 
      loanAddress1
      (negotiationBeaconCurrencySymbol,"Ask")

  otherOfferRefs <- take 1 <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @OfferDatum 
      loanAddress1
      (negotiationBeaconCurrencySymbol,"Offer")

  let [activeDatum2] = flip map otherOfferRefs $ \(offerRef,_) ->
        createAcceptanceDatumFromOffer borrowerCred1 offerRef (slotToPosixTime newStartSlot) offerDatum
      [loanIdBeacon2] = map (genLoanId . fst) otherOfferRefs

  activeUTxOs <-
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      loanAddress1
      (activeBeaconCurrencySymbol,"Active")

  let sampleRollovers acs = flip map acs $ 
        \(_,Just ad@ActiveDatum{..}) ->
          Output
            { outputAddress = loanAddress1
            , outputValue = utxoValue 4_000_000 $ mconcat
                [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                , uncurry PV2.singleton (_unAsset collateral1) 10
                ]
            , outputDatum = OutputDatum $ toDatum $ createPostInterestActiveDatum 1 ad
            , outputReferenceScript = toReferenceScript Nothing
            }

  -- Try to accept the offer when applying interest to another loan.
  void $ transact borrowerPersonalAddr1 [loanAddress1,refScriptAddress] [borrowerPayPrivKey1] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon1,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon1,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon2,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
              , mintPolicy = toVersionedMintingPolicy activeBeaconScript
              , mintReference = Just activeRef
              }
          ]
      , inputs = concat
          [ flip map otherOfferRefs $ \(offerRef,_) -> 
              Input
                { inputId = offerRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum (toRedeemer AcceptOffer)
                }
          , flip map otherAskRefs $ \(askRef,_) ->
              Input 
                { inputId = askRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum (toRedeemer AcceptOffer)
                }
          , flip map activeUTxOs $ \(activeUtxoRef,_) ->
              Input
                { inputId = activeUtxoRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ ApplyInterest 0 1)
                }
          ]
      , outputs = mconcat
          [ sampleRollovers activeUTxOs
          , [ Output
                { outputAddress = loanAddress1
                , outputValue = utxoValue 4_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon1) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon2) 1
                    , uncurry PV2.singleton (_unAsset collateral1) 10
                    ]
                , outputDatum = OutputDatum $ toDatum activeDatum2
                , outputReferenceScript = toReferenceScript Nothing
                }
            , Output
                { outputAddress = toCardanoApiAddress lenderAddr1
                , outputValue = utxoValue 4_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon2) 1 ]
                , outputDatum = OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon2)
                , outputReferenceScript = toReferenceScript Nothing
                }
            ]
          ]
      , referenceInputs = [negotiationRef,activeRef,loanRef,interestObserverRef]
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash interestObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference interestObserverRef $ 
                    toRedeemer ObserveInterest
              }
          ]
      , extraKeyWitnesses = [borrowerPubKey1]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just newStartSlot
          , validityRangeUpperBound = Just $ newStartSlot + 1
          }
      }

-- | Unlock lost collateral in the same transaction where a partial payment is made.
regressionTest5 :: MonadEmulator m => m ()
regressionTest5 = do
  let borrowerWallet1 = Mock.knownMockWallet 1
      borrowerPersonalAddr1 = Mock.mockWalletAddress borrowerWallet1
      borrowerPayPrivKey1 = Mock.paymentPrivateKey borrowerWallet1
      borrowerPubKey1 = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash borrowerWallet1
      borrowerCred1 = PV2.PubKeyCredential borrowerPubKey1
      borrowerBeacon1 = genBorrowerId borrowerCred1
      loanAddress1 = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash loanScript) (Just $ PV2.StakingHash borrowerCred1)

      lenderWallet1 = Mock.knownMockWallet 2
      lenderPersonalAddr1 = Mock.mockWalletAddress lenderWallet1
      lenderPayPrivKey1 = Mock.paymentPrivateKey lenderWallet1
      lenderPubKey1 = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash lenderWallet1
      lenderCred1 = PV2.PubKeyCredential lenderPubKey1
      lenderBeacon1 = genLenderId lenderCred1
      lenderAddr1 = 
        PV2.Address (PV2.ScriptCredential $ scriptHash proxyScript) (Just $ PV2.StakingHash lenderCred1)

      -- Loan Info
      loanAsset = Asset (adaSymbol,adaToken)
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      loanBeacon = genLoanAssetBeaconName loanAsset
      askDatum = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred1
        , _loanAsset = loanAsset
        , _loanPrincipal = 10_000_000
        , _loanTerm = 3600
        , _collateral = [collateral1]
        }
      offerDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred1
        , _lenderAddress = lenderAddr1
        , _loanAsset = loanAsset
        , _loanPrincipal = 10_000_000
        , _epochDuration = Nothing
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _compoundingInterest = True
        , _minPayment = 0
        , _penalty = NoPenalty
        , _collateralization = [(collateral1,Fraction(1,1_000_000))]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet1 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr1 [refScriptAddress] [borrowerPayPrivKey1] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",2),(_unAssetBeacon loanBeacon,2)]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred1
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum askDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum askDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [borrowerPubKey1]
      }

  -- Create the Offer UTxO.
  void $ transact lenderPersonalAddr1 [refScriptAddress] [lenderPayPrivKey1] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",2)
                  , (_unAssetBeacon loanBeacon,2)
                  , (_unLenderId lenderBeacon1,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateOffer lenderCred1
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon1) 1
                  , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
                  ]
              , outputDatum = OutputDatum $ toDatum offerDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon1) 1
                  , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
                  ]
              , outputDatum = OutputDatum $ toDatum offerDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey1]
      }

  startSlot <- currentSlot

  askRefs <- take 1 <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @AskDatum 
      loanAddress1
      (negotiationBeaconCurrencySymbol,"Ask")

  offerRefs <- take 1 <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @OfferDatum 
      loanAddress1
      (negotiationBeaconCurrencySymbol,"Offer")

  let [activeDatum1] = flip map offerRefs $ \(offerRef,_) ->
        createAcceptanceDatumFromOffer borrowerCred1 offerRef (slotToPosixTime startSlot) offerDatum
      [loanIdBeacon1] = map (genLoanId . fst) offerRefs

  -- Try to accept the offer.
  void $ transact borrowerPersonalAddr1 [loanAddress1,refScriptAddress] [borrowerPayPrivKey1] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon1,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon1,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon1,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
              , mintPolicy = toVersionedMintingPolicy activeBeaconScript
              , mintReference = Just activeRef
              }
          ]
      , inputs = concat
          [ flip map offerRefs $ \(offerRef,_) -> 
              Input
                { inputId = offerRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum (toRedeemer AcceptOffer)
                }
          , flip map askRefs $ \(askRef,_) ->
              Input 
                { inputId = askRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum (toRedeemer AcceptOffer)
                }
          ]
      , outputs =
          [ Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon1) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon1) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr1
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon1) 1 ]
              , outputDatum = OutputDatum $ toDatum $ 
                  PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon1)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey1]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just startSlot
          , validityRangeUpperBound = Nothing
          }
      }
  
  currentSlot >>= awaitTime . slotToPosixTime . (+3600) . (+3600)
  newStartSlot <- currentSlot

  otherAskRefs <- take 1 <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @AskDatum 
      loanAddress1
      (negotiationBeaconCurrencySymbol,"Ask")

  otherOfferRefs <- take 1 <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @OfferDatum 
      loanAddress1
      (negotiationBeaconCurrencySymbol,"Offer")

  let [activeDatum2] = flip map otherOfferRefs $ \(offerRef,_) ->
        createAcceptanceDatumFromOffer borrowerCred1 offerRef (slotToPosixTime newStartSlot) offerDatum
      [loanIdBeacon2] = map (genLoanId . fst) otherOfferRefs

  -- Accept the second offer.
  void $ transact borrowerPersonalAddr1 [loanAddress1,refScriptAddress] [borrowerPayPrivKey1] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon1,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon1,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon2,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
              , mintPolicy = toVersionedMintingPolicy activeBeaconScript
              , mintReference = Just activeRef
              }
          ]
      , inputs = concat
          [ flip map otherOfferRefs $ \(offerRef,_) -> 
              Input
                { inputId = offerRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum (toRedeemer AcceptOffer)
                }
          , flip map otherAskRefs $ \(askRef,_) ->
              Input 
                { inputId = askRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum (toRedeemer AcceptOffer)
                }
          ]
      , outputs = mconcat
          [ [ Output
                { outputAddress = loanAddress1
                , outputValue = utxoValue 4_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon1) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon2) 1
                    , uncurry PV2.singleton (_unAsset collateral1) 10
                    ]
                , outputDatum = OutputDatum $ toDatum activeDatum2
                , outputReferenceScript = toReferenceScript Nothing
                }
            , Output
                { outputAddress = toCardanoApiAddress lenderAddr1
                , outputValue = utxoValue 4_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon2) 1 ]
                , outputDatum = OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon2)
                , outputReferenceScript = toReferenceScript Nothing
                }
            ]
          ]
      , referenceInputs = [negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey1]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just newStartSlot
          , validityRangeUpperBound = Nothing
          }
      }

  unlockSlot <- currentSlot

  lostUTxO <- head <$>
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      loanAddress1
      (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon1)

  liveUTxO <- head <$>
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      loanAddress1
      (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon2)

  let sampleCleanUp acs = flip map acs $
        \(_,Just ad@ActiveDatum{..}) ->
          TokenMint
            { mintTokens = 
                [ ("Active",-1)
                , (_unAssetBeacon _assetBeacon,-1)
                , (_unBorrowerId _borrowerId,-1)
                , (_unLoanId _loanId,-1)
                ]
            , mintRedeemer = toRedeemer BurnAndUnlockLost
            , mintPolicy = toVersionedMintingPolicy activeBeaconScript
            , mintReference = Just activeRef
            }

  let samplePayments acs = flip concatMap acs $ 
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 6
                  ]
              , outputDatum = OutputDatum $ toDatum $ createPostPaymentActiveDatum 5_000_000 ad
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr1
              , outputValue = utxoValue 5_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  -- Try to unlock lost collateral when making a partial payment.
  void $ transact borrowerPersonalAddr1 [loanAddress1,refScriptAddress] [borrowerPayPrivKey1] $
    emptyTxParams
      { tokens = sampleCleanUp [lostUTxO]
      , inputs =
          [ Input
              { inputId = fst lostUTxO
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer Unlock)
              }
          , Input
              { inputId = fst liveUTxO
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
              }
          ]
      , outputs = samplePayments [liveUTxO]
      , referenceInputs = [activeRef,loanRef,paymentObserverRef]
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , extraKeyWitnesses = [borrowerPubKey1]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just unlockSlot
          , validityRangeUpperBound = Just $ unlockSlot + 1
          }
      }

-- | Claim expired collateral in the same transaction where a partial payment is made.
regressionTest6 :: MonadEmulator m => m ()
regressionTest6 = do
  let borrowerWallet1 = Mock.knownMockWallet 1
      borrowerPersonalAddr1 = Mock.mockWalletAddress borrowerWallet1
      borrowerPayPrivKey1 = Mock.paymentPrivateKey borrowerWallet1
      borrowerPubKey1 = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash borrowerWallet1
      borrowerCred1 = PV2.PubKeyCredential borrowerPubKey1
      borrowerBeacon1 = genBorrowerId borrowerCred1
      loanAddress1 = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash loanScript) (Just $ PV2.StakingHash borrowerCred1)

      lenderWallet1 = Mock.knownMockWallet 2
      lenderPersonalAddr1 = Mock.mockWalletAddress lenderWallet1
      lenderPayPrivKey1 = Mock.paymentPrivateKey lenderWallet1
      lenderPubKey1 = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash lenderWallet1
      lenderCred1 = PV2.PubKeyCredential lenderPubKey1
      lenderBeacon1 = genLenderId lenderCred1
      lenderAddr1 = 
        PV2.Address (PV2.ScriptCredential $ scriptHash proxyScript) (Just $ PV2.StakingHash lenderCred1)

      -- Loan Info
      loanAsset = Asset (adaSymbol,adaToken)
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      loanBeacon = genLoanAssetBeaconName loanAsset
      askDatum = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred1
        , _loanAsset = loanAsset
        , _loanPrincipal = 10_000_000
        , _loanTerm = 3600
        , _collateral = [collateral1]
        }
      offerDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred1
        , _lenderAddress = lenderAddr1
        , _loanAsset = loanAsset
        , _loanPrincipal = 10_000_000
        , _epochDuration = Nothing
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _compoundingInterest = True
        , _minPayment = 0
        , _penalty = NoPenalty
        , _collateralization = [(collateral1,Fraction(1,1_000_000))]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet1 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr1 [refScriptAddress] [borrowerPayPrivKey1] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",2),(_unAssetBeacon loanBeacon,2)]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred1
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum askDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum askDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [borrowerPubKey1]
      }

  -- Create the Offer UTxO.
  void $ transact lenderPersonalAddr1 [refScriptAddress] [lenderPayPrivKey1] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",2)
                  , (_unAssetBeacon loanBeacon,2)
                  , (_unLenderId lenderBeacon1,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateOffer lenderCred1
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon1) 1
                  , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
                  ]
              , outputDatum = OutputDatum $ toDatum offerDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon1) 1
                  , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
                  ]
              , outputDatum = OutputDatum $ toDatum offerDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey1]
      }

  startSlot <- currentSlot

  askRefs <- take 1 <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @AskDatum 
      loanAddress1
      (negotiationBeaconCurrencySymbol,"Ask")

  offerRefs <- take 1 <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @OfferDatum 
      loanAddress1
      (negotiationBeaconCurrencySymbol,"Offer")

  let [activeDatum1] = flip map offerRefs $ \(offerRef,_) ->
        createAcceptanceDatumFromOffer borrowerCred1 offerRef (slotToPosixTime startSlot) offerDatum
      [loanIdBeacon1] = map (genLoanId . fst) offerRefs

  -- Try to accept the offer.
  void $ transact borrowerPersonalAddr1 [loanAddress1,refScriptAddress] [borrowerPayPrivKey1] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon1,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon1,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon1,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
              , mintPolicy = toVersionedMintingPolicy activeBeaconScript
              , mintReference = Just activeRef
              }
          ]
      , inputs = concat
          [ flip map offerRefs $ \(offerRef,_) -> 
              Input
                { inputId = offerRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum (toRedeemer AcceptOffer)
                }
          , flip map askRefs $ \(askRef,_) ->
              Input 
                { inputId = askRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum (toRedeemer AcceptOffer)
                }
          ]
      , outputs =
          [ Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon1) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon1) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr1
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon1) 1 ]
              , outputDatum = OutputDatum $ toDatum $ 
                  PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon1)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey1]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just startSlot
          , validityRangeUpperBound = Nothing
          }
      }
  
  currentSlot >>= awaitTime . slotToPosixTime . (+3600) . (+3600)
  newStartSlot <- currentSlot

  otherAskRefs <- take 1 <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @AskDatum 
      loanAddress1
      (negotiationBeaconCurrencySymbol,"Ask")

  otherOfferRefs <- take 1 <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @OfferDatum 
      loanAddress1
      (negotiationBeaconCurrencySymbol,"Offer")

  let [activeDatum2] = flip map otherOfferRefs $ \(offerRef,_) ->
        createAcceptanceDatumFromOffer borrowerCred1 offerRef (slotToPosixTime newStartSlot) offerDatum
      [loanIdBeacon2] = map (genLoanId . fst) otherOfferRefs

  -- Accept the second offer.
  void $ transact borrowerPersonalAddr1 [loanAddress1,refScriptAddress] [borrowerPayPrivKey1] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon1,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon1,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon2,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
              , mintPolicy = toVersionedMintingPolicy activeBeaconScript
              , mintReference = Just activeRef
              }
          ]
      , inputs = concat
          [ flip map otherOfferRefs $ \(offerRef,_) -> 
              Input
                { inputId = offerRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum (toRedeemer AcceptOffer)
                }
          , flip map otherAskRefs $ \(askRef,_) ->
              Input 
                { inputId = askRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum (toRedeemer AcceptOffer)
                }
          ]
      , outputs = mconcat
          [ [ Output
                { outputAddress = loanAddress1
                , outputValue = utxoValue 4_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon1) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon2) 1
                    , uncurry PV2.singleton (_unAsset collateral1) 10
                    ]
                , outputDatum = OutputDatum $ toDatum activeDatum2
                , outputReferenceScript = toReferenceScript Nothing
                }
            , Output
                { outputAddress = toCardanoApiAddress lenderAddr1
                , outputValue = utxoValue 4_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon2) 1 ]
                , outputDatum = OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon2)
                , outputReferenceScript = toReferenceScript Nothing
                }
            ]
          ]
      , referenceInputs = [negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey1]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just newStartSlot
          , validityRangeUpperBound = Nothing
          }
      }

  unlockSlot <- currentSlot

  lostUTxO <- head <$>
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      loanAddress1
      (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon1)

  liveUTxO <- head <$>
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      loanAddress1
      (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon2)

  keyUTxOs <- fmap concat $ forM [lostUTxO] $ \(_,Just ad@ActiveDatum{_loanId}) ->
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      (toCardanoApiAddress lenderAddr1) 
      (activeBeaconCurrencySymbol,_unLoanId _loanId)

  let sampleCleanUp acs = flip map acs $
        \(_,Just ad@ActiveDatum{..}) ->
          TokenMint
            { mintTokens = 
                [ ("Active",-1)
                , (_unAssetBeacon _assetBeacon,-1)
                , (_unBorrowerId _borrowerId,-1)
                , (_unLoanId _loanId,-2)
                ]
            , mintRedeemer = toRedeemer BurnKeyAndClaimExpired
            , mintPolicy = toVersionedMintingPolicy activeBeaconScript
            , mintReference = Just activeRef
            }

  let samplePayments acs = flip concatMap acs $ 
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 6
                  ]
              , outputDatum = OutputDatum $ toDatum $ createPostPaymentActiveDatum 5_000_000 ad
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr1
              , outputValue = utxoValue 5_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  -- Try to unlock lost collateral when making a partial payment.
  void $ transact 
    borrowerPersonalAddr1 
    [loanAddress1,refScriptAddress,toCardanoApiAddress lenderAddr1] 
    [borrowerPayPrivKey1,lenderPayPrivKey1]
    emptyTxParams
      { tokens = sampleCleanUp [lostUTxO]
      , inputs =
          [ Input
              { inputId = fst lostUTxO
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer SpendWithKeyNFT)
              }
          , Input
              { inputId = fst liveUTxO
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
              }
          , Input
              { inputId = fst $ head keyUTxOs
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = samplePayments [liveUTxO]
      , referenceInputs = [activeRef,loanRef,paymentObserverRef,proxyRef]
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , extraKeyWitnesses = [borrowerPubKey1,lenderPubKey1]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just unlockSlot
          , validityRangeUpperBound = Just $ unlockSlot + 1
          }
      }

-------------------------------------------------
-- Miscelleneous Failure Tests
-------------------------------------------------
-- | Make a full payment in the same transaction where an offer is accepted.
failureTest1 :: MonadEmulator m => m ()
failureTest1 = do
  let borrowerWallet1 = Mock.knownMockWallet 1
      borrowerPersonalAddr1 = Mock.mockWalletAddress borrowerWallet1
      borrowerPayPrivKey1 = Mock.paymentPrivateKey borrowerWallet1
      borrowerPubKey1 = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash borrowerWallet1
      borrowerCred1 = PV2.PubKeyCredential borrowerPubKey1
      borrowerBeacon1 = genBorrowerId borrowerCred1
      loanAddress1 = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash loanScript) (Just $ PV2.StakingHash borrowerCred1)

      lenderWallet1 = Mock.knownMockWallet 2
      lenderPersonalAddr1 = Mock.mockWalletAddress lenderWallet1
      lenderPayPrivKey1 = Mock.paymentPrivateKey lenderWallet1
      lenderPubKey1 = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash lenderWallet1
      lenderCred1 = PV2.PubKeyCredential lenderPubKey1
      lenderBeacon1 = genLenderId lenderCred1
      lenderAddr1 = 
        PV2.Address (PV2.ScriptCredential $ scriptHash proxyScript) (Just $ PV2.StakingHash lenderCred1)

      -- Loan Info
      loanAsset = Asset (adaSymbol,adaToken)
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      loanBeacon = genLoanAssetBeaconName loanAsset
      askDatum = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred1
        , _loanAsset = loanAsset
        , _loanPrincipal = 10_000_000
        , _loanTerm = 3600
        , _collateral = [collateral1]
        }
      offerDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred1
        , _lenderAddress = lenderAddr1
        , _loanAsset = loanAsset
        , _loanPrincipal = 10_000_000
        , _epochDuration = Nothing
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _compoundingInterest = True
        , _minPayment = 0
        , _penalty = NoPenalty
        , _collateralization = [(collateral1,Fraction(1,1_000_000))]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet1 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr1 [refScriptAddress] [borrowerPayPrivKey1] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",2),(_unAssetBeacon loanBeacon,2)]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred1
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum askDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum askDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [borrowerPubKey1]
      }

  -- Create the Offer UTxO.
  void $ transact lenderPersonalAddr1 [refScriptAddress] [lenderPayPrivKey1] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",2)
                  , (_unAssetBeacon loanBeacon,2)
                  , (_unLenderId lenderBeacon1,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateOffer lenderCred1
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon1) 1
                  , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
                  ]
              , outputDatum = OutputDatum $ toDatum offerDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon1) 1
                  , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
                  ]
              , outputDatum = OutputDatum $ toDatum offerDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey1]
      }

  startSlot <- currentSlot

  askRefs <- take 1 <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @AskDatum 
      loanAddress1
      (negotiationBeaconCurrencySymbol,"Ask")

  offerRefs <- take 1 <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @OfferDatum 
      loanAddress1
      (negotiationBeaconCurrencySymbol,"Offer")

  let [activeDatum1] = flip map offerRefs $ \(offerRef,_) ->
        createAcceptanceDatumFromOffer borrowerCred1 offerRef (slotToPosixTime startSlot) offerDatum
      [loanIdBeacon1] = map (genLoanId . fst) offerRefs

  -- Try to accept the offer.
  void $ transact borrowerPersonalAddr1 [loanAddress1,refScriptAddress] [borrowerPayPrivKey1] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon1,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon1,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon1,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
              , mintPolicy = toVersionedMintingPolicy activeBeaconScript
              , mintReference = Just activeRef
              }
          ]
      , inputs = concat
          [ flip map offerRefs $ \(offerRef,_) -> 
              Input
                { inputId = offerRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum (toRedeemer AcceptOffer)
                }
          , flip map askRefs $ \(askRef,_) ->
              Input 
                { inputId = askRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum (toRedeemer AcceptOffer)
                }
          ]
      , outputs =
          [ Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon1) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon1) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr1
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon1) 1 ]
              , outputDatum = OutputDatum $ toDatum $ 
                  PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon1)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey1]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just startSlot
          , validityRangeUpperBound = Nothing
          }
      }

  newStartSlot <- currentSlot

  otherAskRefs <- take 1 <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @AskDatum 
      loanAddress1
      (negotiationBeaconCurrencySymbol,"Ask")

  otherOfferRefs <- take 1 <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @OfferDatum 
      loanAddress1
      (negotiationBeaconCurrencySymbol,"Offer")

  let [activeDatum2] = flip map otherOfferRefs $ \(offerRef,_) ->
        createAcceptanceDatumFromOffer borrowerCred1 offerRef (slotToPosixTime newStartSlot) offerDatum
      [loanIdBeacon2] = map (genLoanId . fst) otherOfferRefs

  activeUTxOs <-
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      loanAddress1
      (activeBeaconCurrencySymbol,"Active")

  let samplePayments acs = flip concatMap acs $ 
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                  ]
              , outputDatum = OutputDatum $ toDatum $ createPostPaymentActiveDatum 11_000_000 ad
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr1
              , outputValue = utxoValue 11_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  -- Try to accept the offer when making another payment.
  void $ transact borrowerPersonalAddr1 [loanAddress1,refScriptAddress] [borrowerPayPrivKey1] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon1,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon2,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
              , mintPolicy = toVersionedMintingPolicy activeBeaconScript
              , mintReference = Just activeRef
              }
          ]
      , inputs = concat
          [ flip map otherOfferRefs $ \(offerRef,_) -> 
              Input
                { inputId = offerRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum (toRedeemer AcceptOffer)
                }
          , flip map otherAskRefs $ \(askRef,_) ->
              Input 
                { inputId = askRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum (toRedeemer AcceptOffer)
                }
          , flip map activeUTxOs $ \(activeUtxoRef,_) ->
              Input
                { inputId = activeUtxoRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 11_000_000)
                }
          ]
      , outputs = mconcat
          [ [ Output
                { outputAddress = loanAddress1
                , outputValue = utxoValue 4_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon1) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon2) 1
                    , uncurry PV2.singleton (_unAsset collateral1) 10
                    ]
                , outputDatum = OutputDatum $ toDatum activeDatum2
                , outputReferenceScript = toReferenceScript Nothing
                }
            , Output
                { outputAddress = toCardanoApiAddress lenderAddr1
                , outputValue = utxoValue 4_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon2) 1 ]
                , outputDatum = OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon2)
                , outputReferenceScript = toReferenceScript Nothing
                }
            ]
          , samplePayments activeUTxOs
          ]
      , referenceInputs = [negotiationRef,activeRef,loanRef,paymentObserverRef]
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , extraKeyWitnesses = [borrowerPubKey1]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just newStartSlot
          , validityRangeUpperBound = Just $ newStartSlot + 1
          }
      }

-- | Unlock lost collateral in the same transaction where a full payment is made. The BorrowerId
-- is burned using BurnAndUnlockLost.
failureTest2 :: MonadEmulator m => m ()
failureTest2 = do
  let borrowerWallet1 = Mock.knownMockWallet 1
      borrowerPersonalAddr1 = Mock.mockWalletAddress borrowerWallet1
      borrowerPayPrivKey1 = Mock.paymentPrivateKey borrowerWallet1
      borrowerPubKey1 = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash borrowerWallet1
      borrowerCred1 = PV2.PubKeyCredential borrowerPubKey1
      borrowerBeacon1 = genBorrowerId borrowerCred1
      loanAddress1 = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash loanScript) (Just $ PV2.StakingHash borrowerCred1)

      lenderWallet1 = Mock.knownMockWallet 2
      lenderPersonalAddr1 = Mock.mockWalletAddress lenderWallet1
      lenderPayPrivKey1 = Mock.paymentPrivateKey lenderWallet1
      lenderPubKey1 = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash lenderWallet1
      lenderCred1 = PV2.PubKeyCredential lenderPubKey1
      lenderBeacon1 = genLenderId lenderCred1
      lenderAddr1 = 
        PV2.Address (PV2.ScriptCredential $ scriptHash proxyScript) (Just $ PV2.StakingHash lenderCred1)

      -- Loan Info
      loanAsset = Asset (adaSymbol,adaToken)
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      loanBeacon = genLoanAssetBeaconName loanAsset
      askDatum = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred1
        , _loanAsset = loanAsset
        , _loanPrincipal = 10_000_000
        , _loanTerm = 3600
        , _collateral = [collateral1]
        }
      offerDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred1
        , _lenderAddress = lenderAddr1
        , _loanAsset = loanAsset
        , _loanPrincipal = 10_000_000
        , _epochDuration = Nothing
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _compoundingInterest = True
        , _minPayment = 0
        , _penalty = NoPenalty
        , _collateralization = [(collateral1,Fraction(1,1_000_000))]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet1 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr1 [refScriptAddress] [borrowerPayPrivKey1] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",2),(_unAssetBeacon loanBeacon,2)]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred1
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum askDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum askDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [borrowerPubKey1]
      }

  -- Create the Offer UTxO.
  void $ transact lenderPersonalAddr1 [refScriptAddress] [lenderPayPrivKey1] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",2)
                  , (_unAssetBeacon loanBeacon,2)
                  , (_unLenderId lenderBeacon1,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateOffer lenderCred1
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon1) 1
                  , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
                  ]
              , outputDatum = OutputDatum $ toDatum offerDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon1) 1
                  , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
                  ]
              , outputDatum = OutputDatum $ toDatum offerDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey1]
      }

  startSlot <- currentSlot

  askRefs <- take 1 <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @AskDatum 
      loanAddress1
      (negotiationBeaconCurrencySymbol,"Ask")

  offerRefs <- take 1 <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @OfferDatum 
      loanAddress1
      (negotiationBeaconCurrencySymbol,"Offer")

  let [activeDatum1] = flip map offerRefs $ \(offerRef,_) ->
        createAcceptanceDatumFromOffer borrowerCred1 offerRef (slotToPosixTime startSlot) offerDatum
      [loanIdBeacon1] = map (genLoanId . fst) offerRefs

  -- Try to accept the offer.
  void $ transact borrowerPersonalAddr1 [loanAddress1,refScriptAddress] [borrowerPayPrivKey1] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon1,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon1,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon1,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
              , mintPolicy = toVersionedMintingPolicy activeBeaconScript
              , mintReference = Just activeRef
              }
          ]
      , inputs = concat
          [ flip map offerRefs $ \(offerRef,_) -> 
              Input
                { inputId = offerRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum (toRedeemer AcceptOffer)
                }
          , flip map askRefs $ \(askRef,_) ->
              Input 
                { inputId = askRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum (toRedeemer AcceptOffer)
                }
          ]
      , outputs =
          [ Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon1) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon1) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr1
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon1) 1 ]
              , outputDatum = OutputDatum $ toDatum $ 
                  PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon1)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey1]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just startSlot
          , validityRangeUpperBound = Nothing
          }
      }
  
  currentSlot >>= awaitTime . slotToPosixTime . (+3600) . (+3600)
  newStartSlot <- currentSlot

  otherAskRefs <- take 1 <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @AskDatum 
      loanAddress1
      (negotiationBeaconCurrencySymbol,"Ask")

  otherOfferRefs <- take 1 <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @OfferDatum 
      loanAddress1
      (negotiationBeaconCurrencySymbol,"Offer")

  let [activeDatum2] = flip map otherOfferRefs $ \(offerRef,_) ->
        createAcceptanceDatumFromOffer borrowerCred1 offerRef (slotToPosixTime newStartSlot) offerDatum
      [loanIdBeacon2] = map (genLoanId . fst) otherOfferRefs

  -- Accept the second offer.
  void $ transact borrowerPersonalAddr1 [loanAddress1,refScriptAddress] [borrowerPayPrivKey1] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon1,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon1,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon2,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
              , mintPolicy = toVersionedMintingPolicy activeBeaconScript
              , mintReference = Just activeRef
              }
          ]
      , inputs = concat
          [ flip map otherOfferRefs $ \(offerRef,_) -> 
              Input
                { inputId = offerRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum (toRedeemer AcceptOffer)
                }
          , flip map otherAskRefs $ \(askRef,_) ->
              Input 
                { inputId = askRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum (toRedeemer AcceptOffer)
                }
          ]
      , outputs = mconcat
          [ [ Output
                { outputAddress = loanAddress1
                , outputValue = utxoValue 4_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon1) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon2) 1
                    , uncurry PV2.singleton (_unAsset collateral1) 10
                    ]
                , outputDatum = OutputDatum $ toDatum activeDatum2
                , outputReferenceScript = toReferenceScript Nothing
                }
            , Output
                { outputAddress = toCardanoApiAddress lenderAddr1
                , outputValue = utxoValue 4_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon2) 1 ]
                , outputDatum = OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon2)
                , outputReferenceScript = toReferenceScript Nothing
                }
            ]
          ]
      , referenceInputs = [negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey1]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just newStartSlot
          , validityRangeUpperBound = Nothing
          }
      }

  unlockSlot <- currentSlot

  lostUTxO <- head <$>
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      loanAddress1
      (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon1)

  liveUTxO <- head <$>
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      loanAddress1
      (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon2)

  let sampleCleanUp acs = flip map acs $
        \(_,Just ad@ActiveDatum{..}) ->
          TokenMint
            { mintTokens = 
                [ ("Active",-1)
                , (_unAssetBeacon _assetBeacon,-1)
                , (_unBorrowerId _borrowerId,-1)
                , (_unLoanId _loanId,-1)
                ]
            , mintRedeemer = toRedeemer BurnAndUnlockLost
            , mintPolicy = toVersionedMintingPolicy activeBeaconScript
            , mintReference = Just activeRef
            }

  let samplePayments acs = flip concatMap acs $ 
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                  ]
              , outputDatum = OutputDatum $ toDatum $ createPostPaymentActiveDatum 11_000_000 ad
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr1
              , outputValue = utxoValue 11_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  -- Try to unlock lost collateral when making a partial payment.
  void $ transact borrowerPersonalAddr1 [loanAddress1,refScriptAddress] [borrowerPayPrivKey1] $
    emptyTxParams
      { tokens = sampleCleanUp [lostUTxO] <>
          [ TokenMint
              { mintTokens = 
                  [ (_unBorrowerId borrowerBeacon1,-1) ]
              , mintRedeemer = toRedeemer BurnAndUnlockLost
              , mintPolicy = toVersionedMintingPolicy activeBeaconScript
              , mintReference = Just activeRef
              }
          ]
      , inputs =
          [ Input
              { inputId = fst lostUTxO
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer Unlock)
              }
          , Input
              { inputId = fst liveUTxO
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 11_000_000)
              }
          ]
      , outputs = samplePayments [liveUTxO]
      , referenceInputs = [activeRef,loanRef,paymentObserverRef]
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , extraKeyWitnesses = [borrowerPubKey1]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just unlockSlot
          , validityRangeUpperBound = Just $ unlockSlot + 1
          }
      }

-- | Claim expired collateral in the same transaction where a full payment is made. The BorrowerId
-- is burned with BurnKeyAndClaimExpired.
failureTest3 :: MonadEmulator m => m ()
failureTest3 = do
  let borrowerWallet1 = Mock.knownMockWallet 1
      borrowerPersonalAddr1 = Mock.mockWalletAddress borrowerWallet1
      borrowerPayPrivKey1 = Mock.paymentPrivateKey borrowerWallet1
      borrowerPubKey1 = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash borrowerWallet1
      borrowerCred1 = PV2.PubKeyCredential borrowerPubKey1
      borrowerBeacon1 = genBorrowerId borrowerCred1
      loanAddress1 = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash loanScript) (Just $ PV2.StakingHash borrowerCred1)

      lenderWallet1 = Mock.knownMockWallet 2
      lenderPersonalAddr1 = Mock.mockWalletAddress lenderWallet1
      lenderPayPrivKey1 = Mock.paymentPrivateKey lenderWallet1
      lenderPubKey1 = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash lenderWallet1
      lenderCred1 = PV2.PubKeyCredential lenderPubKey1
      lenderBeacon1 = genLenderId lenderCred1
      lenderAddr1 = 
        PV2.Address (PV2.ScriptCredential $ scriptHash proxyScript) (Just $ PV2.StakingHash lenderCred1)

      -- Loan Info
      loanAsset = Asset (adaSymbol,adaToken)
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      loanBeacon = genLoanAssetBeaconName loanAsset
      askDatum = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred1
        , _loanAsset = loanAsset
        , _loanPrincipal = 10_000_000
        , _loanTerm = 3600
        , _collateral = [collateral1]
        }
      offerDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred1
        , _lenderAddress = lenderAddr1
        , _loanAsset = loanAsset
        , _loanPrincipal = 10_000_000
        , _epochDuration = Nothing
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _compoundingInterest = True
        , _minPayment = 0
        , _penalty = NoPenalty
        , _collateralization = [(collateral1,Fraction(1,1_000_000))]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet1 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr1 [refScriptAddress] [borrowerPayPrivKey1] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",2),(_unAssetBeacon loanBeacon,2)]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred1
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum askDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum askDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [borrowerPubKey1]
      }

  -- Create the Offer UTxO.
  void $ transact lenderPersonalAddr1 [refScriptAddress] [lenderPayPrivKey1] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",2)
                  , (_unAssetBeacon loanBeacon,2)
                  , (_unLenderId lenderBeacon1,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateOffer lenderCred1
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon1) 1
                  , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
                  ]
              , outputDatum = OutputDatum $ toDatum offerDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon1) 1
                  , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
                  ]
              , outputDatum = OutputDatum $ toDatum offerDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey1]
      }

  startSlot <- currentSlot

  askRefs <- take 1 <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @AskDatum 
      loanAddress1
      (negotiationBeaconCurrencySymbol,"Ask")

  offerRefs <- take 1 <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @OfferDatum 
      loanAddress1
      (negotiationBeaconCurrencySymbol,"Offer")

  let [activeDatum1] = flip map offerRefs $ \(offerRef,_) ->
        createAcceptanceDatumFromOffer borrowerCred1 offerRef (slotToPosixTime startSlot) offerDatum
      [loanIdBeacon1] = map (genLoanId . fst) offerRefs

  -- Try to accept the offer.
  void $ transact borrowerPersonalAddr1 [loanAddress1,refScriptAddress] [borrowerPayPrivKey1] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon1,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon1,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon1,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
              , mintPolicy = toVersionedMintingPolicy activeBeaconScript
              , mintReference = Just activeRef
              }
          ]
      , inputs = concat
          [ flip map offerRefs $ \(offerRef,_) -> 
              Input
                { inputId = offerRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum (toRedeemer AcceptOffer)
                }
          , flip map askRefs $ \(askRef,_) ->
              Input 
                { inputId = askRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum (toRedeemer AcceptOffer)
                }
          ]
      , outputs =
          [ Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon1) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon1) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr1
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon1) 1 ]
              , outputDatum = OutputDatum $ toDatum $ 
                  PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon1)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey1]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just startSlot
          , validityRangeUpperBound = Nothing
          }
      }
  
  currentSlot >>= awaitTime . slotToPosixTime . (+3600) . (+3600)
  newStartSlot <- currentSlot

  otherAskRefs <- take 1 <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @AskDatum 
      loanAddress1
      (negotiationBeaconCurrencySymbol,"Ask")

  otherOfferRefs <- take 1 <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @OfferDatum 
      loanAddress1
      (negotiationBeaconCurrencySymbol,"Offer")

  let [activeDatum2] = flip map otherOfferRefs $ \(offerRef,_) ->
        createAcceptanceDatumFromOffer borrowerCred1 offerRef (slotToPosixTime newStartSlot) offerDatum
      [loanIdBeacon2] = map (genLoanId . fst) otherOfferRefs

  -- Accept the second offer.
  void $ transact borrowerPersonalAddr1 [loanAddress1,refScriptAddress] [borrowerPayPrivKey1] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon1,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon1,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon2,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
              , mintPolicy = toVersionedMintingPolicy activeBeaconScript
              , mintReference = Just activeRef
              }
          ]
      , inputs = concat
          [ flip map otherOfferRefs $ \(offerRef,_) -> 
              Input
                { inputId = offerRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum (toRedeemer AcceptOffer)
                }
          , flip map otherAskRefs $ \(askRef,_) ->
              Input 
                { inputId = askRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum (toRedeemer AcceptOffer)
                }
          ]
      , outputs = mconcat
          [ [ Output
                { outputAddress = loanAddress1
                , outputValue = utxoValue 4_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon1) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon2) 1
                    , uncurry PV2.singleton (_unAsset collateral1) 10
                    ]
                , outputDatum = OutputDatum $ toDatum activeDatum2
                , outputReferenceScript = toReferenceScript Nothing
                }
            , Output
                { outputAddress = toCardanoApiAddress lenderAddr1
                , outputValue = utxoValue 4_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon2) 1 ]
                , outputDatum = OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon2)
                , outputReferenceScript = toReferenceScript Nothing
                }
            ]
          ]
      , referenceInputs = [negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey1]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just newStartSlot
          , validityRangeUpperBound = Nothing
          }
      }

  unlockSlot <- currentSlot

  lostUTxO <- head <$>
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      loanAddress1
      (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon1)

  liveUTxO <- head <$>
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      loanAddress1
      (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon2)

  keyUTxOs <- fmap concat $ forM [lostUTxO] $ \(_,Just ad@ActiveDatum{_loanId}) ->
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      (toCardanoApiAddress lenderAddr1) 
      (activeBeaconCurrencySymbol,_unLoanId _loanId)

  let sampleCleanUp acs = flip map acs $
        \(_,Just ad@ActiveDatum{..}) ->
          TokenMint
            { mintTokens = 
                [ ("Active",-1)
                , (_unAssetBeacon _assetBeacon,-1)
                , (_unBorrowerId _borrowerId,-2)
                , (_unLoanId _loanId,-2)
                ]
            , mintRedeemer = toRedeemer BurnKeyAndClaimExpired
            , mintPolicy = toVersionedMintingPolicy activeBeaconScript
            , mintReference = Just activeRef
            }

  let samplePayments acs = flip concatMap acs $ 
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                  ]
              , outputDatum = OutputDatum $ toDatum $ createPostPaymentActiveDatum 11_000_000 ad
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr1
              , outputValue = utxoValue 11_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  -- Try to unlock lost collateral when making a partial payment.
  void $ transact 
    borrowerPersonalAddr1 
    [loanAddress1,refScriptAddress,toCardanoApiAddress lenderAddr1] 
    [borrowerPayPrivKey1,lenderPayPrivKey1]
    emptyTxParams
      { tokens = sampleCleanUp [lostUTxO]
      , inputs =
          [ Input
              { inputId = fst lostUTxO
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer SpendWithKeyNFT)
              }
          , Input
              { inputId = fst liveUTxO
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 11_000_000)
              }
          , Input
              { inputId = fst $ head keyUTxOs
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = samplePayments [liveUTxO]
      , referenceInputs = [activeRef,loanRef,paymentObserverRef,proxyRef]
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , extraKeyWitnesses = [borrowerPubKey1,lenderPubKey1]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just unlockSlot
          , validityRangeUpperBound = Just $ unlockSlot + 1
          }
      }

-------------------------------------------------
-- TestTree
-------------------------------------------------
-- | A `TestTree` containing all misc scenarios for Active UTxOs. These scenarios are more
-- complicated than the basic ones tested in the other modules. They include compositions and
-- edge-case scenarios.
tests :: TestTree
tests = testGroup "Miscelleneous Tests"
  [ -- Miscelleneous Regression Tests
    mustSucceed "regressionTest1" regressionTest1
  , mustSucceed "regressionTest2" regressionTest2
  , mustSucceed "regressionTest3" regressionTest3
  , mustSucceed "regressionTest4" regressionTest4
  , mustSucceed "regressionTest5" regressionTest5
  , mustSucceed "regressionTest6" regressionTest6

    -- Miscelleneous Failure Tests
  , scriptMustFailWithError "failureTest1" 
      "The wrong active beacons were minted"
      failureTest1
  , scriptMustFail "failureTest2" 
      -- The exact error depends on order of execution.
      failureTest2 
  , scriptMustFail "failureTest3" 
      -- The exact error depends on order of execution.
      failureTest3
  ]
