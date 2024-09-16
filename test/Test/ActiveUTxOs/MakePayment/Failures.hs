{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.ActiveUTxOs.MakePayment.Failures where

import qualified Ledger.Address as LA
import qualified PlutusLedgerApi.V2 as PV2
import qualified Ledger.CardanoWallet as Mock 
import Test.Tasty (TestTree)
import Data.Maybe (isJust)
import Control.Monad (forM_)

import CardanoLoans

import Test.Prelude

-------------------------------------------------
-- Beacon Failures
-------------------------------------------------
-- | When making a single full payment, the BorrowerId is not burned. It is withdrawn instead.
beaconFailure1 :: MonadEmulator m => m ()
beaconFailure1 = do
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
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
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
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

  askRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        ]

  offerRef <-
    txOutRefWithValue $ 
      utxoValue 4_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
        , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
        ]

  let activeDatum = 
        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      loanIdBeacon = genLoanId offerRef

  -- Accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
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
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1 ]
              , outputDatum = 
                  OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon)
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
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                  ]
              , outputDatum = OutputDatum $ toDatum $ createPostPaymentActiveDatum 11_000_000 ad
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 11_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(_unBorrowerId borrowerBeacon,0)]
              , mintRedeemer = toRedeemer BurnActiveBeacons
              , mintPolicy = toVersionedMintingPolicy activeBeaconScript
              , mintReference = Just activeRef
              }
          ]
      , inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 11_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-- | When making a single full payment, the BorrowerId is not burned. It is left in the Active UTxO.
beaconFailure2 :: MonadEmulator m => m ()
beaconFailure2 = do
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
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
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
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

  askRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        ]

  offerRef <-
    txOutRefWithValue $ 
      utxoValue 4_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
        , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
        ]

  let activeDatum = 
        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      loanIdBeacon = genLoanId offerRef

  -- Accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
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
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1 ]
              , outputDatum = 
                  OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon)
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
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                  ]
              , outputDatum = OutputDatum $ toDatum $ createPostPaymentActiveDatum 11_000_000 ad
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 11_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(_unBorrowerId borrowerBeacon,0)]
              , mintRedeemer = toRedeemer BurnActiveBeacons
              , mintPolicy = toVersionedMintingPolicy activeBeaconScript
              , mintReference = Just activeRef
              }
          ]
      , inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 11_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-- | Make a full payment on an invalid Active UTxO.
beaconFailure3 :: MonadEmulator m => m ()
beaconFailure3 = do
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
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
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
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

  askRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        ]

  offerRef <-
    txOutRefWithValue $ 
      utxoValue 4_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
        , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
        ]

  let activeDatum = 
        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      loanIdBeacon = genLoanId offerRef

  -- Accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = [ ]
      , inputs = [ ]
      , outputs =
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat [ ]
              , outputDatum = OutputDatum $ toDatum activeDatum
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

  activeUTxOs <- filter (isJust . snd) <$> txOutRefsAndDatumsAtAddress @ActiveDatum loanAddress 

  let samplePayments acs = flip concatMap acs $ 
        \(_,Just ad@ActiveDatum{}) ->
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat [ ]
              , outputDatum = OutputDatum $ toDatum $ createPostPaymentActiveDatum 11_000_000 ad
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = [ ]
      , inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 11_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-- | When mixing full and partial payments, store the extra BorrowerId from a finished loan with an active loan.
beaconFailure4 :: MonadEmulator m => m ()
beaconFailure4 = do
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",2),(_unAssetBeacon loanBeacon,2)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum askDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = loanAddress
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
      , extraKeyWitnesses = [borrowerPubKey]
      }

  -- Create the Offer UTxO.
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",2)
                  , (_unAssetBeacon loanBeacon,2)
                  , (_unLenderId lenderBeacon,2)
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
          , Output
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

  askRefs <- map fst <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @AskDatum 
      loanAddress 
      (negotiationBeaconCurrencySymbol,"Ask")

  offerRefs <- map fst <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @OfferDatum 
      loanAddress 
      (negotiationBeaconCurrencySymbol,"Offer")

  let [activeDatum1,activeDatum2] = flip map offerRefs $ \offerRef ->
        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      [loanIdBeacon1,loanIdBeacon2] = map genLoanId offerRefs

  -- Accept the offers.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-2)
                  , ("Ask",-2)
                  , (_unAssetBeacon loanBeacon,-4)
                  , (_unLenderId lenderBeacon,-2)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",2)
                  , (_unBorrowerId borrowerBeacon,2)
                  , (_unAssetBeacon loanBeacon,2)
                  , (_unLoanId loanIdBeacon1,2)
                  , (_unLoanId loanIdBeacon2,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
              , mintPolicy = toVersionedMintingPolicy activeBeaconScript
              , mintReference = Just activeRef
              }
          ]
      , inputs = concat
          [ flip map offerRefs $ \offerRef -> 
              Input
                { inputId = offerRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum (toRedeemer AcceptOffer)
                }
          , flip map askRefs $ \askRef ->
              Input 
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
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon1) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon1) 1 ]
              , outputDatum = OutputDatum $ toDatum $ 
                  PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon1)
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon2) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum2
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon2) 1 ]
              , outputDatum = OutputDatum $ toDatum $ 
                  PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon2)
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

  let samplePayments acs = flip concatMap (zip acs [1::Int ..]) $ 
        \((_,Just ad@ActiveDatum{..}),i) ->
          if even i then
            -- full payment
            [ Output
                { outputAddress = loanAddress
                , outputValue = utxoValue 4_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                    ]
                , outputDatum = OutputDatum $ toDatum $ createPostPaymentActiveDatum 11_000_000 ad
                , outputReferenceScript = toReferenceScript Nothing
                }
            , Output
                { outputAddress = toCardanoApiAddress lenderAddr
                , outputValue = utxoValue 11_000_000 mempty
                , outputDatum = 
                    OutputDatum $ toDatum $ 
                      PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
                , outputReferenceScript = toReferenceScript Nothing
                }
            ]
          else
            -- partial payment
            [ Output
                { outputAddress = loanAddress
                , outputValue = utxoValue 4_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 2
                    , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                    , uncurry PV2.singleton (_unAsset collateral1) 6
                    ]
                , outputDatum = OutputDatum $ toDatum $ createPostPaymentActiveDatum 5_000_000 ad
                , outputReferenceScript = toReferenceScript Nothing
                }
            , Output
                { outputAddress = toCardanoApiAddress lenderAddr
                , outputValue = utxoValue 5_000_000 mempty
                , outputDatum = 
                    OutputDatum $ toDatum $ 
                      PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
                , outputReferenceScript = toReferenceScript Nothing
                }
            ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(_unBorrowerId borrowerBeacon,0)]
              , mintRedeemer = toRedeemer BurnActiveBeacons
              , mintPolicy = toVersionedMintingPolicy activeBeaconScript
              , mintReference = Just activeRef
              }
          ]
      , inputs = flip map (zip activeUTxOs [1::Int ..]) $ \((activeUtxoRef,_),i) ->
          if even i then
            -- Full payment
            Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 11_000_000)
              }
          else
            -- partial payment
            Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
              }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-- | When making a single payment, withdraw the Active beacon.
beaconFailure5 :: MonadEmulator m => m ()
beaconFailure5 = do
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
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
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
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

  askRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        ]

  offerRef <-
    txOutRefWithValue $ 
      utxoValue 4_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
        , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
        ]

  let activeDatum = 
        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      loanIdBeacon = genLoanId offerRef

  -- Accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
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
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1 ]
              , outputDatum = 
                  OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon)
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
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 0
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 6
                  ]
              , outputDatum = OutputDatum $ toDatum $ createPostPaymentActiveDatum 5_000_000 ad
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 5_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-- | When making a single payment, withdraw the Asset beacon.
beaconFailure6 :: MonadEmulator m => m ()
beaconFailure6 = do
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
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
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
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

  askRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        ]

  offerRef <-
    txOutRefWithValue $ 
      utxoValue 4_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
        , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
        ]

  let activeDatum = 
        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      loanIdBeacon = genLoanId offerRef

  -- Accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
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
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1 ]
              , outputDatum = 
                  OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon)
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
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 0
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 6
                  ]
              , outputDatum = OutputDatum $ toDatum $ createPostPaymentActiveDatum 5_000_000 ad
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 5_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-- | When making a single payment, withdraw the LoanId.
beaconFailure7 :: MonadEmulator m => m ()
beaconFailure7 = do
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
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
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
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

  askRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        ]

  offerRef <-
    txOutRefWithValue $ 
      utxoValue 4_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
        , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
        ]

  let activeDatum = 
        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      loanIdBeacon = genLoanId offerRef

  -- Accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
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
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1 ]
              , outputDatum = 
                  OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon)
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
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 0
                  , uncurry PV2.singleton (_unAsset collateral1) 6
                  ]
              , outputDatum = OutputDatum $ toDatum $ createPostPaymentActiveDatum 5_000_000 ad
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 5_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-- | When making payments on multiple loans, swap the loan ids in the collateral outputs.
beaconFailure8 :: MonadEmulator m => m ()
beaconFailure8 = do
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

      lenderWallet2 = Mock.knownMockWallet 3
      lenderPersonalAddr2 = Mock.mockWalletAddress lenderWallet2
      lenderPayPrivKey2 = Mock.paymentPrivateKey lenderWallet2
      lenderPubKey2 = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash lenderWallet2
      lenderCred2 = PV2.PubKeyCredential lenderPubKey2
      lenderBeacon2 = genLenderId lenderCred2
      lenderAddr2 = 
        PV2.Address (PV2.ScriptCredential $ scriptHash proxyScript) (Just $ PV2.StakingHash lenderCred2)

      -- Loan Info
      loanAsset1 = Asset (testTokenSymbol,"TestToken10")
      loanAsset2 = Asset (testTokenSymbol,"TestToken11")
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      loanBeacon1 = genLoanAssetBeaconName loanAsset1
      loanBeacon2 = genLoanAssetBeaconName loanAsset2
      askDatum1 = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred1
        , _loanAsset = loanAsset1
        , _loanPrincipal = 10
        , _loanTerm = 3600
        , _collateral = [collateral1]
        }
      askDatum2 = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred1
        , _loanAsset = loanAsset2
        , _loanPrincipal = 20
        , _loanTerm = 3600
        , _collateral = [collateral1]
        }
      offerDatum1 = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred1
        , _lenderAddress = lenderAddr1
        , _loanAsset = loanAsset1
        , _loanPrincipal = 10
        , _epochDuration = Nothing
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _compoundingInterest = True
        , _minPayment = 0
        , _penalty = NoPenalty
        , _collateralization = [(collateral1,Fraction(1,1))]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }
      offerDatum2 = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred2
        , _lenderAddress = lenderAddr2
        , _loanAsset = loanAsset2
        , _loanPrincipal = 20
        , _epochDuration = Nothing
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _compoundingInterest = True
        , _minPayment = 0
        , _penalty = NoPenalty
        , _collateralization = [(collateral1,Fraction(1,1))]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef,paymentObserverRef,activeRef,loanRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet1 10_000_000 [("TestToken1",1000)]
  mintTestTokens lenderWallet1 10_000_000 [("TestToken10",1000),("TestToken11",1000)]
  mintTestTokens lenderWallet2 10_000_000 [("TestToken10",1000),("TestToken11",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr1 [refScriptAddress] [borrowerPayPrivKey1] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Ask",2)
                  , (_unAssetBeacon loanBeacon1,1)
                  , (_unAssetBeacon loanBeacon2,1)
                  ]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon1) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum askDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon2) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum askDatum2
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
                  [ ("Offer",1)
                  , (_unAssetBeacon loanBeacon1,1)
                  , (_unLenderId lenderBeacon1,1)
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon1) 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon1) 1
                  , uncurry PV2.singleton (_unAsset loanAsset1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum offerDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey1]
      }

  -- Create the Offer UTxO.
  void $ transact lenderPersonalAddr2 [refScriptAddress] [lenderPayPrivKey2] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",1)
                  , (_unAssetBeacon loanBeacon2,1)
                  , (_unLenderId lenderBeacon2,1)
                  ]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateOffer lenderCred2
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon2) 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon2) 1
                  , uncurry PV2.singleton (_unAsset loanAsset2) 20
                  ]
              , outputDatum = OutputDatum $ toDatum offerDatum2
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey2]
      }

  startSlot <- currentSlot

  askRefs <- map fst <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @AskDatum 
      loanAddress1
      (negotiationBeaconCurrencySymbol,"Ask")

  offers <- 
    txOutRefsAndDatumsAtAddressWithBeacon @OfferDatum 
      loanAddress1
      (negotiationBeaconCurrencySymbol,"Offer")

  let sampleMints ds = flip concatMap offers $ 
        \(offerRef,Just offerDatum@OfferDatum{_lenderId}) ->
          let ActiveDatum{_loanId,_assetBeacon} = 
                createAcceptanceDatumFromOffer borrowerCred1 offerRef (slotToPosixTime startSlot) offerDatum
          in  [ TokenMint
                { mintTokens = 
                    [ ("Offer",-1)
                    , ("Ask",-1)
                    , (_unAssetBeacon _assetBeacon,-2)
                    , (_unLenderId _lenderId,-1)
                    ]
                , mintRedeemer = toRedeemer BurnNegotiationBeacons
                , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
                , mintReference = Just negotiationRef
                }
              , TokenMint
                  { mintTokens = 
                      [ ("Active",1)
                      , (_unBorrowerId borrowerBeacon1,1)
                      , (_unAssetBeacon _assetBeacon,1)
                      , (_unLoanId _loanId,2)
                      ]
                  , mintRedeemer = 
                      toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
                  , mintPolicy = toVersionedMintingPolicy activeBeaconScript
                  , mintReference = Just activeRef
                  }
              ]

      sampleOutputs ds = flip concatMap offers $ 
        \(offerRef,Just offerDatum@OfferDatum{_lenderId,_loanPrincipal}) ->
          let activeDatum@ActiveDatum{_loanId,_lenderAddress,_assetBeacon} = 
                createAcceptanceDatumFromOffer borrowerCred1 offerRef (slotToPosixTime startSlot) offerDatum
          in  [ Output
                  { outputAddress = loanAddress1
                  , outputValue = utxoValue 4_000_000 $ mconcat
                      [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                      , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                      , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon1) 1
                      , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                      , uncurry PV2.singleton (_unAsset collateral1) _loanPrincipal
                      ]
                  , outputDatum = OutputDatum $ toDatum activeDatum
                  , outputReferenceScript = toReferenceScript Nothing
                  }
              , Output
                  { outputAddress = toCardanoApiAddress _lenderAddress
                  , outputValue = utxoValue 4_000_000 $ mconcat
                      [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1 ]
                  , outputDatum = OutputDatum $ toDatum $ 
                      PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
                  , outputReferenceScript = toReferenceScript Nothing
                  }
              ]

  -- Try to accept the offer.
  void $ transact borrowerPersonalAddr1 [loanAddress1,refScriptAddress] [borrowerPayPrivKey1] $
    emptyTxParams
      { tokens = sampleMints offers
      , inputs = concat
          [ flip map offers $ \(offerRef,_) -> 
              Input
                { inputId = offerRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum (toRedeemer AcceptOffer)
                }
          , flip map askRefs $ \askRef ->
              Input 
                { inputId = askRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum (toRedeemer AcceptOffer)
                }
          ]
      , outputs = sampleOutputs offers
      , referenceInputs = [negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey1]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just startSlot
          , validityRangeUpperBound = Nothing
          }
      }

  activeUTxOs <-
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      loanAddress1
      (activeBeaconCurrencySymbol,"Active")

  let samplePayments [(_,Just ad1),(_,Just ad2)] =
        [ Output
            { outputAddress = loanAddress1
            , outputValue = utxoValue 4_000_000 $ mconcat
                [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon $ ad1._assetBeacon) 1
                , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId $ ad1._borrowerId) 1
                , PV2.singleton activeBeaconCurrencySymbol (_unLoanId $ ad2._loanId) 1
                , uncurry PV2.singleton (_unAsset collateral1) 6
                ]
            , outputDatum = OutputDatum $ toDatum $ createPostPaymentActiveDatum 5_000_000 ad1
            , outputReferenceScript = toReferenceScript Nothing
            }
        , Output
            { outputAddress = toCardanoApiAddress lenderAddr1
            , outputValue = utxoValue 5_000_000 mempty
            , outputDatum = 
                OutputDatum $ toDatum $ 
                  PaymentDatum (activeBeaconCurrencySymbol,_unLoanId $ ad1._loanId)
            , outputReferenceScript = toReferenceScript Nothing
            }
        , Output
            { outputAddress = loanAddress1
            , outputValue = utxoValue 4_000_000 $ mconcat
                [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon $ ad2._assetBeacon) 1
                , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId $ ad2._borrowerId) 1
                , PV2.singleton activeBeaconCurrencySymbol (_unLoanId $ ad1._loanId) 1
                , uncurry PV2.singleton (_unAsset collateral1) 6
                ]
            , outputDatum = OutputDatum $ toDatum $ createPostPaymentActiveDatum 5_000_000 ad2
            , outputReferenceScript = toReferenceScript Nothing
            }
        , Output
            { outputAddress = toCardanoApiAddress lenderAddr1
            , outputValue = utxoValue 5_000_000 mempty
            , outputDatum = 
                OutputDatum $ toDatum $ 
                  PaymentDatum (activeBeaconCurrencySymbol,_unLoanId $ ad2._loanId)
            , outputReferenceScript = toReferenceScript Nothing
            }
        ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr1 [loanAddress1,refScriptAddress] [borrowerPayPrivKey1] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey1]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-- | When making payments on multiple loans, swap the asset beacons in the collateral outputs.
-- The loans are for separate loan assets.
beaconFailure9 :: MonadEmulator m => m ()
beaconFailure9 = do
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

      lenderWallet2 = Mock.knownMockWallet 3
      lenderPersonalAddr2 = Mock.mockWalletAddress lenderWallet2
      lenderPayPrivKey2 = Mock.paymentPrivateKey lenderWallet2
      lenderPubKey2 = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash lenderWallet2
      lenderCred2 = PV2.PubKeyCredential lenderPubKey2
      lenderBeacon2 = genLenderId lenderCred2
      lenderAddr2 = 
        PV2.Address (PV2.ScriptCredential $ scriptHash proxyScript) (Just $ PV2.StakingHash lenderCred2)

      -- Loan Info
      loanAsset1 = Asset (testTokenSymbol,"TestToken10")
      loanAsset2 = Asset (testTokenSymbol,"TestToken11")
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      loanBeacon1 = genLoanAssetBeaconName loanAsset1
      loanBeacon2 = genLoanAssetBeaconName loanAsset2
      askDatum1 = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred1
        , _loanAsset = loanAsset1
        , _loanPrincipal = 10
        , _loanTerm = 3600
        , _collateral = [collateral1]
        }
      askDatum2 = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred1
        , _loanAsset = loanAsset2
        , _loanPrincipal = 20
        , _loanTerm = 3600
        , _collateral = [collateral1]
        }
      offerDatum1 = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred1
        , _lenderAddress = lenderAddr1
        , _loanAsset = loanAsset1
        , _loanPrincipal = 10
        , _epochDuration = Nothing
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _compoundingInterest = True
        , _minPayment = 0
        , _penalty = NoPenalty
        , _collateralization = [(collateral1,Fraction(1,1))]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }
      offerDatum2 = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred2
        , _lenderAddress = lenderAddr2
        , _loanAsset = loanAsset2
        , _loanPrincipal = 20
        , _epochDuration = Nothing
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _compoundingInterest = True
        , _minPayment = 0
        , _penalty = NoPenalty
        , _collateralization = [(collateral1,Fraction(1,1))]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef,paymentObserverRef,activeRef,loanRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet1 10_000_000 [("TestToken1",1000)]
  mintTestTokens lenderWallet1 10_000_000 [("TestToken10",1000),("TestToken11",1000)]
  mintTestTokens lenderWallet2 10_000_000 [("TestToken10",1000),("TestToken11",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr1 [refScriptAddress] [borrowerPayPrivKey1] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Ask",2)
                  , (_unAssetBeacon loanBeacon1,1)
                  , (_unAssetBeacon loanBeacon2,1)
                  ]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon1) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum askDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon2) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum askDatum2
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
                  [ ("Offer",1)
                  , (_unAssetBeacon loanBeacon1,1)
                  , (_unLenderId lenderBeacon1,1)
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon1) 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon1) 1
                  , uncurry PV2.singleton (_unAsset loanAsset1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum offerDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey1]
      }

  -- Create the Offer UTxO.
  void $ transact lenderPersonalAddr2 [refScriptAddress] [lenderPayPrivKey2] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",1)
                  , (_unAssetBeacon loanBeacon2,1)
                  , (_unLenderId lenderBeacon2,1)
                  ]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateOffer lenderCred2
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon2) 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon2) 1
                  , uncurry PV2.singleton (_unAsset loanAsset2) 20
                  ]
              , outputDatum = OutputDatum $ toDatum offerDatum2
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey2]
      }

  startSlot <- currentSlot

  askRefs <- map fst <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @AskDatum 
      loanAddress1
      (negotiationBeaconCurrencySymbol,"Ask")

  offers <- 
    txOutRefsAndDatumsAtAddressWithBeacon @OfferDatum 
      loanAddress1
      (negotiationBeaconCurrencySymbol,"Offer")

  let sampleMints ds = flip concatMap offers $ 
        \(offerRef,Just offerDatum@OfferDatum{_lenderId}) ->
          let ActiveDatum{_loanId,_assetBeacon} = 
                createAcceptanceDatumFromOffer borrowerCred1 offerRef (slotToPosixTime startSlot) offerDatum
          in  [ TokenMint
                { mintTokens = 
                    [ ("Offer",-1)
                    , ("Ask",-1)
                    , (_unAssetBeacon _assetBeacon,-2)
                    , (_unLenderId _lenderId,-1)
                    ]
                , mintRedeemer = toRedeemer BurnNegotiationBeacons
                , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
                , mintReference = Just negotiationRef
                }
              , TokenMint
                  { mintTokens = 
                      [ ("Active",1)
                      , (_unBorrowerId borrowerBeacon1,1)
                      , (_unAssetBeacon _assetBeacon,1)
                      , (_unLoanId _loanId,2)
                      ]
                  , mintRedeemer = 
                      toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
                  , mintPolicy = toVersionedMintingPolicy activeBeaconScript
                  , mintReference = Just activeRef
                  }
              ]

      sampleOutputs ds = flip concatMap offers $ 
        \(offerRef,Just offerDatum@OfferDatum{_lenderId,_loanPrincipal}) ->
          let activeDatum@ActiveDatum{_loanId,_lenderAddress,_assetBeacon} = 
                createAcceptanceDatumFromOffer borrowerCred1 offerRef (slotToPosixTime startSlot) offerDatum
          in  [ Output
                  { outputAddress = loanAddress1
                  , outputValue = utxoValue 4_000_000 $ mconcat
                      [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                      , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                      , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon1) 1
                      , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                      , uncurry PV2.singleton (_unAsset collateral1) _loanPrincipal
                      ]
                  , outputDatum = OutputDatum $ toDatum activeDatum
                  , outputReferenceScript = toReferenceScript Nothing
                  }
              , Output
                  { outputAddress = toCardanoApiAddress _lenderAddress
                  , outputValue = utxoValue 4_000_000 $ mconcat
                      [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1 ]
                  , outputDatum = OutputDatum $ toDatum $ 
                      PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
                  , outputReferenceScript = toReferenceScript Nothing
                  }
              ]

  -- Try to accept the offer.
  void $ transact borrowerPersonalAddr1 [loanAddress1,refScriptAddress] [borrowerPayPrivKey1] $
    emptyTxParams
      { tokens = sampleMints offers
      , inputs = concat
          [ flip map offers $ \(offerRef,_) -> 
              Input
                { inputId = offerRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum (toRedeemer AcceptOffer)
                }
          , flip map askRefs $ \askRef ->
              Input 
                { inputId = askRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum (toRedeemer AcceptOffer)
                }
          ]
      , outputs = sampleOutputs offers
      , referenceInputs = [negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey1]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just startSlot
          , validityRangeUpperBound = Nothing
          }
      }

  activeUTxOs <-
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      loanAddress1
      (activeBeaconCurrencySymbol,"Active")

  let samplePayments [(_,Just ad1),(_,Just ad2)] =
        [ Output
            { outputAddress = loanAddress1
            , outputValue = utxoValue 4_000_000 $ mconcat
                [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon $ ad2._assetBeacon) 1
                , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId $ ad1._borrowerId) 1
                , PV2.singleton activeBeaconCurrencySymbol (_unLoanId $ ad1._loanId) 1
                , uncurry PV2.singleton (_unAsset collateral1) 6
                ]
            , outputDatum = OutputDatum $ toDatum $ createPostPaymentActiveDatum 5_000_000 ad1
            , outputReferenceScript = toReferenceScript Nothing
            }
        , Output
            { outputAddress = toCardanoApiAddress lenderAddr1
            , outputValue = utxoValue 5_000_000 mempty
            , outputDatum = 
                OutputDatum $ toDatum $ 
                  PaymentDatum (activeBeaconCurrencySymbol,_unLoanId $ ad1._loanId)
            , outputReferenceScript = toReferenceScript Nothing
            }
        , Output
            { outputAddress = loanAddress1
            , outputValue = utxoValue 4_000_000 $ mconcat
                [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon $ ad1._assetBeacon) 1
                , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId $ ad2._borrowerId) 1
                , PV2.singleton activeBeaconCurrencySymbol (_unLoanId $ ad2._loanId) 1
                , uncurry PV2.singleton (_unAsset collateral1) 6
                ]
            , outputDatum = OutputDatum $ toDatum $ createPostPaymentActiveDatum 5_000_000 ad2
            , outputReferenceScript = toReferenceScript Nothing
            }
        , Output
            { outputAddress = toCardanoApiAddress lenderAddr1
            , outputValue = utxoValue 5_000_000 mempty
            , outputDatum = 
                OutputDatum $ toDatum $ 
                  PaymentDatum (activeBeaconCurrencySymbol,_unLoanId $ ad2._loanId)
            , outputReferenceScript = toReferenceScript Nothing
            }
        ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr1 [loanAddress1,refScriptAddress] [borrowerPayPrivKey1] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey1]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-- | Make a partial payment on a invalid Active UTxO (it does not have beacons). There is no
-- required output.
beaconFailure10 :: MonadEmulator m => m ()
beaconFailure10 = do
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
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
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
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

  askRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        ]

  offerRef <-
    txOutRefWithValue $ 
      utxoValue 4_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
        , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
        ]

  let activeDatum = 
        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      loanIdBeacon = genLoanId offerRef

  -- Accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = []
      , inputs = []
      , outputs =
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_001 mempty
              , outputDatum = OutputDatum $ toDatum activeDatum
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

  activeUTxORef <- txOutRefWithValue $ utxoValue 4_000_001 mempty

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs =
          [ Input
              { inputId = activeUTxORef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
              }
          ]
      , outputs = []
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-- | When making a single payment, withdraw all of the beacons.
beaconFailure11 :: MonadEmulator m => m ()
beaconFailure11 = do
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
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
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
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

  askRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        ]

  offerRef <-
    txOutRefWithValue $ 
      utxoValue 4_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
        , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
        ]

  let activeDatum = 
        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      loanIdBeacon = genLoanId offerRef

  -- Accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
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
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1 ]
              , outputDatum = 
                  OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon)
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
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 0
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 0
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 0
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 0
                  , uncurry PV2.singleton (_unAsset collateral1) 6
                  ]
              , outputDatum = OutputDatum $ toDatum $ createPostPaymentActiveDatum 5_000_000 ad
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 5_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-------------------------------------------------
-- Address Failures
-------------------------------------------------
-- | When making a single partial payment, the new collateral output is at the wrong address.
addressFailure1 :: MonadEmulator m => m ()
addressFailure1 = do
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

      wrongLoanAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash loanScript) 
                    (Just $ PV2.StakingHash lenderCred)
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
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
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
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

  askRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        ]

  offerRef <-
    txOutRefWithValue $ 
      utxoValue 4_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
        , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
        ]

  let activeDatum = 
        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      loanIdBeacon = genLoanId offerRef

  -- Accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
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
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1 ]
              , outputDatum = 
                  OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon)
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
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = wrongLoanAddress
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
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 5_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-- | When making a single payment, the payment went to the wrong address.
addressFailure2 :: MonadEmulator m => m ()
addressFailure2 = do
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
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
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
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

  askRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        ]

  offerRef <-
    txOutRefWithValue $ 
      utxoValue 4_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
        , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
        ]

  let activeDatum = 
        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      loanIdBeacon = genLoanId offerRef

  -- Accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
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
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1 ]
              , outputDatum = 
                  OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon)
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
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
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
              { outputAddress = loanAddress
              , outputValue = utxoValue 5_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-- | Make payments on multiple loans that come from different borrower addresses. The required
-- credentials approve the transaction.
addressFailure3 :: MonadEmulator m => m ()
addressFailure3 = do
  let -- Borrower1 Info
      borrowerWallet1 = Mock.knownMockWallet 1
      borrowerPersonalAddr1 = Mock.mockWalletAddress borrowerWallet1
      borrowerPayPrivKey1 = Mock.paymentPrivateKey borrowerWallet1
      borrowerPubKey1 = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash borrowerWallet1
      borrowerCred1 = PV2.PubKeyCredential borrowerPubKey1
      borrowerBeacon1 = genBorrowerId borrowerCred1
      loanAddress1 = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash loanScript) 
                    (Just $ PV2.StakingHash borrowerCred1)

     -- Borrower2 Info
      borrowerWallet2 = Mock.knownMockWallet 2
      borrowerPersonalAddr2 = Mock.mockWalletAddress borrowerWallet2
      borrowerPayPrivKey2 = Mock.paymentPrivateKey borrowerWallet2
      borrowerPubKey2 = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash borrowerWallet2
      borrowerCred2 = PV2.PubKeyCredential borrowerPubKey2
      borrowerBeacon2 = genBorrowerId borrowerCred2
      loanAddress2 = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash loanScript) 
                    (Just $ PV2.StakingHash borrowerCred2)

      -- Lender Info
      lenderWallet = Mock.knownMockWallet 3
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
      askDatum1 = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred1
        , _loanAsset = loanAsset
        , _loanPrincipal = 10_000_000
        , _loanTerm = 3600
        , _collateral = [collateral1]
        }
      askDatum2 = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred2
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet1 10_000_000 [("TestToken1",1000)]
  mintTestTokens borrowerWallet2 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr1 [refScriptAddress] [borrowerPayPrivKey1] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon,1)]
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
              , outputDatum = OutputDatum $ toDatum askDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [borrowerPubKey1]
      }

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr2 [refScriptAddress] [borrowerPayPrivKey2] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon,1)]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred2
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = loanAddress2
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum askDatum2
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [borrowerPubKey2]
      }

  -- Create the Offer UTxO.
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",2)
                  , (_unAssetBeacon loanBeacon,2)
                  , (_unLenderId lenderBeacon,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateOffer lenderCred
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
                  , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
                  ]
              , outputDatum = OutputDatum $ toDatum offerDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = loanAddress2
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

  askUTxOs1 <-
    txOutRefsAndDatumsAtAddressWithBeacon @AskDatum 
      loanAddress1
      (negotiationBeaconCurrencySymbol,"Ask")

  askUTxOs2 <-
    txOutRefsAndDatumsAtAddressWithBeacon @AskDatum 
      loanAddress2
      (negotiationBeaconCurrencySymbol,"Ask")

  offerUTxOs1 <- 
    txOutRefsAndDatumsAtAddressWithBeacon @OfferDatum
      loanAddress1
      (negotiationBeaconCurrencySymbol,"Offer")

  offerUTxOs2 <- 
    txOutRefsAndDatumsAtAddressWithBeacon @OfferDatum
      loanAddress2
      (negotiationBeaconCurrencySymbol,"Offer")

  let sampleBurns borrowerCred os as = concat $ (flip . flip zipWith) os as $
        \(offerRef,Just OfferDatum{_assetBeacon=oLoanBeacon,_lenderId}) 
         (_,Just AskDatum{_assetBeacon=aLoanBeacon,_borrowerId}) ->
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
                    , (_unBorrowerId _borrowerId,1)
                    , (_unAssetBeacon oLoanBeacon,1)
                    , (_unLoanId $ genLoanId offerRef,2)
                    ]
                , mintRedeemer = toRedeemer $ 
                    CreateActive negotiationBeaconCurrencySymbol
                , mintPolicy = toVersionedMintingPolicy activeBeaconScript
                , mintReference = Just activeRef
                }
            ]

      sampleOutputs start borrowerCred loanAddress os = flip concatMap os $ 
        \(offerRef,Just od@OfferDatum{_assetBeacon}) ->
          let activeDatum@ActiveDatum{_loanId,_lenderAddress,_borrowerId} = 
                createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime start) od
          in [ Output
                { outputAddress = loanAddress
                , outputValue = utxoValue 4_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
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

  startSlot1 <- currentSlot

  -- Accept the offer for borrower1.
  void $ transact borrowerPersonalAddr1 [loanAddress1,refScriptAddress] [borrowerPayPrivKey1] $
    emptyTxParams
      { tokens = sampleBurns borrowerCred1 offerUTxOs1 askUTxOs1
      , inputs = 
          concat $ (flip . flip zipWith) offerUTxOs1 askUTxOs1 $ \(offerRef,_) (askRef,_) ->
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
      , outputs = sampleOutputs startSlot1 borrowerCred1 loanAddress1 offerUTxOs1
      , referenceInputs = [negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey1]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just startSlot1
          , validityRangeUpperBound = Nothing
          }
      }

  startSlot2 <- currentSlot

  -- Accept the offer for borrower1.
  void $ transact borrowerPersonalAddr2 [loanAddress2,refScriptAddress] [borrowerPayPrivKey2] $
    emptyTxParams
      { tokens = sampleBurns borrowerCred2 offerUTxOs2 askUTxOs2
      , inputs = 
          concat $ (flip . flip zipWith) offerUTxOs2 askUTxOs2 $ \(offerRef,_) (askRef,_) ->
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
      , outputs = sampleOutputs startSlot2 borrowerCred2 loanAddress2 offerUTxOs2
      , referenceInputs = [negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey2]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just startSlot2
          , validityRangeUpperBound = Nothing
          }
      }

  activeUTxOs1 <-
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      loanAddress1
      (activeBeaconCurrencySymbol,"Active")

  activeUTxOs2 <-
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      loanAddress2
      (activeBeaconCurrencySymbol,"Active")

  let samplePayments loanAddress acs = flip concatMap acs $ 
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
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
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 5_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to rollover a loan.
  void $ transact 
    borrowerPersonalAddr1 
    [loanAddress1,loanAddress2,refScriptAddress] 
    [borrowerPayPrivKey1,borrowerPayPrivKey2]
    emptyTxParams
      { inputs = flip map (activeUTxOs1 <> activeUTxOs2) $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
            }
      , outputs = mconcat
          [ samplePayments loanAddress1 activeUTxOs1 
          , samplePayments loanAddress2 activeUTxOs2
          ]
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey1,borrowerPubKey2]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-- | When making payments on multiple loans that come from different borrowers, mix up the lender
-- payments (ie, send them to the wrong lender addresses).
addressFailure4 :: MonadEmulator m => m ()
addressFailure4 = do
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

      lenderWallet2 = Mock.knownMockWallet 3
      lenderPersonalAddr2 = Mock.mockWalletAddress lenderWallet2
      lenderPayPrivKey2 = Mock.paymentPrivateKey lenderWallet2
      lenderPubKey2 = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash lenderWallet2
      lenderCred2 = PV2.PubKeyCredential lenderPubKey2
      lenderBeacon2 = genLenderId lenderCred2
      lenderAddr2 = 
        PV2.Address (PV2.ScriptCredential $ scriptHash proxyScript) (Just $ PV2.StakingHash lenderCred2)

      -- Loan Info
      loanAsset1 = Asset (testTokenSymbol,"TestToken10")
      loanAsset2 = Asset (testTokenSymbol,"TestToken11")
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      loanBeacon1 = genLoanAssetBeaconName loanAsset1
      loanBeacon2 = genLoanAssetBeaconName loanAsset2
      askDatum1 = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred1
        , _loanAsset = loanAsset1
        , _loanPrincipal = 10
        , _loanTerm = 3600
        , _collateral = [collateral1]
        }
      askDatum2 = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred1
        , _loanAsset = loanAsset2
        , _loanPrincipal = 10
        , _loanTerm = 3600
        , _collateral = [collateral1]
        }
      offerDatum1 = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred1
        , _lenderAddress = lenderAddr1
        , _loanAsset = loanAsset1
        , _loanPrincipal = 10
        , _epochDuration = Nothing
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _compoundingInterest = True
        , _minPayment = 0
        , _penalty = NoPenalty
        , _collateralization = [(collateral1,Fraction(1,1))]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }
      offerDatum2 = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred2
        , _lenderAddress = lenderAddr2
        , _loanAsset = loanAsset2
        , _loanPrincipal = 10
        , _epochDuration = Nothing
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _compoundingInterest = True
        , _minPayment = 0
        , _penalty = NoPenalty
        , _collateralization = [(collateral1,Fraction(1,1))]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{paymentObserverRef,negotiationRef,activeRef,loanRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet1 10_000_000 [("TestToken1",1000)]
  mintTestTokens lenderWallet1 10_000_000 [("TestToken10",1000),("TestToken11",1000)]
  mintTestTokens lenderWallet2 10_000_000 [("TestToken10",1000),("TestToken11",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr1 [refScriptAddress] [borrowerPayPrivKey1] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Ask",2)
                  , (_unAssetBeacon loanBeacon1,1)
                  , (_unAssetBeacon loanBeacon2,1)
                  ]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon1) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum askDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon2) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum askDatum2
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
                  [ ("Offer",1)
                  , (_unAssetBeacon loanBeacon1,1)
                  , (_unLenderId lenderBeacon1,1)
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon1) 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon1) 1
                  , uncurry PV2.singleton (_unAsset loanAsset1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum offerDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey1]
      }

  -- Create the Offer UTxO.
  void $ transact lenderPersonalAddr2 [refScriptAddress] [lenderPayPrivKey2] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",1)
                  , (_unAssetBeacon loanBeacon2,1)
                  , (_unLenderId lenderBeacon2,1)
                  ]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateOffer lenderCred2
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs =
          [ Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon2) 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon2) 1
                  , uncurry PV2.singleton (_unAsset loanAsset2) 10
                  ]
              , outputDatum = OutputDatum $ toDatum offerDatum2
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey2]
      }

  startSlot <- currentSlot

  askRefs <- map fst <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @AskDatum 
      loanAddress1
      (negotiationBeaconCurrencySymbol,"Ask")

  offers <- 
    txOutRefsAndDatumsAtAddressWithBeacon @OfferDatum 
      loanAddress1
      (negotiationBeaconCurrencySymbol,"Offer")

  let sampleMints ds = flip concatMap offers $ 
        \(offerRef,Just offerDatum@OfferDatum{_lenderId}) ->
          let ActiveDatum{_loanId,_assetBeacon} = 
                createAcceptanceDatumFromOffer borrowerCred1 offerRef (slotToPosixTime startSlot) offerDatum
          in  [ TokenMint
                { mintTokens = 
                    [ ("Offer",-1)
                    , ("Ask",-1)
                    , (_unAssetBeacon _assetBeacon,-2)
                    , (_unLenderId _lenderId,-1)
                    ]
                , mintRedeemer = toRedeemer BurnNegotiationBeacons
                , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
                , mintReference = Just negotiationRef
                }
              , TokenMint
                  { mintTokens = 
                      [ ("Active",1)
                      , (_unBorrowerId borrowerBeacon1,1)
                      , (_unAssetBeacon _assetBeacon,1)
                      , (_unLoanId _loanId,2)
                      ]
                  , mintRedeemer = 
                      toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
                  , mintPolicy = toVersionedMintingPolicy activeBeaconScript
                  , mintReference = Just activeRef
                  }
              ]

      sampleOutputs ds = flip concatMap offers $ 
        \(offerRef,Just offerDatum@OfferDatum{_lenderId,_loanPrincipal}) ->
          let activeDatum@ActiveDatum{_loanId,_lenderAddress,_assetBeacon} = 
                createAcceptanceDatumFromOffer borrowerCred1 offerRef (slotToPosixTime startSlot) offerDatum
          in  [ Output
                  { outputAddress = loanAddress1
                  , outputValue = utxoValue 4_000_000 $ mconcat
                      [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                      , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                      , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon1) 1
                      , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                      , uncurry PV2.singleton (_unAsset collateral1) _loanPrincipal
                      ]
                  , outputDatum = OutputDatum $ toDatum activeDatum
                  , outputReferenceScript = toReferenceScript Nothing
                  }
              , Output
                  { outputAddress = toCardanoApiAddress _lenderAddress
                  , outputValue = utxoValue 4_000_000 $ mconcat
                      [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1 ]
                  , outputDatum = OutputDatum $ toDatum $ 
                      PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
                  , outputReferenceScript = toReferenceScript Nothing
                  }
              ]

  -- Try to accept the offer.
  void $ transact borrowerPersonalAddr1 [loanAddress1,refScriptAddress] [borrowerPayPrivKey1] $
    emptyTxParams
      { tokens = sampleMints offers
      , inputs = concat
          [ flip map offers $ \(offerRef,_) -> 
              Input
                { inputId = offerRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum (toRedeemer AcceptOffer)
                }
          , flip map askRefs $ \askRef ->
              Input 
                { inputId = askRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum (toRedeemer AcceptOffer)
                }
          ]
      , outputs = sampleOutputs offers
      , referenceInputs = [negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey1]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just startSlot
          , validityRangeUpperBound = Nothing
          }
      }

  activeUTxOs <-
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      loanAddress1
      (activeBeaconCurrencySymbol,"Active")

  let samplePayments [(_,Just ad1),(_,Just ad2)] =
        [ Output
            { outputAddress = loanAddress1
            , outputValue = utxoValue 4_000_000 $ mconcat
                [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon $ ad1._assetBeacon) 1
                , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId $ ad1._borrowerId) 1
                , PV2.singleton activeBeaconCurrencySymbol (_unLoanId $ ad1._loanId) 1
                , uncurry PV2.singleton (_unAsset collateral1) 6
                ]
            , outputDatum = OutputDatum $ toDatum $ createPostPaymentActiveDatum 5 ad1
            , outputReferenceScript = toReferenceScript Nothing
            }
        , Output
            { outputAddress = toCardanoApiAddress lenderAddr2
            , outputValue = utxoValue 2_000_000 $ mconcat
                [ uncurry PV2.singleton (_unAsset loanAsset1) 5 ]
            , outputDatum = 
                OutputDatum $ toDatum $ 
                  PaymentDatum (activeBeaconCurrencySymbol,_unLoanId $ ad1._loanId)
            , outputReferenceScript = toReferenceScript Nothing
            }
        , Output
            { outputAddress = loanAddress1
            , outputValue = utxoValue 4_000_000 $ mconcat
                [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon $ ad2._assetBeacon) 1
                , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId $ ad2._borrowerId) 1
                , PV2.singleton activeBeaconCurrencySymbol (_unLoanId $ ad2._loanId) 1
                , uncurry PV2.singleton (_unAsset collateral1) 6
                ]
            , outputDatum = OutputDatum $ toDatum $ createPostPaymentActiveDatum 5 ad2
            , outputReferenceScript = toReferenceScript Nothing
            }
        , Output
            { outputAddress = toCardanoApiAddress lenderAddr1
            , outputValue = utxoValue 2_000_000 $ mconcat
                [ uncurry PV2.singleton (_unAsset loanAsset2) 5 ]
            , outputDatum = 
                OutputDatum $ toDatum $ 
                  PaymentDatum (activeBeaconCurrencySymbol,_unLoanId $ ad2._loanId)
            , outputReferenceScript = toReferenceScript Nothing
            }
        ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr1 [loanAddress1,refScriptAddress] [borrowerPayPrivKey1] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey1]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-------------------------------------------------
-- Approval Failures
-------------------------------------------------
-- | The borrower did not approve the payment.
approvalFailure1 :: MonadEmulator m => m ()
approvalFailure1 = do
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
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
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
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

  askRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        ]

  offerRef <-
    txOutRefWithValue $ 
      utxoValue 4_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
        , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
        ]

  let activeDatum = 
        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      loanIdBeacon = genLoanId offerRef

  -- Accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
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
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1 ]
              , outputDatum = 
                  OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon)
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
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
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
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 5_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact lenderPersonalAddr [loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-------------------------------------------------
-- Time Failures
-------------------------------------------------
-- | The loan is expired.
timeFailure1 :: MonadEmulator m => m ()
timeFailure1 = do
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
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
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
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

  askRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        ]

  offerRef <-
    txOutRefWithValue $ 
      utxoValue 4_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
        , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
        ]

  let activeDatum = 
        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      loanIdBeacon = genLoanId offerRef

  -- Accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
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
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1 ]
              , outputDatum = 
                  OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon)
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
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
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
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 5_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  currentSlot >>= awaitTime . (+3600) . slotToPosixTime
  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-- | Invalid hereafter not specified.
timeFailure2 :: MonadEmulator m => m ()
timeFailure2 = do
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
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
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
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

  askRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        ]

  offerRef <-
    txOutRefWithValue $ 
      utxoValue 4_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
        , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
        ]

  let activeDatum = 
        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      loanIdBeacon = genLoanId offerRef

  -- Accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
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
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1 ]
              , outputDatum = 
                  OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon)
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
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
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
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 5_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Nothing
          }
      }

-- | An interest application is required. There is no penalty but interest is compounding and
-- > 0.
timeFailure3 :: MonadEmulator m => m ()
timeFailure3 = do
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
        , _epochDuration = Just 3600
        , _loanTerm = 10000
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
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
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
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

  askRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        ]

  offerRef <-
    txOutRefWithValue $ 
      utxoValue 4_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
        , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
        ]

  let activeDatum = 
        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      loanIdBeacon = genLoanId offerRef

  -- Accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
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
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1 ]
              , outputDatum = 
                  OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon)
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
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
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
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 5_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  currentSlot >>= awaitTime . (+3600) . slotToPosixTime
  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-- | An interest application is required. The interest is non-compounding but there is a penalty.
timeFailure4 :: MonadEmulator m => m ()
timeFailure4 = do
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
        , _epochDuration = Just 3600
        , _loanTerm = 10000
        , _loanInterest = Fraction (1,10)
        , _compoundingInterest = False
        , _minPayment = 1_000_000
        , _penalty = FixedFee 1_000_000
        , _collateralization = [(collateral1,Fraction(1,1_000_000))]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
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
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
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

  askRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        ]

  offerRef <-
    txOutRefWithValue $ 
      utxoValue 4_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
        , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
        ]

  let activeDatum = 
        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      loanIdBeacon = genLoanId offerRef

  -- Accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
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
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1 ]
              , outputDatum = 
                  OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon)
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
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
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
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 5_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  currentSlot >>= awaitTime . (+3600) . slotToPosixTime
  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-------------------------------------------------
-- Output Order Failures
-------------------------------------------------
-- | When making payments on multiple loans, the first collateral output is invalid.
orderFailure1 :: MonadEmulator m => m ()
orderFailure1 = do
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
      lenderWallet1 = Mock.knownMockWallet 2
      lenderPersonalAddr1 = Mock.mockWalletAddress lenderWallet1
      lenderPayPrivKey1 = Mock.paymentPrivateKey lenderWallet1
      lenderPubKey1 = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash lenderWallet1
      lenderCred1 = PV2.PubKeyCredential lenderPubKey1
      lenderBeacon1 = genLenderId lenderCred1
      lenderAddr1 = 
        PV2.Address (PV2.ScriptCredential $ scriptHash proxyScript) 
                    (Just $ PV2.StakingHash lenderCred1)

      -- Lender Info
      lenderWallet2 = Mock.knownMockWallet 3
      lenderPersonalAddr2 = Mock.mockWalletAddress lenderWallet2
      lenderPayPrivKey2 = Mock.paymentPrivateKey lenderWallet2
      lenderPubKey2 = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash lenderWallet2
      lenderCred2 = PV2.PubKeyCredential lenderPubKey2
      lenderBeacon2 = genLenderId lenderCred2
      lenderAddr2 = 
        PV2.Address (PV2.ScriptCredential $ scriptHash proxyScript) 
                    (Just $ PV2.StakingHash lenderCred2)

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
      offerDatum1 = unsafeCreateOfferDatum $ NewOfferInfo
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
      offerDatum2 = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred2
        , _lenderAddress = lenderAddr2
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",2),(_unAssetBeacon loanBeacon,2)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum askDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = loanAddress
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
      , extraKeyWitnesses = [borrowerPubKey]
      }

  -- Create the Offer UTxO.
  void $ transact lenderPersonalAddr1 [refScriptAddress] [lenderPayPrivKey1] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLenderId lenderBeacon1,1)
                  ]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateOffer lenderCred1
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon1) 1
                  , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
                  ]
              , outputDatum = OutputDatum $ toDatum offerDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey1]
      }

  -- Create the Offer UTxO.
  void $ transact lenderPersonalAddr2 [refScriptAddress] [lenderPayPrivKey2] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLenderId lenderBeacon2,1)
                  ]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateOffer lenderCred2
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon2) 1
                  , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
                  ]
              , outputDatum = OutputDatum $ toDatum offerDatum2
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey2]
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
                { outputAddress = toCardanoApiAddress _lenderAddress
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

  activeUTxOs <-
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      loanAddress 
      (activeBeaconCurrencySymbol,"Active")

  let samplePayments acs = flip concatMap (zip acs [1::Int ..]) $ 
        \((_,Just ad@ActiveDatum{..}),i) ->
          if even i then
            [ Output
                { outputAddress = borrowerPersonalAddr
                , outputValue = utxoValue 5_000_000 mempty
                , outputDatum = NoOutputDatum
                , outputReferenceScript = toReferenceScript Nothing
                }
            , Output
                { outputAddress = toCardanoApiAddress _lenderAddress
                , outputValue = utxoValue 5_000_000 mempty
                , outputDatum = 
                    OutputDatum $ toDatum $ 
                      PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
                , outputReferenceScript = toReferenceScript Nothing
                }
            , Output
                { outputAddress = borrowerPersonalAddr
                , outputValue = utxoValue 5_000_000 mempty
                , outputDatum = NoOutputDatum
                , outputReferenceScript = toReferenceScript Nothing
                }
            , Output
                { outputAddress = loanAddress
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
                { outputAddress = borrowerPersonalAddr
                , outputValue = utxoValue 5_000_000 mempty
                , outputDatum = NoOutputDatum
                , outputReferenceScript = toReferenceScript Nothing
                }
            ]
          else reverse
            [ Output
                { outputAddress = borrowerPersonalAddr
                , outputValue = utxoValue 5_000_000 mempty
                , outputDatum = NoOutputDatum
                , outputReferenceScript = toReferenceScript Nothing
                }
            , Output
                { outputAddress = toCardanoApiAddress _lenderAddress
                , outputValue = utxoValue 5_000_000 mempty
                , outputDatum = 
                    OutputDatum $ toDatum $ 
                      PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
                , outputReferenceScript = toReferenceScript Nothing
                }
            , Output
                { outputAddress = borrowerPersonalAddr
                , outputValue = utxoValue 5_000_000 mempty
                , outputDatum = NoOutputDatum
                , outputReferenceScript = toReferenceScript Nothing
                }
            , Output
                { outputAddress = loanAddress
                , outputValue = utxoValue 4_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                    ]
                , outputDatum = OutputDatum $ toDatum $ createPostPaymentActiveDatum 5_000_000 ad
                , outputReferenceScript = toReferenceScript Nothing
                }
            , Output
                { outputAddress = borrowerPersonalAddr
                , outputValue = utxoValue 5_000_000 mempty
                , outputDatum = NoOutputDatum
                , outputReferenceScript = toReferenceScript Nothing
                }
            ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-- | When making payments on multiple loans, the second collateral output is invalid.
orderFailure2 :: MonadEmulator m => m ()
orderFailure2 = do
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
      lenderWallet1 = Mock.knownMockWallet 2
      lenderPersonalAddr1 = Mock.mockWalletAddress lenderWallet1
      lenderPayPrivKey1 = Mock.paymentPrivateKey lenderWallet1
      lenderPubKey1 = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash lenderWallet1
      lenderCred1 = PV2.PubKeyCredential lenderPubKey1
      lenderBeacon1 = genLenderId lenderCred1
      lenderAddr1 = 
        PV2.Address (PV2.ScriptCredential $ scriptHash proxyScript) 
                    (Just $ PV2.StakingHash lenderCred1)

      -- Lender Info
      lenderWallet2 = Mock.knownMockWallet 3
      lenderPersonalAddr2 = Mock.mockWalletAddress lenderWallet2
      lenderPayPrivKey2 = Mock.paymentPrivateKey lenderWallet2
      lenderPubKey2 = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash lenderWallet2
      lenderCred2 = PV2.PubKeyCredential lenderPubKey2
      lenderBeacon2 = genLenderId lenderCred2
      lenderAddr2 = 
        PV2.Address (PV2.ScriptCredential $ scriptHash proxyScript) 
                    (Just $ PV2.StakingHash lenderCred2)

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
      offerDatum1 = unsafeCreateOfferDatum $ NewOfferInfo
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
      offerDatum2 = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred2
        , _lenderAddress = lenderAddr2
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",2),(_unAssetBeacon loanBeacon,2)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum askDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = loanAddress
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
      , extraKeyWitnesses = [borrowerPubKey]
      }

  -- Create the Offer UTxO.
  void $ transact lenderPersonalAddr1 [refScriptAddress] [lenderPayPrivKey1] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLenderId lenderBeacon1,1)
                  ]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateOffer lenderCred1
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon1) 1
                  , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
                  ]
              , outputDatum = OutputDatum $ toDatum offerDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey1]
      }

  -- Create the Offer UTxO.
  void $ transact lenderPersonalAddr2 [refScriptAddress] [lenderPayPrivKey2] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLenderId lenderBeacon2,1)
                  ]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateOffer lenderCred2
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon2) 1
                  , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
                  ]
              , outputDatum = OutputDatum $ toDatum offerDatum2
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey2]
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
                { outputAddress = toCardanoApiAddress _lenderAddress
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

  activeUTxOs <-
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      loanAddress 
      (activeBeaconCurrencySymbol,"Active")

  let samplePayments acs = flip concatMap (zip acs [1::Int ..]) $ 
        \((_,Just ad@ActiveDatum{..}),i) ->
          if even i then
            [ Output
                { outputAddress = borrowerPersonalAddr
                , outputValue = utxoValue 5_000_000 mempty
                , outputDatum = NoOutputDatum
                , outputReferenceScript = toReferenceScript Nothing
                }
            , Output
                { outputAddress = toCardanoApiAddress _lenderAddress
                , outputValue = utxoValue 5_000_000 mempty
                , outputDatum = 
                    OutputDatum $ toDatum $ 
                      PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
                , outputReferenceScript = toReferenceScript Nothing
                }
            , Output
                { outputAddress = borrowerPersonalAddr
                , outputValue = utxoValue 5_000_000 mempty
                , outputDatum = NoOutputDatum
                , outputReferenceScript = toReferenceScript Nothing
                }
            , Output
                { outputAddress = loanAddress
                , outputValue = utxoValue 4_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                    ]
                , outputDatum = OutputDatum $ toDatum $ createPostPaymentActiveDatum 5_000_000 ad
                , outputReferenceScript = toReferenceScript Nothing
                }
            , Output
                { outputAddress = borrowerPersonalAddr
                , outputValue = utxoValue 5_000_000 mempty
                , outputDatum = NoOutputDatum
                , outputReferenceScript = toReferenceScript Nothing
                }
            ]
          else reverse
            [ Output
                { outputAddress = borrowerPersonalAddr
                , outputValue = utxoValue 5_000_000 mempty
                , outputDatum = NoOutputDatum
                , outputReferenceScript = toReferenceScript Nothing
                }
            , Output
                { outputAddress = toCardanoApiAddress _lenderAddress
                , outputValue = utxoValue 5_000_000 mempty
                , outputDatum = 
                    OutputDatum $ toDatum $ 
                      PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
                , outputReferenceScript = toReferenceScript Nothing
                }
            , Output
                { outputAddress = borrowerPersonalAddr
                , outputValue = utxoValue 5_000_000 mempty
                , outputDatum = NoOutputDatum
                , outputReferenceScript = toReferenceScript Nothing
                }
            , Output
                { outputAddress = loanAddress
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
                { outputAddress = borrowerPersonalAddr
                , outputValue = utxoValue 5_000_000 mempty
                , outputDatum = NoOutputDatum
                , outputReferenceScript = toReferenceScript Nothing
                }
            ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-- | When making payments on multiple loans, the first payment output is invalid.
orderFailure3 :: MonadEmulator m => m ()
orderFailure3 = do
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
      lenderWallet1 = Mock.knownMockWallet 2
      lenderPersonalAddr1 = Mock.mockWalletAddress lenderWallet1
      lenderPayPrivKey1 = Mock.paymentPrivateKey lenderWallet1
      lenderPubKey1 = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash lenderWallet1
      lenderCred1 = PV2.PubKeyCredential lenderPubKey1
      lenderBeacon1 = genLenderId lenderCred1
      lenderAddr1 = 
        PV2.Address (PV2.ScriptCredential $ scriptHash proxyScript) 
                    (Just $ PV2.StakingHash lenderCred1)

      -- Lender Info
      lenderWallet2 = Mock.knownMockWallet 3
      lenderPersonalAddr2 = Mock.mockWalletAddress lenderWallet2
      lenderPayPrivKey2 = Mock.paymentPrivateKey lenderWallet2
      lenderPubKey2 = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash lenderWallet2
      lenderCred2 = PV2.PubKeyCredential lenderPubKey2
      lenderBeacon2 = genLenderId lenderCred2
      lenderAddr2 = 
        PV2.Address (PV2.ScriptCredential $ scriptHash proxyScript) 
                    (Just $ PV2.StakingHash lenderCred2)

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
      offerDatum1 = unsafeCreateOfferDatum $ NewOfferInfo
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
      offerDatum2 = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred2
        , _lenderAddress = lenderAddr2
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",2),(_unAssetBeacon loanBeacon,2)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum askDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = loanAddress
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
      , extraKeyWitnesses = [borrowerPubKey]
      }

  -- Create the Offer UTxO.
  void $ transact lenderPersonalAddr1 [refScriptAddress] [lenderPayPrivKey1] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLenderId lenderBeacon1,1)
                  ]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateOffer lenderCred1
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon1) 1
                  , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
                  ]
              , outputDatum = OutputDatum $ toDatum offerDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey1]
      }

  -- Create the Offer UTxO.
  void $ transact lenderPersonalAddr2 [refScriptAddress] [lenderPayPrivKey2] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLenderId lenderBeacon2,1)
                  ]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateOffer lenderCred2
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon2) 1
                  , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
                  ]
              , outputDatum = OutputDatum $ toDatum offerDatum2
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey2]
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
                { outputAddress = toCardanoApiAddress _lenderAddress
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

  activeUTxOs <-
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      loanAddress 
      (activeBeaconCurrencySymbol,"Active")

  let samplePayments acs = flip concatMap (zip acs [1::Int ..]) $ 
        \((_,Just ad@ActiveDatum{..}),i) ->
          if even i then
            [ Output
                { outputAddress = borrowerPersonalAddr
                , outputValue = utxoValue 5_000_000 mempty
                , outputDatum = NoOutputDatum
                , outputReferenceScript = toReferenceScript Nothing
                }
            , Output
                { outputAddress = toCardanoApiAddress _lenderAddress
                , outputValue = utxoValue 5_000_000 mempty
                , outputDatum = 
                    OutputDatum $ toDatum $ 
                      PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
                , outputReferenceScript = toReferenceScript Nothing
                }
            , Output
                { outputAddress = borrowerPersonalAddr
                , outputValue = utxoValue 5_000_000 mempty
                , outputDatum = NoOutputDatum
                , outputReferenceScript = toReferenceScript Nothing
                }
            , Output
                { outputAddress = loanAddress
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
                { outputAddress = borrowerPersonalAddr
                , outputValue = utxoValue 5_000_000 mempty
                , outputDatum = NoOutputDatum
                , outputReferenceScript = toReferenceScript Nothing
                }
            ]
          else reverse
            [ Output
                { outputAddress = borrowerPersonalAddr
                , outputValue = utxoValue 5_000_000 mempty
                , outputDatum = NoOutputDatum
                , outputReferenceScript = toReferenceScript Nothing
                }
            , Output
                { outputAddress = toCardanoApiAddress _lenderAddress
                , outputValue = utxoValue 3_000_000 mempty
                , outputDatum = 
                    OutputDatum $ toDatum $ 
                      PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
                , outputReferenceScript = toReferenceScript Nothing
                }
            , Output
                { outputAddress = borrowerPersonalAddr
                , outputValue = utxoValue 5_000_000 mempty
                , outputDatum = NoOutputDatum
                , outputReferenceScript = toReferenceScript Nothing
                }
            , Output
                { outputAddress = loanAddress
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
                { outputAddress = borrowerPersonalAddr
                , outputValue = utxoValue 5_000_000 mempty
                , outputDatum = NoOutputDatum
                , outputReferenceScript = toReferenceScript Nothing
                }
            ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-- | When making payments on multiple loans, the second payment output is invalid.
orderFailure4 :: MonadEmulator m => m ()
orderFailure4 = do
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
      lenderWallet1 = Mock.knownMockWallet 2
      lenderPersonalAddr1 = Mock.mockWalletAddress lenderWallet1
      lenderPayPrivKey1 = Mock.paymentPrivateKey lenderWallet1
      lenderPubKey1 = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash lenderWallet1
      lenderCred1 = PV2.PubKeyCredential lenderPubKey1
      lenderBeacon1 = genLenderId lenderCred1
      lenderAddr1 = 
        PV2.Address (PV2.ScriptCredential $ scriptHash proxyScript) 
                    (Just $ PV2.StakingHash lenderCred1)

      -- Lender Info
      lenderWallet2 = Mock.knownMockWallet 3
      lenderPersonalAddr2 = Mock.mockWalletAddress lenderWallet2
      lenderPayPrivKey2 = Mock.paymentPrivateKey lenderWallet2
      lenderPubKey2 = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash lenderWallet2
      lenderCred2 = PV2.PubKeyCredential lenderPubKey2
      lenderBeacon2 = genLenderId lenderCred2
      lenderAddr2 = 
        PV2.Address (PV2.ScriptCredential $ scriptHash proxyScript) 
                    (Just $ PV2.StakingHash lenderCred2)

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
      offerDatum1 = unsafeCreateOfferDatum $ NewOfferInfo
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
      offerDatum2 = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred2
        , _lenderAddress = lenderAddr2
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",2),(_unAssetBeacon loanBeacon,2)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum askDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = loanAddress
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
      , extraKeyWitnesses = [borrowerPubKey]
      }

  -- Create the Offer UTxO.
  void $ transact lenderPersonalAddr1 [refScriptAddress] [lenderPayPrivKey1] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLenderId lenderBeacon1,1)
                  ]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateOffer lenderCred1
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon1) 1
                  , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
                  ]
              , outputDatum = OutputDatum $ toDatum offerDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey1]
      }

  -- Create the Offer UTxO.
  void $ transact lenderPersonalAddr2 [refScriptAddress] [lenderPayPrivKey2] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLenderId lenderBeacon2,1)
                  ]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateOffer lenderCred2
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon2) 1
                  , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
                  ]
              , outputDatum = OutputDatum $ toDatum offerDatum2
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey2]
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
                { outputAddress = toCardanoApiAddress _lenderAddress
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

  activeUTxOs <-
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      loanAddress 
      (activeBeaconCurrencySymbol,"Active")

  let samplePayments acs = flip concatMap (zip acs [1::Int ..]) $ 
        \((_,Just ad@ActiveDatum{..}),i) ->
          if even i then
            [ Output
                { outputAddress = borrowerPersonalAddr
                , outputValue = utxoValue 5_000_000 mempty
                , outputDatum = NoOutputDatum
                , outputReferenceScript = toReferenceScript Nothing
                }
            , Output
                { outputAddress = toCardanoApiAddress _lenderAddress
                , outputValue = utxoValue 4_000_000 mempty
                , outputDatum = 
                    OutputDatum $ toDatum $ 
                      PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
                , outputReferenceScript = toReferenceScript Nothing
                }
            , Output
                { outputAddress = borrowerPersonalAddr
                , outputValue = utxoValue 5_000_000 mempty
                , outputDatum = NoOutputDatum
                , outputReferenceScript = toReferenceScript Nothing
                }
            , Output
                { outputAddress = loanAddress
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
                { outputAddress = borrowerPersonalAddr
                , outputValue = utxoValue 5_000_000 mempty
                , outputDatum = NoOutputDatum
                , outputReferenceScript = toReferenceScript Nothing
                }
            ]
          else reverse
            [ Output
                { outputAddress = borrowerPersonalAddr
                , outputValue = utxoValue 5_000_000 mempty
                , outputDatum = NoOutputDatum
                , outputReferenceScript = toReferenceScript Nothing
                }
            , Output
                { outputAddress = toCardanoApiAddress _lenderAddress
                , outputValue = utxoValue 5_000_000 mempty
                , outputDatum = 
                    OutputDatum $ toDatum $ 
                      PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
                , outputReferenceScript = toReferenceScript Nothing
                }
            , Output
                { outputAddress = borrowerPersonalAddr
                , outputValue = utxoValue 5_000_000 mempty
                , outputDatum = NoOutputDatum
                , outputReferenceScript = toReferenceScript Nothing
                }
            , Output
                { outputAddress = loanAddress
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
                { outputAddress = borrowerPersonalAddr
                , outputValue = utxoValue 5_000_000 mempty
                , outputDatum = NoOutputDatum
                , outputReferenceScript = toReferenceScript Nothing
                }
            ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-- | When making payments on multiple loans, the lender outputs are out of order relative
-- to the inputs.
orderFailure5 :: MonadEmulator m => m ()
orderFailure5 = do
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",2),(_unAssetBeacon loanBeacon,2)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum askDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = loanAddress
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
      , extraKeyWitnesses = [borrowerPubKey]
      }

  -- Create the Offer UTxO.
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",2)
                  , (_unAssetBeacon loanBeacon,2)
                  , (_unLenderId lenderBeacon,2)
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
          , Output
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

  askRefs <- map fst <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @AskDatum 
      loanAddress 
      (negotiationBeaconCurrencySymbol,"Ask")

  offerRefs <- map fst <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @OfferDatum 
      loanAddress 
      (negotiationBeaconCurrencySymbol,"Offer")

  let [activeDatum1,activeDatum2] = flip map offerRefs $ \offerRef ->
        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      [loanIdBeacon1,loanIdBeacon2] = map genLoanId offerRefs

  -- Accept the offers.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-2)
                  , ("Ask",-2)
                  , (_unAssetBeacon loanBeacon,-4)
                  , (_unLenderId lenderBeacon,-2)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",2)
                  , (_unBorrowerId borrowerBeacon,2)
                  , (_unAssetBeacon loanBeacon,2)
                  , (_unLoanId loanIdBeacon1,2)
                  , (_unLoanId loanIdBeacon2,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
              , mintPolicy = toVersionedMintingPolicy activeBeaconScript
              , mintReference = Just activeRef
              }
          ]
      , inputs = concat
          [ flip map offerRefs $ \offerRef -> 
              Input
                { inputId = offerRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum (toRedeemer AcceptOffer)
                }
          , flip map askRefs $ \askRef ->
              Input 
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
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon1) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon1) 1 ]
              , outputDatum = OutputDatum $ toDatum $ 
                  PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon1)
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon2) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum2
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon2) 1 ]
              , outputDatum = OutputDatum $ toDatum $ 
                  PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon2)
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

  let extraOutput = 
        [ Output
            { outputAddress = borrowerPersonalAddr
            , outputValue = utxoValue 4_000_000 mempty
            , outputDatum = NoOutputDatum
            , outputReferenceScript = toReferenceScript Nothing
            }
        ]

  let sampleCollateral acs = flip map acs $ 
        \(_,Just ad@ActiveDatum{..}) ->
          Output
            { outputAddress = loanAddress
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

  let samplePayments acs = flip map acs $ 
        \(_,Just ad@ActiveDatum{..}) ->
          Output
            { outputAddress = toCardanoApiAddress lenderAddr
            , outputValue = utxoValue 5_000_000 mempty
            , outputDatum = 
                OutputDatum $ toDatum $ 
                  PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
            , outputReferenceScript = toReferenceScript Nothing
            }

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
            }
      , outputs = mconcat
          [ extraOutput
          , sampleCollateral activeUTxOs
          , extraOutput
          , reverse $ samplePayments activeUTxOs
          , extraOutput
          ]
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-- | When making payments on multiple loans, the collateral outputs are out of order relative
-- to the inputs.
orderFailure6 :: MonadEmulator m => m ()
orderFailure6 = do
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",2),(_unAssetBeacon loanBeacon,2)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum askDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = loanAddress
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
      , extraKeyWitnesses = [borrowerPubKey]
      }

  -- Create the Offer UTxO.
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",2)
                  , (_unAssetBeacon loanBeacon,2)
                  , (_unLenderId lenderBeacon,2)
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
          , Output
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

  askRefs <- map fst <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @AskDatum 
      loanAddress 
      (negotiationBeaconCurrencySymbol,"Ask")

  offerRefs <- map fst <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @OfferDatum 
      loanAddress 
      (negotiationBeaconCurrencySymbol,"Offer")

  let [activeDatum1,activeDatum2] = flip map offerRefs $ \offerRef ->
        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      [loanIdBeacon1,loanIdBeacon2] = map genLoanId offerRefs

  -- Accept the offers.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-2)
                  , ("Ask",-2)
                  , (_unAssetBeacon loanBeacon,-4)
                  , (_unLenderId lenderBeacon,-2)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",2)
                  , (_unBorrowerId borrowerBeacon,2)
                  , (_unAssetBeacon loanBeacon,2)
                  , (_unLoanId loanIdBeacon1,2)
                  , (_unLoanId loanIdBeacon2,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
              , mintPolicy = toVersionedMintingPolicy activeBeaconScript
              , mintReference = Just activeRef
              }
          ]
      , inputs = concat
          [ flip map offerRefs $ \offerRef -> 
              Input
                { inputId = offerRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum (toRedeemer AcceptOffer)
                }
          , flip map askRefs $ \askRef ->
              Input 
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
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon1) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon1) 1 ]
              , outputDatum = OutputDatum $ toDatum $ 
                  PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon1)
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon2) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum2
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon2) 1 ]
              , outputDatum = OutputDatum $ toDatum $ 
                  PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon2)
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

  let extraOutput = 
        [ Output
            { outputAddress = borrowerPersonalAddr
            , outputValue = utxoValue 4_000_000 mempty
            , outputDatum = NoOutputDatum
            , outputReferenceScript = toReferenceScript Nothing
            }
        ]

  let sampleCollateral acs = flip map acs $ 
        \(_,Just ad@ActiveDatum{..}) ->
          Output
            { outputAddress = loanAddress
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

  let samplePayments acs = flip map acs $ 
        \(_,Just ad@ActiveDatum{..}) ->
          Output
            { outputAddress = toCardanoApiAddress lenderAddr
            , outputValue = utxoValue 5_000_000 mempty
            , outputDatum = 
                OutputDatum $ toDatum $ 
                  PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
            , outputReferenceScript = toReferenceScript Nothing
            }

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
            }
      , outputs = mconcat
          [ extraOutput
          , reverse $ sampleCollateral activeUTxOs
          , extraOutput
          ,samplePayments activeUTxOs
          , extraOutput
          ]
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-------------------------------------------------
-- Collateral Value Failures
-------------------------------------------------
-- | Take too much collateral.
collateralFailure1 :: MonadEmulator m => m ()
collateralFailure1 = do
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
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
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
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

  askRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        ]

  offerRef <-
    txOutRefWithValue $ 
      utxoValue 4_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
        , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
        ]

  let activeDatum = 
        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      loanIdBeacon = genLoanId offerRef

  -- Accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
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
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1 ]
              , outputDatum = 
                  OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon)
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
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 4
                  ]
              , outputDatum = OutputDatum $ toDatum $ createPostPaymentActiveDatum 5_000_000 ad
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 5_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-- | When making a partial payment, the collateral is swapped out even though it is not 
-- allowed.
collateralFailure2 :: MonadEmulator m => m ()
collateralFailure2 = do
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

      -- Loan Info
      loanAsset = Asset (adaSymbol,adaToken)
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      collateral3 = Asset (testTokenSymbol,"TestToken3")
      loanBeacon = genLoanAssetBeaconName loanAsset
      askDatum = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred
        , _loanAsset = loanAsset
        , _loanPrincipal = 10_000_000
        , _loanTerm = 3600
        , _collateral = [collateral1,collateral2,collateral3]
        }
      offerDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrincipal = 10_000_000
        , _epochDuration = Nothing
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _compoundingInterest = True
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
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
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 1
                  , uncurry PV2.singleton (_unAsset collateral2) 1
                  , uncurry PV2.singleton (_unAsset collateral3) 1
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

  askRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        , uncurry PV2.singleton (_unAsset collateral2) 1
        , uncurry PV2.singleton (_unAsset collateral3) 1
        ]

  offerRef <-
    txOutRefWithValue $ 
      utxoValue 4_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
        , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
        ]

  let activeDatum = 
        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      loanIdBeacon = genLoanId offerRef

  -- Accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
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
              , outputValue = utxoValue 5_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1 ]
              , outputDatum = 
                  OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon)
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
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 5_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                  , uncurry PV2.singleton (_unAsset collateral2) 6
                  ]
              , outputDatum = OutputDatum $ toDatum $ createPostPaymentActiveDatum 5_000_000 ad
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 5_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-------------------------------------------------
-- Payment Output Failures
-------------------------------------------------
-- | The payment made does not match the redeemer. The datum and the lender output match.
paymentFailure1 :: MonadEmulator m => m ()
paymentFailure1 = do
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
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
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
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

  askRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        ]

  offerRef <-
    txOutRefWithValue $ 
      utxoValue 4_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
        , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
        ]

  let activeDatum = 
        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      loanIdBeacon = genLoanId offerRef

  -- Accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
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
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1 ]
              , outputDatum = 
                  OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon)
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
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
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
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 5_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 4_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-- | The payment made does not match the redeemer. The datum matches the redeemer.
paymentFailure2 :: MonadEmulator m => m ()
paymentFailure2 = do
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
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
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
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

  askRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        ]

  offerRef <-
    txOutRefWithValue $ 
      utxoValue 4_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
        , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
        ]

  let activeDatum = 
        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      loanIdBeacon = genLoanId offerRef

  -- Accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
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
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1 ]
              , outputDatum = 
                  OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon)
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
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
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
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-- | The amount in the redeemer is negative.
paymentFailure3 :: MonadEmulator m => m ()
paymentFailure3 = do
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

      -- Loan Info
      loanAsset = Asset (testTokenSymbol,"TestToken2")
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      loanBeacon = genLoanAssetBeaconName loanAsset
      askDatum = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred
        , _loanAsset = loanAsset
        , _loanPrincipal = 10
        , _loanTerm = 3600
        , _collateral = [collateral1]
        }
      offerDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrincipal = 10
        , _epochDuration = Nothing
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _compoundingInterest = True
        , _minPayment = 0
        , _penalty = NoPenalty
        , _collateralization = [(collateral1,Fraction(1,1))]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]
  mintTestTokens lenderWallet 10_000_000 [("TestToken2",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
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
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
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
                  , uncurry PV2.singleton (_unAsset loanAsset) 10
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
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        ]

  offerRef <-
    txOutRefWithValue $ 
      utxoValue 4_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
        , uncurry PV2.singleton (_unAsset loanAsset) 10
        ]

  let activeDatum = 
        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      loanIdBeacon = genLoanId offerRef

  -- Accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
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
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1 ]
              , outputDatum = 
                  OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon)
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
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum $ createPostPaymentActiveDatum (-1) ad
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 2_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment $ negate 1)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-- | Swap out the collateral without making a payment. The MakePayment redeemer is set to 0.
paymentFailure4 :: MonadEmulator m => m ()
paymentFailure4 = do
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

      -- Loan Info
      loanAsset = Asset (adaSymbol,adaToken)
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      collateral3 = Asset (testTokenSymbol,"TestToken3")
      loanBeacon = genLoanAssetBeaconName loanAsset
      askDatum = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred
        , _loanAsset = loanAsset
        , _loanPrincipal = 10_000_000
        , _loanTerm = 3600
        , _collateral = [collateral1,collateral2,collateral3]
        }
      offerDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrincipal = 10_000_000
        , _epochDuration = Nothing
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _compoundingInterest = True
        , _minPayment = 0
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1_000_000))
            , (collateral2,Fraction(1,1_000_000))
            , (collateral3,Fraction(1,1_000_000))
            ]
        , _collateralIsSwappable = True
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
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
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 1
                  , uncurry PV2.singleton (_unAsset collateral2) 1
                  , uncurry PV2.singleton (_unAsset collateral3) 1
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

  askRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        , uncurry PV2.singleton (_unAsset collateral2) 1
        , uncurry PV2.singleton (_unAsset collateral3) 1
        ]

  offerRef <-
    txOutRefWithValue $ 
      utxoValue 4_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
        , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
        ]

  let activeDatum = 
        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      loanIdBeacon = genLoanId offerRef

  -- Accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
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
              , outputValue = utxoValue 5_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1 ]
              , outputDatum = 
                  OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon)
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
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 5_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                  , uncurry PV2.singleton (_unAsset collateral2) 10
                  ]
              , outputDatum = OutputDatum $ toDatum $ createPostPaymentActiveDatum 0 ad
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 0)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-- | The payment output datum has the wrong policy id.
paymentFailure5 :: MonadEmulator m => m ()
paymentFailure5 = do
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
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
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
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

  askRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        ]

  offerRef <-
    txOutRefWithValue $ 
      utxoValue 4_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
        , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
        ]

  let activeDatum = 
        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      loanIdBeacon = genLoanId offerRef

  -- Accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
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
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1 ]
              , outputDatum = 
                  OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon)
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
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
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
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 5_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum ("",_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-- | The payment output datum has the wrong loan id.
paymentFailure6 :: MonadEmulator m => m ()
paymentFailure6 = do
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
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
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
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

  askRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        ]

  offerRef <-
    txOutRefWithValue $ 
      utxoValue 4_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
        , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
        ]

  let activeDatum = 
        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      loanIdBeacon = genLoanId offerRef

  -- Accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
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
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1 ]
              , outputDatum = 
                  OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon)
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
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
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
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 5_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,"")
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-------------------------------------------------
-- Collateral Datum Failures
-------------------------------------------------
-- | The collateral datum has the wrong active beacon id.
datumFailure1 :: MonadEmulator m => m ()
datumFailure1 = do
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
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
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
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

  askRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        ]

  offerRef <-
    txOutRefWithValue $ 
      utxoValue 4_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
        , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
        ]

  let activeDatum = 
        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      loanIdBeacon = genLoanId offerRef

  -- Accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
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
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1 ]
              , outputDatum = 
                  OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon)
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
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 6
                  ]
              , outputDatum = 
                  let newDatum = createPostPaymentActiveDatum 5_000_000 ad
                  in OutputDatum $ 
                      toDatum @ActiveDatum newDatum{_activeBeaconId = ActiveBeaconId ""}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 5_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-- | The collateral datum has the wrong borrower id.
datumFailure2 :: MonadEmulator m => m ()
datumFailure2 = do
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
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
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
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

  askRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        ]

  offerRef <-
    txOutRefWithValue $ 
      utxoValue 4_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
        , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
        ]

  let activeDatum = 
        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      loanIdBeacon = genLoanId offerRef

  -- Accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
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
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1 ]
              , outputDatum = 
                  OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon)
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
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 6
                  ]
              , outputDatum = 
                  let newDatum = createPostPaymentActiveDatum 5_000_000 ad
                  in OutputDatum $ 
                      toDatum @ActiveDatum newDatum{_borrowerId = BorrowerId ""}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 5_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-- | The collateral datum has the wrong lender address.
datumFailure3 :: MonadEmulator m => m ()
datumFailure3 = do
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
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
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
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

  askRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        ]

  offerRef <-
    txOutRefWithValue $ 
      utxoValue 4_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
        , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
        ]

  let activeDatum = 
        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      loanIdBeacon = genLoanId offerRef

  -- Accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
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
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1 ]
              , outputDatum = 
                  OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon)
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
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 6
                  ]
              , outputDatum = 
                  let newDatum = createPostPaymentActiveDatum 5_000_000 ad
                  in OutputDatum $ toDatum @ActiveDatum 
                       newDatum{_lenderAddress = PV2.Address borrowerCred Nothing}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 5_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-- | The collateral datum has the wrong loan asset.
datumFailure4 :: MonadEmulator m => m ()
datumFailure4 = do
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
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
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
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

  askRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        ]

  offerRef <-
    txOutRefWithValue $ 
      utxoValue 4_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
        , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
        ]

  let activeDatum = 
        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      loanIdBeacon = genLoanId offerRef

  -- Accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
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
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1 ]
              , outputDatum = 
                  OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon)
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
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 6
                  ]
              , outputDatum = 
                  let newDatum = createPostPaymentActiveDatum 5_000_000 ad
                  in OutputDatum $ toDatum @ActiveDatum 
                       newDatum{_loanAsset = Asset (testTokenSymbol,"TestToken1")}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 5_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-- | The collateral datum has the wrong asset beacon.
datumFailure5 :: MonadEmulator m => m ()
datumFailure5 = do
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
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
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
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

  askRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        ]

  offerRef <-
    txOutRefWithValue $ 
      utxoValue 4_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
        , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
        ]

  let activeDatum = 
        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      loanIdBeacon = genLoanId offerRef

  -- Accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
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
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1 ]
              , outputDatum = 
                  OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon)
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
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 6
                  ]
              , outputDatum = 
                  let newDatum = createPostPaymentActiveDatum 5_000_000 ad
                  in OutputDatum $ toDatum @ActiveDatum 
                       newDatum{_assetBeacon = AssetBeacon ""}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 5_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-- | The collateral datum has the wrong loan principal.
datumFailure6 :: MonadEmulator m => m ()
datumFailure6 = do
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
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
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
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

  askRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        ]

  offerRef <-
    txOutRefWithValue $ 
      utxoValue 4_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
        , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
        ]

  let activeDatum = 
        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      loanIdBeacon = genLoanId offerRef

  -- Accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
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
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1 ]
              , outputDatum = 
                  OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon)
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
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 6
                  ]
              , outputDatum = 
                  let newDatum = createPostPaymentActiveDatum 5_000_000 ad
                  in OutputDatum $ toDatum @ActiveDatum 
                       newDatum{_loanPrincipal = 0}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 5_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-- | The collateral datum has the wrong epoch duration.
datumFailure7 :: MonadEmulator m => m ()
datumFailure7 = do
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
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
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
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

  askRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        ]

  offerRef <-
    txOutRefWithValue $ 
      utxoValue 4_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
        , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
        ]

  let activeDatum = 
        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      loanIdBeacon = genLoanId offerRef

  -- Accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
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
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1 ]
              , outputDatum = 
                  OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon)
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
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 6
                  ]
              , outputDatum = 
                  let newDatum = createPostPaymentActiveDatum 5_000_000 ad
                  in OutputDatum $ toDatum @ActiveDatum 
                       newDatum{_epochDuration = Just 999}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 5_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-- | The collateral datum has the wrong last epoch boundary.
datumFailure8 :: MonadEmulator m => m ()
datumFailure8 = do
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
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
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
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

  askRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        ]

  offerRef <-
    txOutRefWithValue $ 
      utxoValue 4_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
        , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
        ]

  let activeDatum = 
        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      loanIdBeacon = genLoanId offerRef

  -- Accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
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
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1 ]
              , outputDatum = 
                  OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon)
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
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 6
                  ]
              , outputDatum = 
                  let newDatum = createPostPaymentActiveDatum 5_000_000 ad
                  in OutputDatum $ toDatum @ActiveDatum 
                       newDatum{_lastEpochBoundary = 999}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 5_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-- | The collateral datum has the wrong loan term.
datumFailure9 :: MonadEmulator m => m ()
datumFailure9 = do
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
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
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
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

  askRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        ]

  offerRef <-
    txOutRefWithValue $ 
      utxoValue 4_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
        , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
        ]

  let activeDatum = 
        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      loanIdBeacon = genLoanId offerRef

  -- Accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
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
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1 ]
              , outputDatum = 
                  OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon)
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
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 6
                  ]
              , outputDatum = 
                  let newDatum = createPostPaymentActiveDatum 5_000_000 ad
                  in OutputDatum $ toDatum @ActiveDatum 
                       newDatum{_loanTerm = 0}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 5_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-- | The collateral datum has the wrong loan interest.
datumFailure10 :: MonadEmulator m => m ()
datumFailure10 = do
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
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
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
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

  askRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        ]

  offerRef <-
    txOutRefWithValue $ 
      utxoValue 4_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
        , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
        ]

  let activeDatum = 
        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      loanIdBeacon = genLoanId offerRef

  -- Accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
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
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1 ]
              , outputDatum = 
                  OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon)
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
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 6
                  ]
              , outputDatum = 
                  let newDatum = createPostPaymentActiveDatum 5_000_000 ad
                  in OutputDatum $ toDatum @ActiveDatum 
                       newDatum{_loanInterest = Fraction (0,10)}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 5_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-- | The collateral datum has the wrong minimum payment.
datumFailure11 :: MonadEmulator m => m ()
datumFailure11 = do
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
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
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
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

  askRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        ]

  offerRef <-
    txOutRefWithValue $ 
      utxoValue 4_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
        , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
        ]

  let activeDatum = 
        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      loanIdBeacon = genLoanId offerRef

  -- Accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
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
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1 ]
              , outputDatum = 
                  OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon)
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
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 6
                  ]
              , outputDatum = 
                  let newDatum = createPostPaymentActiveDatum 5_000_000 ad
                  in OutputDatum $ toDatum @ActiveDatum 
                       newDatum{_minPayment = -1}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 5_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-- | The collateral datum has the wrong collateralization.
datumFailure12 :: MonadEmulator m => m ()
datumFailure12 = do
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
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
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
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

  askRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        ]

  offerRef <-
    txOutRefWithValue $ 
      utxoValue 4_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
        , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
        ]

  let activeDatum = 
        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      loanIdBeacon = genLoanId offerRef

  -- Accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
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
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1 ]
              , outputDatum = 
                  OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon)
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
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 6
                  ]
              , outputDatum = 
                  let newDatum = createPostPaymentActiveDatum 5_000_000 ad
                  in OutputDatum $ toDatum @ActiveDatum 
                       newDatum{_collateralization = Collateralization []}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 5_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-- | The collateral datum has the wrong collateral is swappable.
datumFailure13 :: MonadEmulator m => m ()
datumFailure13 = do
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
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
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
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

  askRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        ]

  offerRef <-
    txOutRefWithValue $ 
      utxoValue 4_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
        , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
        ]

  let activeDatum = 
        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      loanIdBeacon = genLoanId offerRef

  -- Accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
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
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1 ]
              , outputDatum = 
                  OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon)
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
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 6
                  ]
              , outputDatum = 
                  let newDatum = createPostPaymentActiveDatum 5_000_000 ad
                  in OutputDatum $ toDatum @ActiveDatum 
                       newDatum{_collateralIsSwappable = True}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 5_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-- | The collateral datum has the wrong claim period expiration.
datumFailure14 :: MonadEmulator m => m ()
datumFailure14 = do
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
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
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
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

  askRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        ]

  offerRef <-
    txOutRefWithValue $ 
      utxoValue 4_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
        , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
        ]

  let activeDatum = 
        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      loanIdBeacon = genLoanId offerRef

  -- Accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
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
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1 ]
              , outputDatum = 
                  OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon)
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
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 6
                  ]
              , outputDatum = 
                  let newDatum = createPostPaymentActiveDatum 5_000_000 ad
                  in OutputDatum $ toDatum @ActiveDatum 
                       newDatum{_claimExpiration = 0}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 5_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-- | The collateral datum has the wrong loan expiration.
datumFailure15 :: MonadEmulator m => m ()
datumFailure15 = do
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
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
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
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

  askRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        ]

  offerRef <-
    txOutRefWithValue $ 
      utxoValue 4_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
        , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
        ]

  let activeDatum = 
        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      loanIdBeacon = genLoanId offerRef

  -- Accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
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
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1 ]
              , outputDatum = 
                  OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon)
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
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 6
                  ]
              , outputDatum = 
                  let newDatum = createPostPaymentActiveDatum 5_000_000 ad
                  in OutputDatum $ toDatum @ActiveDatum 
                       newDatum{_loanExpiration = 0}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 5_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-- | The collateral datum has the wrong outstanding balance.
datumFailure16 :: MonadEmulator m => m ()
datumFailure16 = do
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
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
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
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

  askRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        ]

  offerRef <-
    txOutRefWithValue $ 
      utxoValue 4_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
        , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
        ]

  let activeDatum = 
        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      loanIdBeacon = genLoanId offerRef

  -- Accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
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
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1 ]
              , outputDatum = 
                  OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon)
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
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 6
                  ]
              , outputDatum = 
                  let newDatum = createPostPaymentActiveDatum 5_000_000 ad
                  in OutputDatum $ toDatum @ActiveDatum 
                       newDatum{_loanOutstanding = Fraction (0,0)}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 5_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-- | The collateral datum has the wrong loan id.
datumFailure17 :: MonadEmulator m => m ()
datumFailure17 = do
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
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
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
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

  askRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        ]

  offerRef <-
    txOutRefWithValue $ 
      utxoValue 4_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
        , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
        ]

  let activeDatum = 
        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      loanIdBeacon = genLoanId offerRef

  -- Accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
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
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1 ]
              , outputDatum = 
                  OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon)
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
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 6
                  ]
              , outputDatum = 
                  let newDatum = createPostPaymentActiveDatum 5_000_000 ad
                  in OutputDatum $ toDatum @ActiveDatum 
                       newDatum{_loanId = LoanId ""}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 5_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-- | The collateral output datum is a datum hash.
datumFailure18 :: MonadEmulator m => m ()
datumFailure18 = do
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
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
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
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

  askRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        ]

  offerRef <-
    txOutRefWithValue $ 
      utxoValue 4_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
        , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
        ]

  let activeDatum = 
        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      loanIdBeacon = genLoanId offerRef

  -- Accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
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
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1 ]
              , outputDatum = 
                  OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon)
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
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 6
                  ]
              , outputDatum = OutputDatumHash $ datumHash $ createPostPaymentActiveDatum 5_000_000 ad
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 5_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-- | The lender payment datum is a datum hash.
datumFailure19 :: MonadEmulator m => m ()
datumFailure19 = do
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
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
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
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

  askRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        ]

  offerRef <-
    txOutRefWithValue $ 
      utxoValue 4_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
        , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
        ]

  let activeDatum = 
        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      loanIdBeacon = genLoanId offerRef

  -- Accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
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
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1 ]
              , outputDatum = 
                  OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon)
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
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
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
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 5_000_000 mempty
              , outputDatum = 
                  OutputDatumHash $ datumHash $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-- | The collateral output datum is not an ActiveDatum.
datumFailure20 :: MonadEmulator m => m ()
datumFailure20 = do
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
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
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
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

  askRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        ]

  offerRef <-
    txOutRefWithValue $ 
      utxoValue 4_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
        , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
        ]

  let activeDatum = 
        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      loanIdBeacon = genLoanId offerRef

  -- Accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
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
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1 ]
              , outputDatum = 
                  OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon)
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
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 6
                  ]
              , outputDatum = OutputDatum $ toDatum askDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 5_000_000 mempty
              , outputDatum = 
                  OutputDatumHash $ datumHash $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-- | The collateral datum has the wrong payment observer hash.
datumFailure21 :: MonadEmulator m => m ()
datumFailure21 = do
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
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
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
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

  askRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        ]

  offerRef <-
    txOutRefWithValue $ 
      utxoValue 4_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
        , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
        ]

  let activeDatum = 
        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      loanIdBeacon = genLoanId offerRef

  -- Accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
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
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1 ]
              , outputDatum = 
                  OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon)
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
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 6
                  ]
              , outputDatum = 
                  let newDatum = createPostPaymentActiveDatum 5_000_000 ad
                  in OutputDatum $ toDatum @ActiveDatum 
                       newDatum{_paymentObserverHash = ""}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 5_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-- | The collateral datum has the wrong interest observer hash.
datumFailure22 :: MonadEmulator m => m ()
datumFailure22 = do
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
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
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
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

  askRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        ]

  offerRef <-
    txOutRefWithValue $ 
      utxoValue 4_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
        , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
        ]

  let activeDatum = 
        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      loanIdBeacon = genLoanId offerRef

  -- Accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
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
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1 ]
              , outputDatum = 
                  OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon)
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
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 6
                  ]
              , outputDatum = 
                  let newDatum = createPostPaymentActiveDatum 5_000_000 ad
                  in OutputDatum $ toDatum @ActiveDatum 
                       newDatum{_interestObserverHash = ""}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 5_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-- | The collateral datum has the wrong address update observer hash.
datumFailure23 :: MonadEmulator m => m ()
datumFailure23 = do
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
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
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
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

  askRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        ]

  offerRef <-
    txOutRefWithValue $ 
      utxoValue 4_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
        , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
        ]

  let activeDatum = 
        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      loanIdBeacon = genLoanId offerRef

  -- Accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
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
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1 ]
              , outputDatum = 
                  OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon)
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
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 6
                  ]
              , outputDatum = 
                  let newDatum = createPostPaymentActiveDatum 5_000_000 ad
                  in OutputDatum $ toDatum @ActiveDatum 
                       newDatum{_addressUpdateObserverHash = ""}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 5_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-- | The collateral datum has the wrong penalty.
datumFailure24 :: MonadEmulator m => m ()
datumFailure24 = do
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
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
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
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

  askRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        ]

  offerRef <-
    txOutRefWithValue $ 
      utxoValue 4_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
        , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
        ]

  let activeDatum = 
        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      loanIdBeacon = genLoanId offerRef

  -- Accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
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
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1 ]
              , outputDatum = 
                  OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon)
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
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 6
                  ]
              , outputDatum = 
                  let newDatum = createPostPaymentActiveDatum 5_000_000 ad
                  in OutputDatum $ toDatum @ActiveDatum 
                       newDatum{_penalty = FixedFee 1000}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 5_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-- | The collateral datum has the wrong total epoch payments.
datumFailure25 :: MonadEmulator m => m ()
datumFailure25 = do
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
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
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
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

  askRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        ]

  offerRef <-
    txOutRefWithValue $ 
      utxoValue 4_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
        , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
        ]

  let activeDatum = 
        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      loanIdBeacon = genLoanId offerRef

  -- Accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
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
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1 ]
              , outputDatum = 
                  OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon)
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
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 6
                  ]
              , outputDatum = 
                  let newDatum = createPostPaymentActiveDatum 5_000_000 ad
                  in OutputDatum $ toDatum @ActiveDatum 
                       newDatum{_totalEpochPayments = 0}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 5_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer ObservePayment
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-------------------------------------------------
-- Observer Redeemer Failures.
-------------------------------------------------
-- | Use the register observer redeemer.
observerRedeemerFailure1 :: MonadEmulator m => m ()
observerRedeemerFailure1 = do
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
  References{negotiationRef,activeRef,loanRef,paymentObserverRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
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
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
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

  askRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        ]

  offerRef <-
    txOutRefWithValue $ 
      utxoValue 4_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
        , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
        ]

  let activeDatum = 
        createAcceptanceDatumFromOffer borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      loanIdBeacon = genLoanId offerRef

  -- Accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-1)
                  , ("Ask",-1)
                  , (_unAssetBeacon loanBeacon,-2)
                  , (_unLenderId lenderBeacon,-1)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",1)
                  , (_unBorrowerId borrowerBeacon,1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLoanId loanIdBeacon,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive negotiationBeaconCurrencySymbol
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
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1 ]
              , outputDatum = 
                  OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon)
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
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
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
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 5_000_000 mempty
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  paymentSlot <- (1+) <$> currentSlot

  -- Try to make a partial payment.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs = flip map activeUTxOs $ \(activeUtxoRef,_) ->
          Input
            { inputId = activeUtxoRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ MakePayment 5_000_000)
            }
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash paymentObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference paymentObserverRef $ 
                    toRedeemer RegisterPaymentObserverScript
              }
          ]
      , referenceInputs = [paymentObserverRef,negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just paymentSlot
          }
      }

-------------------------------------------------
-- TestTree
-------------------------------------------------
-- | A `TestTree` containing all failure scenarios for making payments.
tests :: [TestTree]
tests =
  [ -- Beacon Failure Tests
    scriptMustFailWithError "beaconFailure1" 
      "The wrong active beacons were burned"
      beaconFailure1
  , scriptMustFailWithError "beaconFailure2" 
      "The wrong active beacons were burned"
      beaconFailure2
  , scriptMustFailWithError "beaconFailure3" 
      "Invalid Active UTxO found"
      beaconFailure3
  , scriptMustFailWithError "beaconFailure4" 
      "UTxO has too many BorrowerIds"
      beaconFailure4
  , scriptMustFailWithError "beaconFailure5" 
      "UTxO does not have the right beacons"
      beaconFailure5
  , scriptMustFailWithError "beaconFailure6" 
      "UTxO does not have the right beacons"
      beaconFailure6
  , scriptMustFailWithError "beaconFailure7" 
      "UTxO does not have the right beacons"
      beaconFailure7
  , scriptMustFailWithError "beaconFailure8" 
      "Not all required payment outputs found"
      beaconFailure8
  , scriptMustFailWithError "beaconFailure9" 
      "Not all required payment outputs found"
      beaconFailure9
  , scriptMustFailWithError "beaconFailure10" 
      "Payment UTxO missing beacons"
      beaconFailure10
  , scriptMustFailWithError "beaconFailure11" 
      "Payment UTxO missing beacons"
      beaconFailure11

    -- Address Failure Tests
  , scriptMustFailWithError "addressFailure1" 
      "Not all required collateral outputs found"
      addressFailure1
  , scriptMustFailWithError "addressFailure2" 
      "Not all required payment outputs found"
      addressFailure2
  , scriptMustFailWithError "addressFailure3" 
      "All loan inputs must be for the same borrower"
      addressFailure3
  , scriptMustFail "addressFailure4" 
      -- exact error depends on order of execution
      addressFailure4

    -- Approval Failure Tests
  , scriptMustFailWithError "approvalFailure1" 
      "Borrower credential did not approve"
      approvalFailure1

    -- Time Failure Tests
  , scriptMustFailWithError "timeFailure1" 
      "Loan is expired"
      timeFailure1
  , scriptMustFailWithError "timeFailure2" 
      "invalid-hereafter not specified"
      timeFailure2
  , scriptMustFailWithError "timeFailure3" 
      "Next interest and/or penalty application required"
      timeFailure3
  , scriptMustFailWithError "timeFailure4" 
      "Next interest and/or penalty application required"
      timeFailure4
      
    -- Output Order Failure Tests
  , scriptMustFailWithError "orderFailure1" 
      "Too much collateral taken"
      orderFailure1
  , scriptMustFailWithError "orderFailure2" 
      "Too much collateral taken"
      orderFailure2
  , scriptMustFailWithError "orderFailure3" 
      "Lender payment amount does not match redeemer"
      orderFailure3
  , scriptMustFailWithError "orderFailure4" 
      "Lender payment amount does not match redeemer"
      orderFailure4
  , scriptMustFailWithError "orderFailure5" 
      "Not all required payment outputs found"
      orderFailure5
  , scriptMustFailWithError "orderFailure6" 
      "Not all required collateral outputs found"
      orderFailure6

    -- Collateral Failure Tests
  , scriptMustFailWithError "collateralFailure1" 
      "Too much collateral taken"
      collateralFailure1
  , scriptMustFailWithError "collateralFailure2" 
      "Collateral is not swappable"
      collateralFailure2

    -- Payment Failure Tests
  , scriptMustFailWithError "paymentFailure1" 
      "Not all required collateral outputs found"
      paymentFailure1
  , scriptMustFailWithError "paymentFailure2" 
      "Lender payment amount does not match redeemer"
      paymentFailure2
  , scriptMustFailWithError "paymentFailure3" 
      "Too much collateral taken"
      paymentFailure3
  , scriptMustFailWithError "paymentFailure4" 
      "Not all required payment outputs found"
      paymentFailure4
  , scriptMustFailWithError "paymentFailure5" 
      "Not all required payment outputs found"
      paymentFailure5
  , scriptMustFailWithError "paymentFailure6" 
      "Not all required payment outputs found"
      paymentFailure6

    -- Datum Failure Tests
  , scriptMustFailWithError "datumFailure1" 
      "Not all required collateral outputs found"
      datumFailure1
  , scriptMustFailWithError "datumFailure2" 
      "Not all required collateral outputs found"
      datumFailure2
  , scriptMustFailWithError "datumFailure3" 
      "Not all required collateral outputs found"
      datumFailure3
  , scriptMustFailWithError "datumFailure4" 
      "Not all required collateral outputs found"
      datumFailure4
  , scriptMustFailWithError "datumFailure5" 
      "Not all required collateral outputs found"
      datumFailure5
  , scriptMustFailWithError "datumFailure6" 
      "Not all required collateral outputs found"
      datumFailure6
  , scriptMustFailWithError "datumFailure7" 
      "Not all required collateral outputs found"
      datumFailure7
  , scriptMustFailWithError "datumFailure8" 
      "Not all required collateral outputs found"
      datumFailure8
  , scriptMustFailWithError "datumFailure9" 
      "Not all required collateral outputs found"
      datumFailure9
  , scriptMustFailWithError "datumFailure10" 
      "Not all required collateral outputs found"
      datumFailure10
  , scriptMustFailWithError "datumFailure11" 
      "Not all required collateral outputs found"
      datumFailure11
  , scriptMustFailWithError "datumFailure12" 
      "Not all required collateral outputs found"
      datumFailure12
  , scriptMustFailWithError "datumFailure13" 
      "Not all required collateral outputs found"
      datumFailure13
  , scriptMustFailWithError "datumFailure14" 
      "Not all required collateral outputs found"
      datumFailure14
  , scriptMustFailWithError "datumFailure15" 
      "Not all required collateral outputs found"
      datumFailure15
  , scriptMustFailWithError "datumFailure16" 
      "Not all required collateral outputs found"
      datumFailure16
  , scriptMustFailWithError "datumFailure17" 
      "Not all required collateral outputs found"
      datumFailure17
  , scriptMustFailWithError "datumFailure18" 
      "Not all required collateral outputs found"
      datumFailure18
  , scriptMustFailWithError "datumFailure19" 
      "Not all required payment outputs found"
      datumFailure19
  , scriptMustFailWithError "datumFailure20" 
      "Not all required collateral outputs found"
      datumFailure20
  , scriptMustFailWithError "datumFailure21" 
      "Not all required collateral outputs found"
      datumFailure21
  , scriptMustFailWithError "datumFailure22" 
      "Not all required collateral outputs found"
      datumFailure22
  , scriptMustFailWithError "datumFailure23" 
      "Not all required collateral outputs found"
      datumFailure23
  , scriptMustFailWithError "datumFailure24" 
      "Not all required collateral outputs found"
      datumFailure24
  , scriptMustFailWithError "datumFailure25" 
      "Not all required collateral outputs found"
      datumFailure25

    -- Observer Redeemer Failures
  , scriptMustFailWithError "observerRedeemerFailure1" 
      "This redeemer can only be used to register the payment observer script"
      observerRedeemerFailure1
  ]
