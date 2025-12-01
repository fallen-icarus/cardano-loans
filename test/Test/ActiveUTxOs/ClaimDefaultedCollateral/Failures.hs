{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.ActiveUTxOs.ClaimDefaultedCollateral.Failures where

import qualified Ledger.Address as LA
import qualified PlutusLedgerApi.V2 as PV2
import qualified Ledger.CardanoWallet as Mock 
import Test.Tasty (TestTree)
import Control.Monad (forM)
import Data.Maybe (isJust)

import CardanoLoans

import Test.Prelude

-------------------------------------------------
-- Time Failures
-------------------------------------------------
-- | Invalid-before not specified.
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
      newLenderAddr = 
        PV2.Address lenderCred Nothing

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
        , _maxConsecutiveMisses = Nothing
        , _collateralization = [(collateral1,Fraction(1,1_000_000))]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        , _correspondingAsk = Nothing
        }

  -- Initialize scenario
  References{negotiationRef,activeRef,loanRef,addressUpdateObserverRef,proxyRef} <- initializeReferenceScripts 
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
                , (_unLoanId loanIdBeacon,-2)
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
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer SpendWithKeyNFT)
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
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Nothing
          }
      }

-- | The loan is not expired.
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
      newLenderAddr = 
        PV2.Address lenderCred Nothing

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
        , _maxConsecutiveMisses = Nothing
        , _collateralization = [(collateral1,Fraction(1,1_000_000))]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        , _correspondingAsk = Nothing
        }

  -- Initialize scenario
  References{negotiationRef,activeRef,loanRef,addressUpdateObserverRef,proxyRef} <- initializeReferenceScripts 
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
                , (_unLoanId loanIdBeacon,-2)
                ]
            , mintRedeemer = toRedeemer BurnKeyAndClaimDefaulted
            , mintPolicy = toVersionedMintingPolicy activeBeaconScript
            , mintReference = Just activeRef
            }

  claimSlot <- currentSlot

  -- Try to claim the collateral.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens = sampleKeyBurns activeUTxOs
      , inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer SpendWithKeyNFT)
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
-- Beacon Failures
-------------------------------------------------
-- | When claiming collateral for a single expired loan, don't burn the Active beacon.
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
      newLenderAddr = 
        PV2.Address lenderCred Nothing

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
        , _maxConsecutiveMisses = Nothing
        , _collateralization = [(collateral1,Fraction(1,1_000_000))]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        , _correspondingAsk = Nothing
        }

  -- Initialize scenario
  References{negotiationRef,activeRef,loanRef,addressUpdateObserverRef,proxyRef} <- initializeReferenceScripts 
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

  keyUTxOs <- fmap concat $ forM activeUTxOs $ \(_,Just ad@ActiveDatum{_loanId}) ->
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      (toCardanoApiAddress lenderAddr) 
      (activeBeaconCurrencySymbol,_unLoanId _loanId)

  let sampleKeyBurns acs = flip map acs $
        \(_,Just ad@ActiveDatum{..}) ->
          TokenMint
            { mintTokens = 
                [ ("Active",0)
                , (_unBorrowerId _borrowerId,-1)
                , (_unAssetBeacon _assetBeacon,-1)
                , (_unLoanId loanIdBeacon,-2)
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
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer SpendWithKeyNFT)
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

-- | When claiming collateral for a single expired loan, don't burn the BorrowerId.
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
      newLenderAddr = 
        PV2.Address lenderCred Nothing

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
        , _maxConsecutiveMisses = Nothing
        , _collateralization = [(collateral1,Fraction(1,1_000_000))]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        , _correspondingAsk = Nothing
        }

  -- Initialize scenario
  References{negotiationRef,activeRef,loanRef,addressUpdateObserverRef,proxyRef} <- initializeReferenceScripts 
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

  keyUTxOs <- fmap concat $ forM activeUTxOs $ \(_,Just ad@ActiveDatum{_loanId}) ->
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      (toCardanoApiAddress lenderAddr) 
      (activeBeaconCurrencySymbol,_unLoanId _loanId)

  let sampleKeyBurns acs = flip map acs $
        \(_,Just ad@ActiveDatum{..}) ->
          TokenMint
            { mintTokens = 
                [ ("Active",-1)
                , (_unBorrowerId _borrowerId,0)
                , (_unAssetBeacon _assetBeacon,-1)
                , (_unLoanId loanIdBeacon,-2)
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
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer SpendWithKeyNFT)
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

-- | When claiming collateral for a single expired loan, don't burn the Asset beacon.
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
      newLenderAddr = 
        PV2.Address lenderCred Nothing

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
        , _maxConsecutiveMisses = Nothing
        , _collateralization = [(collateral1,Fraction(1,1_000_000))]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        , _correspondingAsk = Nothing
        }

  -- Initialize scenario
  References{negotiationRef,activeRef,loanRef,addressUpdateObserverRef,proxyRef} <- initializeReferenceScripts 
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
                , (_unAssetBeacon _assetBeacon,0)
                , (_unLoanId loanIdBeacon,-2)
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
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer SpendWithKeyNFT)
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

-- | When claiming collateral for a single expired loan, don't burn any LoanIds. They are both
-- still present.
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
      newLenderAddr = 
        PV2.Address lenderCred Nothing

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
        , _maxConsecutiveMisses = Nothing
        , _collateralization = [(collateral1,Fraction(1,1_000_000))]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        , _correspondingAsk = Nothing
        }

  -- Initialize scenario
  References{negotiationRef,activeRef,loanRef,addressUpdateObserverRef,proxyRef} <- initializeReferenceScripts 
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
                , (_unLoanId loanIdBeacon,0)
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
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer SpendWithKeyNFT)
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

-- | When claiming collateral for a single expired loan, only burn one LoanId. They are both
-- still present.
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
      newLenderAddr = 
        PV2.Address lenderCred Nothing

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
        , _maxConsecutiveMisses = Nothing
        , _collateralization = [(collateral1,Fraction(1,1_000_000))]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        , _correspondingAsk = Nothing
        }

  -- Initialize scenario
  References{negotiationRef,activeRef,loanRef,addressUpdateObserverRef,proxyRef} <- initializeReferenceScripts 
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
                , (_unLoanId loanIdBeacon,-1)
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
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer SpendWithKeyNFT)
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

-- | When claiming collateral for a single expired loan, mint an Active Beacon.
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
      newLenderAddr = 
        PV2.Address lenderCred Nothing

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
        , _maxConsecutiveMisses = Nothing
        , _collateralization = [(collateral1,Fraction(1,1_000_000))]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        , _correspondingAsk = Nothing
        }

  -- Initialize scenario
  References{negotiationRef,activeRef,loanRef,addressUpdateObserverRef,proxyRef} <- initializeReferenceScripts 
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

  keyUTxOs <- fmap concat $ forM activeUTxOs $ \(_,Just ad@ActiveDatum{_loanId}) ->
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      (toCardanoApiAddress lenderAddr) 
      (activeBeaconCurrencySymbol,_unLoanId _loanId)

  let sampleKeyBurns acs = flip map acs $
        \(_,Just ad@ActiveDatum{..}) ->
          TokenMint
            { mintTokens = 
                [ ("Active",1)
                , (_unBorrowerId _borrowerId,-1)
                , (_unAssetBeacon _assetBeacon,-1)
                , (_unLoanId loanIdBeacon,-2)
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
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer SpendWithKeyNFT)
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

-- | When claiming collateral for a single expired loan, mint a BorrowerId.
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
      newLenderAddr = 
        PV2.Address lenderCred Nothing

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
        , _maxConsecutiveMisses = Nothing
        , _collateralization = [(collateral1,Fraction(1,1_000_000))]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        , _correspondingAsk = Nothing
        }

  -- Initialize scenario
  References{negotiationRef,activeRef,loanRef,addressUpdateObserverRef,proxyRef} <- initializeReferenceScripts 
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

  keyUTxOs <- fmap concat $ forM activeUTxOs $ \(_,Just ad@ActiveDatum{_loanId}) ->
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      (toCardanoApiAddress lenderAddr) 
      (activeBeaconCurrencySymbol,_unLoanId _loanId)

  let sampleKeyBurns acs = flip map acs $
        \(_,Just ad@ActiveDatum{..}) ->
          TokenMint
            { mintTokens = 
                [ ("Active",-1)
                , (_unBorrowerId _borrowerId,1)
                , (_unAssetBeacon _assetBeacon,-1)
                , (_unLoanId loanIdBeacon,-2)
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
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer SpendWithKeyNFT)
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

-- | When claiming collateral for a single expired loan, mint an Asset beacon.
beaconFailure8 :: MonadEmulator m => m ()
beaconFailure8 = do
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
      newLenderAddr = 
        PV2.Address lenderCred Nothing

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
        , _maxConsecutiveMisses = Nothing
        , _collateralization = [(collateral1,Fraction(1,1_000_000))]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        , _correspondingAsk = Nothing
        }

  -- Initialize scenario
  References{negotiationRef,activeRef,loanRef,addressUpdateObserverRef,proxyRef} <- initializeReferenceScripts 
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
                , (_unAssetBeacon _assetBeacon,1)
                , (_unLoanId loanIdBeacon,-2)
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
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer SpendWithKeyNFT)
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

-- | When claiming collateral for a single expired loan, mint a LoanId. They are both
-- still present.
beaconFailure9 :: MonadEmulator m => m ()
beaconFailure9 = do
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
      newLenderAddr = 
        PV2.Address lenderCred Nothing

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
        , _maxConsecutiveMisses = Nothing
        , _collateralization = [(collateral1,Fraction(1,1_000_000))]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        , _correspondingAsk = Nothing
        }

  -- Initialize scenario
  References{negotiationRef,activeRef,loanRef,addressUpdateObserverRef,proxyRef} <- initializeReferenceScripts 
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
                , (_unLoanId loanIdBeacon,1)
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
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer SpendWithKeyNFT)
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

-- | When claiming collateral for a single expired loan, only one LoanId is present in the
-- transaction and it is burned.
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
      newLenderAddr = 
        PV2.Address lenderCred Nothing

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
        , _maxConsecutiveMisses = Nothing
        , _collateralization = [(collateral1,Fraction(1,1_000_000))]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        , _correspondingAsk = Nothing
        }

  -- Initialize scenario
  References{negotiationRef,activeRef,loanRef,addressUpdateObserverRef,proxyRef} <- initializeReferenceScripts 
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
                , (_unLoanId loanIdBeacon,-1)
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
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer SpendWithKeyNFT)
              }
          ]
      , referenceInputs = [activeRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just claimSlot
          , validityRangeUpperBound = Nothing
          }
      }

-- | Claim collateral for an invalid Active UTxO. The beacon script is not executed.
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
        , _maxConsecutiveMisses = Nothing
        , _collateralization = [(collateral1,Fraction(1,1_000_000))]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        , _correspondingAsk = Nothing
        }

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
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
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ ]
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

  currentSlot >>= awaitTime . slotToPosixTime . (+3600)
  claimSlot <- currentSlot

  -- Try to claim the collateral.
  void $ transact 
    lenderPersonalAddr 
    [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress]
    [lenderPayPrivKey]
    emptyTxParams
      { tokens = []
      , inputs = flip concatMap activeUTxOs $ \(activeUtxoRef,_) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer SpendWithKeyNFT)
              }
          ]
      , referenceInputs = [activeRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just claimSlot
          , validityRangeUpperBound = Nothing
          }
      }

-- | Use SpendWithKeyNFT to claim an invalid UTxO (ie, a UTxO missing beacons). The beacon script is
-- executed to claim collateral for another valid Active UTxO.
beaconFailure12 :: MonadEmulator m => m ()
beaconFailure12 = do
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
        , _maxConsecutiveMisses = Nothing
        , _collateralization = [(collateral1,Fraction(1,1_000_000))]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        , _correspondingAsk = Nothing
        }

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
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
          , Output
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

  invalidRef <- txOutRefWithValue $ utxoValue 4_000_001 mempty

  activeUTxOs <-
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
                , (_unAssetBeacon _assetBeacon,-1)
                , (_unBorrowerId _borrowerId,-1)
                , (_unLoanId _loanId,-2)
                ]
            , mintRedeemer = toRedeemer BurnKeyAndClaimDefaulted
            , mintPolicy = toVersionedMintingPolicy activeBeaconScript
            , mintReference = Just activeRef
            }

  currentSlot >>= awaitTime . slotToPosixTime . (+3600)
  claimSlot <- currentSlot

  -- Try to claim the collateral.
  void $ transact 
    lenderPersonalAddr 
    [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress]
    [lenderPayPrivKey]
    emptyTxParams
      { tokens = sampleKeyBurns activeUTxOs
      , inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer SpendWithKeyNFT)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          , Input
              { inputId = invalidRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer SpendWithKeyNFT)
              }
          ]
      , referenceInputs = [activeRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just claimSlot
          , validityRangeUpperBound = Nothing
          }
      }

-- | Without spending any UTxOs using the SpendWithKeyNFT spending redeemer, mint an Active beacon using
-- BurnKeyAndClaimDefaulted.
beaconFailure13 :: MonadEmulator m => m ()
beaconFailure13 = do
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
        , _maxConsecutiveMisses = Nothing
        , _collateralization = [(collateral1,Fraction(1,1_000_000))]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        , _correspondingAsk = Nothing
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

  currentSlot >>= awaitTime . slotToPosixTime . (+3600) . (+3600)
  unlockSlot <- currentSlot

  invalidUTxO <- txOutRefWithValue $ utxoValue 4_000_001 mempty

  -- Try to claim the UTxO.
  void $ transact 
    borrowerPersonalAddr 
    [loanAddress,refScriptAddress] 
    [borrowerPayPrivKey]
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = [ ("Active",1) ]
              , mintRedeemer = toRedeemer BurnKeyAndClaimDefaulted
              , mintPolicy = toVersionedMintingPolicy activeBeaconScript
              , mintReference = Just activeRef
              }
          ]
      , inputs = [ ]
      , referenceInputs = [loanRef,activeRef]
      , extraKeyWitnesses = [borrowerPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just unlockSlot
          , validityRangeUpperBound = Nothing
          }
      }

-------------------------------------------------
-- UTxO Type Failure
-------------------------------------------------
-- | Use SpendWithKeyNFT to claim a non-active UTxO.
typeFailure1 :: MonadEmulator m => m ()
typeFailure1 = do
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
        , _maxConsecutiveMisses = Nothing
        , _collateralization = [(collateral1,Fraction(1,1_000_000))]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        , _correspondingAsk = Nothing
        }

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 
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
              , outputValue = utxoValue 3_000_001 $ mconcat
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

  otherRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_001 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        ]

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

  finishedUTxO <-
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      loanAddress 
      (activeBeaconCurrencySymbol,"Active")

  keyUTxOs <- fmap concat $ forM finishedUTxO $ \(_,Just ad@ActiveDatum{_loanId}) ->
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      (toCardanoApiAddress lenderAddr) 
      (activeBeaconCurrencySymbol,_unLoanId _loanId)

  let sampleKeyBurns acs = flip map acs $
        \(_,Just ad@ActiveDatum{..}) ->
          TokenMint
            { mintTokens = 
                [ ("Active",-1)
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
  void $ transact 
    lenderPersonalAddr 
    [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress]
    [lenderPayPrivKey]
    emptyTxParams
      { tokens = sampleKeyBurns finishedUTxO
      , inputs = flip concatMap (zip finishedUTxO keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer SpendWithKeyNFT)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          , Input
              { inputId = otherRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer SpendWithKeyNFT)
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
-- Default Status Failures
-------------------------------------------------
-- | Try to terminate a loan early even though _maxConsecutiveMisses has not been exceeded.
defaultStatusFailure1 :: MonadEmulator m => m ()
defaultStatusFailure1 = do
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
        , _epochDuration = Just 1_000
        , _loanTerm = 100_000
        , _loanInterest = Fraction (1,10)
        , _compoundingInterest = True
        , _minPayment = 1_000_000
        , _penalty = NoPenalty
        , _maxConsecutiveMisses = Just 3
        , _collateralization = [(collateral1,Fraction(1,1_000_000))]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        , _correspondingAsk = Nothing
        }

  -- Initialize scenario
  References{negotiationRef,activeRef,loanRef,proxyRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet1 10_000_000 [("TestToken1",1000)]

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
        , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon1) 1
        , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
        ]

  let activeDatum = 
        createAcceptanceDatumFromOffer borrowerCred1 offerRef (slotToPosixTime startSlot) offerDatum
      loanIdBeacon = genLoanId offerRef

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
              { outputAddress = loanAddress1
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId borrowerBeacon1) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr1
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId loanIdBeacon) 1 ]
              , outputDatum = OutputDatum $ toDatum $ 
                  PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanIdBeacon)
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

  activeUTxOs <-
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      loanAddress1 
      (activeBeaconCurrencySymbol,"Active")

  keyUTxOs <- fmap concat $ forM activeUTxOs $ \(_,Just ad@ActiveDatum{_loanId}) ->
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      (toCardanoApiAddress lenderAddr1) 
      (activeBeaconCurrencySymbol,_unLoanId _loanId)

  let sampleKeyBurns acs = flip map acs $
        \(_,Just ad@ActiveDatum{..}) ->
          TokenMint
            { mintTokens = 
                [ ("Active",-1)
                , (_unBorrowerId _borrowerId,-1)
                , (_unAssetBeacon _assetBeacon,-1)
                , (_unLoanId loanIdBeacon,-2)
                ]
            , mintRedeemer = toRedeemer BurnKeyAndClaimDefaulted
            , mintPolicy = toVersionedMintingPolicy activeBeaconScript
            , mintReference = Just activeRef
            }

  claimSlot <- currentSlot

  -- Try to claim the collateral.
  void $ transact lenderPersonalAddr1 [toCardanoApiAddress lenderAddr1,loanAddress1,refScriptAddress] [lenderPayPrivKey1] $
    emptyTxParams
      { tokens = sampleKeyBurns activeUTxOs
      , inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer SpendWithKeyNFT)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , referenceInputs = [activeRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey1]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just claimSlot
          , validityRangeUpperBound = Nothing
          }
      }

-------------------------------------------------
-- TestTree
-------------------------------------------------
-- | A `TestTree` containing all failure scenarios for claiming expired collateral.
tests :: [TestTree]
tests =
  [ -- Time Failures
    scriptMustFailWithError "timeFailure1" 
      "invalid-before not specified"
      timeFailure1
  , scriptMustFailWithError "timeFailure2" 
      "Loan is not in default"
      timeFailure2

    -- Beacon Failures
  , scriptMustFailWithError "beaconFailure1" 
      "The wrong active beacons were burned"
      beaconFailure1
  , scriptMustFailWithError "beaconFailure2" 
      "The wrong active beacons were burned"
      beaconFailure2
  , scriptMustFailWithError "beaconFailure3" 
      "The wrong active beacons were burned"
      beaconFailure3
  , scriptMustFailWithError "beaconFailure4" 
      "The wrong active beacons were burned"
      beaconFailure4
  , scriptMustFailWithError "beaconFailure5" 
      "The wrong active beacons were burned"
      beaconFailure5
  , scriptMustFailWithError "beaconFailure6" 
      "The wrong active beacons were burned"
      beaconFailure6
  , scriptMustFailWithError "beaconFailure7" 
      "The wrong active beacons were burned"
      beaconFailure7
  , scriptMustFailWithError "beaconFailure8" 
      "The wrong active beacons were burned"
      beaconFailure8
  , scriptMustFailWithError "beaconFailure9" 
      "The wrong active beacons were burned"
      beaconFailure9
  , scriptMustFailWithError "beaconFailure10" 
      "The wrong active beacons were burned"
      beaconFailure10
  , scriptMustFailWithError "beaconFailure11" 
      "Active beacon script not executed with proper redeemer"
      beaconFailure11
  , scriptMustFailWithError "beaconFailure12" 
      "Invalid loan UTxO among inputs"
      beaconFailure12
  , scriptMustFailWithError "beaconFailure13" 
      "The wrong active beacons were burned"
      beaconFailure13

    -- UTxO Type Failures
  , scriptMustFailWithError "typeFailure1" 
      "UTxO is not an Active UTxO"
      typeFailure1

    -- Default Status Failrues
  , scriptMustFailWithError "defaultStatusFailure1" 
      "Loan is not in default"
      defaultStatusFailure1
  ]
