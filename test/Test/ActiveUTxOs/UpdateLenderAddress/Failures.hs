{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.ActiveUTxOs.UpdateLenderAddress.Failures where

import qualified Ledger.Address as LA
import qualified PlutusLedgerApi.V2 as PV2
import qualified Ledger.CardanoWallet as Mock 
import Test.Tasty (TestTree)
import Control.Monad (forM,forM_)
import Data.List (sortOn)

import CardanoLoans

import Test.Prelude

-------------------------------------------------
-- New Address Failures
-------------------------------------------------
-- | Update the address to use a non-proxy script address.
newAddressFailure1 :: MonadEmulator m => m ()
newAddressFailure1 = do
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
      newLenderAddr = PV2.Address (PV2.ScriptCredential $ scriptHash loanScript) Nothing

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
              , outputDatum = OutputDatum $ toDatum @ActiveDatum ad{_lenderAddress = newLenderAddr}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress newLenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ UpdateLenderAddress newLenderAddr 0)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
          }
      }

-- | Update the address to use a proxy script without staking.
newAddressFailure2 :: MonadEmulator m => m ()
newAddressFailure2 = do
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
        PV2.Address (PV2.ScriptCredential $ scriptHash proxyScript) Nothing

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
              , outputDatum = OutputDatum $ toDatum @ActiveDatum ad{_lenderAddress = newLenderAddr}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress newLenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ UpdateLenderAddress newLenderAddr 0)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
          }
      }

-- | When updating the address, the key NFT is not locked at the new address.
newAddressFailure3 :: MonadEmulator m => m ()
newAddressFailure3 = do
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
              , outputDatum = OutputDatum $ toDatum @ActiveDatum ad{_lenderAddress = newLenderAddr}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ UpdateLenderAddress newLenderAddr 0)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
          }
      }

-- | The address in the spending redeemer does not match the address used for the new
-- lender address.
newAddressFailure4 :: MonadEmulator m => m ()
newAddressFailure4 = do
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
              , outputDatum = OutputDatum $ toDatum @ActiveDatum ad{_lenderAddress = newLenderAddr}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress newLenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum $
                    toRedeemer $ UpdateLenderAddress lenderAddr 0
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
          }
      }

-------------------------------------------------
-- Key NFT Datum Failures
-------------------------------------------------
-- | Don't store the Key NFT with the required datum.
keyDatumFailure1 :: MonadEmulator m => m ()
keyDatumFailure1 = do
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
              , outputDatum = OutputDatum $ toDatum @ActiveDatum ad{_lenderAddress = newLenderAddr}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress newLenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
              , outputDatum = NoOutputDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ UpdateLenderAddress newLenderAddr 0)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
          }
      }

-- | Store the Key NFT with a datum hash.
keyDatumFailure2 :: MonadEmulator m => m ()
keyDatumFailure2 = do
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
              , outputDatum = OutputDatum $ toDatum @ActiveDatum ad{_lenderAddress = newLenderAddr}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress newLenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
              , outputDatum = 
                  OutputDatumHash $ datumHash $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ UpdateLenderAddress newLenderAddr 0)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
          }
      }

-- | Store the Key NFT with a datum with the wrong loan id.
keyDatumFailure3 :: MonadEmulator m => m ()
keyDatumFailure3 = do
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
              , outputDatum = OutputDatum $ toDatum @ActiveDatum ad{_lenderAddress = newLenderAddr}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress newLenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,"")
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ UpdateLenderAddress newLenderAddr 0)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
          }
      }

-- | Store the Key NFT with a datum with the wrong active beacon id.
keyDatumFailure4 :: MonadEmulator m => m ()
keyDatumFailure4 = do
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
              , outputDatum = OutputDatum $ toDatum @ActiveDatum ad{_lenderAddress = newLenderAddr}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress newLenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum ("",_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ UpdateLenderAddress newLenderAddr 0)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
          }
      }

-------------------------------------------------
-- Deposit Increase Failures
-------------------------------------------------
-- | The deposit increase is negative.
depositFailure1 :: MonadEmulator m => m ()
depositFailure1 = do
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

  keyUTxOs <- fmap concat $ forM activeUTxOs $ \(_,Just ad@ActiveDatum{_loanId}) ->
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      (toCardanoApiAddress lenderAddr) 
      (activeBeaconCurrencySymbol,_unLoanId _loanId)

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
              , outputDatum = OutputDatum $ toDatum @ActiveDatum ad{_lenderAddress = newLenderAddr}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress newLenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum $ toRedeemer $ 
                    UpdateLenderAddress newLenderAddr (-1_000_000)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
          }
      }

-- | The actual deposit increase does not match the redeemer. The actual deposit increase was less.
depositFailure2 :: MonadEmulator m => m ()
depositFailure2 = do
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

  let samplePayments acs = flip concatMap acs $ 
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_500_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum @ActiveDatum ad{_lenderAddress = newLenderAddr}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress newLenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum $
                    toRedeemer $ UpdateLenderAddress newLenderAddr 1_000_000
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
          }
      }

-- | The actual deposit increase does not match the redeemer. The actual deposit increase was more.
depositFailure3 :: MonadEmulator m => m ()
depositFailure3 = do
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

  let samplePayments acs = flip concatMap acs $ 
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 5_500_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum @ActiveDatum ad{_lenderAddress = newLenderAddr}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress newLenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum $
                    toRedeemer $ UpdateLenderAddress newLenderAddr 1_000_000
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
          }
      }

-------------------------------------------------
-- Approval Failures
-------------------------------------------------
-- | When updating a single lender address, the required Key NFT is not found among the inputs.
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
      newLenderAddr = 
        PV2.Address lenderCred Nothing

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
              , outputDatum = OutputDatum $ toDatum @ActiveDatum ad{_lenderAddress = newLenderAddr}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ UpdateLenderAddress newLenderAddr 0)
              }
          ]
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
          }
      }

-- | Update multiple lender payment addresses for loans from the same borrower, one of the required
-- Key NFTs is not in the transaction.
approvalFailure2 :: MonadEmulator m => m ()
approvalFailure2 = do
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
        , _loanTerm = 10000
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
  References{negotiationRef,activeRef,loanRef,addressUpdateObserverRef,proxyRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 900_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",3),(_unAssetBeacon loanBeacon,3)]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs = replicate 3
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
                  [ ("Offer",3)
                  , (_unAssetBeacon loanBeacon,3)
                  , (_unLenderId lenderBeacon,3)
                  ]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateOffer lenderCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs = replicate 3
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

  activeUTxOs <- take 3 <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      loanAddress 
      (activeBeaconCurrencySymbol,"Active")

  keyUTxOs <- fmap concat $ forM activeUTxOs $ \(_,Just ad@ActiveDatum{_loanId}) ->
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      (toCardanoApiAddress lenderAddr) 
      (activeBeaconCurrencySymbol,_unLoanId _loanId)

  let samplePayments acs = flip concatMap (zip acs [1::Int ..3]) $ 
        \((_,Just ad@ActiveDatum{..}),i) ->
          if i /= 3 then
            [ Output
                { outputAddress = loanAddress
                , outputValue = utxoValue 4_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                    , uncurry PV2.singleton (_unAsset collateral1) 10
                    ]
                , outputDatum = OutputDatum $ toDatum @ActiveDatum ad{_lenderAddress = newLenderAddr}
                , outputReferenceScript = toReferenceScript Nothing
                }
            , Output
                { outputAddress = toCardanoApiAddress newLenderAddr
                , outputValue = utxoValue 4_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
                , outputDatum = 
                    OutputDatum $ toDatum $ 
                      PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
                , outputReferenceScript = toReferenceScript Nothing
                }
            ]
          else
            [ Output
                { outputAddress = loanAddress
                , outputValue = utxoValue 4_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                    , uncurry PV2.singleton (_unAsset collateral1) 10
                    ]
                , outputDatum = OutputDatum $ toDatum @ActiveDatum ad{_lenderAddress = newLenderAddr}
                , outputReferenceScript = toReferenceScript Nothing
                }
            ]

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = init $ flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ UpdateLenderAddress newLenderAddr 0)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
          }
      }

-------------------------------------------------
-- Borrower Address Failures
-------------------------------------------------
-- | When updating multiple lender addresses for loans from different borrowers, the collateral
-- outputs are stored at the wrong borrower addresses. They were swapped.
loanAddressFailure1 :: MonadEmulator m => m ()
loanAddressFailure1 = do
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
      borrowerWallet2 = Mock.knownMockWallet 3
      borrowerPersonalAddr2 = Mock.mockWalletAddress borrowerWallet2
      borrowerPayPrivKey2 = Mock.paymentPrivateKey borrowerWallet2
      borrowerPubKey2 = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash borrowerWallet2
      borrowerCred2 = PV2.PubKeyCredential borrowerPubKey2
      borrowerBeacon2 = genBorrowerId borrowerCred2
      loanAddress2 = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash loanScript) 
                    (Just $ PV2.StakingHash borrowerCred2)

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
      askDatum1 = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred1
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _loanTerm = 3600
        , _collateral = [collateral1]
        }
      askDatum2 = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred2
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
        , _loanTerm = 10000
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
  References{negotiationRef,activeRef,loanRef,addressUpdateObserverRef,proxyRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet1 900_000_000 [("TestToken1",1000)]
  mintTestTokens borrowerWallet2 900_000_000 [("TestToken1",1000)]

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
      , outputs = replicate 1
          Output
            { outputAddress = loanAddress1
            , outputValue = utxoValue 3_000_000 $ mconcat
                [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                , uncurry PV2.singleton (_unAsset collateral1) 1
                ]
            , outputDatum = OutputDatum $ toDatum askDatum1
            , outputReferenceScript = toReferenceScript Nothing
            }
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
      , outputs = replicate 1
          Output
            { outputAddress = loanAddress2
            , outputValue = utxoValue 3_000_000 $ mconcat
                [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                , uncurry PV2.singleton (_unAsset collateral1) 1
                ]
            , outputDatum = OutputDatum $ toDatum askDatum2
            , outputReferenceScript = toReferenceScript Nothing
            }
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

  askUTxOs1 <-
    txOutRefsAndDatumsAtAddressWithBeacon @AskDatum 
      loanAddress1
      (negotiationBeaconCurrencySymbol,"Ask")

  offerUTxOs1 <- 
    txOutRefsAndDatumsAtAddressWithBeacon @OfferDatum
      loanAddress1
      (negotiationBeaconCurrencySymbol,"Offer")

  askUTxOs2 <-
    txOutRefsAndDatumsAtAddressWithBeacon @AskDatum 
      loanAddress2
      (negotiationBeaconCurrencySymbol,"Ask")

  offerUTxOs2 <- 
    txOutRefsAndDatumsAtAddressWithBeacon @OfferDatum
      loanAddress2
      (negotiationBeaconCurrencySymbol,"Offer")

  let sampleBurns os as = concat $ (flip . flip zipWith) os as $
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

      sampleOutputs borrowerCred loanAddress start os = flip concatMap os $ 
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
  forM_ (zip (grouped 5 offerUTxOs1) (grouped 5 askUTxOs1)) $ \(offers,asks) -> do
      startSlot <- currentSlot

      transact borrowerPersonalAddr1 [loanAddress1,refScriptAddress] [borrowerPayPrivKey1] $
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
          , outputs = sampleOutputs borrowerCred1 loanAddress1 startSlot offers
          , referenceInputs = [negotiationRef,activeRef,loanRef]
          , extraKeyWitnesses = [borrowerPubKey1]
          , validityRange = ValidityRange
              { validityRangeLowerBound = Just startSlot
              , validityRangeUpperBound = Nothing
              }
          }

  -- Accept the offers.
  forM_ (zip (grouped 5 offerUTxOs2) (grouped 5 askUTxOs2)) $ \(offers,asks) -> do
      startSlot <- currentSlot

      transact borrowerPersonalAddr2 [loanAddress2,refScriptAddress] [borrowerPayPrivKey2] $
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
          , outputs = sampleOutputs borrowerCred2 loanAddress2 startSlot offers
          , referenceInputs = [negotiationRef,activeRef,loanRef]
          , extraKeyWitnesses = [borrowerPubKey2]
          , validityRange = ValidityRange
              { validityRangeLowerBound = Just startSlot
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

  keyUTxOs1 <- fmap concat $ forM activeUTxOs1 $ \(_,Just ad@ActiveDatum{_loanId}) ->
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      (toCardanoApiAddress lenderAddr) 
      (activeBeaconCurrencySymbol,_unLoanId _loanId)

  keyUTxOs2 <- fmap concat $ forM activeUTxOs2 $ \(_,Just ad@ActiveDatum{_loanId}) ->
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      (toCardanoApiAddress lenderAddr) 
      (activeBeaconCurrencySymbol,_unLoanId _loanId)

  let activeUTxOs = sortOn (fst . fst) $ 
        (map (,loanAddress2) activeUTxOs1) <> (map (,loanAddress1) activeUTxOs2)
      keyUTxOs = keyUTxOs1 <> keyUTxOs2
      samplePayments acs = flip concatMap acs $ 
        \((_,Just ad@ActiveDatum{..}),loanAddress) ->
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum @ActiveDatum ad{_lenderAddress = newLenderAddr}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress newLenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact 
    lenderPersonalAddr 
    [toCardanoApiAddress lenderAddr,loanAddress1,loanAddress2,refScriptAddress] 
    [lenderPayPrivKey]
    emptyTxParams
      { inputs = mconcat
          [ flip map activeUTxOs $ \((activeUtxoRef,_),_) ->
              Input
                { inputId = activeUtxoRef
                , inputWitness = 
                    SpendWithPlutusReference loanRef InlineDatum $ toRedeemer $ 
                      UpdateLenderAddress newLenderAddr 0
                }
          , flip map keyUTxOs $ \(keyRef,_) ->
              Input
                { inputId = keyRef
                , inputWitness = 
                    SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
                }
          ]
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
          }
      }

-------------------------------------------------
-- Collateral Value Failures
-------------------------------------------------
-- | Take some of the collateral.
collateralValueFailure1 :: MonadEmulator m => m ()
collateralValueFailure1 = do
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

  let samplePayments acs = flip concatMap acs $ 
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 5
                  ]
              , outputDatum = OutputDatum $ toDatum @ActiveDatum ad{_lenderAddress = newLenderAddr}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress newLenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ UpdateLenderAddress newLenderAddr 0)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
          }
      }

-- | Take the Active beacon.
collateralValueFailure2 :: MonadEmulator m => m ()
collateralValueFailure2 = do
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

  let samplePayments acs = flip concatMap acs $ 
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 0
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum @ActiveDatum ad{_lenderAddress = newLenderAddr}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress newLenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ UpdateLenderAddress newLenderAddr 0)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
          }
      }

-- | Take the Asset beacon.
collateralValueFailure3 :: MonadEmulator m => m ()
collateralValueFailure3 = do
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

  let samplePayments acs = flip concatMap acs $ 
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 0
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum @ActiveDatum ad{_lenderAddress = newLenderAddr}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress newLenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ UpdateLenderAddress newLenderAddr 0)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
          }
      }

-- | Take the BorrowerId.
collateralValueFailure4 :: MonadEmulator m => m ()
collateralValueFailure4 = do
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

  let samplePayments acs = flip concatMap acs $ 
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 0
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum @ActiveDatum ad{_lenderAddress = newLenderAddr}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress newLenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ UpdateLenderAddress newLenderAddr 0)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
          }
      }

-- | Take the LoanId.
collateralValueFailure5 :: MonadEmulator m => m ()
collateralValueFailure5 = do
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

  let samplePayments acs = flip concatMap acs $ 
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 0
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum @ActiveDatum ad{_lenderAddress = newLenderAddr}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress newLenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ UpdateLenderAddress newLenderAddr 0)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
          }
      }

-- | The collateral input does not have a BorrowerId. It is a finished loan.
collateralValueFailure6 :: MonadEmulator m => m ()
collateralValueFailure6 = do
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
              { mintTokens = [(_unBorrowerId borrowerBeacon,-1)]
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

  newActiveUTxOs <-
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      loanAddress 
      (activeBeaconCurrencySymbol,"Active")

  keyUTxOs <- fmap concat $ forM newActiveUTxOs $ \(_,Just ad@ActiveDatum{_loanId}) ->
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      (toCardanoApiAddress lenderAddr) 
      (activeBeaconCurrencySymbol,_unLoanId _loanId)

  let sampleUpdates acs = flip concatMap acs $ 
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                  ]
              , outputDatum = OutputDatum $ toDatum @ActiveDatum ad{_lenderAddress = newLenderAddr}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress newLenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip newActiveUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ UpdateLenderAddress newLenderAddr 0)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = sampleUpdates newActiveUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
          }
      }

-- | Instead of storing the Key NFT at the new address, store it inside the collateral UTxO. The
-- collateral output is first in the list of outputs.
collateralValueFailure7 :: MonadEmulator m => m ()
collateralValueFailure7 = do
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

  let samplePayments acs = flip concatMap acs $ 
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 2
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum @ActiveDatum ad{_lenderAddress = newLenderAddr}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress newLenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 0]
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ UpdateLenderAddress newLenderAddr 0)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
          }
      }

-- | Instead of storing the LoanId with the collateral UTxO, store it with the Key NFT. The
-- collateral output is first in the list of outputs.
collateralValueFailure8 :: MonadEmulator m => m ()
collateralValueFailure8 = do
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

  let samplePayments acs = flip concatMap acs $ 
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 0
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum @ActiveDatum ad{_lenderAddress = newLenderAddr}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress newLenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 2]
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ UpdateLenderAddress newLenderAddr 0)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
          }
      }

-- | Instead of storing the LoanId with the collateral UTxO, store it with the Key NFT. The
-- collateral output is second in the list of outputs.
collateralValueFailure9 :: MonadEmulator m => m ()
collateralValueFailure9 = do
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

  let samplePayments acs = flip concatMap acs $ 
        \(_,Just ad@ActiveDatum{..}) ->
          [ Output
              { outputAddress = toCardanoApiAddress newLenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 2]
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                  , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 0
                  , uncurry PV2.singleton (_unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum @ActiveDatum ad{_lenderAddress = newLenderAddr}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ UpdateLenderAddress newLenderAddr 0)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
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
      newLenderAddr = 
        PV2.Address lenderCred Nothing

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
              , outputDatum = OutputDatum $ toDatum @ActiveDatum 
                  ad{ _lenderAddress = newLenderAddr
                    , _activeBeaconId = ActiveBeaconId ""
                    }
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress newLenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ UpdateLenderAddress newLenderAddr 0)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
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
      newLenderAddr = 
        PV2.Address lenderCred Nothing

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
              , outputDatum = OutputDatum $ toDatum @ActiveDatum 
                  ad{ _lenderAddress = newLenderAddr
                    , _borrowerId = BorrowerId ""
                    }
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress newLenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ UpdateLenderAddress newLenderAddr 0)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
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
      newLenderAddr = 
        PV2.Address lenderCred Nothing

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
              , outputDatum = OutputDatum $ toDatum @ActiveDatum 
                  ad{ _lenderAddress = lenderAddr
                    }
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress newLenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ UpdateLenderAddress newLenderAddr 0)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
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
      newLenderAddr = 
        PV2.Address lenderCred Nothing

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
              , outputDatum = OutputDatum $ toDatum @ActiveDatum 
                  ad{ _lenderAddress = lenderAddr
                    , _loanAsset = collateral1
                    }
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress newLenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ UpdateLenderAddress newLenderAddr 0)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
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
      newLenderAddr = 
        PV2.Address lenderCred Nothing

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
              , outputDatum = OutputDatum $ toDatum @ActiveDatum 
                  ad{ _lenderAddress = lenderAddr
                    , _assetBeacon = AssetBeacon ""
                    }
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress newLenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ UpdateLenderAddress newLenderAddr 0)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
          }
      }

-- | The collateral datum has the wrong loan principle.
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
      newLenderAddr = 
        PV2.Address lenderCred Nothing

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
              , outputDatum = OutputDatum $ toDatum @ActiveDatum 
                  ad{ _lenderAddress = lenderAddr
                    , _loanPrinciple = 0
                    }
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress newLenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ UpdateLenderAddress newLenderAddr 0)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
          }
      }

-- | The collateral datum has the wrong compound frequency.
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
      newLenderAddr = 
        PV2.Address lenderCred Nothing

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
              , outputDatum = OutputDatum $ toDatum @ActiveDatum 
                  ad{ _lenderAddress = lenderAddr
                    , _compoundFrequency = Just 999
                    }
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress newLenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ UpdateLenderAddress newLenderAddr 0)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
          }
      }

-- | The collateral datum has the wrong last compounding.
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
      newLenderAddr = 
        PV2.Address lenderCred Nothing

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
              , outputDatum = OutputDatum $ toDatum @ActiveDatum 
                  ad{ _lenderAddress = lenderAddr
                    , _lastCompounding = 999
                    }
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress newLenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ UpdateLenderAddress newLenderAddr 0)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
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
      newLenderAddr = 
        PV2.Address lenderCred Nothing

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
              , outputDatum = OutputDatum $ toDatum @ActiveDatum 
                  ad{ _lenderAddress = lenderAddr
                    , _loanTerm = 0
                    }
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress newLenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ UpdateLenderAddress newLenderAddr 0)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
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
      newLenderAddr = 
        PV2.Address lenderCred Nothing

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
              , outputDatum = OutputDatum $ toDatum @ActiveDatum 
                  ad{ _lenderAddress = lenderAddr
                    , _loanInterest = Fraction (999,1)
                    }
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress newLenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ UpdateLenderAddress newLenderAddr 0)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
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
      newLenderAddr = 
        PV2.Address lenderCred Nothing

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
              , outputDatum = OutputDatum $ toDatum @ActiveDatum 
                  ad{ _lenderAddress = lenderAddr
                    , _minPayment = 999
                    }
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress newLenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ UpdateLenderAddress newLenderAddr 0)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
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
      newLenderAddr = 
        PV2.Address lenderCred Nothing

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
              , outputDatum = OutputDatum $ toDatum @ActiveDatum 
                  ad{ _lenderAddress = lenderAddr
                    , _collateralization = Collateralization []
                    }
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress newLenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ UpdateLenderAddress newLenderAddr 0)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
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
      newLenderAddr = 
        PV2.Address lenderCred Nothing

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
              , outputDatum = OutputDatum $ toDatum @ActiveDatum 
                  ad{ _lenderAddress = lenderAddr
                    , _collateralIsSwappable = True
                    }
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress newLenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ UpdateLenderAddress newLenderAddr 0)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
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
      newLenderAddr = 
        PV2.Address lenderCred Nothing

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
              , outputDatum = OutputDatum $ toDatum @ActiveDatum 
                  ad{ _lenderAddress = lenderAddr
                    , _claimExpiration = 9
                    }
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress newLenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ UpdateLenderAddress newLenderAddr 0)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
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
      newLenderAddr = 
        PV2.Address lenderCred Nothing

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
              , outputDatum = OutputDatum $ toDatum @ActiveDatum 
                  ad{ _lenderAddress = lenderAddr
                    , _loanExpiration = 9
                    }
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress newLenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ UpdateLenderAddress newLenderAddr 0)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
          }
      }

-- | The collateral datum has the wrong loan outstanding balance.
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
      newLenderAddr = 
        PV2.Address lenderCred Nothing

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
              , outputDatum = OutputDatum $ toDatum @ActiveDatum 
                  ad{ _lenderAddress = lenderAddr
                    , _loanOutstanding = Fraction (0,1)
                    }
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress newLenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ UpdateLenderAddress newLenderAddr 0)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
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
      newLenderAddr = 
        PV2.Address lenderCred Nothing

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
              , outputDatum = OutputDatum $ toDatum @ActiveDatum 
                  ad{ _lenderAddress = lenderAddr
                    , _loanId = LoanId ""
                    }
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress newLenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ UpdateLenderAddress newLenderAddr 0)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
          }
      }

-- | The collateral datum has the wrong payment observer hash.
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
      newLenderAddr = 
        PV2.Address lenderCred Nothing

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
              , outputDatum = OutputDatum $ toDatum @ActiveDatum 
                  ad{ _lenderAddress = lenderAddr
                    , _paymentObserverHash = ""
                    }
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress newLenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ UpdateLenderAddress newLenderAddr 0)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
          }
      }

-- | The collateral datum has the wrong interest observer hash.
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
      newLenderAddr = 
        PV2.Address lenderCred Nothing

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
              , outputDatum = OutputDatum $ toDatum @ActiveDatum 
                  ad{ _lenderAddress = lenderAddr
                    , _interestObserverHash = ""
                    }
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress newLenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ UpdateLenderAddress newLenderAddr 0)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
          }
      }

-- | The collateral datum has the wrong address update observer hash.
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
      newLenderAddr = 
        PV2.Address lenderCred Nothing

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
              , outputDatum = OutputDatum $ toDatum @ActiveDatum 
                  ad{ _lenderAddress = lenderAddr
                    , _addressUpdateObserverHash = ""
                    }
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress newLenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ UpdateLenderAddress newLenderAddr 0)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
          }
      }

-- | The collateral datum is a datum hash.
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
      newLenderAddr = 
        PV2.Address lenderCred Nothing

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
              , outputDatum = OutputDatumHash $ datumHash @ActiveDatum ad{_lenderAddress = newLenderAddr}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress newLenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ UpdateLenderAddress newLenderAddr 0)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
          }
      }

-- | The collateral datum is not an ActiveDatum.
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
      newLenderAddr = 
        PV2.Address lenderCred Nothing

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
              , outputDatum = OutputDatum $ toDatum askDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress newLenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ UpdateLenderAddress newLenderAddr 0)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
          }
      }

-- | The input does not have an ActiveDatum.
datumFailure23 :: MonadEmulator m => m ()
datumFailure23 = do
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
      loanAsset = Asset (adaSymbol,adaToken)
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _loanTerm = 3600
        , _collateral = [collateral1]
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
              , outputDatum = OutputDatum $ toDatum loanDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [borrowerPubKey]
      }

  askRef <- 
    txOutRefWithValue $ 
      utxoValue 3_000_000 $ mconcat
        [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        ]

  -- Try to close the Ask UTxO.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs =
          [ Input
              { inputId = askRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum $ toRedeemer $
                    UpdateLenderAddress (PV2.Address borrowerCred Nothing) 0
              }
          ]
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
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
      newLenderAddr = 
        PV2.Address lenderCred Nothing

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
              , outputDatum = OutputDatum $ toDatum @ActiveDatum 
                  ad{ _lenderAddress = lenderAddr
                    , _penalty = FixedFee 1000
                    }
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress newLenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ UpdateLenderAddress newLenderAddr 0)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
          }
      }

-- | The collateral datum has the wrong totalEpochPayments.
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
      newLenderAddr = 
        PV2.Address lenderCred Nothing

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
              , outputDatum = OutputDatum $ toDatum @ActiveDatum 
                  ad{ _lenderAddress = lenderAddr
                    , _totalEpochPayments = 1000
                    }
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress newLenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ UpdateLenderAddress newLenderAddr 0)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
          }
      }

-------------------------------------------------
-- Output Order Failures
-------------------------------------------------
-- | The collateral outputs are not in the same order as the inputs.
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
        , _loanTerm = 10000
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
  References{negotiationRef,activeRef,loanRef,addressUpdateObserverRef,proxyRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 900_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",3),(_unAssetBeacon loanBeacon,3)]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs = replicate 3
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
                  [ ("Offer",3)
                  , (_unAssetBeacon loanBeacon,3)
                  , (_unLenderId lenderBeacon,3)
                  ]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateOffer lenderCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs = replicate 3
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

  activeUTxOs <- take 3 <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      loanAddress 
      (activeBeaconCurrencySymbol,"Active")

  keyUTxOs <- fmap concat $ forM activeUTxOs $ \(_,Just ad@ActiveDatum{_loanId}) ->
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      (toCardanoApiAddress lenderAddr) 
      (activeBeaconCurrencySymbol,_unLoanId _loanId)

  let sampleCollateral acs = flip map acs $ 
        \(_,Just ad@ActiveDatum{..}) ->
          Output
            { outputAddress = loanAddress
            , outputValue = utxoValue 4_000_000 $ mconcat
                [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                , uncurry PV2.singleton (_unAsset collateral1) 10
                ]
            , outputDatum = OutputDatum $ toDatum @ActiveDatum ad{_lenderAddress = newLenderAddr}
            , outputReferenceScript = toReferenceScript Nothing
            }

  let sampleKeys acs = flip map acs $ 
        \(_,Just ad@ActiveDatum{..}) ->
          Output
            { outputAddress = toCardanoApiAddress newLenderAddr
            , outputValue = utxoValue 4_000_000 $ mconcat
                [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
            , outputDatum = 
                OutputDatum $ toDatum $ 
                  PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
            , outputReferenceScript = toReferenceScript Nothing
            }

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ UpdateLenderAddress newLenderAddr 0)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = mconcat
          [ reverse $ sampleCollateral activeUTxOs
          , sampleKeys activeUTxOs
          ]
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
          }
      }

-- | The key outputs are not in the same order as the inputs.
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
        , _loanTerm = 10000
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
  References{negotiationRef,activeRef,loanRef,addressUpdateObserverRef,proxyRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 900_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",3),(_unAssetBeacon loanBeacon,3)]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs = replicate 3
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
                  [ ("Offer",3)
                  , (_unAssetBeacon loanBeacon,3)
                  , (_unLenderId lenderBeacon,3)
                  ]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateOffer lenderCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs = replicate 3
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

  activeUTxOs <- take 3 <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      loanAddress 
      (activeBeaconCurrencySymbol,"Active")

  keyUTxOs <- fmap concat $ forM activeUTxOs $ \(_,Just ad@ActiveDatum{_loanId}) ->
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      (toCardanoApiAddress lenderAddr) 
      (activeBeaconCurrencySymbol,_unLoanId _loanId)

  let sampleCollateral acs = flip map acs $ 
        \(_,Just ad@ActiveDatum{..}) ->
          Output
            { outputAddress = loanAddress
            , outputValue = utxoValue 4_000_000 $ mconcat
                [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                , uncurry PV2.singleton (_unAsset collateral1) 10
                ]
            , outputDatum = OutputDatum $ toDatum @ActiveDatum ad{_lenderAddress = newLenderAddr}
            , outputReferenceScript = toReferenceScript Nothing
            }

  let sampleKeys acs = flip map acs $ 
        \(_,Just ad@ActiveDatum{..}) ->
          Output
            { outputAddress = toCardanoApiAddress newLenderAddr
            , outputValue = utxoValue 4_000_000 $ mconcat
                [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
            , outputDatum = 
                OutputDatum $ toDatum $ 
                  PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
            , outputReferenceScript = toReferenceScript Nothing
            }

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ UpdateLenderAddress newLenderAddr 0)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = mconcat
          [ sampleCollateral activeUTxOs
          , reverse $ sampleKeys activeUTxOs
          ]
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
          }
      }

-- | When updating multiple lender addresses, the first collateral output is invalid.
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
        , _loanTerm = 10000
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
  References{negotiationRef,activeRef,loanRef,addressUpdateObserverRef,proxyRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 900_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",3),(_unAssetBeacon loanBeacon,3)]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs = replicate 3
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
                  [ ("Offer",3)
                  , (_unAssetBeacon loanBeacon,3)
                  , (_unLenderId lenderBeacon,3)
                  ]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateOffer lenderCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs = replicate 3
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

  activeUTxOs <- take 2 <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      loanAddress 
      (activeBeaconCurrencySymbol,"Active")

  keyUTxOs <- fmap concat $ forM activeUTxOs $ \(_,Just ad@ActiveDatum{_loanId}) ->
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      (toCardanoApiAddress lenderAddr) 
      (activeBeaconCurrencySymbol,_unLoanId _loanId)

  let samplePayments acs = flip concatMap (zip acs [1::Int ..2]) $ 
        \((_,Just ad@ActiveDatum{..}),i) ->
          if odd i then 
            [ Output
                { outputAddress = loanAddress
                , outputValue = utxoValue 4_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                    , uncurry PV2.singleton (_unAsset collateral1) 5
                    ]
                , outputDatum = OutputDatum $ toDatum @ActiveDatum ad{_lenderAddress = newLenderAddr}
                , outputReferenceScript = toReferenceScript Nothing
                }
            , Output
                { outputAddress = toCardanoApiAddress newLenderAddr
                , outputValue = utxoValue 4_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
                , outputDatum = 
                    OutputDatum $ toDatum $ 
                      PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
                , outputReferenceScript = toReferenceScript Nothing
                }
            ]
          else
            [ Output
                { outputAddress = loanAddress
                , outputValue = utxoValue 4_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                    , uncurry PV2.singleton (_unAsset collateral1) 10
                    ]
                , outputDatum = OutputDatum $ toDatum @ActiveDatum ad{_lenderAddress = newLenderAddr}
                , outputReferenceScript = toReferenceScript Nothing
                }
            , Output
                { outputAddress = toCardanoApiAddress newLenderAddr
                , outputValue = utxoValue 4_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
                , outputDatum = 
                    OutputDatum $ toDatum $ 
                      PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
                , outputReferenceScript = toReferenceScript Nothing
                }
            ]

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ UpdateLenderAddress newLenderAddr 0)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
          }
      }

-- | When updating multiple lender addresses, the second collateral output is invalid.
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
        , _loanTerm = 10000
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
  References{negotiationRef,activeRef,loanRef,addressUpdateObserverRef,proxyRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 900_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",3),(_unAssetBeacon loanBeacon,3)]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs = replicate 3
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
                  [ ("Offer",3)
                  , (_unAssetBeacon loanBeacon,3)
                  , (_unLenderId lenderBeacon,3)
                  ]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateOffer lenderCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs = replicate 3
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

  activeUTxOs <- take 2 <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      loanAddress 
      (activeBeaconCurrencySymbol,"Active")

  keyUTxOs <- fmap concat $ forM activeUTxOs $ \(_,Just ad@ActiveDatum{_loanId}) ->
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      (toCardanoApiAddress lenderAddr) 
      (activeBeaconCurrencySymbol,_unLoanId _loanId)

  let samplePayments acs = flip concatMap (zip acs [1::Int ..2]) $ 
        \((_,Just ad@ActiveDatum{..}),i) ->
          if even i then 
            [ Output
                { outputAddress = loanAddress
                , outputValue = utxoValue 4_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                    , uncurry PV2.singleton (_unAsset collateral1) 5
                    ]
                , outputDatum = OutputDatum $ toDatum @ActiveDatum ad{_lenderAddress = newLenderAddr}
                , outputReferenceScript = toReferenceScript Nothing
                }
            , Output
                { outputAddress = toCardanoApiAddress newLenderAddr
                , outputValue = utxoValue 4_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
                , outputDatum = 
                    OutputDatum $ toDatum $ 
                      PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
                , outputReferenceScript = toReferenceScript Nothing
                }
            ]
          else
            [ Output
                { outputAddress = loanAddress
                , outputValue = utxoValue 4_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                    , uncurry PV2.singleton (_unAsset collateral1) 10
                    ]
                , outputDatum = OutputDatum $ toDatum @ActiveDatum ad{_lenderAddress = newLenderAddr}
                , outputReferenceScript = toReferenceScript Nothing
                }
            , Output
                { outputAddress = toCardanoApiAddress newLenderAddr
                , outputValue = utxoValue 4_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
                , outputDatum = 
                    OutputDatum $ toDatum $ 
                      PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
                , outputReferenceScript = toReferenceScript Nothing
                }
            ]

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ UpdateLenderAddress newLenderAddr 0)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
          }
      }

-- | When updating multiple lender addresses, the first key output is invalid.
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
      newLenderAddr = PV2.Address lenderCred Nothing

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
        , _loanTerm = 10000
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
  References{negotiationRef,activeRef,loanRef,addressUpdateObserverRef,proxyRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 900_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",3),(_unAssetBeacon loanBeacon,3)]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs = replicate 3
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
                  [ ("Offer",3)
                  , (_unAssetBeacon loanBeacon,3)
                  , (_unLenderId lenderBeacon,3)
                  ]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateOffer lenderCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs = replicate 3
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

  activeUTxOs <- take 2 <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      loanAddress 
      (activeBeaconCurrencySymbol,"Active")

  keyUTxOs <- fmap concat $ forM activeUTxOs $ \(_,Just ad@ActiveDatum{_loanId}) ->
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      (toCardanoApiAddress lenderAddr) 
      (activeBeaconCurrencySymbol,_unLoanId _loanId)

  let samplePayments acs = flip concatMap (zip acs [1::Int ..2]) $ 
        \((_,Just ad@ActiveDatum{..}),i) ->
          if odd i then 
            [ Output
                { outputAddress = loanAddress
                , outputValue = utxoValue 4_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                    , uncurry PV2.singleton (_unAsset collateral1) 10
                    ]
                , outputDatum = OutputDatum $ toDatum @ActiveDatum ad{_lenderAddress = newLenderAddr}
                , outputReferenceScript = toReferenceScript Nothing
                }
            , Output
                { outputAddress = toCardanoApiAddress newLenderAddr
                , outputValue = utxoValue 4_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
                , outputDatum = 
                    OutputDatum $ toDatum $ 
                      PaymentDatum (activeBeaconCurrencySymbol,"")
                , outputReferenceScript = toReferenceScript Nothing
                }
            ]
          else
            [ Output
                { outputAddress = loanAddress
                , outputValue = utxoValue 4_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                    , uncurry PV2.singleton (_unAsset collateral1) 10
                    ]
                , outputDatum = OutputDatum $ toDatum @ActiveDatum ad{_lenderAddress = newLenderAddr}
                , outputReferenceScript = toReferenceScript Nothing
                }
            , Output
                { outputAddress = toCardanoApiAddress newLenderAddr
                , outputValue = utxoValue 4_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
                , outputDatum = 
                    OutputDatum $ toDatum $ 
                      PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
                , outputReferenceScript = toReferenceScript Nothing
                }
            ]

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ UpdateLenderAddress newLenderAddr 0)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
          }
      }

-- | When updating multiple lender addresses, the second key output is invalid.
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
      newLenderAddr = PV2.Address lenderCred Nothing

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
        , _loanTerm = 10000
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
  References{negotiationRef,activeRef,loanRef,addressUpdateObserverRef,proxyRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 900_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",3),(_unAssetBeacon loanBeacon,3)]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs = replicate 3
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
                  [ ("Offer",3)
                  , (_unAssetBeacon loanBeacon,3)
                  , (_unLenderId lenderBeacon,3)
                  ]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateOffer lenderCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs = replicate 3
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

  activeUTxOs <- take 2 <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      loanAddress 
      (activeBeaconCurrencySymbol,"Active")

  keyUTxOs <- fmap concat $ forM activeUTxOs $ \(_,Just ad@ActiveDatum{_loanId}) ->
    txOutRefsAndDatumsAtAddressWithBeacon @ActiveDatum 
      (toCardanoApiAddress lenderAddr) 
      (activeBeaconCurrencySymbol,_unLoanId _loanId)

  let samplePayments acs = flip concatMap (zip acs [1::Int ..2]) $ 
        \((_,Just ad@ActiveDatum{..}),i) ->
          if even i then 
            [ Output
                { outputAddress = loanAddress
                , outputValue = utxoValue 4_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                    , uncurry PV2.singleton (_unAsset collateral1) 10
                    ]
                , outputDatum = OutputDatum $ toDatum @ActiveDatum ad{_lenderAddress = newLenderAddr}
                , outputReferenceScript = toReferenceScript Nothing
                }
            , Output
                { outputAddress = toCardanoApiAddress newLenderAddr
                , outputValue = utxoValue 4_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
                , outputDatum = 
                    OutputDatum $ toDatum $ 
                      PaymentDatum (activeBeaconCurrencySymbol,"")
                , outputReferenceScript = toReferenceScript Nothing
                }
            ]
          else
            [ Output
                { outputAddress = loanAddress
                , outputValue = utxoValue 4_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unBorrowerId _borrowerId) 1
                    , PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1
                    , uncurry PV2.singleton (_unAsset collateral1) 10
                    ]
                , outputDatum = OutputDatum $ toDatum @ActiveDatum ad{_lenderAddress = newLenderAddr}
                , outputReferenceScript = toReferenceScript Nothing
                }
            , Output
                { outputAddress = toCardanoApiAddress newLenderAddr
                , outputValue = utxoValue 4_000_000 $ mconcat
                    [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
                , outputDatum = 
                    OutputDatum $ toDatum $ 
                      PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
                , outputReferenceScript = toReferenceScript Nothing
                }
            ]

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ UpdateLenderAddress newLenderAddr 0)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
          }
      }

-------------------------------------------------
-- Time Failures
-------------------------------------------------
-- | Invalid-hereafter not specified.
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
              , outputDatum = OutputDatum $ toDatum @ActiveDatum ad{_lenderAddress = newLenderAddr}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress newLenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ UpdateLenderAddress newLenderAddr 0)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Nothing
          }
      }

-- | The loan being updated is already expired.
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
              , outputDatum = OutputDatum $ toDatum @ActiveDatum ad{_lenderAddress = newLenderAddr}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress newLenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  currentSlot >>= awaitTime . slotToPosixTime . (+3600)
  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ UpdateLenderAddress newLenderAddr 0)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer ObserveAddressUpdate
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
          }
      }

-------------------------------------------------
-- Observer Redeemer Failures
-------------------------------------------------
-- | Use the Register redeemer for the observer script.
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
      newLenderAddr = 
        PV2.Address lenderCred Nothing

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
              , outputDatum = OutputDatum $ toDatum @ActiveDatum ad{_lenderAddress = newLenderAddr}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress newLenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (_unLoanId _loanId) 1]
              , outputDatum = 
                  OutputDatum $ toDatum $ 
                    PaymentDatum (activeBeaconCurrencySymbol,_unLoanId _loanId)
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]

  updateSlot <- (1+) <$> currentSlot

  -- Try to update the lender's address.
  void $ transact lenderPersonalAddr [toCardanoApiAddress lenderAddr,loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { inputs = flip concatMap (zip activeUTxOs keyUTxOs) $ \((activeUtxoRef,_),(keyRef,_)) ->
          [ Input
              { inputId = activeUtxoRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer $ UpdateLenderAddress newLenderAddr 0)
              }
          , Input
              { inputId = keyRef
              , inputWitness = 
                  SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
              }
          ]
      , outputs = samplePayments activeUTxOs
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash addressUpdateObserverScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference addressUpdateObserverRef $ 
                    toRedeemer RegisterAddressUpdateObserverScript
              }
          ]
      , referenceInputs = [addressUpdateObserverRef,loanRef,proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Nothing
          , validityRangeUpperBound = Just updateSlot
          }
      }

-------------------------------------------------
-- TestTree
-------------------------------------------------
-- | A `TestTree` containing all failure scenarios for updating lender addresses.
tests :: [TestTree]
tests =
  [ -- New Address Failures
    scriptMustFailWithError "newAddressFailure1" 
      "Invalid new lender address"
      newAddressFailure1
  , scriptMustFailWithError "newAddressFailure2" 
      "Invalid new lender address"
      newAddressFailure2
  , scriptMustFailWithError "newAddressFailure3" 
      "Not all required outputs found"
      newAddressFailure3
  , scriptMustFailWithError "newAddressFailure4" 
      "Not all required outputs found"
      newAddressFailure4

    -- Key NFT Datum Failures
  , scriptMustFailWithError "keyDatumFailure1" 
      "Not all required outputs found"
      keyDatumFailure1
  , scriptMustFailWithError "keyDatumFailure2" 
      "Not all required outputs found"
      keyDatumFailure2
  , scriptMustFailWithError "keyDatumFailure3" 
      "Not all required outputs found"
      keyDatumFailure3
  , scriptMustFailWithError "keyDatumFailure4" 
      "Not all required outputs found"
      keyDatumFailure4

    -- Deposit Increaes Failures
  , scriptMustFailWithError "depositFailure1" 
      "Deposit increase not >= 0"
      depositFailure1
  , scriptMustFailWithError "depositFailure2" 
      "Collateral output has wrong value"
      depositFailure2
  , scriptMustFailWithError "depositFailure3" 
      "Collateral output has wrong value"
      depositFailure3

    -- Approval Failures
  , scriptMustFailWithError "approvalFailure1" 
      "Not all required outputs found"
      approvalFailure1
  , scriptMustFailWithError "approvalFailure2" 
      "Not all required outputs found"
      approvalFailure2

    -- Loan Address Failures
  , scriptMustFailWithError "loanAddressFailure1" 
      "Not all required outputs found"
      loanAddressFailure1

    -- Collateral Value Failures 
  , scriptMustFailWithError "collateralValueFailure1" 
      "Collateral output has wrong value"
      collateralValueFailure1
  , scriptMustFailWithError "collateralValueFailure2" 
      "Collateral output has wrong value"
      collateralValueFailure2
  , scriptMustFailWithError "collateralValueFailure3" 
      "Collateral output has wrong value"
      collateralValueFailure3
  , scriptMustFailWithError "collateralValueFailure4" 
      "Collateral output has wrong value"
      collateralValueFailure4
  , scriptMustFailWithError "collateralValueFailure5" 
      "Collateral output has wrong value"
      collateralValueFailure5
  , scriptMustFailWithError "collateralValueFailure6" 
      "Address update input missing Borrower Id"
      collateralValueFailure6
  , scriptMustFailWithError "collateralValueFailure7" 
      "Collateral output has wrong value"
      collateralValueFailure7
  , scriptMustFailWithError "collateralValueFailure8" 
      "Collateral output has wrong value"
      collateralValueFailure8
  , scriptMustFailWithError "collateralValueFailure9" 
      "Key output does not have exactly one Key NFT"
      collateralValueFailure9

    -- Datum Failure Tests
  , scriptMustFailWithError "datumFailure1" 
      "Not all required outputs found"
      datumFailure1
  , scriptMustFailWithError "datumFailure2" 
      "Not all required outputs found"
      datumFailure2
  , scriptMustFailWithError "datumFailure3" 
      "Not all required outputs found"
      datumFailure3
  , scriptMustFailWithError "datumFailure4" 
      "Not all required outputs found"
      datumFailure4
  , scriptMustFailWithError "datumFailure5" 
      "Not all required outputs found"
      datumFailure5
  , scriptMustFailWithError "datumFailure6" 
      "Not all required outputs found"
      datumFailure6
  , scriptMustFailWithError "datumFailure7" 
      "Not all required outputs found"
      datumFailure7
  , scriptMustFailWithError "datumFailure8" 
      "Not all required outputs found"
      datumFailure8
  , scriptMustFailWithError "datumFailure9" 
      "Not all required outputs found"
      datumFailure9
  , scriptMustFailWithError "datumFailure10" 
      "Not all required outputs found"
      datumFailure10
  , scriptMustFailWithError "datumFailure11" 
      "Not all required outputs found"
      datumFailure11
  , scriptMustFailWithError "datumFailure12" 
      "Not all required outputs found"
      datumFailure12
  , scriptMustFailWithError "datumFailure13" 
      "Not all required outputs found"
      datumFailure13
  , scriptMustFailWithError "datumFailure14" 
      "Not all required outputs found"
      datumFailure14
  , scriptMustFailWithError "datumFailure15" 
      "Not all required outputs found"
      datumFailure15
  , scriptMustFailWithError "datumFailure16" 
      "Not all required outputs found"
      datumFailure16
  , scriptMustFailWithError "datumFailure17" 
      "Not all required outputs found"
      datumFailure17
  , scriptMustFailWithError "datumFailure18" 
      "Not all required outputs found"
      datumFailure18
  , scriptMustFailWithError "datumFailure19" 
      "Not all required outputs found"
      datumFailure19
  , scriptMustFailWithError "datumFailure20" 
      "Not all required outputs found"
      datumFailure20
  , scriptMustFailWithError "datumFailure21" 
      "Not all required outputs found"
      datumFailure21
  , scriptMustFailWithError "datumFailure22" 
      "Not all required outputs found"
      datumFailure22
  , scriptMustFailWithError "datumFailure23" 
      "UTxO is not an Active UTxO"
      datumFailure23
  , scriptMustFailWithError "datumFailure24" 
      "Not all required outputs found"
      datumFailure24
  , scriptMustFailWithError "datumFailure25" 
      "Not all required outputs found"
      datumFailure25

    -- Output Order Failures
  , scriptMustFailWithError "orderFailure1" 
      "Not all required outputs found"
      orderFailure1
  , scriptMustFailWithError "orderFailure2" 
      "Not all required outputs found"
      orderFailure2
  , scriptMustFailWithError "orderFailure3" 
      "Collateral output has wrong value"
      orderFailure3
  , scriptMustFailWithError "orderFailure4" 
      "Collateral output has wrong value"
      orderFailure4
  , scriptMustFailWithError "orderFailure5" 
      "Not all required outputs found"
      orderFailure5
  , scriptMustFailWithError "orderFailure6" 
      "Not all required outputs found"
      orderFailure6

    -- Time Failures
  , scriptMustFailWithError "timeFailure1" 
      "invalid-hereafter not specified"
      timeFailure1
  , scriptMustFailWithError "timeFailure2" 
      "Loan is expired"
      timeFailure2

    -- Observer Redeemer Failures
  , scriptMustFailWithError "observerRedeemerFailure1" 
      "This redeemer can only be used to register the address update observer script"
      observerRedeemerFailure1
  ]
