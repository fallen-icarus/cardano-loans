{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.OfferUTxOs.CloseOffer.Failures where

import qualified Ledger.Address as LA
import qualified PlutusLedgerApi.V2 as PV2
import qualified Ledger.CardanoWallet as Mock 
import Test.Tasty (TestTree)

import CardanoLoans

import Test.Prelude

-------------------------------------------------
-- Beacon Failures
-------------------------------------------------
-- | Close a single valid Offer UTxO but withdraw the beacons instead of burning them. The
-- negotiation beacon script is not executed.
beaconFailure1 :: MonadEmulator m => m ()
beaconFailure1 = do
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
  mintTestTokens lenderWallet 10_000_000 [("TestToken1",1000)]

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
              , outputDatum = OutputDatum $ toDatum loanDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

  offerUTxOs <- txOutRefsAndDatumsAtAddress @OfferDatum loanAddress

  -- Try to close the Offer UTxOs.
  void $ transact lenderPersonalAddr [loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens = []
      , inputs = flip map offerUTxOs $ \(offerRef,_) ->
          Input
            { inputId = offerRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer CloseOrUpdateOffer)
            }
      , referenceInputs = [negotiationRef,loanRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

-- | Close a single valid Offer UTxO but only burn the Offer beacon. Withdraw the other two
-- beacons.
beaconFailure2 :: MonadEmulator m => m ()
beaconFailure2 = do
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
  mintTestTokens lenderWallet 10_000_000 [("TestToken1",1000)]

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
              , outputDatum = OutputDatum $ toDatum loanDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

  offerUTxOs <- txOutRefsAndDatumsAtAddress @OfferDatum loanAddress

  -- Try to close the Offer UTxOs.
  void $ transact lenderPersonalAddr [loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens = flip map offerUTxOs $ const
         TokenMint
            { mintTokens = 
                [ ("Offer",-1)
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

-- | Close a single valid Offer UTxO but only burn the LenderId. Withdraw the other two beacons.
beaconFailure3 :: MonadEmulator m => m ()
beaconFailure3 = do
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
  mintTestTokens lenderWallet 10_000_000 [("TestToken1",1000)]

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
              , outputDatum = OutputDatum $ toDatum loanDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

  offerUTxOs <- txOutRefsAndDatumsAtAddress @OfferDatum loanAddress

  -- Try to close the Offer UTxOs.
  void $ transact lenderPersonalAddr [loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens = flip map offerUTxOs $ const
         TokenMint
            { mintTokens = 
                [ (_unLenderId lenderBeacon,-1)
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

-- | Close a single valid Offer UTxO but only burn the asset beacon. Withdraw the other two 
-- beacons.
beaconFailure4 :: MonadEmulator m => m ()
beaconFailure4 = do
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
  mintTestTokens lenderWallet 10_000_000 [("TestToken1",1000)]

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
              , outputDatum = OutputDatum $ toDatum loanDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

  offerUTxOs <- txOutRefsAndDatumsAtAddress @OfferDatum loanAddress

  -- Try to close the Offer UTxOs.
  void $ transact lenderPersonalAddr [loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens = flip map offerUTxOs $ const
         TokenMint
            { mintTokens = 
                [ (_unAssetBeacon loanBeacon,-1)
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
-- Approval Failures
-------------------------------------------------
-- | The lender does not approve closing the valid Offer UTxO. The CreateCloseOrUpdateOffer
-- redeemer has the correct credential.
approvalFailure1 :: MonadEmulator m => m ()
approvalFailure1 = do
  let -- Borrower Info
      borrowerWallet = Mock.knownMockWallet 1
      borrowerPersonalAddr = Mock.mockWalletAddress borrowerWallet
      borrowerPayPrivKey = Mock.paymentPrivateKey borrowerWallet
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
  mintTestTokens lenderWallet 10_000_000 [("TestToken1",1000)]

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
              , outputDatum = OutputDatum $ toDatum loanDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

  offerUTxOs <- txOutRefsAndDatumsAtAddress @OfferDatum loanAddress

  -- Try to close the Offer UTxOs.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
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
      , extraKeyWitnesses = [borrowerPubKey]
      }

-- | The lender does not approve closing the valid Offer UTxO. The CreateCloseOrUpdateOffer
-- redeemer has the wrong credential.
approvalFailure2 :: MonadEmulator m => m ()
approvalFailure2 = do
  let -- Borrower Info
      borrowerWallet = Mock.knownMockWallet 1
      borrowerPersonalAddr = Mock.mockWalletAddress borrowerWallet
      borrowerPayPrivKey = Mock.paymentPrivateKey borrowerWallet
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
  mintTestTokens lenderWallet 10_000_000 [("TestToken1",1000)]

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
              , outputDatum = OutputDatum $ toDatum loanDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

  offerUTxOs <- txOutRefsAndDatumsAtAddress @OfferDatum loanAddress

  -- Try to close the Offer UTxOs.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = flip map offerUTxOs $ const
         TokenMint
            { mintTokens = 
                [ ("Offer",-1)
                , (_unAssetBeacon loanBeacon,-1)
                , (_unLenderId lenderBeacon,-1)
                ]
            , mintRedeemer = toRedeemer $ CreateCloseOrUpdateOffer borrowerCred
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
      , extraKeyWitnesses = [borrowerPubKey]
      }

-- | The borrower does not approve closing an invalid Offer UTxO located at their valid
-- loan address.
approvalFailure3 :: MonadEmulator m => m ()
approvalFailure3 = do
  let -- Borrower Info
      borrowerWallet = Mock.knownMockWallet 1
      borrowerPersonalAddr = Mock.mockWalletAddress borrowerWallet
      borrowerPayPrivKey = Mock.paymentPrivateKey borrowerWallet
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
  mintTestTokens lenderWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Offer UTxO.
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens = []
      , outputs =
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ uncurry PV2.singleton (_unAsset loanAsset) 10_000_000 ]
              , outputDatum = OutputDatum $ toDatum loanDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

  offerUTxOs <- txOutRefsAndDatumsAtAddress @OfferDatum loanAddress

  -- Try to close the Offer UTxOs.
  void $ transact lenderPersonalAddr [loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens = []
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
-- Spending Redeemer Failures
-------------------------------------------------
-- | Close a single valid Offer UTxO using the CloseOrUpdateAsk redeemer. The beacon script
-- is executed using CreateCloseOrUpdateAsk.
spendingRedeemerFailure1 :: MonadEmulator m => m ()
spendingRedeemerFailure1 = do
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

      -- Loan Info
      loanAsset = Asset (adaSymbol,adaToken)
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
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
        , _collateralization = [(collateral1,Fraction(1,1))]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef,loanRef} <- initializeReferenceScripts 
  mintTestTokens lenderWallet 10_000_000 [("TestToken1",1000)]

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
              , outputDatum = OutputDatum $ toDatum loanDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

  offerUTxOs <- txOutRefsAndDatumsAtAddress @OfferDatum loanAddress

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
            , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk lenderCred
            , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
            , mintReference = Just negotiationRef
            }
      , inputs = flip map offerUTxOs $ \(offerRef,_) ->
          Input
            { inputId = offerRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer CloseOrUpdateAsk)
            }
      , referenceInputs = [negotiationRef,loanRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

-- | Close a single valid Offer UTxO using the CloseOrUpdateAsk redeemer. The negotiation
-- beacon script is not executed.
spendingRedeemerFailure2 :: MonadEmulator m => m ()
spendingRedeemerFailure2 = do
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

      -- Loan Info
      loanAsset = Asset (adaSymbol,adaToken)
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
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
        , _collateralization = [(collateral1,Fraction(1,1))]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef,loanRef} <- initializeReferenceScripts 
  mintTestTokens lenderWallet 10_000_000 [("TestToken1",1000)]

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
              , outputDatum = OutputDatum $ toDatum loanDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

  offerUTxOs <- txOutRefsAndDatumsAtAddress @OfferDatum loanAddress

  -- Try to close the Offer UTxOs.
  void $ transact lenderPersonalAddr [loanAddress,refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens = []
      , inputs = flip map offerUTxOs $ \(offerRef,_) ->
          Input
            { inputId = offerRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer CloseOrUpdateAsk)
            }
      , referenceInputs = [negotiationRef,loanRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

-------------------------------------------------
-- TestTree
-------------------------------------------------
-- | A `TestTree` containing all failure scenarios for closing Offer UTxOs.
tests :: [TestTree]
tests =
  [ -- Beacon Failure Tests
    scriptMustFailWithError "beaconFailure1" 
      "Negotiation beacon script not executed with proper redeemer"
      beaconFailure1
  , scriptMustFailWithError "beaconFailure2" 
      "Offer UTxOs must have exactly three kinds of beacons"
      beaconFailure2
  , scriptMustFailWithError "beaconFailure3" 
      "Offer UTxOs must have exactly three kinds of beacons"
      beaconFailure3
  , scriptMustFailWithError "beaconFailure4" 
      "Offer UTxOs must have exactly three kinds of beacons"
      beaconFailure4

    -- Approval Failures
  , scriptMustFailWithError "approvalFailure1" 
      "Lender credential did not approve"
      approvalFailure1
  , scriptMustFailWithError "approvalFailure2" 
      "Negotiation beacon script not executed with proper redeemer"
      approvalFailure2
  , scriptMustFailWithError "approvalFailure3" 
      "Borrower did not approve"
      approvalFailure3

    -- Spending Redeemer Failures
  , scriptMustFailWithError "spendingRedeemerFailure1" 
      "UTxO is not an Ask UTxO"
      spendingRedeemerFailure1
  , scriptMustFailWithError "spendingRedeemerFailure2" 
      "UTxO is not an Ask UTxO"
      spendingRedeemerFailure2

  ]
