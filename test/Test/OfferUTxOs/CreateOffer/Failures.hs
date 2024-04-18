{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.OfferUTxOs.CreateOffer.Failures where

import qualified Ledger.Address as LA
import qualified PlutusLedgerApi.V2 as PV2
import qualified Ledger.CardanoWallet as Mock 
import Test.Tasty (TestTree)

import CardanoLoans

import Test.Prelude

-- | Create the prefixed LenderId from the lender's staking credential. The prefixes are
-- swapped.
genWrongLenderId :: Credential -> LenderId
genWrongLenderId cred = LenderId $ TokenName $ case cred of
  PV2.PubKeyCredential (PV2.PubKeyHash pkh) -> unsafeToBuiltinByteString "01" <> pkh
  PV2.ScriptCredential (PV2.ScriptHash sh) -> unsafeToBuiltinByteString "00" <> sh

-------------------------------------------------
-- Beacon Failures
-------------------------------------------------
-- | Mint the Offer beacon, LenderId beacon, and asset beacon but withdraw them.
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Just 10
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 10
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
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
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

-- | Mint just the Offer beacon and withdraw it.
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Just 10
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 10
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",1)
                  ]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateOffer lenderCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

-- | Mint just the LenderId and withdraw it.
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Just 10
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 10
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ (_unLenderId lenderBeacon,1)
                  ]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateOffer lenderCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

-- | Mint just the Asset beacon and withdraw it.
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Just 10
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 10
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ (_unAssetBeacon loanBeacon,1)
                  ]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateOffer lenderCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

-- | Mint three completely unrelated tokens and withdraw them.
beaconFailure5 :: MonadEmulator m => m ()
beaconFailure5 = do
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Just 10
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 10
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Other1",1)
                  , ("Other2",1)
                  , ("Other3",1)
                  ]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateOffer lenderCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

-- | When creating a single valid Offer UTxO, mint an unrelated token and store it with the Offer
-- UTxO.
beaconFailure6 :: MonadEmulator m => m ()
beaconFailure6 = do
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Just 10
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 10
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLenderId lenderBeacon,1)
                  , ("Other",1)
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
                  , PV2.singleton negotiationBeaconCurrencySymbol "Other" 1
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

-- | Mint three unrelated tokens and store them at the DApp address with the proper datum.
beaconFailure7 :: MonadEmulator m => m ()
beaconFailure7 = do
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Just 10
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 10
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Other1",1)
                  , ("Other2",1)
                  , ("Other3",1)
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
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Other1" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol "Other2" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol "Other3" 1
                  , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

-- | Create an Offer UTxO with the correct datum, Offer beacon, and LenderId, but with the
-- wrong asset beacon. The asset beacon in the datum is correct.
beaconFailure8 :: MonadEmulator m => m ()
beaconFailure8 = do
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Just 10
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 10
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",1)
                  , ("Other",1)
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
                  , PV2.singleton negotiationBeaconCurrencySymbol "Other" 1
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

-- | Create an Offer UTxO with the correct datum, Offer beacon, and asset beacon, but with the
-- wrong LenderId. The LenderId in the datum is correct and corresponds to the lender credential
-- in the redeemer.
beaconFailure9 :: MonadEmulator m => m ()
beaconFailure9 = do
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Just 10
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 10
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",1)
                  , (_unAssetBeacon loanBeacon,1)
                  , ("Other",1)
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
                  , PV2.singleton negotiationBeaconCurrencySymbol "Other" 1
                  , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

-- | Create an Offer UTxO with the correct datum, LenderId, and asset beacon, but with the
-- wrong Offer beacon.
beaconFailure10 :: MonadEmulator m => m ()
beaconFailure10 = do
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Just 10
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 10
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Other",1)
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
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Other" 1
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

-- | Create an Offer UTxO with the correct datum, Offer beacon, and LenderId, but with the
-- wrong asset beacon. The asset beacon in the datum matches the one minted but the datum still
-- has the right loan asset.
beaconFailure11 :: MonadEmulator m => m ()
beaconFailure11 = do
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Just 10
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 10
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",1)
                  , ("Other",1)
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
                  , PV2.singleton negotiationBeaconCurrencySymbol "Other" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
                  , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
                  ]
              , outputDatum = OutputDatum $ 
                  toDatum @OfferDatum loanDatum{_assetBeacon = AssetBeacon "Other"}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

-- | Create an Offer UTxO with the correct datum, Offer beacon, and LenderId, but with the
-- wrong asset beacon. The LenderId in the datum matches the one minted but the redeemer 
-- has the correct credential.
beaconFailure12 :: MonadEmulator m => m ()
beaconFailure12 = do
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Just 10
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 10
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",1)
                  , (_unAssetBeacon loanBeacon,1)
                  , ("Other",1)
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
                  , PV2.singleton negotiationBeaconCurrencySymbol "Other" 1
                  , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
                  ]
              , outputDatum = OutputDatum $ 
                  toDatum @OfferDatum loanDatum{_lenderId = LenderId "Other"}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

-- | Mint one of the Offer and asset beacons but mint two of the LenderIds. Store them all
-- together in the new Offer UTxO.
beaconFailure13 :: MonadEmulator m => m ()
beaconFailure13 = do
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Just 10
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 10
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",1)
                  , (_unAssetBeacon loanBeacon,1)
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 2
                  , uncurry PV2.singleton (_unAsset loanAsset) 10_000_000
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

-- | Mint one of the LenderId and asset beacons but mint two of the Offer beacons. Store them all
-- together in the new Offer UTxO.
beaconFailure14 :: MonadEmulator m => m ()
beaconFailure14 = do
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Just 10
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 10
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",2)
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
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 2
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

-- | Mint one of the LenderId and Offer beacons but mint two of the asset beacons. Store them all
-- together in the new Offer UTxO.
beaconFailure15 :: MonadEmulator m => m ()
beaconFailure15 = do
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Just 10
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 10
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",1)
                  , (_unAssetBeacon loanBeacon,2)
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 2
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

-- | The LenderId name is prefixed with "01" even though it is a pubkey. The datum also has the
-- wrong lenderId name.
beaconFailure16 :: MonadEmulator m => m ()
beaconFailure16 = do
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
      lenderBeacon = genWrongLenderId lenderCred
      lenderAddr = 
        PV2.Address (PV2.ScriptCredential $ scriptHash proxyScript) 
                    (Just $ PV2.StakingHash lenderCred)

      -- Loan Info
      loanAsset = Asset (adaSymbol,adaToken)
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Just 10
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 10
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
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
              , outputDatum = OutputDatum $ 
                  toDatum @OfferDatum loanDatum{_lenderId = lenderBeacon}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

-- | The LenderId name is prefixed with "01" even though it is a pubkey. The datum has the correct
-- LenderId name.
beaconFailure17 :: MonadEmulator m => m ()
beaconFailure17 = do
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
      lenderBeacon = genWrongLenderId lenderCred
      lenderAddr = 
        PV2.Address (PV2.ScriptCredential $ scriptHash proxyScript) 
                    (Just $ PV2.StakingHash lenderCred)

      -- Loan Info
      loanAsset = Asset (adaSymbol,adaToken)
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Just 10
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 10
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
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

-- | The loan asset is one of the three required beacons.
beaconFailure18 :: MonadEmulator m => m ()
beaconFailure18 = do
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
      loanAsset = Asset (negotiationBeaconCurrencySymbol,"Offer")
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 1
        , _compoundFrequency = Just 10
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 10
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
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
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

-------------------------------------------------
-- Approval Failures
-------------------------------------------------
-- | The lender did not approve the transaction. The redeemer has the credential that should
-- approve.
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Just 10
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 10
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
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
      , extraKeyWitnesses = [borrowerPubKey]
      }

-- | The lender did not approve the transaction. The redeemer has the wrong credential.
approvalFailure2 :: MonadEmulator m => m ()
approvalFailure2 = do
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Just 10
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 10
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",1)
                  , (_unAssetBeacon loanBeacon,1)
                  , (_unLenderId lenderBeacon,1)
                  ]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateOffer borrowerCred
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

-------------------------------------------------
-- Datum Failures
-------------------------------------------------
-- | Mint the Offer beacon, LenderId, and asset beacon but store them at the DApp address
-- without a datum.
datumFailure1 :: MonadEmulator m => m ()
datumFailure1 = do
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Just 10
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 10
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
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
              , outputDatum = NoOutputDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

-- | Mint the Offer beacon, LenderId, and asset beacon but store them at the DApp address
-- with a datum hash.
datumFailure2 :: MonadEmulator m => m ()
datumFailure2 = do
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Just 10
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 10
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
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
              , outputDatum = OutputDatumHash $ datumHash loanDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

-- | The Offer UTxO does not have an OfferDatum.
datumFailure3 :: MonadEmulator m => m ()
datumFailure3 = do
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _loanTerm = 3600
        , _collateral = [collateral1]
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
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

-- | The OfferDatum has the wrong negotiation beacon id.
datumFailure4 :: MonadEmulator m => m ()
datumFailure4 = do
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Just 10
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 10
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
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
              , outputDatum = OutputDatum $ 
                  toDatum @OfferDatum loanDatum{_negotiationBeaconId = NegotiationBeaconId ""}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

-- | The OfferDatum has the wrong active beacon id.
datumFailure5 :: MonadEmulator m => m ()
datumFailure5 = do
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Just 10
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 10
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
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
              , outputDatum = OutputDatum $ 
                  toDatum @OfferDatum loanDatum{_activeBeaconId = ActiveBeaconId ""}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

-- | The OfferDatum has the wrong lender id.
datumFailure6 :: MonadEmulator m => m ()
datumFailure6 = do
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Just 10
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 10
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
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
              , outputDatum = OutputDatum $ 
                  toDatum @OfferDatum loanDatum{_lenderId = LenderId ""}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

-- | The OfferDatum has a lender address using a script that is not the proxy script.
datumFailure7 :: MonadEmulator m => m ()
datumFailure7 = do
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
        PV2.Address (PV2.ScriptCredential $ scriptHash loanScript) 
                    (Just $ PV2.StakingHash lenderCred)

      -- Loan Info
      loanAsset = Asset (adaSymbol,adaToken)
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Just 10
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 10
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
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

-- | The OfferDatum has a lender address using the proxy script but without a staking credential.
datumFailure8 :: MonadEmulator m => m ()
datumFailure8 = do
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
                    Nothing

      -- Loan Info
      loanAsset = Asset (adaSymbol,adaToken)
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Just 10
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 10
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
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

-- | The OfferDatum has the wrong loan asset.
datumFailure9 :: MonadEmulator m => m ()
datumFailure9 = do
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Just 10
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 10
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
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
              , outputDatum = OutputDatum $ 
                  toDatum @OfferDatum loanDatum{_loanAsset = collateral2}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

-- | The OfferDatum has the wrong asset beacon.
datumFailure10 :: MonadEmulator m => m ()
datumFailure10 = do
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Just 10
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 10
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
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
              , outputDatum = OutputDatum $ 
                  toDatum @OfferDatum loanDatum{_assetBeacon = AssetBeacon ""}
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

-- | The OfferDatum has a zero principle.
datumFailure11 :: MonadEmulator m => m ()
datumFailure11 = do
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 0
        , _compoundFrequency = Just 10
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 10
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
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

-- | The OfferDatum has a negative principle.
datumFailure12 :: MonadEmulator m => m ()
datumFailure12 = do
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = -1
        , _compoundFrequency = Just 10
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 10
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
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

-- | The OfferDatum has a zero compound frequency.
datumFailure13 :: MonadEmulator m => m ()
datumFailure13 = do
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Just 0
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 10
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
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

-- | The OfferDatum has a negative compound frequency.
datumFailure14 :: MonadEmulator m => m ()
datumFailure14 = do
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Just (-1)
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 10
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
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

-- | The OfferDatum has a zero loan term.
datumFailure15 :: MonadEmulator m => m ()
datumFailure15 = do
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Nothing
        , _loanTerm = 0
        , _loanInterest = Fraction (1,10)
        , _minPayment = 10
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
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

-- | The OfferDatum has a negative loan term.
datumFailure16 :: MonadEmulator m => m ()
datumFailure16 = do
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Nothing
        , _loanTerm = -1
        , _loanInterest = Fraction (1,10)
        , _minPayment = 10
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
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

-- | The OfferDatum has a negative interest numerator.
datumFailure17 :: MonadEmulator m => m ()
datumFailure17 = do
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Nothing
        , _loanTerm = 3600
        , _loanInterest = Fraction (-1,10)
        , _minPayment = 10
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
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

-- | The OfferDatum has a negative interest denominator.
datumFailure18 :: MonadEmulator m => m ()
datumFailure18 = do
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Nothing
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,-10)
        , _minPayment = 10
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
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

-- | The OfferDatum has a zero interest denominator.
datumFailure19 :: MonadEmulator m => m ()
datumFailure19 = do
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Nothing
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,0)
        , _minPayment = 10
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
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

-- | The OfferDatum has a negative minimum payment.
datumFailure20 :: MonadEmulator m => m ()
datumFailure20 = do
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Nothing
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = -10
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
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

-- | The OfferDatum has an empty collateral list.
datumFailure21 :: MonadEmulator m => m ()
datumFailure21 = do
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
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
        , _collateralization = []
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
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

-- | The OfferDatum has duplicates in the collateral list.
datumFailure22 :: MonadEmulator m => m ()
datumFailure22 = do
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Nothing
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 20
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            , (collateral2,Fraction(2,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
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

-- | The OfferDatum has collateral list is not sorted lexicographically.
datumFailure23 :: MonadEmulator m => m ()
datumFailure23 = do
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Nothing
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 20
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral2,Fraction(1,1))
            , (collateral1,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
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

-- | The OfferDatum's collateral list has a relative price with a negative numerator.
datumFailure24 :: MonadEmulator m => m ()
datumFailure24 = do
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Nothing
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 20
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(-1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
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

-- | The OfferDatum's collateral list has a relative price with a negative denominator.
datumFailure25 :: MonadEmulator m => m ()
datumFailure25 = do
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Nothing
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 20
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,-1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
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

-- | The OfferDatum's collateral list has a relative price with a zero denominator.
datumFailure26 :: MonadEmulator m => m ()
datumFailure26 = do
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Nothing
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 20
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,-1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
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

-- | The OfferDatum has a negative claim period.
datumFailure27 :: MonadEmulator m => m ()
datumFailure27 = do
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Nothing
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 20
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = -3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
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

-- | The OfferDatum has a zero claim period.
datumFailure28 :: MonadEmulator m => m ()
datumFailure28 = do
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Nothing
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 20
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 0
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
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

-- | The OfferDatum has a zero offer deposit.
datumFailure29 :: MonadEmulator m => m ()
datumFailure29 = do
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Nothing
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 20
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 10
        , _offerDeposit = 0
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
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

-- | The OfferDatum has a negative offer deposit.
datumFailure30 :: MonadEmulator m => m ()
datumFailure30 = do
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Nothing
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 20
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 10
        , _offerDeposit = -1
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
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

-- | The OfferDatum has a negative offer expiration.
datumFailure31 :: MonadEmulator m => m ()
datumFailure31 = do
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Nothing
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 20
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 10
        , _offerDeposit = 4_000_000
        , _offerExpiration = Just (-1)
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
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

-- | The OfferDatum has a zero offer expiration.
datumFailure32 :: MonadEmulator m => m ()
datumFailure32 = do
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Nothing
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 20
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 10
        , _offerDeposit = 4_000_000
        , _offerExpiration = Just 0
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
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

-- | The OfferDatum has a fixed fee of zero.
datumFailure33 :: MonadEmulator m => m ()
datumFailure33 = do
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Just 10
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 10
        , _penalty = FixedFee 0
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
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

-- | The OfferDatum has a percent fee with a numerator of 0..
datumFailure34 :: MonadEmulator m => m ()
datumFailure34 = do
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Just 10
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 10
        , _penalty = PercentFee $ Fraction (0,1)
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
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

-- | The OfferDatum has a percent fee with a negative numerator.
datumFailure35 :: MonadEmulator m => m ()
datumFailure35 = do
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Just 10
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 10
        , _penalty = PercentFee $ Fraction (-1,1)
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
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

-- | The OfferDatum has a percent fee with a denominator of zero.
datumFailure36 :: MonadEmulator m => m ()
datumFailure36 = do
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Just 10
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 10
        , _penalty = PercentFee $ Fraction (1,0)
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
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

-- | The OfferDatum has a percent fee with a negative denominator.
datumFailure37 :: MonadEmulator m => m ()
datumFailure37 = do
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Just 10
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 10
        , _penalty = PercentFee $ Fraction (1,-1)
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
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

-- | Create an offer with a minPayment of 0, but a fixed fee penalty.
datumFailure38 :: MonadEmulator m => m ()
datumFailure38 = do
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Just 10
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 0
        , _penalty = FixedFee 10
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
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

-- | Create an offer with a minPayment of 0, but a percent penalty.
datumFailure39 :: MonadEmulator m => m ()
datumFailure39 = do
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Just 10
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 0
        , _penalty = PercentFee $ Fraction (1,10)
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
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

-------------------------------------------------
-- Address Failures
-------------------------------------------------
-- | Create a single valid Offer UTxO but store it at a DApp address without staking.
addressFailure1 :: MonadEmulator m => m ()
addressFailure1 = do
  let -- Borrower Info
      borrowerWallet = Mock.knownMockWallet 1
      borrowerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash borrowerWallet
      borrowerCred = PV2.PubKeyCredential borrowerPubKey
      loanAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash loanScript) 
                    Nothing

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
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Just 10
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 10
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
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

-- | Create a single valid Offer UTxO but store it at a non-DApp address.
addressFailure2 :: MonadEmulator m => m ()
addressFailure2 = do
  let -- Borrower Info
      borrowerWallet = Mock.knownMockWallet 1
      borrowerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash borrowerWallet
      borrowerCred = PV2.PubKeyCredential borrowerPubKey
      loanAddress = toCardanoApiAddress $
        PV2.Address borrowerCred
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
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Just 10
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 10
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
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

-------------------------------------------------
-- Value Failures
-------------------------------------------------
-- | The Offer UTxO is missing the loan principle. The loan asset is a native token.
valueFailure1 :: MonadEmulator m => m ()
valueFailure1 = do
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
      loanAsset = Asset (testTokenSymbol,"TestToken10")
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Just 10
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 10
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
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
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

-- | The Offer deposit is missing the offer deposit. The loan asset is a native token.
valueFailure2 :: MonadEmulator m => m ()
valueFailure2 = do
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
      loanAsset = Asset (testTokenSymbol,"TestToken10")
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10
        , _compoundFrequency = Just 10
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 10
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 5_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 
  mintTestTokens lenderWallet 10_000_000 [("TestToken10",1000)]

  -- Try to create the Offer UTxO.
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
              , outputDatum = OutputDatum $ toDatum loanDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

-- | When ADA is the loan asset, the total amount of ADA is not offer deposit + principle.
valueFailure3 :: MonadEmulator m => m ()
valueFailure3 = do
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Just 10
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 10
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
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
              , outputValue = utxoValue 3_000_000 $ mconcat
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

-- | When the loan asset is ada, the Offer UTxO contains extraneous assets.
valueFailure4 :: MonadEmulator m => m ()
valueFailure4 = do
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Just 10
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 10
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 
  mintTestTokens lenderWallet 10_000_000 [("TestToken1",1000)]

  -- Try to create the Offer UTxO.
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
                  , PV2.singleton testTokenSymbol "TestToken1" 1
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

-- | When the loan asset is native asset, the Offer UTxO contains extraneous assets (another native
-- asset with the same policy id as the loan asset).
valueFailure5 :: MonadEmulator m => m ()
valueFailure5 = do
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
      loanAsset = Asset (testTokenSymbol,"TestToken10")
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10
        , _compoundFrequency = Just 10
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 10
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 
  mintTestTokens lenderWallet 10_000_000 [("TestToken1",1000),("TestToken10",1000)]

  -- Try to create the Offer UTxO.
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
                  , PV2.singleton testTokenSymbol "TestToken1" 1
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

-------------------------------------------------
-- Output Order Failures
-------------------------------------------------
-- | The first Offer Output is invalid.
orderFailure1 :: MonadEmulator m => m ()
orderFailure1 = do
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Just 10
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 10
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
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
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum
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
              , outputDatum = OutputDatum $ toDatum loanDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

-- | The second Offer Output is invalid.
orderFailure2 :: MonadEmulator m => m ()
orderFailure2 = do
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
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum = unsafeCreateOfferDatum $ NewOfferInfo
        { _lenderId = lenderCred
        , _lenderAddress = lenderAddr
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _compoundFrequency = Just 10
        , _loanTerm = 3600
        , _loanInterest = Fraction (1,10)
        , _minPayment = 10
        , _penalty = NoPenalty
        , _collateralization = 
            [ (collateral1,Fraction(1,1))
            , (collateral2,Fraction(1,1))
            ]
        , _collateralIsSwappable = False
        , _claimPeriod = 3600
        , _offerDeposit = 4_000_000
        , _offerExpiration = Nothing
        }

  -- Initialize scenario
  References{negotiationRef} <- initializeReferenceScripts 

  -- Try to create the Offer UTxO.
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
              , outputDatum = OutputDatum $ toDatum loanDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Offer" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unLenderId lenderBeacon) 1
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

-------------------------------------------------
-- TestTree
-------------------------------------------------
-- | A `TestTree` containing all failure scenarios for creating Offer UTxOs.
tests :: [TestTree]
tests =
  [ -- Beacon Failures
    scriptMustFailWithError "beaconFailure1" 
      "Beacon must go to a DApp address with a valid staking credential"
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
  , scriptMustFailWithError "beaconFailure5" 
      "Beacon must go to a DApp address with a valid staking credential"
      beaconFailure5
  , scriptMustFailWithError "beaconFailure6" 
      "Offer UTxOs must have exactly three kinds of beacons"
      beaconFailure6
  , scriptMustFailWithError "beaconFailure7" 
      "UTxO has a beacon with the wrong name"
      beaconFailure7
  , scriptMustFailWithError "beaconFailure8" 
      "UTxO has a beacon with the wrong name"
      beaconFailure8
  , scriptMustFailWithError "beaconFailure9" 
      "UTxO has a beacon with the wrong name"
      beaconFailure9
  , scriptMustFailWithError "beaconFailure10" 
      "UTxO has a beacon with the wrong name"
      beaconFailure10
  , scriptMustFailWithError "beaconFailure11" 
      "UTxO has a beacon with the wrong name"
      beaconFailure11
  , scriptMustFailWithError "beaconFailure12" 
      "UTxO has a beacon with the wrong name"
      beaconFailure12
  , scriptMustFailWithError "beaconFailure13" 
      "UTxO has too many lender ids"
      beaconFailure13
  , scriptMustFailWithError "beaconFailure14" 
      "UTxO has too many Offer beacons"
      beaconFailure14
  , scriptMustFailWithError "beaconFailure15" 
      "UTxO has too many asset beacons"
      beaconFailure15
  , scriptMustFailWithError "beaconFailure16" 
      "UTxO has a beacon with the wrong name"
      beaconFailure16
  , scriptMustFailWithError "beaconFailure17" 
      "UTxO has a beacon with the wrong name"
      beaconFailure17
  , scriptMustFailWithError "beaconFailure18" 
      "UTxO has wrong value"
      beaconFailure18

    -- Approval Failures
  , scriptMustFailWithError "approvalFailure1" 
      "Lender credential did not approve"
      approvalFailure1
  , scriptMustFailWithError "approvalFailure2" 
      "UTxO has a beacon with the wrong name"
      approvalFailure2

    -- Datum Failures
  , scriptMustFailWithError "datumFailure1" 
      "All datums must be inline datums"
      datumFailure1
  , scriptMustFailWithError "datumFailure2" 
      "All datums must be inline datums"
      datumFailure2
  , scriptMustFailWithError "datumFailure3" 
      "UTxO not stored with an OfferDatum"
      datumFailure3
  , scriptMustFailWithError "datumFailure4" 
      "Datum has wrong negotiation beacon id"
      datumFailure4
  , scriptMustFailWithError "datumFailure5" 
      "Datum has wrong active beacon id"
      datumFailure5
  , scriptMustFailWithError "datumFailure6" 
      "Datum has wrong lender id"
      datumFailure6
  , scriptMustFailWithError "datumFailure7" 
      "Datum has wrong lender address"
      datumFailure7
  , scriptMustFailWithError "datumFailure8" 
      "Datum has wrong lender address"
      datumFailure8
  , scriptMustFailWithError "datumFailure9" 
      "UTxO has a beacon with the wrong name"
      datumFailure9
  , scriptMustFailWithError "datumFailure10" 
      "Datum has wrong asset beacon"
      datumFailure10
  , scriptMustFailWithError "datumFailure11" 
      "Datum has wrong principle"
      datumFailure11
  , scriptMustFailWithError "datumFailure12" 
      "Datum has wrong principle"
      datumFailure12
  , scriptMustFailWithError "datumFailure13" 
      "Datum has wrong compound frequency"
      datumFailure13
  , scriptMustFailWithError "datumFailure14" 
      "Datum has wrong compound frequency"
      datumFailure14
  , scriptMustFailWithError "datumFailure15" 
      "Datum has wrong term"
      datumFailure15
  , scriptMustFailWithError "datumFailure16" 
      "Datum has wrong term"
      datumFailure16
  , scriptMustFailWithError "datumFailure17" 
      "Datum has wrong loan interest"
      datumFailure17
  , scriptMustFailWithError "datumFailure18" 
      "Datum has wrong loan interest"
      datumFailure18
  , scriptMustFailWithError "datumFailure19" 
      "Datum has wrong loan interest"
      datumFailure19
  , scriptMustFailWithError "datumFailure20" 
      "Datum has wrong min payment"
      datumFailure20
  , scriptMustFailWithError "datumFailure21" 
      "Datum has wrong collateralization"
      datumFailure21
  , scriptMustFailWithError "datumFailure22" 
      "Duplicate collateral found"
      datumFailure22
  , scriptMustFailWithError "datumFailure23" 
      "Datum has wrong collateralization"
      datumFailure23
  , scriptMustFailWithError "datumFailure24" 
      "Datum has wrong collateralization"
      datumFailure24
  , scriptMustFailWithError "datumFailure25" 
      "Datum has wrong collateralization"
      datumFailure25
  , scriptMustFailWithError "datumFailure26" 
      "Datum has wrong collateralization"
      datumFailure26
  , scriptMustFailWithError "datumFailure27" 
      "Datum has wrong claim period"
      datumFailure27
  , scriptMustFailWithError "datumFailure28" 
      "Datum has wrong claim period"
      datumFailure28
  , scriptMustFailWithError "datumFailure29" 
      "Datum has wrong offer deposit"
      datumFailure29
  , scriptMustFailWithError "datumFailure30" 
      "Datum has wrong offer deposit"
      datumFailure30
  , scriptMustFailWithError "datumFailure31" 
      "Datum has wrong offer expiration"
      datumFailure31
  , scriptMustFailWithError "datumFailure32" 
      "Datum has wrong offer expiration"
      datumFailure32
  , scriptMustFailWithError "datumFailure33" 
      "Datum has wrong penalty"
      datumFailure33
  , scriptMustFailWithError "datumFailure34" 
      "Datum has wrong penalty"
      datumFailure34
  , scriptMustFailWithError "datumFailure35" 
      "Datum has wrong penalty"
      datumFailure35
  , scriptMustFailWithError "datumFailure36" 
      "Datum has wrong penalty"
      datumFailure36
  , scriptMustFailWithError "datumFailure37" 
      "Datum has wrong penalty"
      datumFailure37
  , scriptMustFailWithError "datumFailure38" 
      "Datum has wrong penalty"
      datumFailure38
  , scriptMustFailWithError "datumFailure39" 
      "Datum has wrong penalty"
      datumFailure39
    
    -- Address Failures
  , scriptMustFailWithError "addressFailure1" 
      "Beacon must go to a DApp address with a valid staking credential"
      addressFailure1
  , scriptMustFailWithError "addressFailure2" 
      "Beacon must go to a DApp address with a valid staking credential"
      addressFailure2

    -- Value Failures
  , scriptMustFailWithError "valueFailure1" 
      "UTxO has wrong value"
      valueFailure1
  , scriptMustFailWithError "valueFailure2" 
      "UTxO has wrong value"
      valueFailure2
  , scriptMustFailWithError "valueFailure3" 
      "UTxO has wrong value"
      valueFailure3
  , scriptMustFailWithError "valueFailure4" 
      "No extraneous assets allowed in the UTxO"
      valueFailure4
  , scriptMustFailWithError "valueFailure5" 
      "No extraneous assets allowed in the UTxO"
      valueFailure5

    -- Output Order Failures
  , scriptMustFailWithError "orderFailure1" 
      "UTxO has wrong value"
      orderFailure1
  , scriptMustFailWithError "orderFailure2" 
      "UTxO has wrong value"
      orderFailure2

  ]
