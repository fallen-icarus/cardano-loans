{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.AcceptOffer
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

import CardanoLoans

import Test.Prelude

-------------------------------------------------
-- Regression Tests
-------------------------------------------------
-- | Accept a single loan offer. The loan asset is ADA and one native asset is used as collateral.
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
  (negotiationRef,activeRef,loanRef,_) <- initializeReferenceScripts 
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

  -- Try to accept the offer.
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

-- | Accept multiple loan offers. The loan asset is ADA and one native asset is used as collateral.
regressionTest2 :: MonadEmulator m => m ()
regressionTest2 = do
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
  (negotiationRef,activeRef,loanRef,_) <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",2),(unAssetBeacon loanBeacon,2)]
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
          , Output
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
                  [ ("Offer",2)
                  , (unAssetBeacon loanBeacon,2)
                  , (unLenderID lenderBeacon,2)
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
          , Output
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

  askRefs <- map fst <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @AskDatum 
      loanAddress 
      (negotiationBeaconCurrencySymbol,"Ask")

  offerRefs <- map fst <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @OfferDatum 
      loanAddress 
      (negotiationBeaconCurrencySymbol,"Offer")

  let [activeDatum1,activeDatum2] = flip map offerRefs $ \offerRef ->
        createAcceptanceDatum borrowerCred offerRef (slotToPosixTime startSlot) offerDatum
      [loanIdBeacon1,loanIdBeacon2] = map genLoanId offerRefs

  -- Try to accept the offer.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = 
          [ TokenMint
              { mintTokens = 
                  [ ("Offer",-2)
                  , ("Ask",-2)
                  , (unAssetBeacon loanBeacon,-4)
                  , (unLenderID lenderBeacon,-2)
                  ]
              , mintRedeemer = toRedeemer BurnNegotiationBeacons
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          , TokenMint
              { mintTokens = 
                  [ ("Active",2)
                  , (unBorrowerID borrowerBeacon,2)
                  , (unAssetBeacon loanBeacon,2)
                  , (unLoanID loanIdBeacon1,2)
                  , (unLoanID loanIdBeacon2,2)
                  ]
              , mintRedeemer = toRedeemer $ CreateActive borrowerCred negotiationBeaconCurrencySymbol
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
                  , PV2.singleton activeBeaconCurrencySymbol (unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (unBorrowerID borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (unLoanID loanIdBeacon1) 1
                  , uncurry PV2.singleton (unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (unLoanID loanIdBeacon1) 1 ]
              , outputDatum = 
                  OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,"Accepted")
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol "Active" 1
                  , PV2.singleton activeBeaconCurrencySymbol (unAssetBeacon loanBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (unBorrowerID borrowerBeacon) 1
                  , PV2.singleton activeBeaconCurrencySymbol (unLoanID loanIdBeacon2) 1
                  , uncurry PV2.singleton (unAsset collateral1) 10
                  ]
              , outputDatum = OutputDatum $ toDatum activeDatum2
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = toCardanoApiAddress lenderAddr
              , outputValue = utxoValue 4_000_000 $ mconcat
                  [ PV2.singleton activeBeaconCurrencySymbol (unLoanID loanIdBeacon2) 1 ]
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

-------------------------------------------------
-- Benchmark Tests
-------------------------------------------------
-- | Accept a multiple loan offers all for the exact same terms and from the same lender.
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
  (negotiationRef,activeRef,loanRef,_) <- initializeReferenceScripts 
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

  startSlot <- currentSlot

  askUTxOs <- take number <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @AskDatum 
      loanAddress 
      (negotiationBeaconCurrencySymbol,"Ask")

  offerUTxOs <- take number <$> 
    txOutRefsAndDatumsAtAddressWithBeacon @OfferDatum
      loanAddress 
      (negotiationBeaconCurrencySymbol,"Offer")

  -- let sampleBurns =
  --       [ TokenMint
  --           { mintTokens = 
  --               [ ("Offer",negate $ fromIntegral number)
  --               , ("Ask",negate $ fromIntegral number)
  --               , (unAssetBeacon loanBeacon,2 * negate (fromIntegral number))
  --               , (unLenderID lenderBeacon,negate $ fromIntegral number)
  --               ]
  --           , mintRedeemer = toRedeemer BurnNegotiationBeacons
  --           , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
  --           , mintReference = Just negotiationRef
  --           }
  --       , TokenMint
  --           { mintTokens = 
  --               [ ("Active",fromIntegral number)
  --               , (unBorrowerID borrowerBeacon,fromIntegral number)
  --               , (unAssetBeacon loanBeacon,fromIntegral number)
  --               ] <> map ((,2) . unLoanID . genLoanId . fst) offerUTxOs
  --           , mintRedeemer = toRedeemer $ CreateActive borrowerCred negotiationBeaconCurrencySymbol
  --           , mintPolicy = toVersionedMintingPolicy activeBeaconScript
  --           , mintReference = Just activeRef
  --           }
  --       ]

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

      sampleOutputs os = flip concatMap os $ 
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
                  createAcceptanceDatum borrowerCred offerRef (slotToPosixTime startSlot) od
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
-- Test Function
-------------------------------------------------
-- | A `TestTree` containing all accept offer scenarios.
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
    -- , mustSucceed "benchTest1" $ benchTest1 42
    -- , mustSucceed "benchTest2" $ benchTest2 32
    -- , mustSucceed "benchTest3" $ benchTest3 32
    -- , mustSucceed "benchTest4" $ benchTest4 24

      -- Performance Increase Tests
    -- , mustExceedTxLimits "perfIncreaseTest1" $ benchTest1 43
    -- , mustExceedTxLimits "perfIncreaseTest2" $ benchTest2 33
    -- , mustExceedTxLimits "perfIncreaseTest3" $ benchTest3 33
    -- , mustExceedTxLimits "perfIncreaseTest4" $ benchTest4 25
    ]
