{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.AskUTxOs.Misc where

import qualified Ledger.Address as LA
import qualified PlutusLedgerApi.V2 as PV2
import qualified Ledger.CardanoWallet as Mock 
import Test.Tasty (TestTree,testGroup)

import CardanoLoans

import Test.Prelude

-------------------------------------------------
-- Miscelleneous Regression Tests
-------------------------------------------------
-- | Close Ask UTxOs for different borrowers in the same transaction. A minting execution is used
-- for one borrower and a staking execution is used for the second borrower. Both credentials
-- approve the transaction. This succeeds since the negotiationBeaconScript does not check inputs when 
-- used with CreateCloseOrUpdateAsk.
regressionTest1 :: MonadEmulator m => m ()
regressionTest1 = do
  let -- Borrower1 Info
      borrowerWallet1 = Mock.knownMockWallet 1
      borrowerPersonalAddr1 = Mock.mockWalletAddress borrowerWallet1
      borrowerPayPrivKey1 = Mock.paymentPrivateKey borrowerWallet1
      borrowerPubKey1 = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash borrowerWallet1
      borrowerCred1 = PV2.PubKeyCredential borrowerPubKey1
      loanAddress1 = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash loanScript) 
                    (Just $ PV2.StakingHash borrowerCred1)

     -- Borrower2 Info
      borrowerWallet2 = Mock.knownMockWallet 2
      borrowerPersonalAddr2 = Mock.mockWalletAddress borrowerWallet2
      borrowerPayPrivKey2 = Mock.paymentPrivateKey borrowerWallet2
      borrowerPubKey2 = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash borrowerWallet2
      borrowerCred2 = PV2.PubKeyCredential borrowerPubKey2
      loanAddress2 = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash loanScript) 
                    (Just $ PV2.StakingHash borrowerCred2)

      -- Loan Info
      loanAsset = Asset (adaSymbol,adaToken)
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum1 = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred1
        , _loanAsset = loanAsset
        , _loanPrincipal = 10_000_000
        , _loanTerm = 3600
        , _collateral = [collateral1]
        }
      loanDatum2 = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred2
        , _loanAsset = loanAsset
        , _loanPrincipal = 10_000_000
        , _loanTerm = 3600
        , _collateral = [collateral1]
        }

  -- Initialize scenario
  References{negotiationRef,loanRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet1 10_000_000 [("TestToken1",1000)]
  mintTestTokens borrowerWallet2 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO for the first borrower which will be updated in the next transaction.
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
              , outputDatum = OutputDatum $ toDatum loanDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [borrowerPubKey1]
      }

  -- Create the Ask for the second borrower.
  void $ transact borrowerPersonalAddr2 [refScriptAddress] [borrowerPayPrivKey2]
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
              , outputDatum = OutputDatum $ toDatum loanDatum2
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [borrowerPubKey2]
      }

  askUTxOs <- (<>) <$> txOutRefsAndDatumsAtAddress @AskDatum loanAddress1
                   <*> txOutRefsAndDatumsAtAddress @AskDatum loanAddress2

  -- Try to close both Ask UTxOs.
  void $ transact 
    borrowerPersonalAddr1 
    [loanAddress1,loanAddress2,refScriptAddress] 
    [borrowerPayPrivKey1,borrowerPayPrivKey2]
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",-2),(_unAssetBeacon loanBeacon,-2)]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred1
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , inputs = flip map askUTxOs $ \(askRef,_) ->
          Input
            { inputId = askRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer CloseOrUpdateAsk)
            }
      , referenceInputs = [negotiationRef,loanRef]
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash negotiationBeaconScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference negotiationRef $ 
                    toRedeemer $ CreateCloseOrUpdateAsk borrowerCred2
              }
          ]
      , extraKeyWitnesses = [borrowerPubKey1,borrowerPubKey2]
      }

-------------------------------------------------
-- Miscelleneous Failure Tests
-------------------------------------------------
-- | Use CreateCloseOrUpdateAsk to create Ask UTxOs for one borrower with a minting execution
-- and use it to update Ask UTxOs for a different borrower using a staking exectution. Both
-- borrower credentials sign the transaction.
failureTest1 :: MonadEmulator m => m ()
failureTest1 = do
  let -- Borrower1 Info
      borrowerWallet1 = Mock.knownMockWallet 1
      borrowerPersonalAddr1 = Mock.mockWalletAddress borrowerWallet1
      borrowerPayPrivKey1 = Mock.paymentPrivateKey borrowerWallet1
      borrowerPubKey1 = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash borrowerWallet1
      borrowerCred1 = PV2.PubKeyCredential borrowerPubKey1
      loanAddress1 = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash loanScript) 
                    (Just $ PV2.StakingHash borrowerCred1)

     -- Borrower2 Info
      borrowerWallet2 = Mock.knownMockWallet 2
      borrowerPersonalAddr2 = Mock.mockWalletAddress borrowerWallet2
      borrowerPayPrivKey2 = Mock.paymentPrivateKey borrowerWallet2
      borrowerPubKey2 = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash borrowerWallet2
      borrowerCred2 = PV2.PubKeyCredential borrowerPubKey2
      loanAddress2 = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash loanScript) 
                    (Just $ PV2.StakingHash borrowerCred2)

      -- Loan Info
      loanAsset = Asset (adaSymbol,adaToken)
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum1 = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred1
        , _loanAsset = loanAsset
        , _loanPrincipal = 10_000_000
        , _loanTerm = 3600
        , _collateral = [collateral1]
        }
      loanDatum2 = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred2
        , _loanAsset = loanAsset
        , _loanPrincipal = 10_000_000
        , _loanTerm = 3600
        , _collateral = [collateral1]
        }

  -- Initialize scenario
  References{negotiationRef,loanRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet1 10_000_000 [("TestToken1",1000)]
  mintTestTokens borrowerWallet2 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO for the first borrower which will be updated in the next transaction.
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
              , outputDatum = OutputDatum $ toDatum loanDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [borrowerPubKey1]
      }

  askUTxOs <- txOutRefsAndDatumsAtAddress @AskDatum loanAddress1

  -- Try to update an Ask for one borrower identity in the same transaction where another identity
  -- creates an Ask.
  void $ transact 
    borrowerPersonalAddr1 
    [loanAddress1,loanAddress2,refScriptAddress] 
    [borrowerPayPrivKey1,borrowerPayPrivKey2]
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon,1)]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred2
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , inputs = flip map askUTxOs $ \(askRef,_) ->
          Input
            { inputId = askRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer CloseOrUpdateAsk)
            }
      , outputs =
          [ Output
              { outputAddress = loanAddress1
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum @AskDatum loanDatum1{_loanTerm = 5000}
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = loanAddress2
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum2
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash negotiationBeaconScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference negotiationRef $ 
                    toRedeemer $ CreateCloseOrUpdateAsk borrowerCred1
              }
          ]
      , referenceInputs = [negotiationRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey1,borrowerPubKey2]
      }

-- | Create Ask UTxOs for different borrowers in the same transaction. A minting execution is used
-- for one borrower and a staking execution is used for the second borrower.
failureTest2 :: MonadEmulator m => m ()
failureTest2 = do
  let -- Borrower1 Info
      borrowerWallet1 = Mock.knownMockWallet 1
      borrowerPersonalAddr1 = Mock.mockWalletAddress borrowerWallet1
      borrowerPayPrivKey1 = Mock.paymentPrivateKey borrowerWallet1
      borrowerPubKey1 = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash borrowerWallet1
      borrowerCred1 = PV2.PubKeyCredential borrowerPubKey1
      loanAddress1 = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash loanScript) 
                    (Just $ PV2.StakingHash borrowerCred1)

     -- Borrower2 Info
      borrowerWallet2 = Mock.knownMockWallet 2
      borrowerPersonalAddr2 = Mock.mockWalletAddress borrowerWallet2
      borrowerPayPrivKey2 = Mock.paymentPrivateKey borrowerWallet2
      borrowerPubKey2 = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash borrowerWallet2
      borrowerCred2 = PV2.PubKeyCredential borrowerPubKey2
      loanAddress2 = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash loanScript) 
                    (Just $ PV2.StakingHash borrowerCred2)

      -- Loan Info
      loanAsset = Asset (adaSymbol,adaToken)
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum1 = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred1
        , _loanAsset = loanAsset
        , _loanPrincipal = 10_000_000
        , _loanTerm = 3600
        , _collateral = [collateral1]
        }
      loanDatum2 = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred2
        , _loanAsset = loanAsset
        , _loanPrincipal = 10_000_000
        , _loanTerm = 3600
        , _collateral = [collateral1]
        }

  -- Initialize scenario
  References{negotiationRef,loanRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet1 10_000_000 [("TestToken1",1000)]
  mintTestTokens borrowerWallet2 10_000_000 [("TestToken1",1000)]

  -- Try to create UTxOs for two separate borrowers by using two separate types of executions.
  void $ transact 
    borrowerPersonalAddr1 
    [loanAddress1,loanAddress2,refScriptAddress] 
    [borrowerPayPrivKey1,borrowerPayPrivKey2]
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
              , outputDatum = OutputDatum $ toDatum loanDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = loanAddress2
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum2
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef,loanRef]
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash negotiationBeaconScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference negotiationRef $ 
                    toRedeemer $ CreateCloseOrUpdateAsk borrowerCred2
              }
          ]
      , extraKeyWitnesses = [borrowerPubKey1,borrowerPubKey2]
      }

-- | Create a new Ask in the same transaction where an Offer is accepted. The Ask uses the
-- same Asset beacon as the Offer accepted.
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
                  , ("Ask",0)
                  , (_unAssetBeacon loanBeacon,-1)
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
      , referenceInputs = [negotiationRef,activeRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey1]
      , validityRange = ValidityRange
          { validityRangeLowerBound = Just startSlot
          , validityRangeUpperBound = Nothing
          }
      }

-------------------------------------------------
-- TestTree
-------------------------------------------------
-- | A `TestTree` containing all misc scenarios for creating Ask UTxOs. These scenarios are more
-- complicated than the basic ones tested in the other modules. They include compositions and
-- edge-case scenarios.
tests :: TestTree
tests = testGroup "Miscelleneous Tests"
  [ -- Miscelleneous Regression Tests
    mustSucceed "regressionTest1" regressionTest1

    -- Miscelleneous Failure Tests
  , scriptMustFailWithError "failureTest1" 
      "Beacons must go to a DApp address with the borrower's staking credential"
      failureTest1
  , scriptMustFailWithError "failureTest2" 
      "Beacons must go to a DApp address with the borrower's staking credential"
      failureTest2
  , scriptMustFailWithError "failureTest3" 
      "The wrong negotiation beacons were burned"
      failureTest3
  ]
