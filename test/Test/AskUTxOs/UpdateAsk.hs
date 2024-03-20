{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-
 
The logic used is exactly the same as for closing and creating Ask UTxOs. Therefore, this
module only focuses on the use cases that are unique to updating Ask UTxOs.

-}
module Test.AskUTxOs.UpdateAsk
  ( 
    -- * Scenarios Tested
    -- ** Scenarios that should succeed
    regressionTest1
  , regressionTest2
  , regressionTest3
  , regressionTest4
  , regressionTest5

    -- ** Scenarios that should fail
    -- *** Beacon Failures
  , beaconFailure1

    -- *** Credential Failures
  , addressFailure1
  , addressFailure2

    -- ** Benchmark Tests
  , benchTest1
  , benchTest2
  , benchTest3
    
    -- * Full TestTree
  , tests
  ) where

import qualified Ledger.Address as LA
import qualified PlutusLedgerApi.V2 as PV2
import qualified Ledger.CardanoWallet as Mock 
import Test.Tasty (TestTree,testGroup)
import Data.String (fromString)
import Control.Monad (forM_)

import CardanoLoans

import Test.Prelude

-------------------------------------------------
-- Regression Tests
-------------------------------------------------
-- | Convert a single valid Ask UTxO to one that uses a different loan asset. 
-- The same native asset is used as the only collateral asset both before and after the
-- conversion.
regressionTest1 :: MonadEmulator m => m ()
regressionTest1 = do
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
      loanAsset1 = Asset (adaSymbol,adaToken)
      loanAsset2 = Asset (testTokenSymbol,"TestToken10")
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      loanBeacon1 = genLoanAssetBeaconName loanAsset1
      loanBeacon2 = genLoanAssetBeaconName loanAsset2
      loanDatum1 = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred
        , _loanAsset = loanAsset1
        , _loanPrinciple = 10_000_000
        , _loanTerm = 3600
        , _collateral = [collateral1]
        }
      loanDatum2 = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred
        , _loanAsset = loanAsset2
        , _loanPrinciple = 10
        , _loanTerm = 3600
        , _collateral = [collateral1]
        }

  -- Initialize scenario
  References{negotiationRef,loanRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon1,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon1) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum1
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
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon1) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        ]

  -- Try to convert the Ask UTxO.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(_unAssetBeacon loanBeacon1,-1),(_unAssetBeacon loanBeacon2,1)]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , inputs =
          [ Input
              { inputId = askRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer CloseOrUpdateAsk)
              }
          ]
      , outputs =
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon2) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum2
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      }

-- | Convert multiple valid Ask UTxO to one that uses a different loan asset. 
-- The same native asset is used as the only collateral asset both before and after the
-- conversion.
regressionTest2 :: MonadEmulator m => m ()
regressionTest2 = do
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
      loanAsset1 = Asset (adaSymbol,adaToken)
      loanAsset2 = Asset (testTokenSymbol,"TestToken10")
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      loanBeacon1 = genLoanAssetBeaconName loanAsset1
      loanBeacon2 = genLoanAssetBeaconName loanAsset2
      loanDatum1 = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred
        , _loanAsset = loanAsset1
        , _loanPrinciple = 10_000_000
        , _loanTerm = 3600
        , _collateral = [collateral1]
        }
      loanDatum2 = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred
        , _loanAsset = loanAsset2
        , _loanPrinciple = 10
        , _loanTerm = 3600
        , _collateral = [collateral1]
        }

  -- Initialize scenario
  References{negotiationRef,loanRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",2),(_unAssetBeacon loanBeacon1,2)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon1) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon1) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [borrowerPubKey]
      }

  askUTxOs <- txOutRefsAndDatumsAtAddress @AskDatum loanAddress

  -- Try to convert the Ask UTxO.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(_unAssetBeacon loanBeacon1,-2),(_unAssetBeacon loanBeacon2,2)]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred
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
              { outputAddress = loanAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon2) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum2
              , outputReferenceScript = toReferenceScript Nothing
              }
          , Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon2) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum2
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      }

-- | Update the terms for a single valid Ask UTxO. The loan asset and collateral
-- does not change. 
regressionTest3 :: MonadEmulator m => m ()
regressionTest3 = do
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
      loanAsset1 = Asset (adaSymbol,adaToken)
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      loanBeacon1 = genLoanAssetBeaconName loanAsset1
      loanDatum1 = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred
        , _loanAsset = loanAsset1
        , _loanPrinciple = 10_000_000
        , _loanTerm = 3600
        , _collateral = [collateral1]
        }
      loanDatum2 = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred
        , _loanAsset = loanAsset1
        , _loanPrinciple = 10_000_000
        , _loanTerm = 4600
        , _collateral = [collateral1]
        }

  -- Initialize scenario
  References{negotiationRef,loanRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon1,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon1) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [borrowerPubKey]
      }

  askUTxOs <- txOutRefsAndDatumsAtAddress @AskDatum loanAddress

  -- Try to update the Ask UTxO.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs = flip map askUTxOs $ \(askRef,_) ->
          Input
            { inputId = askRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer CloseOrUpdateAsk)
            }
      , outputs =
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon1) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash negotiationBeaconScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference negotiationRef $ 
                    toRedeemer $ CreateCloseOrUpdateAsk borrowerCred
              }
          ]
      , referenceInputs = [negotiationRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      }

-- | Update the terms for a single valid Ask UTxO. The loan asset does not change but the collateral
-- does change. 
regressionTest4 :: MonadEmulator m => m ()
regressionTest4 = do
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
      loanAsset1 = Asset (adaSymbol,adaToken)
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      loanBeacon1 = genLoanAssetBeaconName loanAsset1
      loanDatum1 = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred
        , _loanAsset = loanAsset1
        , _loanPrinciple = 10_000_000
        , _loanTerm = 3600
        , _collateral = [collateral1]
        }
      loanDatum2 = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred
        , _loanAsset = loanAsset1
        , _loanPrinciple = 10_000_000
        , _loanTerm = 4600
        , _collateral = [collateral2]
        }

  -- Initialize scenario
  References{negotiationRef,loanRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000),("TestToken2",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon1,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon1) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [borrowerPubKey]
      }

  askUTxOs <- txOutRefsAndDatumsAtAddress @AskDatum loanAddress

  -- Try to update the Ask UTxO.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs = flip map askUTxOs $ \(askRef,_) ->
          Input
            { inputId = askRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer CloseOrUpdateAsk)
            }
      , outputs =
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon1) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum1
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , withdrawals =
          [ Withdrawal
              { withdrawalCredential = PV2.ScriptCredential $ scriptHash negotiationBeaconScript
              , withdrawalAmount = 0
              , withdrawalWitness = 
                  StakeWithPlutusReference negotiationRef $ 
                    toRedeemer $ CreateCloseOrUpdateAsk borrowerCred
              }
          ]
      , referenceInputs = [negotiationRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      }

-- | Convert a single valid Ask UTxO to one that uses a different loan asset. 
-- The collateral used also changes. The starting collateral was just a single native asset
-- but it gets changed to using two other native assets.
regressionTest5 :: MonadEmulator m => m ()
regressionTest5 = do
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
      loanAsset1 = Asset (adaSymbol,adaToken)
      loanAsset2 = Asset (testTokenSymbol,"TestToken10")
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      collateral3 = Asset (testTokenSymbol,"TestToken3")
      loanBeacon1 = genLoanAssetBeaconName loanAsset1
      loanBeacon2 = genLoanAssetBeaconName loanAsset2
      loanDatum1 = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred
        , _loanAsset = loanAsset1
        , _loanPrinciple = 10_000_000
        , _loanTerm = 3600
        , _collateral = [collateral1]
        }
      loanDatum2 = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred
        , _loanAsset = loanAsset2
        , _loanPrinciple = 10
        , _loanTerm = 3600
        , _collateral = [collateral2,collateral3]
        }

  -- Initialize scenario
  References{negotiationRef,loanRef} <- initializeReferenceScripts 
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
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon1,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon1) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum1
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
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon1) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        ]

  -- Try to convert the Ask UTxO.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(_unAssetBeacon loanBeacon1,-1),(_unAssetBeacon loanBeacon2,1)]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , inputs =
          [ Input
              { inputId = askRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer CloseOrUpdateAsk)
              }
          ]
      , outputs =
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon2) 1
                  , uncurry PV2.singleton (_unAsset collateral2) 1
                  , uncurry PV2.singleton (_unAsset collateral3) 1
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum2
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      }

-------------------------------------------------
-- Failure Tests
-------------------------------------------------
{------------------------
-- Beacon Failures

1) It should not be possible to withraw and asset beacon that is no longer being used.
------------------------}
-- | When converting an Ask UTxO to one that uses a different loan asset, withdraw the
-- asset beacon instead of burning it.
beaconFailure1 :: MonadEmulator m => m ()
beaconFailure1 = do
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
      loanAsset1 = Asset (adaSymbol,adaToken)
      loanAsset2 = Asset (testTokenSymbol,"TestToken10")
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      loanBeacon1 = genLoanAssetBeaconName loanAsset1
      loanBeacon2 = genLoanAssetBeaconName loanAsset2
      loanDatum1 = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred
        , _loanAsset = loanAsset1
        , _loanPrinciple = 10_000_000
        , _loanTerm = 3600
        , _collateral = [collateral1]
        }
      loanDatum2 = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred
        , _loanAsset = loanAsset2
        , _loanPrinciple = 10
        , _loanTerm = 3600
        , _collateral = [collateral1]
        }

  -- Initialize scenario
  References{negotiationRef,loanRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 10_000_000 [("TestToken1",1000)]

  -- Create the Ask UTxO.
  void $ transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [("Ask",1),(_unAssetBeacon loanBeacon1,1)]
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
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon1) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum1
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
        , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon1) 1
        , uncurry PV2.singleton (_unAsset collateral1) 1
        ]

  -- Try to convert the Ask UTxO.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens =
          [ TokenMint
              { mintTokens = [(_unAssetBeacon loanBeacon1,0),(_unAssetBeacon loanBeacon2,1)]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , inputs =
          [ Input
              { inputId = askRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer CloseOrUpdateAsk)
              }
          ]
      , outputs =
          [ Output
              { outputAddress = loanAddress
              , outputValue = utxoValue 3_000_000 $ mconcat
                  [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                  , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon2) 1
                  , uncurry PV2.singleton (_unAsset collateral1) 1
                  ]
              , outputDatum = OutputDatum $ toDatum loanDatum2
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = [negotiationRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      }

{------------------------
-- Address Failures

1) It should not be possible to move an Ask to a new borrower address. This should require two
   separate transactions.
------------------------}
-- | The borrower moves the Ask UTxO to a new borrower credential. The CreateCloseOrUpdateAsk
-- redeemer uses the new credential. 
addressFailure1 :: MonadEmulator m => m ()
addressFailure1 = do
  let -- Borrower Info
      borrowerWallet = Mock.knownMockWallet 1
      borrowerPersonalAddr = Mock.mockWalletAddress borrowerWallet
      borrowerPayPrivKey = Mock.paymentPrivateKey borrowerWallet
      borrowerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash borrowerWallet
      borrowerCred = PV2.PubKeyCredential borrowerPubKey
      loanAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash loanScript) 
                    (Just $ PV2.StakingHash borrowerCred)

      -- New Borrower Info
      newBorrowerWallet = Mock.knownMockWallet 2
      newBorrowerPersonalAddr = Mock.mockWalletAddress newBorrowerWallet
      newBorrowerPayPrivKey = Mock.paymentPrivateKey newBorrowerWallet
      newBorrowerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash newBorrowerWallet
      newBorrowerCred = PV2.PubKeyCredential newBorrowerPubKey
      newLoanAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash loanScript) 
                    (Just $ PV2.StakingHash newBorrowerCred)

      -- Loan Info
      loanAsset = Asset (adaSymbol,adaToken)
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum1 = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _loanTerm = 3600
        , _collateral = [collateral1]
        }
      loanDatum2 = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = newBorrowerCred
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _loanTerm = 3600
        , _collateral = [collateral1]
        }

  -- Initialize scenario
  References{negotiationRef,loanRef} <- initializeReferenceScripts 
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
              , outputDatum = OutputDatum $ toDatum loanDatum1
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

  -- Try to update the Ask UTxO.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs =
          [ Input
              { inputId = askRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer CloseOrUpdateAsk)
              }
          ]
      , outputs =
          [ Output
              { outputAddress = newLoanAddress
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
                    toRedeemer $ CreateCloseOrUpdateAsk newBorrowerCred
              }
          ]
      , referenceInputs = [negotiationRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey,newBorrowerPubKey]
      }

-- | The borrower moves the Ask UTxO to a new borrower credential. The CreateCloseOrUpdateAsk
-- redeemer uses the old credential. 
addressFailure2 :: MonadEmulator m => m ()
addressFailure2 = do
  let -- Borrower Info
      borrowerWallet = Mock.knownMockWallet 1
      borrowerPersonalAddr = Mock.mockWalletAddress borrowerWallet
      borrowerPayPrivKey = Mock.paymentPrivateKey borrowerWallet
      borrowerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash borrowerWallet
      borrowerCred = PV2.PubKeyCredential borrowerPubKey
      loanAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash loanScript) 
                    (Just $ PV2.StakingHash borrowerCred)

      -- New Borrower Info
      newBorrowerWallet = Mock.knownMockWallet 2
      newBorrowerPersonalAddr = Mock.mockWalletAddress newBorrowerWallet
      newBorrowerPayPrivKey = Mock.paymentPrivateKey newBorrowerWallet
      newBorrowerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash newBorrowerWallet
      newBorrowerCred = PV2.PubKeyCredential newBorrowerPubKey
      newLoanAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash loanScript) 
                    (Just $ PV2.StakingHash newBorrowerCred)

      -- Loan Info
      loanAsset = Asset (adaSymbol,adaToken)
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      loanBeacon = genLoanAssetBeaconName loanAsset
      loanDatum1 = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _loanTerm = 3600
        , _collateral = [collateral1]
        }
      loanDatum2 = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = newBorrowerCred
        , _loanAsset = loanAsset
        , _loanPrinciple = 10_000_000
        , _loanTerm = 3600
        , _collateral = [collateral1]
        }

  -- Initialize scenario
  References{negotiationRef,loanRef} <- initializeReferenceScripts 
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
              , outputDatum = OutputDatum $ toDatum loanDatum1
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

  -- Try to update the Ask UTxO.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { inputs =
          [ Input
              { inputId = askRef
              , inputWitness = 
                  SpendWithPlutusReference loanRef InlineDatum (toRedeemer CloseOrUpdateAsk)
              }
          ]
      , outputs =
          [ Output
              { outputAddress = newLoanAddress
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
                    toRedeemer $ CreateCloseOrUpdateAsk borrowerCred
              }
          ]
      , referenceInputs = [negotiationRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey,newBorrowerPubKey]
      }

-------------------------------------------------
-- Benchmark Tests
-------------------------------------------------
-- | Convert multiple valid Ask UTxO to ones that use a different loan asset. All asks undergo
-- the same conversion. Three native assets are used as collateral both before and after the 
-- conversion.
benchTest1 :: MonadEmulator m => Int -> m ()
benchTest1 number = do
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
      loanAsset1 = Asset (adaSymbol,adaToken)
      loanAsset2 = Asset (testTokenSymbol,"TestToken10")
      collateral1 = Asset (testTokenSymbol,"TestToken1")
      collateral2 = Asset (testTokenSymbol,"TestToken2")
      collateral3 = Asset (testTokenSymbol,"TestToken3")
      loanBeacon1 = genLoanAssetBeaconName loanAsset1
      loanBeacon2 = genLoanAssetBeaconName loanAsset2
      loanDatum1 = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred
        , _loanAsset = loanAsset1
        , _loanPrinciple = 10_000_000
        , _loanTerm = 3600
        , _collateral = [collateral1,collateral2,collateral3]
        }
      loanDatum2 = unsafeCreateAskDatum $ NewAskInfo
        { _borrowerId = borrowerCred
        , _loanAsset = loanAsset2
        , _loanPrinciple = 10
        , _loanTerm = 3600
        , _collateral = [collateral1,collateral2,collateral3]
        }

  -- Initialize scenario
  References{negotiationRef,loanRef} <- initializeReferenceScripts 
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
              { mintTokens = [("Ask",20),(_unAssetBeacon loanBeacon1,20)]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs = replicate 20 $
          Output
            { outputAddress = loanAddress
            , outputValue = utxoValue 3_000_000 $ mconcat
                [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon1) 1
                , uncurry PV2.singleton (_unAsset collateral1) 1
                , uncurry PV2.singleton (_unAsset collateral2) 1
                , uncurry PV2.singleton (_unAsset collateral3) 1
                ]
            , outputDatum = OutputDatum $ toDatum loanDatum1
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
              { mintTokens = [("Ask",20),(_unAssetBeacon loanBeacon1,20)]
              , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred
              , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
              , mintReference = Just negotiationRef
              }
          ]
      , outputs = replicate 20 $
          Output
            { outputAddress = loanAddress
            , outputValue = utxoValue 3_000_000 $ mconcat
                [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon1) 1
                , uncurry PV2.singleton (_unAsset collateral1) 1
                , uncurry PV2.singleton (_unAsset collateral2) 1
                , uncurry PV2.singleton (_unAsset collateral3) 1
                ]
            , outputDatum = OutputDatum $ toDatum loanDatum1
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [negotiationRef]
      , extraKeyWitnesses = [borrowerPubKey]
      }

  askUTxOs <- take number <$> txOutRefsAndDatumsAtAddress @AskDatum loanAddress

  -- Try to update the Ask UTxO.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = flip map askUTxOs $ const
          TokenMint
            { mintTokens = [(_unAssetBeacon loanBeacon1,-1),(_unAssetBeacon loanBeacon2,1)]
            , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred
            , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
            , mintReference = Just negotiationRef
            }
      , inputs = flip map askUTxOs $ \(askRef,_) ->
          Input
            { inputId = askRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer CloseOrUpdateAsk)
            }
      , outputs = replicate number $
          Output
            { outputAddress = loanAddress
            , outputValue = utxoValue 3_000_000 $ mconcat
                [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
                , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon loanBeacon2) 1
                , uncurry PV2.singleton (_unAsset collateral1) 1
                , uncurry PV2.singleton (_unAsset collateral2) 1
                , uncurry PV2.singleton (_unAsset collateral3) 1
                ]
            , outputDatum = OutputDatum $ toDatum loanDatum2
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = [negotiationRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      }

-- | Convert multiple valid Ask UTxOs. All Ask UTxOs start with unique loan assets and collateral
-- and are converted to Ask UTxOs with unique loan assets and collateral. Each Ask uses
-- one native asset for collateral.
benchTest2 :: MonadEmulator m => Int -> m ()
benchTest2 number = do
  let -- Borrower Info
      borrowerWallet = Mock.knownMockWallet 1
      borrowerPersonalAddr = Mock.mockWalletAddress borrowerWallet
      borrowerPayPrivKey = Mock.paymentPrivateKey borrowerWallet
      borrowerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash borrowerWallet
      borrowerCred = PV2.PubKeyCredential borrowerPubKey
      loanAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash loanScript) 
                    (Just $ PV2.StakingHash borrowerCred)

      -- Other Info
      assetNames = map (\i -> fromString $ "TestToken" <> show @Int i) [1..120]
      
      -- Loan Info
      loanAssets = map (\name -> Asset (testTokenSymbol,name)) $ drop 60 assetNames
      collateralAssets = map (\name -> Asset (testTokenSymbol,name)) $ take 60 assetNames
      pairs = zip loanAssets collateralAssets
      datums = 
        flip map pairs $ \(loan,col) -> 
          unsafeCreateAskDatum $ NewAskInfo
            { _borrowerId = borrowerCred
            , _loanAsset = loan
            , _loanPrinciple = 10
            , _loanTerm = 3600
            , _collateral = [col]
            }
      
      beforeDatums = take 30 datums
      afterDatums = drop 30 datums

      sampleOutputs ds = flip map ds $ \datum@AskDatum{..} ->
        Output
          { outputAddress = loanAddress
          , outputValue = utxoValue 3_000_000 $ mconcat $
              [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
              , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
              ] <> map (\(Asset x) -> uncurry PV2.singleton x 1) (_unCollateral _collateral)
          , outputDatum = OutputDatum $ toDatum datum
          , outputReferenceScript = toReferenceScript Nothing
          }

  -- Initialize scenario
  References{negotiationRef,loanRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 100_000_000 $ zip assetNames $ repeat 100

  -- Create the Ask UTxO.
  forM_ (grouped 20 beforeDatums) $ \ds -> 
    transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
      emptyTxParams
        { tokens =
            [ TokenMint
                { mintTokens = flip concatMap ds $ \AskDatum{..} ->
                    [ (_unAssetBeacon _assetBeacon,1)
                    , ("Ask",1)
                    ]
                , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred
                , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
                , mintReference = Just negotiationRef
                }
            ]
        , outputs = sampleOutputs ds
        , referenceInputs = [negotiationRef]
        , extraKeyWitnesses = [borrowerPubKey]
        }

  askUTxOs <- take number <$> txOutRefsAndDatumsAtAddress @AskDatum loanAddress

  let sampleBurns = 
        zipWith (\(_,Just AskDatum{_assetBeacon=beforeBeacon}) AskDatum{_assetBeacon=afterBeacon} ->
                  TokenMint 
                    { mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
                    , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred
                    , mintTokens = 
                        [ 
                          (_unAssetBeacon beforeBeacon,-1)
                        , (_unAssetBeacon afterBeacon,1)
                        ]
                    , mintReference = Just negotiationRef
                    })
                askUTxOs
                afterDatums

  -- Try to convert the Ask UTxOs.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = sampleBurns
      , inputs = flip map askUTxOs $ \(askRef,_) ->
          Input
            { inputId = askRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer CloseOrUpdateAsk)
            }
      , outputs = sampleOutputs $ take number afterDatums
      , referenceInputs = [negotiationRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      }

-- | Convert multiple valid Ask UTxOs. All Ask UTxOs start with unique loan assets and collateral
-- and are converted to Ask UTxOs with unique loan assets and collateral. Each Ask uses
-- three native assets for collateral.
benchTest3 :: MonadEmulator m => Int -> m ()
benchTest3 number = do
  let -- Borrower Info
      borrowerWallet = Mock.knownMockWallet 1
      borrowerPersonalAddr = Mock.mockWalletAddress borrowerWallet
      borrowerPayPrivKey = Mock.paymentPrivateKey borrowerWallet
      borrowerPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash borrowerWallet
      borrowerCred = PV2.PubKeyCredential borrowerPubKey
      loanAddress = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash loanScript) 
                    (Just $ PV2.StakingHash borrowerCred)

      -- Other Info
      assetNames = map (\i -> fromString $ "TestToken" <> show @Int i) [1..240]
      
      -- Loan Info
      loanAssets = map (\name -> Asset (testTokenSymbol,name)) $ drop 180 assetNames
      collateralAssets = grouped 3 $
        map (\name -> Asset (testTokenSymbol,name)) $ take 180 assetNames
      pairs = zip loanAssets collateralAssets
      datums = 
        flip map pairs $ \(loan,col) -> 
          unsafeCreateAskDatum $ NewAskInfo
            { _borrowerId = borrowerCred
            , _loanAsset = loan
            , _loanPrinciple = 10
            , _loanTerm = 3600
            , _collateral = col
            }
      
      beforeDatums = take 30 datums
      afterDatums = drop 30 datums

      sampleOutputs ds = flip map ds $ \datum@AskDatum{..} ->
        Output
          { outputAddress = loanAddress
          , outputValue = utxoValue 3_000_000 $ mconcat $
              [ PV2.singleton negotiationBeaconCurrencySymbol "Ask" 1
              , PV2.singleton negotiationBeaconCurrencySymbol (_unAssetBeacon _assetBeacon) 1
              ] <> map (\(Asset x) -> uncurry PV2.singleton x 1) (_unCollateral _collateral)
          , outputDatum = OutputDatum $ toDatum datum
          , outputReferenceScript = toReferenceScript Nothing
          }

  -- Initialize scenario
  References{negotiationRef,loanRef} <- initializeReferenceScripts 
  mintTestTokens borrowerWallet 100_000_000 $ zip assetNames $ repeat 100

  -- Create the Ask UTxO.
  forM_ (grouped 20 beforeDatums) $ \ds -> 
    transact borrowerPersonalAddr [refScriptAddress] [borrowerPayPrivKey] $
      emptyTxParams
        { tokens =
            [ TokenMint
                { mintTokens = flip concatMap ds $ \AskDatum{..} ->
                    [ (_unAssetBeacon _assetBeacon,1)
                    , ("Ask",1)
                    ]
                , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred
                , mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
                , mintReference = Just negotiationRef
                }
            ]
        , outputs = sampleOutputs ds
        , referenceInputs = [negotiationRef]
        , extraKeyWitnesses = [borrowerPubKey]
        }

  askUTxOs <- take number <$> txOutRefsAndDatumsAtAddress @AskDatum loanAddress

  let sampleBurns = 
        zipWith (\(_,Just AskDatum{_assetBeacon=beforeBeacon}) AskDatum{_assetBeacon=afterBeacon} ->
                  TokenMint 
                    { mintPolicy = toVersionedMintingPolicy negotiationBeaconScript
                    , mintRedeemer = toRedeemer $ CreateCloseOrUpdateAsk borrowerCred
                    , mintTokens = 
                        [ 
                          (_unAssetBeacon beforeBeacon,-1)
                        , (_unAssetBeacon afterBeacon,1)
                        ]
                    , mintReference = Just negotiationRef
                    })
                askUTxOs
                afterDatums

  -- Try to convert the Ask UTxOs.
  void $ transact borrowerPersonalAddr [loanAddress,refScriptAddress] [borrowerPayPrivKey] $
    emptyTxParams
      { tokens = sampleBurns
      , inputs = flip map askUTxOs $ \(askRef,_) ->
          Input
            { inputId = askRef
            , inputWitness = 
                SpendWithPlutusReference loanRef InlineDatum (toRedeemer CloseOrUpdateAsk)
            }
      , outputs = sampleOutputs $ take number afterDatums
      , referenceInputs = [negotiationRef,loanRef]
      , extraKeyWitnesses = [borrowerPubKey]
      }

-------------------------------------------------
-- Test Function
-------------------------------------------------
-- | A `TestTree` containing all Ask update scenarios.
tests :: TestTree
tests =
  testGroup "Update Ask(s) - Basic"
    [ -- Success Tests (Regression Tests)
      mustSucceed "regressionTest1" regressionTest1
    , mustSucceed "regressionTest2" regressionTest2
    , mustSucceed "regressionTest3" regressionTest3
    , mustSucceed "regressionTest4" regressionTest4
    , mustSucceed "regressionTest5" regressionTest5

      -- Beacon Failure Tests
    , scriptMustFailWithError "beaconFailure1" 
        "Ask UTxOs must have exactly two kinds of beacons"
        beaconFailure1

      -- Credential Failure Tests
    , scriptMustFailWithError "addressFailure1" 
        "Negotiation beacon script not executed with proper redeemer"
        addressFailure1
    , scriptMustFailWithError "addressFailure2" 
        "Beacons must go to a DApp address with the borrower's staking credential"
        addressFailure2

      -- Benchmark Tests
    , mustSucceed "benchTest1" $ benchTest1 21
    , mustSucceed "benchTest2" $ benchTest2 23
    , mustSucceed "benchTest3" $ benchTest3 13

      -- Performance Increase Tests
    , mustExceedTxLimits "perfIncreaseTest1" $ benchTest1 22
    , mustExceedTxLimits "perfIncreaseTest2" $ benchTest2 24
    , mustExceedTxLimits "perfIncreaseTest3" $ benchTest3 14
    ]
