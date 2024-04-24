{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Proxy where

import qualified Ledger.Address as LA
import qualified PlutusLedgerApi.V2 as PV2
import qualified Ledger.CardanoWallet as Mock 
import Test.Tasty (TestTree,testGroup)

import CardanoLoans

import Test.Prelude

-------------------------------------------------
-- Regression Scenario Tests
-------------------------------------------------
-- | Spend a single UTxO from the proxy address.
regressionTest1 :: MonadEmulator m => m ()
regressionTest1 = do
  let -- Lender Info
      lenderWallet = Mock.knownMockWallet 2
      lenderPersonalAddr = Mock.mockWalletAddress lenderWallet
      lenderPayPrivKey = Mock.paymentPrivateKey lenderWallet
      lenderPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash lenderWallet
      lenderCred = PV2.PubKeyCredential lenderPubKey
      lenderAddr = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash proxyScript) 
                    (Just $ PV2.StakingHash lenderCred)

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 

  -- Create the UTxOs at the proxy address.
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens = []
      , outputs =
          [ Output
              { outputAddress = lenderAddr
              , outputValue = utxoValue 3_000_000 mempty
              , outputDatum = OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,"")
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = []
      , extraKeyWitnesses = []
      }

  proxyUTxOs <- txOutRefsAndDatumsAtAddress @PaymentDatum lenderAddr

  -- Create the UTxOs at the proxy address.
  void $ transact lenderPersonalAddr [refScriptAddress,lenderAddr] [lenderPayPrivKey] $
    emptyTxParams
      { tokens = []
      , outputs = []
      , inputs = flip map proxyUTxOs $ \(proxyUTxO,_) ->
          Input
            { inputId = proxyUTxO
            , inputWitness = 
                SpendWithPlutusReference proxyRef InlineDatum (toRedeemer Unlock)
            }
      , referenceInputs = [proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

-- | Spend multiple UTxOs from the proxy address.
regressionTest2 :: MonadEmulator m => m ()
regressionTest2 = do
  let -- Lender Info
      lenderWallet = Mock.knownMockWallet 2
      lenderPersonalAddr = Mock.mockWalletAddress lenderWallet
      lenderPayPrivKey = Mock.paymentPrivateKey lenderWallet
      lenderPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash lenderWallet
      lenderCred = PV2.PubKeyCredential lenderPubKey
      lenderAddr = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash proxyScript) 
                    (Just $ PV2.StakingHash lenderCred)

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 

  -- Create the UTxOs at the proxy address.
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens = []
      , outputs = replicate 2 $
          Output
            { outputAddress = lenderAddr
            , outputValue = utxoValue 3_000_000 mempty
            , outputDatum = OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,"")
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = []
      , extraKeyWitnesses = []
      }

  proxyUTxOs <- txOutRefsAndDatumsAtAddress @PaymentDatum lenderAddr

  -- Create the UTxOs at the proxy address.
  void $ transact lenderPersonalAddr [refScriptAddress,lenderAddr] [lenderPayPrivKey] $
    emptyTxParams
      { tokens = []
      , outputs = []
      , inputs = flip map proxyUTxOs $ \(proxyUTxO,_) ->
          Input
            { inputId = proxyUTxO
            , inputWitness = 
                SpendWithPlutusReference proxyRef InlineDatum (toRedeemer Unlock)
            }
      , referenceInputs = [proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

-- | Spend a single UTxO from the proxy address that has a different datum than one used 
-- by the loans protocol.
regressionTest3 :: MonadEmulator m => m ()
regressionTest3 = do
  let -- Lender Info
      lenderWallet = Mock.knownMockWallet 2
      lenderPersonalAddr = Mock.mockWalletAddress lenderWallet
      lenderPayPrivKey = Mock.paymentPrivateKey lenderWallet
      lenderPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash lenderWallet
      lenderCred = PV2.PubKeyCredential lenderPubKey
      lenderAddr = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash proxyScript) 
                    (Just $ PV2.StakingHash lenderCred)

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 

  -- Create the UTxOs at the proxy address.
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens = []
      , outputs = replicate 2 $
          Output
            { outputAddress = lenderAddr
            , outputValue = utxoValue 3_000_000 mempty
            , outputDatum = OutputDatum $ toDatum ()
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = []
      , extraKeyWitnesses = []
      }

  proxyUTxOs <- txOutRefsAndDatumsAtAddress @PaymentDatum lenderAddr

  -- Create the UTxOs at the proxy address.
  void $ transact lenderPersonalAddr [refScriptAddress,lenderAddr] [lenderPayPrivKey] $
    emptyTxParams
      { tokens = []
      , outputs = []
      , inputs = flip map proxyUTxOs $ \(proxyUTxO,_) ->
          Input
            { inputId = proxyUTxO
            , inputWitness = 
                SpendWithPlutusReference proxyRef InlineDatum (toRedeemer Unlock)
            }
      , referenceInputs = [proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

-- | Spend a single UTxO from the proxy address using a different redeemer than the ones used
-- by the loans protocol.
regressionTest4 :: MonadEmulator m => m ()
regressionTest4 = do
  let -- Lender Info
      lenderWallet = Mock.knownMockWallet 2
      lenderPersonalAddr = Mock.mockWalletAddress lenderWallet
      lenderPayPrivKey = Mock.paymentPrivateKey lenderWallet
      lenderPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash lenderWallet
      lenderCred = PV2.PubKeyCredential lenderPubKey
      lenderAddr = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash proxyScript) 
                    (Just $ PV2.StakingHash lenderCred)

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 

  -- Create the UTxOs at the proxy address.
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens = []
      , outputs =
          [ Output
              { outputAddress = lenderAddr
              , outputValue = utxoValue 3_000_000 mempty
              , outputDatum = OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,"")
              , outputReferenceScript = toReferenceScript Nothing
              }
          ]
      , referenceInputs = []
      , extraKeyWitnesses = []
      }

  proxyUTxOs <- txOutRefsAndDatumsAtAddress @PaymentDatum lenderAddr

  -- Create the UTxOs at the proxy address.
  void $ transact lenderPersonalAddr [refScriptAddress,lenderAddr] [lenderPayPrivKey] $
    emptyTxParams
      { tokens = []
      , outputs = []
      , inputs = flip map proxyUTxOs $ \(proxyUTxO,_) ->
          Input
            { inputId = proxyUTxO
            , inputWitness = 
                SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
            }
      , referenceInputs = [proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

-------------------------------------------------
-- Failure Tests
-------------------------------------------------
-- | The staking credential of the proxy address did not approve.
failureTest1 :: MonadEmulator m => m ()
failureTest1 = do
  let -- Lender Info
      lenderWallet = Mock.knownMockWallet 2
      lenderPersonalAddr = Mock.mockWalletAddress lenderWallet
      lenderPayPrivKey = Mock.paymentPrivateKey lenderWallet
      lenderPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash lenderWallet
      lenderCred = PV2.PubKeyCredential lenderPubKey
      lenderAddr = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash proxyScript) 
                    (Just $ PV2.StakingHash lenderCred)

      -- Other User Info
      lenderWallet2 = Mock.knownMockWallet 1
      lenderPersonalAddr2 = Mock.mockWalletAddress lenderWallet2
      lenderPayPrivKey2 = Mock.paymentPrivateKey lenderWallet2
      lenderPubKey2 = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash lenderWallet2
      lenderCred2 = PV2.PubKeyCredential lenderPubKey2
      lenderAddr2 = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash proxyScript) 
                    (Just $ PV2.StakingHash lenderCred2)
  -- Initialize scenario
  References{..} <- initializeReferenceScripts 

  -- Create the UTxOs at the proxy address.
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens = []
      , outputs = replicate 2 $
          Output
            { outputAddress = lenderAddr
            , outputValue = utxoValue 3_000_000 mempty
            , outputDatum = OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,"")
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = []
      , extraKeyWitnesses = []
      }

  proxyUTxOs <- txOutRefsAndDatumsAtAddress @PaymentDatum lenderAddr

  -- Create the UTxOs at the proxy address.
  void $ transact lenderPersonalAddr2 [refScriptAddress,lenderAddr] [lenderPayPrivKey2] $
    emptyTxParams
      { tokens = []
      , outputs = []
      , inputs = flip map proxyUTxOs $ \(proxyUTxO,_) ->
          Input
            { inputId = proxyUTxO
            , inputWitness = 
                SpendWithPlutusReference proxyRef InlineDatum (toRedeemer ())
            }
      , referenceInputs = [proxyRef]
      , extraKeyWitnesses = [lenderPubKey2]
      }

-------------------------------------------------
-- Benchmark Tests
-------------------------------------------------
-- | Spend multiple UTxOs from the proxy address.
benchTest1 :: MonadEmulator m => Int -> m ()
benchTest1 number = do
  let -- Lender Info
      lenderWallet = Mock.knownMockWallet 2
      lenderPersonalAddr = Mock.mockWalletAddress lenderWallet
      lenderPayPrivKey = Mock.paymentPrivateKey lenderWallet
      lenderPubKey = LA.unPaymentPubKeyHash $ Mock.paymentPubKeyHash lenderWallet
      lenderCred = PV2.PubKeyCredential lenderPubKey
      lenderAddr = toCardanoApiAddress $
        PV2.Address (PV2.ScriptCredential $ scriptHash proxyScript) 
                    (Just $ PV2.StakingHash lenderCred)

  -- Initialize scenario
  References{..} <- initializeReferenceScripts 

  -- Create the UTxOs at the proxy address.
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens = []
      , outputs = replicate 40 $
          Output
            { outputAddress = lenderAddr
            , outputValue = utxoValue 3_000_000 mempty
            , outputDatum = OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,"")
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = []
      , extraKeyWitnesses = []
      }

  -- Create the UTxOs at the proxy address.
  void $ transact lenderPersonalAddr [refScriptAddress] [lenderPayPrivKey] $
    emptyTxParams
      { tokens = []
      , outputs = replicate 40 $
          Output
            { outputAddress = lenderAddr
            , outputValue = utxoValue 3_000_000 mempty
            , outputDatum = OutputDatum $ toDatum $ PaymentDatum (activeBeaconCurrencySymbol,"")
            , outputReferenceScript = toReferenceScript Nothing
            }
      , referenceInputs = []
      , extraKeyWitnesses = []
      }

  proxyUTxOs <- take number <$> txOutRefsAndDatumsAtAddress @PaymentDatum lenderAddr

  -- Create the UTxOs at the proxy address.
  void $ transact lenderPersonalAddr [refScriptAddress,lenderAddr] [lenderPayPrivKey] $
    emptyTxParams
      { tokens = []
      , outputs = []
      , inputs = flip map proxyUTxOs $ \(proxyUTxO,_) ->
          Input
            { inputId = proxyUTxO
            , inputWitness = 
                SpendWithPlutusReference proxyRef InlineDatum (toRedeemer Unlock)
            }
      , referenceInputs = [proxyRef]
      , extraKeyWitnesses = [lenderPubKey]
      }

-------------------------------------------------
-- TestTree
-------------------------------------------------
-- | A `TestTree` containing all scenarios for the proxy script.
tests :: TestTree
tests = testGroup "Proxy Script Tests"
  [ -- Regression Tests
    mustSucceed "regressionTest1" regressionTest1
  , mustSucceed "regressionTest2" regressionTest2
  , mustSucceed "regressionTest3" regressionTest3
  , mustSucceed "regressionTest4" regressionTest4

    -- Failure Tests
  , scriptMustFailWithError "failureTest1" 
      "Staking credential did not approve"
      failureTest1

    -- Benchmark Tests
  , mustSucceed "benchTest1" $ benchTest1 71

    -- Performance Increase Tests
  , mustExceedTxLimits "perfIncreaseTest1" $ benchTest1 72
  ]
