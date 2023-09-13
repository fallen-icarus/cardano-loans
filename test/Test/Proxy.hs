{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Test.Proxy
  (
    -- * Scenarios Tested
    -- ** Scenarios that should succeed
    regressionTest1
    
    -- ** Scenarios that should fail
  , failureTest1

    -- * Full test function
  , tests
  ) where

import Plutus.Trace
import Wallet.Emulator.Wallet
import Plutus.Contract.Test as Test
import Test.Tasty

import Test.Internal
import Test.Config
import CardanoLoans

-------------------------------------------------
-- Regression Tests
-------------------------------------------------
-- | Spend a UTxO from the proxy script.
regressionTest1 :: EmulatorTrace ()
regressionTest1 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = [ ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = proxyAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum
                    , lovelaceValueOf 3_000_000 
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            )
            askDatum

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = [ ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (proxyValidator, Nothing)
              , spendRedeemer = toRedeemer CloseAsk
              , spendFromAddress = proxyAddr
              , spendUtxos = [ ask1 ]
              }
          ]
      , outputs = [ ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  where
    borrowerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1

    asset = (adaSymbol,adaToken)

    assetBeacon = genAssetBeaconName asset

    askDatum = AskDatum
      { beaconSym = beaconCurrencySymbol
      , borrowerId = credentialAsToken borrowerCred
      , loanAsset = asset
      , loanPrinciple = 100_000_000
      , loanTerm = 12000
      , collateral = [testToken1]
      }

    proxyAddr = Address (ScriptCredential proxyValidatorHash) (Just $ StakingHash borrowerCred)

-------------------------------------------------
-- Failure Tests
-------------------------------------------------
-- | The address' staking credential did not approve.
failureTest1 :: EmulatorTrace ()
failureTest1 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints


  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = [ ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = proxyAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum
                    , lovelaceValueOf 3_000_000 
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            )
            askDatum

  callEndpoint @"create-transaction" h2 $
    CreateTransactionParams
      { tokens = [ ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (proxyValidator, Nothing)
              , spendRedeemer = toRedeemer CloseAsk
              , spendFromAddress = proxyAddr
              , spendUtxos = [ ask1 ]
              }
          ]
      , outputs = [ ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  where
    borrowerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1

    asset = (adaSymbol,adaToken)

    assetBeacon = genAssetBeaconName asset

    askDatum = AskDatum
      { beaconSym = beaconCurrencySymbol
      , borrowerId = credentialAsToken borrowerCred
      , loanAsset = asset
      , loanPrinciple = 100_000_000
      , loanTerm = 12000
      , collateral = [testToken1]
      }

    proxyAddr = Address (ScriptCredential proxyValidatorHash) (Just $ StakingHash borrowerCred)

-------------------------------------------------
-- Test Function
-------------------------------------------------
-- | A `TestTree` containing all `CreateAsk` scenarios.
tests :: TestTree
tests = do
  let opts = defaultCheckOptions & emulatorConfig .~ emConfig
  testGroup "Proxy Script"
    [ -- Success Tests (Regression Tests)
      checkPredicateOptions opts "regressionTest1"
        assertNoFailedTransactions regressionTest1
        
      -- Failure Tests
    , checkPredicateOptions opts "failureTest1"
        (assertEvaluationError "Staking credential did not approve") failureTest1
    ]

testTrace :: IO ()
testTrace = runEmulatorTraceIO' def emConfig failureTest1
