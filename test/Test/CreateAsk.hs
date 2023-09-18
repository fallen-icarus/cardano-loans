{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Test.CreateAsk
  (
    -- * Scenarios Tested
    -- ** Scenarios that should succeed
    regressionTest1
  , regressionTest2
  , regressionTest3
  , regressionTest4

    -- ** Scenarios that should fail
  , failureTest1
  , failureTest2
  , failureTest3
  , failureTest4
  , failureTest5
  , failureTest6
  , failureTest7
  , failureTest8
  , failureTest9
  , failureTest10
  , failureTest11
  , failureTest12
  , failureTest13
  , failureTest14
  , failureTest15
  , failureTest16
  , failureTest17
  , failureTest18
  , failureTest19
  , failureTest20
  , failureTest21
  , failureTest22
  , failureTest23
  , failureTest24
  , failureTest25
  , failureTest26
  , failureTest27
  , failureTest28
  , failureTest29
  , failureTest30
  , failureTest31
  , failureTest33

  -- ** Edge case scenarios
  , edgeCase1
  , edgeCase2
    
    -- * Benchmark Tests
  , benchTest1
  , benchTest2
  , benchTest3
  , benchTest4
  , benchTest5
  , benchTest6

    -- * Full test function
  , tests
  ) where

import PlutusTx.Prelude (unsafeRatio)
import Plutus.Trace
import Wallet.Emulator.Wallet
import Plutus.Contract.Test as Test
import Test.Tasty
import Plutus.V2.Ledger.Api (TxId(..))
import Data.String (fromString)

import Test.Internal
import Test.Config
import CardanoLoans

-------------------------------------------------
-- Initialize reference script.
-------------------------------------------------
initializeBeaconPolicy :: EmulatorTrace TxOutRef
initializeBeaconPolicy = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = beaconScript
      , createReferenceScriptAddress = refScriptAddress
      , createReferenceScriptUTxO = 
          ( lovelaceValueOf minUTxOMintRef
          , TxOutDatumInline $ toDatum ()
          )
      }

  void $ waitNSlots 2

  txOutRefWithValue $ lovelaceValueOf minUTxOMintRef

-------------------------------------------------
-- Regression Tests
-------------------------------------------------
-- | Create a single valid Ask UTxO. Mints an unrelated token to an unrelated output
-- in the same transaction to also check if the beacon policy can correcly
-- ignore unrelated tokens and UTxOs. 
regressionTest1 :: EmulatorTrace ()
regressionTest1 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset]
              , mintTokens = [("Ask",1),(assetBeacon,1)]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = refScriptAddress
              , outputUtxos =
                  [ ( Just $ TxOutDatumHash $ toDatum ()
                    , lovelaceValueOf 20_000_000
                    )
                  ]
              }
          ]
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

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | Create multiple valid Ask UTxOs. The Ask UTxOs are for the same Asset beacon. Mints an 
-- unrelated token to an unrelated output in the same transaction to also check if the 
-- beacon policy can correctly ignore unrelated tokens and UTxOs. 
regressionTest2 :: EmulatorTrace ()
regressionTest2 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset]
              , mintTokens = [("Ask",2),(assetBeacon,2)]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  , ( Just $ TxOutDatumInline $ toDatum askDatum{loanPrinciple = 10_000_000}
                    , lovelaceValueOf 3_000_000
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = refScriptAddress
              , outputUtxos =
                  [ ( Just $ TxOutDatumHash $ toDatum ()
                    , lovelaceValueOf 20_000_000
                    )
                  ]
              }
          ]
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

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | Create multiple valid Ask UTxOs. The Ask UTxOs are for different Asset beacons. Mints 
-- an unrelated token to an unrelated output in the same transaction to also check if the 
-- beacon policy can correctly ignore unrelated tokens and UTxOs. 
regressionTest3 :: EmulatorTrace ()
regressionTest3 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset1,asset2]
              , mintTokens = [("Ask",2),(assetBeacon1,1),(assetBeacon2,1)]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum1
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon1 1
                    )
                  , ( Just $ TxOutDatumInline $ toDatum askDatum2
                    , lovelaceValueOf 3_000_000
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon2 1
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = refScriptAddress
              , outputUtxos =
                  [ ( Just $ TxOutDatumHash $ toDatum ()
                    , lovelaceValueOf 20_000_000
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  where
    borrowerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1

    asset1 = (adaSymbol,adaToken)
    asset2 = testToken2

    assetBeacon1 = genAssetBeaconName asset1
    assetBeacon2 = genAssetBeaconName asset2

    askDatum1 = AskDatum
      { beaconSym = beaconCurrencySymbol
      , borrowerId = credentialAsToken borrowerCred
      , loanAsset = asset1
      , loanPrinciple = 100_000_000
      , loanTerm = 12000
      , collateral = [testToken1]
      }
    askDatum2 = AskDatum
      { beaconSym = beaconCurrencySymbol
      , borrowerId = credentialAsToken borrowerCred
      , loanAsset = asset2
      , loanPrinciple = 10
      , loanTerm = 12000
      , collateral = [testToken1]
      }

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | Create multiple valid Ask UTxOs. Some of the AskUTxOs are for the same Asset beacon. 
-- Mints an unrelated token to an unrelated output in the same transaction to also check 
-- if the beacon policy can correctly ignore unrelated tokens and UTxOs. 
regressionTest4 :: EmulatorTrace ()
regressionTest4 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset1,asset2]
              , mintTokens = [("Ask",3),(assetBeacon1,2),(assetBeacon2,1)]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum1
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon1 1
                    )
                  , ( Just $ TxOutDatumInline $ toDatum askDatum2
                    , lovelaceValueOf 3_000_000
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon2 1
                    )
                  , ( Just $ TxOutDatumInline $ toDatum askDatum1
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon1 1
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = refScriptAddress
              , outputUtxos =
                  [ ( Just $ TxOutDatumHash $ toDatum ()
                    , lovelaceValueOf 20_000_000
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  where
    borrowerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1

    asset1 = (adaSymbol,adaToken)
    asset2 = testToken2

    assetBeacon1 = genAssetBeaconName asset1
    assetBeacon2 = genAssetBeaconName asset2

    askDatum1 = AskDatum
      { beaconSym = beaconCurrencySymbol
      , borrowerId = credentialAsToken borrowerCred
      , loanAsset = asset1
      , loanPrinciple = 100_000_000
      , loanTerm = 12000
      , collateral = [testToken1]
      }
    askDatum2 = AskDatum
      { beaconSym = beaconCurrencySymbol
      , borrowerId = credentialAsToken borrowerCred
      , loanAsset = asset2
      , loanPrinciple = 10
      , loanTerm = 12000
      , collateral = [testToken1]
      }

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-------------------------------------------------
-- Failure Cases
-------------------------------------------------
-- | Create a single Ask UTxO with only the Asset beacon. The Ask beacon is not minted.
failureTest1 :: EmulatorTrace ()
failureTest1 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset]
              , mintTokens = [("Ask",0),(assetBeacon,1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  ]
              }
          ]
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

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)
    
-- | Create a single Ask UTxO with an Asset beacon and Ask beacon but the wrong Ask beacon 
-- (it has the wrong name). 
failureTest2 :: EmulatorTrace ()
failureTest2 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset]
              , mintTokens = [("As",1),(assetBeacon,1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "As" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  ]
              }
          ]
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

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | Mint both the proper Ask beacon and Asset beacon but only store the Asset beacon in the
-- Ask Utxo. The Ask beacon is minted to another address. 
failureTest3 :: EmulatorTrace ()
failureTest3 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset]
              , mintTokens = [("Ask",1),(assetBeacon,1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  ]
              }
          ]
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

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | Mint both the proper Ask beacon and Asset beacon but split them up into separate Ask UTxOs
-- at the loan address. Both Ask UTxOs have the same AskDatum. 
failureTest4 :: EmulatorTrace ()
failureTest4 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset]
              , mintTokens = [("Ask",1),(assetBeacon,1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    )
                  , ( Just $ TxOutDatumInline $ toDatum askDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  ]
              }
          ]
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

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | Mint 2 Ask beacons and 1 asset beacon. Create a single Ask UTxO with 1 Asset beacon 
-- but 2 Ask beacons. 
failureTest5 :: EmulatorTrace ()
failureTest5 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset]
              , mintTokens = [("Ask",2),(assetBeacon,1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 2
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  ]
              }
          ]
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

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | Mint 2 Ask beacons and 1 Asset beacon. Create 1 valid Ask UTxO and store the remaining
-- Ask beacon in a separate invalid Ask UTxO at the loan address. Both Ask UTxOs have the
-- same datum.
failureTest6 :: EmulatorTrace ()
failureTest6 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset]
              , mintTokens = [("Ask",2),(assetBeacon,1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  , ( Just $ TxOutDatumInline $ toDatum askDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    )
                  ]
              }
          ]
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

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | The Asset beacon is not minted but is in the redeemer. Only the Ask beacon is minted. 
failureTest7 :: EmulatorTrace ()
failureTest7 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset]
              , mintTokens = [("Ask",1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    )
                  ]
              }
          ]
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

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | The `CreateAsk` redeemer has an empty list of assets. No Asset beacons are minted.
failureTest8 :: EmulatorTrace ()
failureTest8 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred []
              , mintTokens = [("Ask",1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    )
                  ]
              }
          ]
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

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | The Asset beacon that is minted does not correspond to the asset in the redeemer.
failureTest9 :: EmulatorTrace ()
failureTest9 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset]
              , mintTokens = [("Ask",1),(assetBeacon,1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  where
    borrowerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1

    asset = (adaSymbol,adaToken)

    assetBeacon = genAssetBeaconName testToken3 

    askDatum = AskDatum
      { beaconSym = beaconCurrencySymbol
      , borrowerId = credentialAsToken borrowerCred
      , loanAsset = asset
      , loanPrinciple = 100_000_000
      , loanTerm = 12000
      , collateral = [testToken1]
      }

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | The AskDatum has a different loan asset than the asset that corresponds
-- to the Asset beacon.
failureTest10 :: EmulatorTrace ()
failureTest10 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset]
              , mintTokens = [("Ask",1),(assetBeacon,1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  ]
              }
          ]
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
      , loanAsset = testToken3
      , loanPrinciple = 100_000_000
      , loanTerm = 12000
      , collateral = [testToken1]
      }

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | 1 Ask beacon and 2 Asset beacons are minted. The extra Asset beacon is not
-- stored at the loan address.
failureTest11 :: EmulatorTrace ()
failureTest11 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset]
              , mintTokens = [("Ask",1),(assetBeacon,2)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  ]
              }
          ]
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

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | 1 Ask beacon and 2 Asset beacons are minted. The extra Asset beacon is stored
-- in an invalid UTxO at the loan address.
failureTest12 :: EmulatorTrace ()
failureTest12 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset]
              , mintTokens = [("Ask",1),(assetBeacon,2)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  , ( Just $ TxOutDatumInline $ toDatum askDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  ]
              }
          ]
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

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | 1 Ask beacon and 2 Asset beacons are minted. The extra Asset beacon is stored in the
-- same UTxO as the other beacons.
failureTest13 :: EmulatorTrace ()
failureTest13 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset]
              , mintTokens = [("Ask",1),(assetBeacon,2)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon 2
                    )
                  ]
              }
          ]
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

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | The Ask UTxO is correct except for being stored at a non-dapp address.
failureTest14 :: EmulatorTrace ()
failureTest14 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset]
              , mintTokens = [("Ask",1),(assetBeacon,1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  ]
              }
          ]
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

    loanAddr = Address (ScriptCredential alwaysSucceedValidatorHash) 
                       (Just $ StakingHash borrowerCred)

-- | The Ask UTxO is correct except for being stored at a dapp address without
-- a valid staking credential.
failureTest15 :: EmulatorTrace ()
failureTest15 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset]
              , mintTokens = [("Ask",1),(assetBeacon,1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  ]
              }
          ]
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

    loanAddr = Address (ScriptCredential loanValidatorHash) Nothing

-- | The Ask UTxO is correct except for being stored at a dapp address that is using
-- a different staking credential than what is in the redeemer.
failureTest16 :: EmulatorTrace ()
failureTest16 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset]
              , mintTokens = [("Ask",1),(assetBeacon,1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  where
    borrowerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1

    wrongCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2

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

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash wrongCred)

-- | The AskDatum beacon policy id field does not have the policy id for the actual
-- beacon policy.
failureTest17 :: EmulatorTrace ()
failureTest17 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset]
              , mintTokens = [("Ask",1),(assetBeacon,1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  ]
              }
          ]
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
      { beaconSym = adaSymbol
      , borrowerId = credentialAsToken borrowerCred
      , loanAsset = asset
      , loanPrinciple = 100_000_000
      , loanTerm = 12000
      , collateral = [testToken1]
      }

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | The AskDatum borrower id field does not match the credential passed to the redeemer.
failureTest18 :: EmulatorTrace ()
failureTest18 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset]
              , mintTokens = [("Ask",1),(assetBeacon,1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  ]
              }
          ]
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
      , borrowerId = "borrower"
      , loanAsset = asset
      , loanPrinciple = 100_000_000
      , loanTerm = 12000
      , collateral = [testToken1]
      }

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | The AskDatum principle field has a negative integer.
failureTest19 :: EmulatorTrace ()
failureTest19 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset]
              , mintTokens = [("Ask",1),(assetBeacon,1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  ]
              }
          ]
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
      , loanPrinciple = -100_000_000
      , loanTerm = 12000
      , collateral = [testToken1]
      }

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | The AskDatum principle field has zero.
failureTest20 :: EmulatorTrace ()
failureTest20 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset]
              , mintTokens = [("Ask",1),(assetBeacon,1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  ]
              }
          ]
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
      , loanPrinciple = 0
      , loanTerm = 12000
      , collateral = [testToken1]
      }

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | The AskDatum loan term field has a negative integer.
failureTest21 :: EmulatorTrace ()
failureTest21 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset]
              , mintTokens = [("Ask",1),(assetBeacon,1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  ]
              }
          ]
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
      , loanTerm = -12000
      , collateral = [testToken1]
      }

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | The AskDatum loan term field has zero. 
failureTest22 :: EmulatorTrace ()
failureTest22 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset]
              , mintTokens = [("Ask",1),(assetBeacon,1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  ]
              }
          ]
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
      , loanTerm = 0
      , collateral = [testToken1]
      }

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | The AskDatum collateral field is the empty list.
failureTest23 :: EmulatorTrace ()
failureTest23 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset]
              , mintTokens = [("Ask",1),(assetBeacon,1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  ]
              }
          ]
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
      , collateral = []
      }

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | The borrower did not approve the transaction. This test uses a pubkey
-- for the staking credential.
failureTest24 :: EmulatorTrace ()
failureTest24 = do
  h1 <- activateContractWallet (knownWallet 2) endpoints

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset]
              , mintTokens = [("Ask",1),(assetBeacon,1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  ]
              }
          ]
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

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | The AskDatum is not an inline datum.
failureTest25 :: EmulatorTrace ()
failureTest25 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset]
              , mintTokens = [("Ask",1),(assetBeacon,1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumHash $ toDatum askDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  ]
              }
          ]
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

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | Beacon policy is used to mint a third kind of token.
failureTest26 :: EmulatorTrace ()
failureTest26 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset]
              , mintTokens = [("Ask",1),(assetBeacon,1),("other",1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  ]
              }
          ]
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

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | The Ask UTxO has one of the other LoanDatums instead of the AskDatum.
failureTest27 :: EmulatorTrace ()
failureTest27 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset]
              , mintTokens = [("Ask",1),(assetBeacon,1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  where
    borrowerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1

    asset = (adaSymbol,adaToken)

    assetBeacon = genAssetBeaconName asset
    
    askDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = credentialAsToken borrowerCred
        , lenderAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , rolloverFrequency = Nothing
        , loanTerm = 12000
        , minPayment = 10
        , loanInterest = unsafeRatio 1 2
        , collateralization = [(testToken1, unsafeRatio 1 1)]
        , collateralIsSwappable = True
        , claimPeriod = 0
        , offerDeposit = 10
        }
      
    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | The AskDatum loan asset field is one of the beacons.
failureTest28 :: EmulatorTrace ()
failureTest28 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset]
              , mintTokens = [("Ask",1),(assetBeacon,1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  where
    borrowerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1

    asset = (beaconCurrencySymbol,"Ask")

    assetBeacon = genAssetBeaconName asset

    askDatum = AskDatum
      { beaconSym = beaconCurrencySymbol
      , borrowerId = credentialAsToken borrowerCred
      , loanAsset = asset
      , loanPrinciple = 100_000_000
      , loanTerm = 12000
      , collateral = [testToken1]
      }

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | Mint the tokens using the `BurnBeacons` redeemer.
failureTest29 :: EmulatorTrace ()
failureTest29 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ BurnBeacons
              , mintTokens = [("Ask",1),(assetBeacon,1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  ]
              }
          ]
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

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | When creating two Ask UTxOs for the same loan asset, the first Ask UTxO output
-- has too many Ask beacons. This scenario and `failureTest31` are used to explicitly check
-- that the order of UTxO outputs does not impact the overall transaction's validity.
failureTest30 :: EmulatorTrace ()
failureTest30 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset]
              , mintTokens = [("Ask",3),(assetBeacon,2)]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 2
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  , ( Just $ TxOutDatumInline $ toDatum askDatum{loanPrinciple = 10_000_000}
                    , lovelaceValueOf 3_000_000
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = refScriptAddress
              , outputUtxos =
                  [ ( Just $ TxOutDatumHash $ toDatum ()
                    , lovelaceValueOf 20_000_000
                    )
                  ]
              }
          ]
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

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | When creating two Ask UTxOs for the same loan asset, the second Ask UTxO output
-- has too many Ask beacons. This scenario and `failureTest30` are used to explicitly check
-- that the order of UTxO outputs does not impact the overall transaction's validity.
failureTest31 :: EmulatorTrace ()
failureTest31 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset]
              , mintTokens = [("Ask",3),(assetBeacon,2)]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  , ( Just $ TxOutDatumInline $ toDatum askDatum{loanPrinciple = 10_000_000}
                    , lovelaceValueOf 3_000_000
                    <> singleton beaconCurrencySymbol "Ask" 2
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = refScriptAddress
              , outputUtxos =
                  [ ( Just $ TxOutDatumHash $ toDatum ()
                    , lovelaceValueOf 20_000_000
                    )
                  ]
              }
          ]
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

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | When creating two Ask UTxOs for different loan assets, the Asset beacons are swapped
-- among the Ask UTxOs.
failureTest33 :: EmulatorTrace ()
failureTest33 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset1,asset2]
              , mintTokens = [("Ask",2),(assetBeacon1,1),(assetBeacon2,1)]
              }
          , TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum1
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon2 1
                    )
                  , ( Just $ TxOutDatumInline $ toDatum askDatum2
                    , lovelaceValueOf 3_000_000
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon1 1
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = refScriptAddress
              , outputUtxos =
                  [ ( Just $ TxOutDatumHash $ toDatum ()
                    , lovelaceValueOf 20_000_000
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  where
    borrowerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1

    asset1 = (adaSymbol,adaToken)
    asset2 = testToken2

    assetBeacon1 = genAssetBeaconName asset1
    assetBeacon2 = genAssetBeaconName asset2

    askDatum1 = AskDatum
      { beaconSym = beaconCurrencySymbol
      , borrowerId = credentialAsToken borrowerCred
      , loanAsset = asset1
      , loanPrinciple = 100_000_000
      , loanTerm = 12000
      , collateral = [testToken1]
      }
    askDatum2 = AskDatum
      { beaconSym = beaconCurrencySymbol
      , borrowerId = credentialAsToken borrowerCred
      , loanAsset = asset2
      , loanPrinciple = 10
      , loanTerm = 12000
      , collateral = [testToken1]
      }

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-------------------------------------------------
-- Edge Cases
-------------------------------------------------
-- | Mint an Asset beacon for a `TxOutRef`. This transaction will succeed because the
-- `CreateAsk` redeemer does not check whether the loan asset requested is a real asset.
-- This can be used to mint a counterfeit LoanID beacon. The counterfeit LoanID must still 
-- be stored with an Ask beacon. Meanwhile, real LoanIDs will always be found with Active 
-- beacons. As long as this invariant holds, the ability to mint counterfeit LoanIDs cannot 
-- impact the security of the protocol. Checking whether a LoanID is counterfeit or not is 
-- as simple as checking if it is stored with an Active beacon. Databases like Koios can easily
-- do this filtering for end-users so that they never have to see any counterfeit LoanIDs.
edgeCase1 :: EmulatorTrace ()
edgeCase1 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  mintRef@(TxOutRef (TxId txHash) idx) <- initializeBeaconPolicy

  let asset = (CurrencySymbol txHash, fromString $ show idx)
  let assetBeacon = genAssetBeaconName asset
  let askDatum = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = credentialAsToken borrowerCred
        , loanAsset = asset
        , loanPrinciple = 100_000_000
        , loanTerm = 12000
        , collateral = [testToken1]
        }

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset]
              , mintTokens = [("Ask",1),(assetBeacon,1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  where
    borrowerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | Create an Ask UTxO and an Offer UTxO in the same transaction. Since the `CreateAsk` and
-- `CreateOffer` redeemers both require the credential in the redeemer to approve the
-- transaction and an exact match for the beacons minted, this composition should fail 
-- even if the credential used in both redeemers is the same. There is no way to exact 
-- match the mints.
edgeCase2 :: EmulatorTrace ()
edgeCase2 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset]
              , mintTokens = [("Ask",1),(assetBeacon,1)]
              }
          , TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateOffer borrowerCred [asset]
              , mintTokens = [("Offer",1),(assetBeacon,1),(credentialAsToken borrowerCred,1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  , ( Just $ TxOutDatumInline $ toDatum askDatum
                    , lovelaceValueOf 103_000_000 
                    <> singleton beaconCurrencySymbol "Offer" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol (credentialAsToken borrowerCred) 1
                    )
                  ]
              }
          ]
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
    offerDatum = OfferDatum
      { beaconSym = beaconCurrencySymbol
      , lenderId = credentialAsToken borrowerCred
      , lenderAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
      , loanAsset = (adaSymbol,adaToken)
      , loanPrinciple = 100_000_000
      , rolloverFrequency = Nothing
      , loanTerm = 12000
      , minPayment = 10
      , loanInterest = unsafeRatio 1 2
      , collateralization = [(testToken1, unsafeRatio 1 1)]
      , collateralIsSwappable = True
      , claimPeriod = 0
      , offerDeposit = 3_000_000
      }
      
    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-------------------------------------------------
-- Benchmark Tests
-------------------------------------------------
-- | Create multiple Ask UTxOs in the same transaction. All Ask UTxOs are for the same
-- loan asset. Each Ask only used one asset for collateral.
benchTest1 :: Int -> EmulatorTrace ()
benchTest1 numberCreated = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset]
              , mintTokens = 
                  [ ("Ask",fromIntegral numberCreated)
                  , (assetBeacon,fromIntegral numberCreated)
                  ]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = replicate numberCreated 
                  ( Just $ TxOutDatumInline $ toDatum askDatum
                  , lovelaceValueOf 3_000_000 
                  <> singleton beaconCurrencySymbol "Ask" 1
                  <> singleton beaconCurrencySymbol assetBeacon 1
                  )
                  
              }
          ]
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

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | Create multiple Ask UTxOs in the same transaction. All Ask UTxOs are for different
-- loan assets. Each Ask only used one asset for collateral.
benchTest2 :: Int -> EmulatorTrace ()
benchTest2 numberCreated = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  mintRef <- initializeBeaconPolicy
  let assets = map (\i -> (fst $ loanAsset askDatum, fromString $ show @Int i)) [1..100]
  let beacons = map genAssetBeaconName assets
  let sampleOutputs =
        zipWith (\a b ->
                  ( Just $ TxOutDatumInline 
                         $ toDatum askDatum{loanAsset = a}
                  , lovelaceValueOf 3_000_000 
                  <> singleton beaconCurrencySymbol "Ask" 1
                  <> singleton beaconCurrencySymbol b 1
                  )  
                )
                assets
                beacons
  let sampleMints i =
        TokenMint
          { mintWitness = 
              ( beaconMintingPolicy
              , Just (refScriptAddress, mintRef)
              )
          , mintRedeemer = toRedeemer $ CreateAsk borrowerCred $ take i assets
          , mintTokens = ("Ask",fromIntegral i) : take i (zip beacons $ repeat 1 )
          }

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = [ sampleMints numberCreated ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = take numberCreated sampleOutputs
              }
          ]
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

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | Create multiple Ask UTxOs in the same transaction. All Ask UTxOs are for the same
-- loan asset. Each Ask uses two assets for collateral.
benchTest3 :: Int -> EmulatorTrace ()
benchTest3 numberCreated = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset]
              , mintTokens = 
                  [ ("Ask",fromIntegral numberCreated)
                  , (assetBeacon,fromIntegral numberCreated)
                  ]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = replicate numberCreated 
                  ( Just $ TxOutDatumInline $ toDatum askDatum
                  , lovelaceValueOf 3_000_000 
                  <> singleton beaconCurrencySymbol "Ask" 1
                  <> singleton beaconCurrencySymbol assetBeacon 1
                  )
                  
              }
          ]
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
      , collateral = [testToken1,testToken2]
      }

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | Create multiple Ask UTxOs in the same transaction. All Ask UTxOs are for different
-- loan assets. Each Ask uses two assets for collateral.
benchTest4 :: Int -> EmulatorTrace ()
benchTest4 numberCreated = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  mintRef <- initializeBeaconPolicy
  let assets = map (\i -> (fst $ loanAsset askDatum, fromString $ show @Int i)) [1..100]
  let beacons = map genAssetBeaconName assets
  let sampleOutputs =
        zipWith (\a b ->
                  ( Just $ TxOutDatumInline 
                         $ toDatum askDatum{loanAsset = a}
                  , lovelaceValueOf 3_000_000 
                  <> singleton beaconCurrencySymbol "Ask" 1
                  <> singleton beaconCurrencySymbol b 1
                  )  
                )
                assets
                beacons
  let sampleMints i =
        TokenMint
          { mintWitness = 
              ( beaconMintingPolicy
              , Just (refScriptAddress, mintRef)
              )
          , mintRedeemer = toRedeemer $ CreateAsk borrowerCred $ take i assets
          , mintTokens = ("Ask",fromIntegral i) : take i (zip beacons $ repeat 1 )
          }

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = [ sampleMints numberCreated ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = take numberCreated sampleOutputs
              }
          ]
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
      , collateral = [testToken1,testToken2]
      }

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | Create multiple Ask UTxOs in the same transaction. All Ask UTxOs are for the same
-- loan asset. Each Ask uses three assets for collateral.
benchTest5 :: Int -> EmulatorTrace ()
benchTest5 numberCreated = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  mintRef <- initializeBeaconPolicy

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset]
              , mintTokens = 
                  [ ("Ask",fromIntegral numberCreated)
                  , (assetBeacon,fromIntegral numberCreated)
                  ]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = replicate numberCreated 
                  ( Just $ TxOutDatumInline $ toDatum askDatum
                  , lovelaceValueOf 3_000_000 
                  <> singleton beaconCurrencySymbol "Ask" 1
                  <> singleton beaconCurrencySymbol assetBeacon 1
                  )
                  
              }
          ]
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
      , collateral = [testToken1,testToken2,testToken3]
      }

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | Create multiple Ask UTxOs in the same transaction. All Ask UTxOs are for different
-- loan assets. Each Ask uses three assets for collateral.
benchTest6 :: Int -> EmulatorTrace ()
benchTest6 numberCreated = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  mintRef <- initializeBeaconPolicy
  let assets = map (\i -> (fst $ loanAsset askDatum, fromString $ show @Int i)) [1..100]
  let beacons = map genAssetBeaconName assets
  let sampleOutputs =
        zipWith (\a b ->
                  ( Just $ TxOutDatumInline 
                         $ toDatum askDatum{loanAsset = a}
                  , lovelaceValueOf 3_000_000 
                  <> singleton beaconCurrencySymbol "Ask" 1
                  <> singleton beaconCurrencySymbol b 1
                  )  
                )
                assets
                beacons
  let sampleMints i =
        TokenMint
          { mintWitness = 
              ( beaconMintingPolicy
              , Just (refScriptAddress, mintRef)
              )
          , mintRedeemer = toRedeemer $ CreateAsk borrowerCred $ take i assets
          , mintTokens = ("Ask",fromIntegral i) : take i (zip beacons $ repeat 1 )
          }

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = [ sampleMints numberCreated ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = take numberCreated sampleOutputs
              }
          ]
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
      , collateral = [testToken1,testToken2,testToken3]
      }

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

benchTrace :: Int -> IO ()
benchTrace = runEmulatorTraceIO' def emConfig . benchTest6

-------------------------------------------------
-- Test Function
-------------------------------------------------
-- | A `TestTree` containing all `CreateAsk` scenarios.
tests :: TestTree
tests = do
  let opts = defaultCheckOptions & emulatorConfig .~ emConfig
  testGroup "Create Ask(s)"
    [ -- Success Tests (Regression Tests)
      checkPredicateOptions opts "regressionTest1"
        assertNoFailedTransactions regressionTest1
    , checkPredicateOptions opts "regressionTest2"
        assertNoFailedTransactions regressionTest2
    , checkPredicateOptions opts "regressionTest3"
        assertNoFailedTransactions regressionTest3
    , checkPredicateOptions opts "regressionTest4"
        assertNoFailedTransactions regressionTest4

      -- Failure cases
    , checkPredicateOptions opts "failureTest1"
        (assertEvaluationError "Only the Ask beacon and Asset beacons can/must be minted") failureTest1
    , checkPredicateOptions opts "failureTest2"
        (assertEvaluationError "Only the Ask beacon and Asset beacons can/must be minted") failureTest2
    , checkPredicateOptions opts "failureTest3"
        (assertEvaluationError "Beacons not minted to a valid Ask UTxO") failureTest3
    , checkPredicateOptions opts "failureTest4"
        (assertEvaluationError "Beacons not minted to a valid Ask UTxO") failureTest4
    , checkPredicateOptions opts "failureTest5"
        (assertEvaluationError "UTxO has more than 1 of the phase beacon or the Asset beacon") failureTest5
    , checkPredicateOptions opts "failureTest6"
        (assertEvaluationError "Beacons not minted to a valid Ask UTxO") failureTest6
    , checkPredicateOptions opts "failureTest7"
        (assertEvaluationError "Only the Ask beacon and Asset beacons can/must be minted") failureTest7
    , checkPredicateOptions opts "failureTest8"
        (assertEvaluationError "Beacons not minted to a valid Ask UTxO") failureTest8
    , checkPredicateOptions opts "failureTest9"
        (assertEvaluationError "Only the Ask beacon and Asset beacons can/must be minted") failureTest9
    , checkPredicateOptions opts "failureTest10"
        (assertEvaluationError "Invalid AskDatum") failureTest10
    , checkPredicateOptions opts "failureTest11"
        (assertEvaluationError "Beacons not minted to a valid Ask UTxO") failureTest11
    , checkPredicateOptions opts "failureTest12"
        (assertEvaluationError "Beacons not minted to a valid Ask UTxO") failureTest12
    , checkPredicateOptions opts "failureTest13"
        (assertEvaluationError "UTxO has more than 1 of the phase beacon or the Asset beacon") failureTest13
      -- Cannot hardcode error message for failureTest14 since it uses the variable `app_name`.
    , checkPredicateOptions opts "failureTest14"
        (Test.not assertNoFailedTransactions) failureTest14
      -- Cannot hardcode error message for failureTest15 since it uses the variable `app_name`.
    , checkPredicateOptions opts "failureTest15"
        (Test.not assertNoFailedTransactions) failureTest15
      -- Cannot hardcode error message for failureTest16 since it uses the variable `app_name`.
    , checkPredicateOptions opts "failureTest16"
        (Test.not assertNoFailedTransactions) failureTest16
    , checkPredicateOptions opts "failureTest17"
        (assertEvaluationError "Invalid AskDatum") failureTest17
    , checkPredicateOptions opts "failureTest18"
        (assertEvaluationError "Invalid AskDatum") failureTest18
    , checkPredicateOptions opts "failureTest19"
        (assertEvaluationError "Invalid AskDatum") failureTest19
    , checkPredicateOptions opts "failureTest20"
        (assertEvaluationError "Invalid AskDatum") failureTest20
    , checkPredicateOptions opts "failureTest21"
        (assertEvaluationError "Invalid AskDatum") failureTest21
    , checkPredicateOptions opts "failureTest22"
        (assertEvaluationError "Invalid AskDatum") failureTest22
    , checkPredicateOptions opts "failureTest23"
        (assertEvaluationError "Invalid AskDatum") failureTest23
    , checkPredicateOptions opts "failureTest24"
        (assertEvaluationError "Borrower did not approve") failureTest24
    , checkPredicateOptions opts "failureTest25"
        (assertEvaluationError "All datums must be inline datums") failureTest25
    , checkPredicateOptions opts "failureTest26"
        (assertEvaluationError "Only the Ask beacon and Asset beacons can/must be minted") failureTest26
    , checkPredicateOptions opts "failureTest27"
        (assertEvaluationError "Beacons not stored with AskDatum") failureTest27
    , checkPredicateOptions opts "failureTest28"
        (assertEvaluationError "Invalid AskDatum") failureTest28
    , checkPredicateOptions opts "failureTest29"
        (assertEvaluationError "This redeemer can only be used to burn") failureTest29
    , checkPredicateOptions opts "failureTest30"
        (assertEvaluationError "UTxO has more than 1 of the phase beacon or the Asset beacon") failureTest30
    , checkPredicateOptions opts "failureTest31"
        (assertEvaluationError "UTxO has more than 1 of the phase beacon or the Asset beacon") failureTest31
    , checkPredicateOptions opts "failureTest33"
        (assertEvaluationError "Invalid AskDatum") failureTest33
      -- Edge cases
    , checkPredicateOptions opts "edgeCase1"
        assertNoFailedTransactions edgeCase1
    , checkPredicateOptions opts "edgeCase2"
        (assertEvaluationError "Only the Ask beacon and Asset beacons can/must be minted") edgeCase2

      -- Benchmarks
    , checkPredicateOptions opts "benchTest1"
        assertNoFailedTransactions $ benchTest1 59
    , checkPredicateOptions opts "benchTest2"
        assertNoFailedTransactions $ benchTest2 28
    , checkPredicateOptions opts "benchTest3"
        assertNoFailedTransactions $ benchTest3 51
    , checkPredicateOptions opts "benchTest4"
        assertNoFailedTransactions $ benchTest4 28
    , checkPredicateOptions opts "benchTest5"
        assertNoFailedTransactions $ benchTest5 45
    , checkPredicateOptions opts "benchTest6"
        assertNoFailedTransactions $ benchTest6 28
    ]

testTrace :: IO ()
testTrace = runEmulatorTraceIO' def emConfig regressionTest2

