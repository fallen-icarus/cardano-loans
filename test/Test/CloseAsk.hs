{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Test.CloseAsk
  (
    -- * Scenarios Tested
    -- ** Scenarios that should succeed
    regressionTest1
  , regressionTest2
  , regressionTest3
  , regressionTest4
  , regressionTest5
  
    -- ** Scenarios that should fail
  , failureTest1
  , failureTest2
  , failureTest3
  , failureTest4
  , failureTest5
  , failureTest6
  , failureTest7
  , failureTest8

    -- ** Edge Cases
  , edgeCase1
  , edgeCase2

    -- * Benchmark tests
  , benchTest1
  , benchTest2
  , benchTest3
  , benchTest4

    -- * Full test function
  , tests
  ) where

import Plutus.Trace
import Wallet.Emulator.Wallet
import Plutus.Contract.Test as Test
import Test.Tasty
import Data.String (fromString)

import Test.Internal
import Test.Config
import CardanoLoans

-------------------------------------------------
-- Initialize Reference Scripts
-------------------------------------------------
initializeScripts :: EmulatorTrace ( TxOutRef,TxOutRef )
initializeScripts = do
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

  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = loanScript
      , createReferenceScriptAddress = refScriptAddress
      , createReferenceScriptUTxO = 
          ( lovelaceValueOf minUTxOSpendRef
          , TxOutDatumInline $ toDatum ()
          )
      }

  void $ waitNSlots 2

  liftM2 (,) (txOutRefWithValue $ lovelaceValueOf minUTxOMintRef)
             (txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef)

-------------------------------------------------
-- Regression Tests
-------------------------------------------------
-- | Close a single valid Ask UTxO. Mints an unrelated token to an unrelated output
-- in the same transaction to also check if the beacon policy can correcly
-- ignore unrelated tokens and UTxOs. 
regressionTest1 :: EmulatorTrace ()
regressionTest1 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  ( mintRef,spendRef ) <- initializeScripts

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

  void $ waitNSlots 2

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Ask" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            askDatum
  
  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
              , mintTokens = [("Ask",-1),(assetBeacon,-1)]
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
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseAsk
              , spendFromAddress = loanAddr
              , spendUtxos = [ ask1 ]
              }
          ]
      , outputs =
          [ UtxoOutput
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
      , borrowerId = credentialAsToken False borrowerCred
      , loanAsset = asset
      , loanPrinciple = 100_000_000
      , loanTerm = 12000
      , collateral = [testToken1]
      }

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)
    
-- | Close a single invalid Ask UTxO. Mints an unrelated token to an unrelated output
-- in the same transaction to also check if the beacon policy can correcly
-- ignore unrelated tokens and UTxOs. 
regressionTest2 :: EmulatorTrace ()
regressionTest2 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  ( mintRef,spendRef ) <- initializeScripts

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
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
      { tokens = 
          [ 
            TokenMint 
              { mintWitness =
                  ( alwaysSucceedPolicy
                  , Nothing
                  )
              , mintRedeemer = toRedeemer ()
              , mintTokens = [("Other",1)]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseAsk
              , spendFromAddress = loanAddr
              , spendUtxos = [ ask1 ]
              }
          ]
      , outputs =
          [ UtxoOutput
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
      , borrowerId = credentialAsToken False borrowerCred
      , loanAsset = asset
      , loanPrinciple = 100_000_000
      , loanTerm = 12000
      , collateral = [testToken1]
      }

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)
    
-- | Close a single valid Ask UTxO and a single invalid Ask UTxO. Mints an unrelated token to 
-- an unrelated output in the same transaction to also check if the beacon policy can correcly
-- ignore unrelated tokens and UTxOs. 
regressionTest3 :: EmulatorTrace ()
regressionTest3 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  ( mintRef,spendRef ) <- initializeScripts

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
                  , ( Just $ TxOutDatumInline $ toDatum askDatum
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
            <> singleton beaconCurrencySymbol "Ask" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            askDatum
  ask2 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            )
            askDatum
  
  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
              , mintTokens = [("Ask",-1),(assetBeacon,-1)]
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
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseAsk
              , spendFromAddress = loanAddr
              , spendUtxos = [ ask1, ask2 ]
              }
          ]
      , outputs =
          [ UtxoOutput
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
      , borrowerId = credentialAsToken False borrowerCred
      , loanAsset = asset
      , loanPrinciple = 100_000_000
      , loanTerm = 12000
      , collateral = [testToken1]
      }

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | Close a multiple valid Ask UTxOs for the same loan asset. Mints an unrelated token to 
-- an unrelated output in the same transaction to also check if the beacon policy can correcly 
-- ignore unrelated tokens and UTxOs. 
regressionTest4 :: EmulatorTrace ()
regressionTest4 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  ( mintRef,spendRef ) <- initializeScripts

  -- Create the Ask UTxO.
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
                  , ( Just $ TxOutDatumInline $ toDatum askDatum{loanTerm = 22000}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Ask" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            askDatum
  ask2 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Ask" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            askDatum{loanTerm = 22000}
  
  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
              , mintTokens = [("Ask",-2),(assetBeacon,-2)]
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
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseAsk
              , spendFromAddress = loanAddr
              , spendUtxos = [ ask1, ask2 ]
              }
          ]
      , outputs =
          [ UtxoOutput
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
      , borrowerId = credentialAsToken False borrowerCred
      , loanAsset = asset
      , loanPrinciple = 100_000_000
      , loanTerm = 12000
      , collateral = [testToken1]
      }

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | Close a multiple valid Ask UTxOs for different loan assets. Mints an unrelated token to 
-- an unrelated output in the same transaction to also check if the beacon policy can correcly 
-- ignore unrelated tokens and UTxOs. 
regressionTest5 :: EmulatorTrace ()
regressionTest5 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  ( mintRef,spendRef ) <- initializeScripts

  -- Create the Ask UTxO.
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
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Ask" 1
            <> singleton beaconCurrencySymbol assetBeacon1 1
            )
            askDatum1
  ask2 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Ask" 1
            <> singleton beaconCurrencySymbol assetBeacon2 1
            )
            askDatum2
  
  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
              , mintTokens = [("Ask",-2),(assetBeacon1,-1),(assetBeacon2,-1)]
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
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseAsk
              , spendFromAddress = loanAddr
              , spendUtxos = [ ask1, ask2 ]
              }
          ]
      , outputs =
          [ UtxoOutput
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
      , borrowerId = credentialAsToken False borrowerCred
      , loanAsset = asset1
      , loanPrinciple = 100_000_000
      , loanTerm = 12000
      , collateral = [testToken1]
      }

    askDatum2 = AskDatum
      { beaconSym = beaconCurrencySymbol
      , borrowerId = credentialAsToken False borrowerCred
      , loanAsset = asset2
      , loanPrinciple = 100_000_000
      , loanTerm = 12000
      , collateral = [testToken1]
      }

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-------------------------------------------------
-- Failure Tests
-------------------------------------------------
-- | Don't burn the Ask beacon when closing a single valid Ask UTxO. The Asset beacon is
-- still burned.
failureTest1 :: EmulatorTrace ()
failureTest1 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  ( mintRef,spendRef ) <- initializeScripts

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

  void $ waitNSlots 2

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Ask" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            askDatum
  
  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
              , mintTokens = [("Ask",0),(assetBeacon,-1)]
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
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseAsk
              , spendFromAddress = loanAddr
              , spendUtxos = [ ask1 ]
              }
          ]
      , outputs =
          [ UtxoOutput
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
      , borrowerId = credentialAsToken False borrowerCred
      , loanAsset = asset
      , loanPrinciple = 100_000_000
      , loanTerm = 12000
      , collateral = [testToken1]
      }

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)
    
-- | Don't burn the Asset beacon when closing a single valid Ask UTxO. The Ask beacon is
-- still burned.
failureTest2 :: EmulatorTrace ()
failureTest2 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  ( mintRef,spendRef ) <- initializeScripts

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

  void $ waitNSlots 2

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Ask" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            askDatum
  
  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
              , mintTokens = [("Ask",-1),(assetBeacon,0)]
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
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseAsk
              , spendFromAddress = loanAddr
              , spendUtxos = [ ask1 ]
              }
          ]
      , outputs =
          [ UtxoOutput
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
      , borrowerId = credentialAsToken False borrowerCred
      , loanAsset = asset
      , loanPrinciple = 100_000_000
      , loanTerm = 12000
      , collateral = [testToken1]
      }

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)
    
-- | Don't burn either the Ask beacon or the Asset beacon when closing a single valid Ask
-- UTxO.
failureTest3 :: EmulatorTrace ()
failureTest3 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  ( mintRef,spendRef ) <- initializeScripts

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

  void $ waitNSlots 2

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Ask" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            askDatum
  
  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
              , mintTokens = []
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
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseAsk
              , spendFromAddress = loanAddr
              , spendUtxos = [ ask1 ]
              }
          ]
      , outputs =
          [ UtxoOutput
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
      , borrowerId = credentialAsToken False borrowerCred
      , loanAsset = asset
      , loanPrinciple = 100_000_000
      , loanTerm = 12000
      , collateral = [testToken1]
      }

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)
    
-- | The borrower did not approve the transaction.
failureTest4 :: EmulatorTrace ()
failureTest4 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  ( mintRef,spendRef ) <- initializeScripts

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

  void $ waitNSlots 2

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Ask" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            askDatum
  
  callEndpoint @"create-transaction" h2 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
              , mintTokens = [("Ask",-1),(assetBeacon,-1)]
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
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseAsk
              , spendFromAddress = loanAddr
              , spendUtxos = [ ask1 ]
              }
          ]
      , outputs =
          [ UtxoOutput
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
      , borrowerId = credentialAsToken False borrowerCred
      , loanAsset = asset
      , loanPrinciple = 100_000_000
      , loanTerm = 12000
      , collateral = [testToken1]
      }

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)
    
-- | Don't burn all Ask beacons when closing multiple valid Ask UTxOs.
failureTest5 :: EmulatorTrace ()
failureTest5 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  ( mintRef,spendRef ) <- initializeScripts

  -- Create the Ask UTxO.
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
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Ask" 1
            <> singleton beaconCurrencySymbol assetBeacon1 1
            )
            askDatum1
  ask2 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Ask" 1
            <> singleton beaconCurrencySymbol assetBeacon2 1
            )
            askDatum2
  
  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
              , mintTokens = [("Ask",-1),(assetBeacon1,-1),(assetBeacon2,-1)]
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
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseAsk
              , spendFromAddress = loanAddr
              , spendUtxos = [ ask1, ask2 ]
              }
          ]
      , outputs =
          [ UtxoOutput
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
      , borrowerId = credentialAsToken False borrowerCred
      , loanAsset = asset1
      , loanPrinciple = 100_000_000
      , loanTerm = 12000
      , collateral = [testToken1]
      }

    askDatum2 = AskDatum
      { beaconSym = beaconCurrencySymbol
      , borrowerId = credentialAsToken False borrowerCred
      , loanAsset = asset2
      , loanPrinciple = 100_000_000
      , loanTerm = 12000
      , collateral = [testToken1]
      }

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | Don't burn all Asset beacons when closing multiple valid Ask UTxOs for different loan
-- assets.
failureTest6 :: EmulatorTrace ()
failureTest6 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  ( mintRef,spendRef ) <- initializeScripts

  -- Create the Ask UTxO.
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
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Ask" 1
            <> singleton beaconCurrencySymbol assetBeacon1 1
            )
            askDatum1
  ask2 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Ask" 1
            <> singleton beaconCurrencySymbol assetBeacon2 1
            )
            askDatum2
  
  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
              , mintTokens = [("Ask",-2),(assetBeacon1,0),(assetBeacon2,-1)]
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
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseAsk
              , spendFromAddress = loanAddr
              , spendUtxos = [ ask1, ask2 ]
              }
          ]
      , outputs =
          [ UtxoOutput
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
      , borrowerId = credentialAsToken False borrowerCred
      , loanAsset = asset1
      , loanPrinciple = 100_000_000
      , loanTerm = 12000
      , collateral = [testToken1]
      }

    askDatum2 = AskDatum
      { beaconSym = beaconCurrencySymbol
      , borrowerId = credentialAsToken False borrowerCred
      , loanAsset = asset2
      , loanPrinciple = 100_000_000
      , loanTerm = 12000
      , collateral = [testToken1]
      }

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | One of the inputs does not have an AskDatum when closing multiple Ask UTxOs.
failureTest7 :: EmulatorTrace ()
failureTest7 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  ( mintRef,spendRef ) <- initializeScripts

  -- Create the Ask UTxO.
  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset2]
              , mintTokens = [("Ask",1),(assetBeacon2,1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum askDatum1
                    , lovelaceValueOf 3_000_000 
                    )
                  , ( Just $ TxOutDatumInline $ toDatum askDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon2 1
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
            askDatum1
  ask2 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Ask" 1
            <> singleton beaconCurrencySymbol assetBeacon2 1
            )
            askDatum2
  
  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
              , mintTokens = [("Ask",-1),(assetBeacon2,-1)]
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
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseAsk
              , spendFromAddress = loanAddr
              , spendUtxos = [ ask1, ask2 ]
              }
          ]
      , outputs =
          [ UtxoOutput
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

    askDatum1 = OfferDatum
      { beaconSym = beaconCurrencySymbol
      , lenderId = credentialAsToken False borrowerCred
      , lenderAddress = Address borrowerCred Nothing
      , loanAsset = asset1
      , loanPrinciple = 100_000_000
      , rolloverFrequency = Just 1
      , minPayment = 500_000
      , loanTerm = 12000
      , loanInterest = unsafeRatio 1 10
      , collateralization = [(testToken1,unsafeRatio 1 10)]
      , claimPeriod = 10000
      , offerDeposit = 3_000_000
      , collateralIsSwappable = True
      }
    
    askDatum2 = AskDatum
      { beaconSym = beaconCurrencySymbol
      , borrowerId = credentialAsToken False borrowerCred
      , loanAsset = asset2
      , loanPrinciple = 100_000_000
      , loanTerm = 12000
      , collateral = [testToken1]
      }

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | Close a single valid Offer UTxO using the CloseAsk redeemer.
failureTest8 :: EmulatorTrace ()
failureTest8 = do
  h2 <- activateContractWallet (knownWallet 2) endpoints

  ( mintRef,spendRef ) <- initializeScripts

  callEndpoint @"create-transaction" h2 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateOffer lenderCred [asset]
              , mintTokens = [("Offer",1),(assetBeacon,1),(lenderToken,1)]
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
                  [ ( Just $ TxOutDatumInline $ toDatum offerDatum
                    , lovelaceValueOf 103_000_000 
                    <> singleton beaconCurrencySymbol "Offer" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol lenderToken 1
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

  void $ waitNSlots 2

  offer <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 103_000_000 
            <> singleton beaconCurrencySymbol "Offer" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum
  
  callEndpoint @"create-transaction" h2 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer BurnBeacons
              , mintTokens = [("Offer",-1),(assetBeacon,-1),(lenderToken,-1)]
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
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseAsk
              , spendFromAddress = loanAddr
              , spendUtxos = [ offer ]
              }
          ]
      , outputs =
          [ UtxoOutput
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

    
    lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2

    lenderToken = credentialAsToken True lenderCred

    asset = (adaSymbol,adaToken)

    assetBeacon = genAssetBeaconName asset
    
    offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = Address lenderCred Nothing
        , loanAsset = asset
        , loanPrinciple = 100_000_000
        , rolloverFrequency = Just 1
        , minPayment = 500_000
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 10)]
        , claimPeriod = 10000
        , offerDeposit = 3_000_000
        , collateralIsSwappable = True
        }
      
    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-------------------------------------------------
-- Edge Cases
-------------------------------------------------
-- | Accept a loan and close an Ask UTxO in the same transaction. Both the loan and the Ask
-- are for the same loan asset.
edgeCase1 :: EmulatorTrace ()
edgeCase1 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  
  (mintRef,spendRef) <- initializeScripts

  -- Create the Ask UTxO.
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
                  , ( Just $ TxOutDatumInline $ toDatum askDatum{loanTerm = 22000}
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Ask" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2
  
  -- Create the Offer UTxO.
  callEndpoint @"create-transaction" h2 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateOffer lenderCred [asset]
              , mintTokens = [("Offer",2),(assetBeacon,2),(lenderToken,2)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum offerDatum
                    , lovelaceValueOf 103_000_000 
                    <> singleton beaconCurrencySymbol "Offer" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol lenderToken 1
                    )
                  , ( Just $ TxOutDatumInline $ toDatum offerDatum{loanTerm = 22000}
                    , lovelaceValueOf 103_000_000 
                    <> singleton beaconCurrencySymbol "Offer" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol lenderToken 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  startTime <- slotToBeginPOSIXTime def <$> waitNSlots 2

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Ask" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            askDatum
  offer1 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 103_000_000 
            <> singleton beaconCurrencySymbol "Offer" 1
            <> singleton beaconCurrencySymbol lenderToken 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            offerDatum
  ask2 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Ask" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            askDatum{loanTerm = 22000}
  offer2 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 103_000_000 
            <> singleton beaconCurrencySymbol "Offer" 1
            <> singleton beaconCurrencySymbol lenderToken 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            )
            offerDatum{loanTerm = 22000}

  let loanIdToken1 = genLoanId offer1
  let loanIdToken2 = genLoanId offer2
  let activeDatum1 = ActiveDatum
        { beaconSym = beaconCurrencySymbol 
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = asset
        , loanPrinciple = 100_000_000
        , rolloverFrequency = Just 1
        , lastCheckpoint = startTime
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , minPayment = 500_000
        , collateralization = [(testToken1, unsafeRatio 1 10_000_000)]
        , collateralIsSwappable = True
        , claimExpiration = startTime + 12000 + 10000
        , loanExpiration = startTime + 12000
        , loanOutstanding = fromInt 100_000_000 .*. (fromInt 1 .+. unsafeRatio 1 10) 
        , loanId = loanIdToken1
        }
  let activeDatum2 = ActiveDatum
        { beaconSym = beaconCurrencySymbol 
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = asset
        , loanPrinciple = 100_000_000
        , rolloverFrequency = Just 1
        , lastCheckpoint = startTime
        , loanTerm = 22000
        , loanInterest = unsafeRatio 1 10
        , minPayment = 500_000
        , collateralization = [(testToken1, unsafeRatio 1 10_000_000)]
        , collateralIsSwappable = True
        , claimExpiration = startTime + 22000 + 10000
        , loanExpiration = startTime + 22000
        , loanOutstanding = fromInt 100_000_000 .*. (fromInt 1 .+. unsafeRatio 1 10) 
        , loanId = loanIdToken2
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
              , mintRedeemer = toRedeemer $ CreateActive borrowerCred 
                  [ (ask1,offer1)
                  ]
              , mintTokens = 
                  [ ("Active",1)
                  , (assetBeacon,-1)
                  , (lenderToken,-1)
                  , (borrowerToken,1)
                  , ("Ask",-1)
                  , ("Offer",-1)
                  , (loanIdToken1,2)
                  ]
              }
          , TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer BurnBeacons 
              , mintTokens = 
                  [ ("Ask",-1)
                  , (assetBeacon,-1)
                  ]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer AcceptOffer
              , spendFromAddress = loanAddr
              , spendUtxos = 
                  [ ask1
                  , offer1
                  ]
              }
          , ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseAsk
              , spendFromAddress = loanAddr
              , spendUtxos = [ ask2 ]
              }
          ]
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum activeDatum1
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Active" 1
                    <> singleton beaconCurrencySymbol assetBeacon 1
                    <> singleton beaconCurrencySymbol loanIdToken1 1
                    <> singleton beaconCurrencySymbol borrowerToken 1
                    <> uncurry singleton testToken1 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = lenderAddr
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,"Accepted") 
                    , lovelaceValueOf 3_000_000
                    <> singleton beaconCurrencySymbol loanIdToken1 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval (Just startTime) Nothing
      }

  where
    borrowerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
    lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2

    borrowerToken = credentialAsToken False borrowerCred
    lenderToken = credentialAsToken True lenderCred
    
    asset = (adaSymbol,adaToken)
    assetBeacon = genAssetBeaconName asset
    
    askDatum = AskDatum
      { beaconSym = beaconCurrencySymbol
      , borrowerId = borrowerToken
      , loanAsset = asset
      , loanPrinciple = 100_000_000
      , loanTerm = 12000
      , collateral = [testToken1]
      }

    lenderAddr = Address lenderCred Nothing

    offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = asset
        , loanPrinciple = 100_000_000
        , rolloverFrequency = Just 1
        , minPayment = 500_000
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 10_000_000)]
        , claimPeriod = 10000
        , offerDeposit = 3_000_000
        , collateralIsSwappable = True
        }
    
    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | Accept a loan and close an Ask UTxO in the same transaction. The loan and the Ask are for
-- different loan assets.
edgeCase2 :: EmulatorTrace ()
edgeCase2 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  
  (mintRef,spendRef) <- initializeScripts

  -- Create the Ask UTxO.
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
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2
  
  -- Create the Offer UTxO.
  callEndpoint @"create-transaction" h2 $
    CreateTransactionParams
      { tokens = 
          [ 
            TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer $ CreateOffer lenderCred [asset1]
              , mintTokens = [("Offer",1),(assetBeacon1,1),(lenderToken,1)]
              }
          ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum offerDatum
                    , lovelaceValueOf 103_000_000 
                    <> singleton beaconCurrencySymbol "Offer" 1
                    <> singleton beaconCurrencySymbol assetBeacon1 1
                    <> singleton beaconCurrencySymbol lenderToken 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  startTime <- slotToBeginPOSIXTime def <$> waitNSlots 2

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Ask" 1
            <> singleton beaconCurrencySymbol assetBeacon1 1
            )
            askDatum1
  offer1 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 103_000_000 
            <> singleton beaconCurrencySymbol "Offer" 1
            <> singleton beaconCurrencySymbol lenderToken 1
            <> singleton beaconCurrencySymbol assetBeacon1 1
            )
            offerDatum
  ask2 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Ask" 1
            <> singleton beaconCurrencySymbol assetBeacon2 1
            )
            askDatum2

  let loanIdToken1 = genLoanId offer1
  let activeDatum1 = ActiveDatum
        { beaconSym = beaconCurrencySymbol 
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = asset1
        , loanPrinciple = 100_000_000
        , rolloverFrequency = Just 1
        , lastCheckpoint = startTime
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , minPayment = 500_000
        , collateralization = [(testToken1, unsafeRatio 1 10_000_000)]
        , collateralIsSwappable = True
        , claimExpiration = startTime + 12000 + 10000
        , loanExpiration = startTime + 12000
        , loanOutstanding = fromInt 100_000_000 .*. (fromInt 1 .+. unsafeRatio 1 10) 
        , loanId = loanIdToken1
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
              , mintRedeemer = toRedeemer $ CreateActive borrowerCred 
                  [ (ask1,offer1)
                  ]
              , mintTokens = 
                  [ ("Active",1)
                  , (assetBeacon1,-1)
                  , (lenderToken,-1)
                  , (borrowerToken,1)
                  , ("Ask",-1)
                  , ("Offer",-1)
                  , (loanIdToken1,2)
                  ]
              }
          , TokenMint 
              { mintWitness = 
                  ( beaconMintingPolicy
                  , Just (refScriptAddress, mintRef)
                  )
              , mintRedeemer = toRedeemer BurnBeacons 
              , mintTokens = 
                  [ ("Ask",-1)
                  , (assetBeacon2,-1)
                  ]
              }
          ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer AcceptOffer
              , spendFromAddress = loanAddr
              , spendUtxos = 
                  [ ask1
                  , offer1
                  ]
              }
          , ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseAsk
              , spendFromAddress = loanAddr
              , spendUtxos = [ ask2 ]
              }
          ]
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum activeDatum1
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Active" 1
                    <> singleton beaconCurrencySymbol assetBeacon1 1
                    <> singleton beaconCurrencySymbol loanIdToken1 1
                    <> singleton beaconCurrencySymbol borrowerToken 1
                    <> uncurry singleton testToken1 10
                    )
                  ]
              }
          , UtxoOutput
              { toAddress = lenderAddr
              , outputUtxos =
                  [ ( Just $ TxOutDatumInline
                           $ toDatum 
                           $ PaymentDatum (beaconCurrencySymbol,"Accepted") 
                    , lovelaceValueOf 3_000_000
                    <> singleton beaconCurrencySymbol loanIdToken1 1
                    )
                  ]
              }
          ]
      , validityRange = ValidityInterval (Just startTime) Nothing
      }

  where
    borrowerCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 1
    lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2

    borrowerToken = credentialAsToken False borrowerCred
    lenderToken = credentialAsToken True lenderCred
    
    asset1 = (adaSymbol,adaToken)
    assetBeacon1 = genAssetBeaconName asset1
    asset2 = testToken3
    assetBeacon2 = genAssetBeaconName asset2
    
    askDatum1 = AskDatum
      { beaconSym = beaconCurrencySymbol
      , borrowerId = borrowerToken
      , loanAsset = asset1
      , loanPrinciple = 100_000_000
      , loanTerm = 12000
      , collateral = [testToken1]
      }

    askDatum2 = AskDatum
      { beaconSym = beaconCurrencySymbol
      , borrowerId = borrowerToken
      , loanAsset = asset2
      , loanPrinciple = 10
      , loanTerm = 12000
      , collateral = [testToken1]
      }

    lenderAddr = Address lenderCred Nothing

    offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = asset1
        , loanPrinciple = 100_000_000
        , rolloverFrequency = Just 1
        , minPayment = 500_000
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 10_000_000)]
        , claimPeriod = 10000
        , offerDeposit = 3_000_000
        , collateralIsSwappable = True
        }
    
    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-------------------------------------------------
-- Benchmark Tests
-------------------------------------------------
-- | Close multiple Ask UTxOs in the same transaction. All Ask UTxOs are for the same
-- loan asset. Each Ask only used one asset for collateral.
benchTest1 :: Int -> EmulatorTrace ()
benchTest1 numberClosed = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  ( mintRef,spendRef ) <- initializeScripts

  let sampleOutputs =
        map (\i ->
               ( Just $ TxOutDatumInline 
                      $ toDatum askDatum
               , lovelaceValueOf (3_000_000 + i)
               <> singleton beaconCurrencySymbol "Ask" 1
               <> singleton beaconCurrencySymbol assetBeacon 1
               )  
            )
            [1..]
  let sampleMints i =
        TokenMint
          { mintWitness = 
              ( beaconMintingPolicy
              , Just (refScriptAddress, mintRef)
              )
          , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset] 
          , mintTokens = [("Ask",fromIntegral i),(assetBeacon,fromIntegral i)]
          }

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = [ sampleMints ( 40 :: Int ) ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = take 40 sampleOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }
  
  void $ waitNSlots 2

  targets <- mapM txOutRefWithValue $ take numberClosed $ map snd sampleOutputs

  let sampleBurn i =
        TokenMint
          { mintWitness = 
              ( beaconMintingPolicy
              , Just (refScriptAddress, mintRef)
              )
          , mintRedeemer = toRedeemer BurnBeacons 
          , mintTokens = 
              [ ("Ask",fromIntegral (-numberClosed))
              , (assetBeacon,fromIntegral (-numberClosed))
              ]
          }

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = [ sampleBurn numberClosed ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseAsk
              , spendFromAddress = loanAddr
              , spendUtxos = targets
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
      , borrowerId = credentialAsToken False borrowerCred
      , loanAsset = asset
      , loanPrinciple = 100_000_000
      , loanTerm = 12000
      , collateral = [testToken1]
      }

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | Close multiple Ask UTxOs in the same transaction. All Ask UTxOs are for the same
-- loan asset. Each Ask used three assets for collateral.
benchTest2 :: Int -> EmulatorTrace ()
benchTest2 numberClosed = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  ( mintRef,spendRef ) <- initializeScripts

  let sampleOutputs =
        map (\i ->
               ( Just $ TxOutDatumInline 
                      $ toDatum askDatum
               , lovelaceValueOf (3_000_000 + i)
               <> singleton beaconCurrencySymbol "Ask" 1
               <> singleton beaconCurrencySymbol assetBeacon 1
               )  
            )
            [1..]
  let sampleMints i =
        TokenMint
          { mintWitness = 
              ( beaconMintingPolicy
              , Just (refScriptAddress, mintRef)
              )
          , mintRedeemer = toRedeemer $ CreateAsk borrowerCred [asset] 
          , mintTokens = [("Ask",fromIntegral i),(assetBeacon,fromIntegral i)]
          }

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = [ sampleMints ( 40 :: Int ) ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = take 40 sampleOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }
  
  void $ waitNSlots 2

  targets <- mapM txOutRefWithValue $ take numberClosed $ map snd sampleOutputs

  let sampleBurn i =
        TokenMint
          { mintWitness = 
              ( beaconMintingPolicy
              , Just (refScriptAddress, mintRef)
              )
          , mintRedeemer = toRedeemer BurnBeacons 
          , mintTokens = 
              [ ("Ask",fromIntegral (-numberClosed))
              , (assetBeacon,fromIntegral (-numberClosed))
              ]
          }

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = [ sampleBurn numberClosed ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseAsk
              , spendFromAddress = loanAddr
              , spendUtxos = targets
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
      , borrowerId = credentialAsToken False borrowerCred
      , loanAsset = asset
      , loanPrinciple = 100_000_000
      , loanTerm = 12000
      , collateral = [testToken1,testToken2,testToken3]
      }

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | Close multiple Ask UTxOs in the same transaction. All Ask UTxOs are for different
-- loan assets. Each Ask only used one asset for collateral.
benchTest3 :: Int -> EmulatorTrace ()
benchTest3 numberClosed = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  ( mintRef,spendRef ) <- initializeScripts

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
      { tokens = [ sampleMints 20 ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = take 20 sampleOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  targets <- mapM txOutRefWithValue $ take numberClosed $ map snd sampleOutputs

  let sampleBurn i =
        TokenMint
          { mintWitness = 
              ( beaconMintingPolicy
              , Just (refScriptAddress, mintRef)
              )
          , mintRedeemer = toRedeemer BurnBeacons
          , mintTokens = ("Ask",fromIntegral (-i)) : take i (zip beacons $ repeat (-1) )
          }

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = [ sampleBurn numberClosed ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseAsk
              , spendFromAddress = loanAddr
              , spendUtxos = targets
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
      , borrowerId = credentialAsToken False borrowerCred
      , loanAsset = asset
      , loanPrinciple = 100_000_000
      , loanTerm = 12000
      , collateral = [testToken1]
      }

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | Close multiple Ask UTxOs in the same transaction. All Ask UTxOs are for different
-- loan assets. Each Ask used three assets for collateral.
benchTest4 :: Int -> EmulatorTrace ()
benchTest4 numberClosed = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  ( mintRef,spendRef ) <- initializeScripts

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
      { tokens = [ sampleMints 20 ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = take 20 sampleOutputs
              }
          ]
      , validityRange = ValidityInterval Nothing Nothing
      }

  void $ waitNSlots 2

  targets <- mapM txOutRefWithValue $ take numberClosed $ map snd sampleOutputs

  let sampleBurn i =
        TokenMint
          { mintWitness = 
              ( beaconMintingPolicy
              , Just (refScriptAddress, mintRef)
              )
          , mintRedeemer = toRedeemer BurnBeacons
          , mintTokens = ("Ask",fromIntegral (-i)) : take i (zip beacons $ repeat (-1) )
          }

  callEndpoint @"create-transaction" h1 $
    CreateTransactionParams
      { tokens = [ sampleBurn numberClosed ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseAsk
              , spendFromAddress = loanAddr
              , spendUtxos = targets
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
      , borrowerId = credentialAsToken False borrowerCred
      , loanAsset = asset
      , loanPrinciple = 100_000_000
      , loanTerm = 12000
      , collateral = [testToken1,testToken2,testToken3]
      }

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

benchTrace :: Int -> IO ()
benchTrace = runEmulatorTraceIO' def emConfig . benchTest1

-------------------------------------------------
-- Test Function
-------------------------------------------------
-- | A `TestTree` containing all `CloseAsk` scenarios.
tests :: TestTree
tests = do
  let opts = defaultCheckOptions & emulatorConfig .~ emConfig
  testGroup "Close Ask(s)"
    [ -- Success tests (Regression tests)
      checkPredicateOptions opts "regressionTest1"
        assertNoFailedTransactions regressionTest1
    , checkPredicateOptions opts "regressionTest2"
        assertNoFailedTransactions regressionTest2
    , checkPredicateOptions opts "regressionTest3"
        assertNoFailedTransactions regressionTest3
    , checkPredicateOptions opts "regressionTest4"
        assertNoFailedTransactions regressionTest4
    , checkPredicateOptions opts "regressionTest5"
        assertNoFailedTransactions regressionTest5

      -- Failure Tests
    , checkPredicateOptions opts "failureTest1"
        (assertEvaluationError "Not all Ask beacons burned") failureTest1
    , checkPredicateOptions opts "failureTest2"
        (assertEvaluationError "Not all Asset beacons burned") failureTest2
    , checkPredicateOptions opts "failureTest3"
        (assertEvaluationError "Not all Ask beacons burned") failureTest3
    , checkPredicateOptions opts "failureTest4"
        (assertEvaluationError "Borrower did not approve") failureTest4
    , checkPredicateOptions opts "failureTest5"
        (assertEvaluationError "Not all Ask beacons burned") failureTest5
    , checkPredicateOptions opts "failureTest6"
        (assertEvaluationError "Not all Asset beacons burned") failureTest6
    , checkPredicateOptions opts "failureTest7"
        (assertEvaluationError "Datum is not an AskDatum") failureTest7
    , checkPredicateOptions opts "failureTest8"
        (assertEvaluationError "Datum is not an AskDatum") failureTest8

      -- Edge Cases
    , checkPredicateOptions opts "edgeCase1"
        (assertEvaluationError "Not all Asset beacons burned") edgeCase1
    , checkPredicateOptions opts "edgeCase2"
        (assertEvaluationError "Wrong number of ask and offer inputs") edgeCase2

      -- Benchmarks
    , checkPredicateOptions opts "benchTest1"
        assertNoFailedTransactions $ benchTest1 9
    , checkPredicateOptions opts "benchTest2"
        assertNoFailedTransactions $ benchTest2 9
    , checkPredicateOptions opts "benchTest3"
        assertNoFailedTransactions $ benchTest3 6
    , checkPredicateOptions opts "benchTest4"
        assertNoFailedTransactions $ benchTest4 6

      -- Performance Increase Tests
    , checkPredicateOptions opts "perfIncreaseTest1"
        (Test.not assertNoFailedTransactions) $ benchTest1 10
    , checkPredicateOptions opts "perfIncreaseTest2"
        (Test.not assertNoFailedTransactions) $ benchTest2 10
    , checkPredicateOptions opts "perfIncreaseTest3"
        (Test.not assertNoFailedTransactions) $ benchTest3 7
    , checkPredicateOptions opts "perfIncreaseTest4"
        (Test.not assertNoFailedTransactions) $ benchTest4 7
    ]

testTrace :: IO ()
testTrace = runEmulatorTraceIO' def emConfig regressionTest2

