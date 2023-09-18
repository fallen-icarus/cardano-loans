{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Test.CloseOffer
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
  , failureTest9
  , failureTest10
  , failureTest11
  , failureTest12
  , failureTest13

    -- * Benchmark Tests
  , benchTest1
  , benchTest2
  , benchTest3
  , benchTest4

    -- * Full test function
  , tests
  ) where

import PlutusTx.Prelude (unsafeRatio)
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
-- | Close a single valid Offer UTxO. Also mints an unrelated token to check if the beacon
-- policy can correctly ignore other tokens being minted/burned. The lender approved the
-- transaction.
regressionTest1 :: EmulatorTrace ()
regressionTest1 = do
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
              , spendRedeemer = toRedeemer CloseOffer
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

    lenderToken = credentialAsToken lenderCred

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

-- | Close a single invalid Offer UTxO. Also mints an unrelated token to check if the beacon
-- policy can correctly ignore other tokens being minted/burned. The borrower approved the
-- transaction.
regressionTest2 :: EmulatorTrace ()
regressionTest2 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
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
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum offerDatum
                    , lovelaceValueOf 103_000_000 
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
            )
            offerDatum
  
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
              , spendRedeemer = toRedeemer CloseOffer
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

    lenderToken = credentialAsToken lenderCred

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

-- | Close multiple valid Offer UTxOs. Also mints an unrelated token to check if the beacon
-- policy can correctly ignore other tokens being minted/burned. The Offer UTxOs are all for
-- the same loan asset. The lender approved the transaction.
regressionTest3 :: EmulatorTrace ()
regressionTest3 = do
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
              , mintTokens = [("Offer",2),(assetBeacon,2),(lenderToken,2)]
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
                  , ( Just $ TxOutDatumInline $ toDatum offerDatum{loanTerm=22000}
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

  offer1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 103_000_000 
            <> singleton beaconCurrencySymbol "Offer" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum
  
  offer2 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 103_000_000 
            <> singleton beaconCurrencySymbol "Offer" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum{loanTerm=22000}

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
              , mintTokens = [("Offer",-2),(assetBeacon,-2),(lenderToken,-2)]
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
              , spendRedeemer = toRedeemer CloseOffer
              , spendFromAddress = loanAddr
              , spendUtxos = [ offer1, offer2 ]
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

    lenderToken = credentialAsToken lenderCred

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

-- | Close multiple invalid Offer UTxOs. Also mints an unrelated token to check if the beacon
-- policy can correctly ignore other tokens being minted/burned. The borrower approved the
-- transaction.
regressionTest4 :: EmulatorTrace ()
regressionTest4 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
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
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum offerDatum
                    , lovelaceValueOf 103_000_000 
                    )
                  , ( Just $ TxOutDatumInline $ toDatum offerDatum
                    , lovelaceValueOf 102_000_000 
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

  offer1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 103_000_000 
            )
            offerDatum
  
  offer2 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 102_000_000 
            )
            offerDatum

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
              , spendRedeemer = toRedeemer CloseOffer
              , spendFromAddress = loanAddr
              , spendUtxos = [ offer1, offer2 ]
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

    lenderToken = credentialAsToken lenderCred

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

-- | Close multiple valid Offer UTxOs. Also mints an unrelated token to check if the beacon
-- policy can correctly ignore other tokens being minted/burned. The Offer UTxOs are for
-- different loan assets. The lender approved the transaction.
regressionTest5 :: EmulatorTrace ()
regressionTest5 = do
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
              , mintRedeemer = toRedeemer $ CreateOffer lenderCred [asset1,asset2]
              , mintTokens = [("Offer",2),(assetBeacon1,1),(assetBeacon2,1),(lenderToken,2)]
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
                  [ ( Just $ TxOutDatumInline $ toDatum offerDatum1
                    , lovelaceValueOf 103_000_000 
                    <> singleton beaconCurrencySymbol "Offer" 1
                    <> singleton beaconCurrencySymbol assetBeacon1 1
                    <> singleton beaconCurrencySymbol lenderToken 1
                    )
                  , ( Just $ TxOutDatumInline $ toDatum offerDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Offer" 1
                    <> singleton beaconCurrencySymbol assetBeacon2 1
                    <> singleton beaconCurrencySymbol lenderToken 1
                    <> uncurry singleton testToken2 10
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

  offer1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 103_000_000 
            <> singleton beaconCurrencySymbol "Offer" 1
            <> singleton beaconCurrencySymbol assetBeacon1 1
            <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum1
  
  offer2 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Offer" 1
            <> singleton beaconCurrencySymbol assetBeacon2 1
            <> singleton beaconCurrencySymbol lenderToken 1
            <> uncurry singleton testToken2 10
            )
            offerDatum2

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
              , mintTokens = [("Offer",-2),(assetBeacon1,-1),(assetBeacon2,-1),(lenderToken,-2)]
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
              , spendRedeemer = toRedeemer CloseOffer
              , spendFromAddress = loanAddr
              , spendUtxos = [ offer1, offer2 ]
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

    lenderToken = credentialAsToken lenderCred

    asset1 = (adaSymbol,adaToken)
    assetBeacon1 = genAssetBeaconName asset1
    asset2 = testToken2
    assetBeacon2 = genAssetBeaconName asset2
    
    offerDatum1 = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = Address lenderCred Nothing
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
      
    offerDatum2 = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = Address lenderCred Nothing
        , loanAsset = asset2
        , loanPrinciple = 10
        , rolloverFrequency = Just 1
        , minPayment = 5
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 10)]
        , claimPeriod = 10000
        , offerDeposit = 3_000_000
        , collateralIsSwappable = True
        }

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-------------------------------------------------
-- Failure Tests
-------------------------------------------------
-- | Close a single valid Offer UTxO. The lender did not approved the transaction.
failureTest1 :: EmulatorTrace ()
failureTest1 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
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
              , spendRedeemer = toRedeemer CloseOffer
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

    lenderToken = credentialAsToken lenderCred

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

-- | Close a single valid Offer UTxO but do not burn the Offer beacon.
failureTest2 :: EmulatorTrace ()
failureTest2 = do
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
              , mintTokens = [("Offer",0),(assetBeacon,-1),(lenderToken,-1)]
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
              , spendRedeemer = toRedeemer CloseOffer
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

    lenderToken = credentialAsToken lenderCred

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

-- | Close a single valid Offer UTxO but do not burn the Asset beacon.
failureTest3 :: EmulatorTrace ()
failureTest3 = do
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
              , mintTokens = [("Offer",-1),(assetBeacon,0),(lenderToken,-1)]
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
              , spendRedeemer = toRedeemer CloseOffer
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

    lenderToken = credentialAsToken lenderCred

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

-- | Close a single valid Offer UTxO but do not burn the LenderID. 
failureTest4 :: EmulatorTrace ()
failureTest4 = do
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
              , mintTokens = [("Offer",-1),(assetBeacon,-1),(lenderToken,0)]
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
              , spendRedeemer = toRedeemer CloseOffer
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

    lenderToken = credentialAsToken lenderCred

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

-- | Close a single valid Offer UTxOs and a single invalid Offer UTxO. The lender approved
-- the transaction but the borrower did not.
failureTest5 :: EmulatorTrace ()
failureTest5 = do
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
                  , ( Just $ TxOutDatumInline $ toDatum offerDatum{loanTerm=22000}
                    , lovelaceValueOf 103_000_000 
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

  offer1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 103_000_000 
            <> singleton beaconCurrencySymbol "Offer" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum
  
  offer2 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 103_000_000 
            )
            offerDatum{loanTerm=22000}

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
              , spendRedeemer = toRedeemer CloseOffer
              , spendFromAddress = loanAddr
              , spendUtxos = [ offer1, offer2 ]
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

    lenderToken = credentialAsToken lenderCred

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

-- | Close a single valid Offer UTxOs and a single invalid Offer UTxO. The borrower approved
-- the transaction but the lender did not.
failureTest6 :: EmulatorTrace ()
failureTest6 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
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
                  , ( Just $ TxOutDatumInline $ toDatum offerDatum{loanTerm=22000}
                    , lovelaceValueOf 103_000_000 
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

  offer1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 103_000_000 
            <> singleton beaconCurrencySymbol "Offer" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum
  
  offer2 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 103_000_000 
            )
            offerDatum{loanTerm=22000}

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
              , spendRedeemer = toRedeemer CloseOffer
              , spendFromAddress = loanAddr
              , spendUtxos = [ offer1, offer2 ]
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

    lenderToken = credentialAsToken lenderCred

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

-- | Close a single invalid Offer UTxO. The borrower did not approved the transaction.
failureTest7 :: EmulatorTrace ()
failureTest7 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
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
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum offerDatum
                    , lovelaceValueOf 103_000_000 
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
              , spendRedeemer = toRedeemer CloseOffer
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

    lenderToken = credentialAsToken lenderCred

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

-- | Close a single invalid Offer UTxO that does not have an OfferDatum.
failureTest8 :: EmulatorTrace ()
failureTest8 = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
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
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = 
                  [ ( Just $ TxOutDatumInline $ toDatum offerDatum
                    , lovelaceValueOf 103_000_000 
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
            )
            offerDatum
  
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
              , spendRedeemer = toRedeemer CloseOffer
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

    lenderToken = credentialAsToken lenderCred

    asset = (adaSymbol,adaToken)

    assetBeacon = genAssetBeaconName asset

    offerDatum = AskDatum
      { beaconSym = beaconCurrencySymbol
      , borrowerId = credentialAsToken borrowerCred
      , loanAsset = asset
      , loanPrinciple = 100_000_000
      , loanTerm = 12000
      , collateral = [testToken1]
      }
    
    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | Close a single valid Ask UTxO using the CloseOffer redeemer.
failureTest9 :: EmulatorTrace ()
failureTest9 = do
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
              , spendRedeemer = toRedeemer CloseOffer
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
      , borrowerId = credentialAsToken borrowerCred
      , loanAsset = asset
      , loanPrinciple = 100_000_000
      , loanTerm = 12000
      , collateral = [testToken1]
      }

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)
    
-- | Close multiple valid Offer UTxOs, all for the same loan asset, but do not burn all
-- the Offer beacons.
failureTest10 :: EmulatorTrace ()
failureTest10 = do
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
              , mintTokens = [("Offer",2),(assetBeacon,2),(lenderToken,2)]
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
                  , ( Just $ TxOutDatumInline $ toDatum offerDatum{loanTerm=22000}
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

  offer1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 103_000_000 
            <> singleton beaconCurrencySymbol "Offer" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum
  
  offer2 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 103_000_000 
            <> singleton beaconCurrencySymbol "Offer" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum{loanTerm=22000}

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
              , mintTokens = [("Offer",-1),(assetBeacon,-2),(lenderToken,-2)]
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
              , spendRedeemer = toRedeemer CloseOffer
              , spendFromAddress = loanAddr
              , spendUtxos = [ offer1, offer2 ]
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

    lenderToken = credentialAsToken lenderCred

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

-- | Close multiple valid Offer UTxOs, all for the same loan asset, but do not burn all 
-- the Asset beacons.
failureTest11 :: EmulatorTrace ()
failureTest11 = do
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
              , mintTokens = [("Offer",2),(assetBeacon,2),(lenderToken,2)]
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
                  , ( Just $ TxOutDatumInline $ toDatum offerDatum{loanTerm=22000}
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

  offer1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 103_000_000 
            <> singleton beaconCurrencySymbol "Offer" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum
  
  offer2 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 103_000_000 
            <> singleton beaconCurrencySymbol "Offer" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum{loanTerm=22000}

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
              , mintTokens = [("Offer",-2),(assetBeacon,-1),(lenderToken,-2)]
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
              , spendRedeemer = toRedeemer CloseOffer
              , spendFromAddress = loanAddr
              , spendUtxos = [ offer1, offer2 ]
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

    lenderToken = credentialAsToken lenderCred

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

-- | Close multiple valid Offer UTxOs, all for the same loan asset, but do not burn all
-- the LenderIDs.
failureTest12 :: EmulatorTrace ()
failureTest12 = do
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
              , mintTokens = [("Offer",2),(assetBeacon,2),(lenderToken,2)]
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
                  , ( Just $ TxOutDatumInline $ toDatum offerDatum{loanTerm=22000}
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

  offer1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 103_000_000 
            <> singleton beaconCurrencySymbol "Offer" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum
  
  offer2 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 103_000_000 
            <> singleton beaconCurrencySymbol "Offer" 1
            <> singleton beaconCurrencySymbol assetBeacon 1
            <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum{loanTerm=22000}

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
              , mintTokens = [("Offer",-2),(assetBeacon,-2),(lenderToken,-1)]
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
              , spendRedeemer = toRedeemer CloseOffer
              , spendFromAddress = loanAddr
              , spendUtxos = [ offer1, offer2 ]
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

    lenderToken = credentialAsToken lenderCred

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

-- | Close multiple valid Offer UTxOs, all for different loan assets, but do not burn all
-- the Asset beacons.
failureTest13 :: EmulatorTrace ()
failureTest13 = do
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
              , mintRedeemer = toRedeemer $ CreateOffer lenderCred [asset1,asset2]
              , mintTokens = [("Offer",2),(assetBeacon1,1),(assetBeacon2,1),(lenderToken,2)]
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
                  [ ( Just $ TxOutDatumInline $ toDatum offerDatum1
                    , lovelaceValueOf 103_000_000 
                    <> singleton beaconCurrencySymbol "Offer" 1
                    <> singleton beaconCurrencySymbol assetBeacon1 1
                    <> singleton beaconCurrencySymbol lenderToken 1
                    )
                  , ( Just $ TxOutDatumInline $ toDatum offerDatum2
                    , lovelaceValueOf 3_000_000 
                    <> singleton beaconCurrencySymbol "Offer" 1
                    <> singleton beaconCurrencySymbol assetBeacon2 1
                    <> singleton beaconCurrencySymbol lenderToken 1
                    <> uncurry singleton testToken2 10
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

  offer1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 103_000_000 
            <> singleton beaconCurrencySymbol "Offer" 1
            <> singleton beaconCurrencySymbol assetBeacon1 1
            <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum1
  
  offer2 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 
            <> singleton beaconCurrencySymbol "Offer" 1
            <> singleton beaconCurrencySymbol assetBeacon2 1
            <> singleton beaconCurrencySymbol lenderToken 1
            <> uncurry singleton testToken2 10
            )
            offerDatum2

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
              , mintTokens = [("Offer",-2),(assetBeacon1,0),(assetBeacon2,-1),(lenderToken,-2)]
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
              , spendRedeemer = toRedeemer CloseOffer
              , spendFromAddress = loanAddr
              , spendUtxos = [ offer1, offer2 ]
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

    lenderToken = credentialAsToken lenderCred

    asset1 = (adaSymbol,adaToken)
    assetBeacon1 = genAssetBeaconName asset1
    asset2 = testToken2
    assetBeacon2 = genAssetBeaconName asset2
    
    offerDatum1 = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = Address lenderCred Nothing
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
      
    offerDatum2 = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = Address lenderCred Nothing
        , loanAsset = asset2
        , loanPrinciple = 10
        , rolloverFrequency = Just 1
        , minPayment = 5
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 10)]
        , claimPeriod = 10000
        , offerDeposit = 3_000_000
        , collateralIsSwappable = True
        }

    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-------------------------------------------------
-- Benchmark Tests
-------------------------------------------------
-- | Close multiple Offer UTxOs in the same transaction. All Offers are for the same loan
-- asset. All offers have one asset for collateral.
benchTest1 :: Int -> EmulatorTrace ()
benchTest1 numberClosed = do
  h2 <- activateContractWallet (knownWallet 2) endpoints

  ( mintRef,spendRef ) <- initializeScripts

  let sampleOutputs =
        map (\i ->
               ( Just $ TxOutDatumInline 
                      $ toDatum offerDatum{offerDeposit = 3_000_000 + i}
               , lovelaceValueOf (103_000_000 + i)
               <> singleton beaconCurrencySymbol "Offer" 1
               <> singleton beaconCurrencySymbol assetBeacon 1
               <> singleton beaconCurrencySymbol lenderToken 1
               )  
            )
            [1..]
  let sampleMints i =
        TokenMint
          { mintWitness = 
              ( beaconMintingPolicy
              , Just (refScriptAddress, mintRef)
              )
          , mintRedeemer = toRedeemer $ CreateOffer lenderCred [asset] 
          , mintTokens = 
              [ ("Offer",fromIntegral i)
              , (assetBeacon,fromIntegral i)
              , (lenderToken,fromIntegral i)
              ]
          }

  callEndpoint @"create-transaction" h2 $
    CreateTransactionParams
      { tokens = [ sampleMints (20 :: Int) ]
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
          , mintTokens = 
              [ ("Offer",fromIntegral (-numberClosed))
              , (assetBeacon,fromIntegral (-numberClosed))
              , (lenderToken, fromIntegral (-numberClosed))
              ]
          }

  callEndpoint @"create-transaction" h2 $
    CreateTransactionParams
      { tokens = [ sampleBurn numberClosed ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOffer
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
    
    lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2

    lenderToken = credentialAsToken lenderCred

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

-- | Close multiple Offer UTxOs in the same transaction. All Offers are for the same loan
-- asset. All offers have three assets for collateral.
benchTest2 :: Int -> EmulatorTrace ()
benchTest2 numberClosed = do
  h2 <- activateContractWallet (knownWallet 2) endpoints

  ( mintRef,spendRef ) <- initializeScripts

  let sampleOutputs =
        map (\i ->
               ( Just $ TxOutDatumInline 
                      $ toDatum offerDatum{offerDeposit = 3_000_000 + i}
               , lovelaceValueOf (103_000_000 + i)
               <> singleton beaconCurrencySymbol "Offer" 1
               <> singleton beaconCurrencySymbol assetBeacon 1
               <> singleton beaconCurrencySymbol lenderToken 1
               )  
            )
            [1..]
  let sampleMints i =
        TokenMint
          { mintWitness = 
              ( beaconMintingPolicy
              , Just (refScriptAddress, mintRef)
              )
          , mintRedeemer = toRedeemer $ CreateOffer lenderCred [asset] 
          , mintTokens = 
              [ ("Offer",fromIntegral i)
              , (assetBeacon,fromIntegral i)
              , (lenderToken,fromIntegral i)
              ]
          }

  callEndpoint @"create-transaction" h2 $
    CreateTransactionParams
      { tokens = [ sampleMints (20 :: Int) ]
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
          , mintTokens = 
              [ ("Offer",fromIntegral (-numberClosed))
              , (assetBeacon,fromIntegral (-numberClosed))
              , (lenderToken, fromIntegral (-numberClosed))
              ]
          }

  callEndpoint @"create-transaction" h2 $
    CreateTransactionParams
      { tokens = [ sampleBurn numberClosed ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOffer
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

    
    lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2

    lenderToken = credentialAsToken lenderCred

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
        , collateralization = 
            [ (testToken1,unsafeRatio 1 10)
            , (testToken2,unsafeRatio 1 10)
            , (testToken3,unsafeRatio 1 10)
            ]
        , claimPeriod = 10000
        , offerDeposit = 3_000_000
        , collateralIsSwappable = True
        }
      
    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | Close multiple Offer UTxOs in the same transaction. All Offers are for different loan
-- assets. All offers have one asset for collateral.
benchTest3 :: Int -> EmulatorTrace ()
benchTest3 numberClosed = do
  h2 <- activateContractWallet (knownWallet 2) endpoints

  ( mintRef,spendRef ) <- initializeScripts

  let assets = map (\i -> (fst testToken1, fromString $ "TestToken" <> show @Int i)) [1..20]
  let beacons = map genAssetBeaconName assets
  let sampleOutputs = 
        zipWith (\a b ->
                  ( Just $ TxOutDatumInline 
                         $ toDatum offerDatum{loanAsset = a}
                  , lovelaceValueOf 3_000_000 
                  <> singleton beaconCurrencySymbol "Offer" 1
                  <> singleton beaconCurrencySymbol b 1
                  <> singleton beaconCurrencySymbol lenderToken 1
                  <> uncurry singleton a 10
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
          , mintRedeemer = toRedeemer $ CreateOffer lenderCred $ take i assets
          , mintTokens = 
              [("Offer",fromIntegral i),(lenderToken,fromIntegral i)] 
              <> take i (zip beacons $ repeat 1 )
          }

  callEndpoint @"create-transaction" h2 $
    CreateTransactionParams
      { tokens = [ sampleMints 12 ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = take 12 sampleOutputs
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
              [("Offer",fromIntegral (-i)),(lenderToken,fromIntegral (-i))] 
              <> take i (zip beacons $ repeat (-1) )
          }

  callEndpoint @"create-transaction" h2 $
    CreateTransactionParams
      { tokens = [ sampleBurn numberClosed ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOffer
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

    
    lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2

    lenderToken = credentialAsToken lenderCred

    asset = (adaSymbol,adaToken)

    assetBeacon = genAssetBeaconName asset
    
    offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = Address lenderCred Nothing
        , loanAsset = asset
        , loanPrinciple = 10
        , rolloverFrequency = Just 1
        , minPayment = 5
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 10)]
        , claimPeriod = 10000
        , offerDeposit = 3_000_000
        , collateralIsSwappable = True
        }
      
    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

-- | Close multiple Offer UTxOs in the same transaction. All Offers are for different loan
-- assets. All offers have three assets for collateral.
benchTest4 :: Int -> EmulatorTrace ()
benchTest4 numberClosed = do
  h2 <- activateContractWallet (knownWallet 2) endpoints

  ( mintRef,spendRef ) <- initializeScripts

  let assets = map (\i -> (fst testToken1, fromString $ "TestToken" <> show @Int i)) [1..20]
  let beacons = map genAssetBeaconName assets
  let sampleOutputs = 
        zipWith (\a b ->
                  ( Just $ TxOutDatumInline 
                         $ toDatum offerDatum{loanAsset = a}
                  , lovelaceValueOf 4_000_000 
                  <> singleton beaconCurrencySymbol "Offer" 1
                  <> singleton beaconCurrencySymbol b 1
                  <> singleton beaconCurrencySymbol lenderToken 1
                  <> uncurry singleton a 10
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
          , mintRedeemer = toRedeemer $ CreateOffer lenderCred $ take i assets
          , mintTokens = 
              [("Offer",fromIntegral i),(lenderToken,fromIntegral i)] 
              <> take i (zip beacons $ repeat 1 )
          }

  callEndpoint @"create-transaction" h2 $
    CreateTransactionParams
      { tokens = [ sampleMints 12 ]
      , inputs = []
      , outputs =
          [ UtxoOutput
              { toAddress = loanAddr
              , outputUtxos = take 12 sampleOutputs
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
              [("Offer",fromIntegral (-i)),(lenderToken,fromIntegral (-i))] 
              <> take i (zip beacons $ repeat (-1) )
          }

  callEndpoint @"create-transaction" h2 $
    CreateTransactionParams
      { tokens = [ sampleBurn numberClosed ]
      , inputs = 
          [ ScriptUtxoInput
              { spendWitness = (loanValidator, Just (refScriptAddress,spendRef))
              , spendRedeemer = toRedeemer CloseOffer
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

    
    lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2

    lenderToken = credentialAsToken lenderCred

    asset = (adaSymbol,adaToken)

    assetBeacon = genAssetBeaconName asset
    
    offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = Address lenderCred Nothing
        , loanAsset = asset
        , loanPrinciple = 10
        , rolloverFrequency = Just 1
        , minPayment = 5
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , collateralization = 
            [ (testToken17,unsafeRatio 1 10)
            , (testToken18,unsafeRatio 1 10)
            , (testToken19,unsafeRatio 1 10)
            ]
        , claimPeriod = 10000
        , offerDeposit = 4_000_000
        , collateralIsSwappable = True
        }
      
    loanAddr = Address (ScriptCredential loanValidatorHash) (Just $ StakingHash borrowerCred)

benchTrace :: Int -> IO ()
benchTrace = runEmulatorTraceIO' def emConfig . benchTest4

-------------------------------------------------
-- Test Function
-------------------------------------------------
-- | A `TestTree` containing all `CloseOffer` scenarios.
tests :: TestTree
tests = do
  let opts = defaultCheckOptions & emulatorConfig .~ emConfig
  testGroup "Close Offer(s)"
    [ -- Success Tests (Regression Tests)
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
        (assertEvaluationError "Lender did not approve") failureTest1
    , checkPredicateOptions opts "failureTest2"
        (assertEvaluationError "Not all Offer beacons burned") failureTest2
    , checkPredicateOptions opts "failureTest3"
        (assertEvaluationError "Not all Asset beacons burned") failureTest3
    , checkPredicateOptions opts "failureTest4"
        (assertEvaluationError "Not all LenderIDs burned") failureTest4
    , checkPredicateOptions opts "failureTest5"
        (assertEvaluationError "Borrower did not approve") failureTest5
    , checkPredicateOptions opts "failureTest6"
        (assertEvaluationError "Lender did not approve") failureTest6
    , checkPredicateOptions opts "failureTest7"
        (assertEvaluationError "Borrower did not approve") failureTest7
    , checkPredicateOptions opts "failureTest8"
        (assertEvaluationError "Datum is not an OfferDatum") failureTest8
    , checkPredicateOptions opts "failureTest9"
        (assertEvaluationError "Datum is not an OfferDatum") failureTest9
    , checkPredicateOptions opts "failureTest10"
        (assertEvaluationError "Not all Offer beacons burned") failureTest10
    , checkPredicateOptions opts "failureTest11"
        (assertEvaluationError "Not all Asset beacons burned") failureTest11
    , checkPredicateOptions opts "failureTest12"
        (assertEvaluationError "Not all LenderIDs burned") failureTest12
    , checkPredicateOptions opts "failureTest13"
        (assertEvaluationError "Not all Asset beacons burned") failureTest13

      -- Benchmarks
    , checkPredicateOptions opts "benchTest1"
        assertNoFailedTransactions $ benchTest1 7
    , checkPredicateOptions opts "benchTest2"
        assertNoFailedTransactions $ benchTest2 7
    , checkPredicateOptions opts "benchTest3"
        assertNoFailedTransactions $ benchTest3 5
    , checkPredicateOptions opts "benchTest4"
        assertNoFailedTransactions $ benchTest4 5
    ]

testTrace :: IO ()
testTrace = runEmulatorTraceIO' def emConfig failureTest13

