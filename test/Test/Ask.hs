{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}

module Test.Ask
(
  tests,
  testTrace
) where

import Prelude (IO)
import Control.Lens hiding (from)
import PlutusTx.Prelude
import Plutus.Trace
import Wallet.Emulator.Wallet
import Plutus.Contract.Test as Test
import Test.Tasty
import Ledger.Ada (lovelaceValueOf)
import Data.Default
import Plutus.V2.Ledger.Api
import Ledger.Address
import Plutus.Script.Utils.V2.Generators (alwaysSucceedValidatorHash)

import Test.Common
import CardanoLoans

-------------------------------------------------
-- Ask Scenarios
-------------------------------------------------
askWithStakePubKey :: EmulatorTrace ()
askWithStakePubKey = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { askBeacon' = (beaconPolicySymbol,"Ask")
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanQuantity' = 100
        , loanTerm' = 12000
        , collateral' = [testToken1]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"ask" h1 $
    AskParams
      { askBeaconsMinted = [("Ask",1)]
      , askBeaconRedeemer = MintAskToken' borrowerPubKey
      , askBeaconPolicy = beaconPolicy
      , askAddress = addr
      , askInfo = 
          [ ( Just askDatum
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1)
          ]
      , askAsInline = True
      }

mintMultipleAskTokens :: EmulatorTrace ()
mintMultipleAskTokens = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { askBeacon' = (beaconPolicySymbol,"Ask")
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanQuantity' = 100
        , loanTerm' = 12000
        , collateral' = [testToken1]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"ask" h1 $
    AskParams
      { askBeaconsMinted = [("Ask",3)]
      , askBeaconRedeemer = MintAskToken' borrowerPubKey
      , askBeaconPolicy = beaconPolicy
      , askAddress = addr
      , askInfo = 
          [ ( Just askDatum
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 3)
          ]
      , askAsInline = True
      }

mintAskTokenWithDifferentName :: EmulatorTrace ()
mintAskTokenWithDifferentName = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { askBeacon' = (beaconPolicySymbol,"Ask")
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanQuantity' = 100
        , loanTerm' = 12000
        , collateral' = [testToken1]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"ask" h1 $
    AskParams
      { askBeaconsMinted = [("Asker",1)]
      , askBeaconRedeemer = MintAskToken' borrowerPubKey
      , askBeaconPolicy = beaconPolicy
      , askAddress = addr
      , askInfo = 
          [ ( Just askDatum
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Asker" 1)
          ]
      , askAsInline = True
      }

mintToPaymentPubKey :: EmulatorTrace ()
mintToPaymentPubKey = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { askBeacon' = (beaconPolicySymbol,"Ask")
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanQuantity' = 100
        , loanTerm' = 12000
        , collateral' = [testToken1]
        }
      addr = Address (PubKeyCredential $ unPaymentPubKeyHash $ mockWalletPaymentPubKeyHash $ knownWallet 2)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"ask" h1 $
    AskParams
      { askBeaconsMinted = [("Ask",1)]
      , askBeaconRedeemer = MintAskToken' borrowerPubKey
      , askBeaconPolicy = beaconPolicy
      , askAddress = addr
      , askInfo = 
          [ ( Just askDatum
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1)
          ]
      , askAsInline = True
      }

mintToDifferentScriptAddress :: EmulatorTrace ()
mintToDifferentScriptAddress = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { askBeacon' = (beaconPolicySymbol,"Ask")
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanQuantity' = 100
        , loanTerm' = 12000
        , collateral' = [testToken1]
        }
      addr = Address (ScriptCredential alwaysSucceedValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"ask" h1 $
    AskParams
      { askBeaconsMinted = [("Ask",1)]
      , askBeaconRedeemer = MintAskToken' borrowerPubKey
      , askBeaconPolicy = beaconPolicy
      , askAddress = addr
      , askInfo = 
          [ ( Just askDatum
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1)
          ]
      , askAsInline = True
      }

mintToAddressWithoutStaking :: EmulatorTrace ()
mintToAddressWithoutStaking = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { askBeacon' = (beaconPolicySymbol,"Ask")
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanQuantity' = 100
        , loanTerm' = 12000
        , collateral' = [testToken1]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     Nothing
  
  callEndpoint @"ask" h1 $
    AskParams
      { askBeaconsMinted = [("Ask",1)]
      , askBeaconRedeemer = MintAskToken' borrowerPubKey
      , askBeaconPolicy = beaconPolicy
      , askAddress = addr
      , askInfo = 
          [ ( Just askDatum
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1)
          ]
      , askAsInline = True
      }

mintToAddressUsingStakingScript :: EmulatorTrace ()
mintToAddressUsingStakingScript = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { askBeacon' = (beaconPolicySymbol,"Ask")
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanQuantity' = 100
        , loanTerm' = 12000
        , collateral' = [testToken1]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ ScriptCredential
                           $ alwaysSucceedValidatorHash)
  
  callEndpoint @"ask" h1 $
    AskParams
      { askBeaconsMinted = [("Ask",1)]
      , askBeaconRedeemer = MintAskToken' borrowerPubKey
      , askBeaconPolicy = beaconPolicy
      , askAddress = addr
      , askInfo = 
          [ ( Just askDatum
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1)
          ]
      , askAsInline = True
      }

mintWithDifferentPubkeyInRedeemer :: EmulatorTrace ()
mintWithDifferentPubkeyInRedeemer = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { askBeacon' = (beaconPolicySymbol,"Ask")
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanQuantity' = 100
        , loanTerm' = 12000
        , collateral' = [testToken1]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"ask" h1 $
    AskParams
      { askBeaconsMinted = [("Ask",1)]
      , askBeaconRedeemer = MintAskToken' $ mockWalletPaymentPubKeyHash $ knownWallet 2
      , askBeaconPolicy = beaconPolicy
      , askAddress = addr
      , askInfo = 
          [ ( Just askDatum
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1)
          ]
      , askAsInline = True
      }

mintToAddressWithDifferentStakingPubKey :: EmulatorTrace ()
mintToAddressWithDifferentStakingPubKey = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { askBeacon' = (beaconPolicySymbol,"Ask")
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanQuantity' = 100
        , loanTerm' = 12000
        , collateral' = [testToken1]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 2)
  
  callEndpoint @"ask" h1 $
    AskParams
      { askBeaconsMinted = [("Ask",1)]
      , askBeaconRedeemer = MintAskToken' borrowerPubKey
      , askBeaconPolicy = beaconPolicy
      , askAddress = addr
      , askInfo = 
          [ ( Just askDatum
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1)
          ]
      , askAsInline = True
      }

wrongAskBeaconInDatum :: EmulatorTrace ()
wrongAskBeaconInDatum = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { askBeacon' = (beaconPolicySymbol,"As")
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanQuantity' = 100
        , loanTerm' = 12000
        , collateral' = [testToken1]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"ask" h1 $
    AskParams
      { askBeaconsMinted = [("Ask",1)]
      , askBeaconRedeemer = MintAskToken' borrowerPubKey
      , askBeaconPolicy = beaconPolicy
      , askAddress = addr
      , askInfo = 
          [ ( Just askDatum
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1)
          ]
      , askAsInline = True
      }

wrongBorrowerIdInDatum :: EmulatorTrace ()
wrongBorrowerIdInDatum = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { askBeacon' = (beaconPolicySymbol,"Ask")
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken $ mockWalletPaymentPubKeyHash $ knownWallet 3)
        , loanAsset' = (adaSymbol,adaToken)
        , loanQuantity' = 100
        , loanTerm' = 12000
        , collateral' = [testToken1]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"ask" h1 $
    AskParams
      { askBeaconsMinted = [("Ask",1)]
      , askBeaconRedeemer = MintAskToken' borrowerPubKey
      , askBeaconPolicy = beaconPolicy
      , askAddress = addr
      , askInfo = 
          [ ( Just askDatum
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1)
          ]
      , askAsInline = True
      }

negativeQuantity :: EmulatorTrace ()
negativeQuantity = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { askBeacon' = (beaconPolicySymbol,"Ask")
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanQuantity' = -100
        , loanTerm' = 12000
        , collateral' = [testToken1]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"ask" h1 $
    AskParams
      { askBeaconsMinted = [("Ask",1)]
      , askBeaconRedeemer = MintAskToken' borrowerPubKey
      , askBeaconPolicy = beaconPolicy
      , askAddress = addr
      , askInfo = 
          [ ( Just askDatum
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1)
          ]
      , askAsInline = True
      }

negativeTerm :: EmulatorTrace ()
negativeTerm = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { askBeacon' = (beaconPolicySymbol,"Ask")
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanQuantity' = 100
        , loanTerm' = -12000
        , collateral' = [testToken1]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"ask" h1 $
    AskParams
      { askBeaconsMinted = [("Ask",1)]
      , askBeaconRedeemer = MintAskToken' borrowerPubKey
      , askBeaconPolicy = beaconPolicy
      , askAddress = addr
      , askInfo = 
          [ ( Just askDatum
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1)
          ]
      , askAsInline = True
      }

emptyCollateral :: EmulatorTrace ()
emptyCollateral = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { askBeacon' = (beaconPolicySymbol,"Ask")
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanQuantity' = 100
        , loanTerm' = 12000
        , collateral' = []
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"ask" h1 $
    AskParams
      { askBeaconsMinted = [("Ask",1)]
      , askBeaconRedeemer = MintAskToken' borrowerPubKey
      , askBeaconPolicy = beaconPolicy
      , askAddress = addr
      , askInfo = 
          [ ( Just askDatum
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1)
          ]
      , askAsInline = True
      }

notInline :: EmulatorTrace ()
notInline = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { askBeacon' = (beaconPolicySymbol,"Ask")
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanQuantity' = 100
        , loanTerm' = 12000
        , collateral' = [testToken1]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"ask" h1 $
    AskParams
      { askBeaconsMinted = [("Ask",1)]
      , askBeaconRedeemer = MintAskToken' borrowerPubKey
      , askBeaconPolicy = beaconPolicy
      , askAddress = addr
      , askInfo = 
          [ ( Just askDatum
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1)
          ]
      , askAsInline = False
      }

receivingAddressDidNotSign :: EmulatorTrace ()
receivingAddressDidNotSign = do
  h1 <- activateContractWallet (knownWallet 2) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { askBeacon' = (beaconPolicySymbol,"Ask")
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanQuantity' = 100
        , loanTerm' = 12000
        , collateral' = [testToken1]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"ask" h1 $
    AskParams
      { askBeaconsMinted = [("Ask",1)]
      , askBeaconRedeemer = MintAskToken' borrowerPubKey
      , askBeaconPolicy = beaconPolicy
      , askAddress = addr
      , askInfo = 
          [ ( Just askDatum
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1)
          ]
      , askAsInline = True
      }

mintMultipleKindsOfTokens :: EmulatorTrace ()
mintMultipleKindsOfTokens = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { askBeacon' = (beaconPolicySymbol,"Ask")
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanQuantity' = 100
        , loanTerm' = 12000
        , collateral' = [testToken1]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"ask" h1 $
    AskParams
      { askBeaconsMinted = [("Ask",1),("Other",1)]
      , askBeaconRedeemer = MintAskToken' borrowerPubKey
      , askBeaconPolicy = beaconPolicy
      , askAddress = addr
      , askInfo = 
          [ ( Just askDatum
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1
           <> singleton beaconPolicySymbol "Other" 1
            )
          ]
      , askAsInline = True
      }

-------------------------------------------------
-- Test Function
-------------------------------------------------
tests :: TestTree
tests = do
  let opts = defaultCheckOptions & emulatorConfig .~ emConfig
  testGroup "Ask for a loan"
    [ checkPredicateOptions opts "Fail if multiple ask tokens minted in tx"
        (Test.not assertNoFailedTransactions) mintMultipleAskTokens
    , checkPredicateOptions opts "Fail if ask token has a different token name than 'Ask'"
        (Test.not assertNoFailedTransactions) mintAskTokenWithDifferentName
    , checkPredicateOptions opts "Fail if multiple kinds of tokens minted in tx"
        (Test.not assertNoFailedTransactions) mintMultipleKindsOfTokens
    , checkPredicateOptions opts "Fail if ask token minted to payment pubkey address"
        (Test.not assertNoFailedTransactions) mintToPaymentPubKey
    , checkPredicateOptions opts "Fail if ask token minted to different payment script address"
        (Test.not assertNoFailedTransactions) mintToDifferentScriptAddress
    , checkPredicateOptions opts "Fail if ask token minted to address without staking"
        (Test.not assertNoFailedTransactions) mintToAddressWithoutStaking
    , checkPredicateOptions opts "Fail if ask token minted to address using a staking script"
        (Test.not assertNoFailedTransactions) mintToAddressUsingStakingScript
    , checkPredicateOptions opts "Fail if redeemer has wrong pubkey hash"
        (Test.not assertNoFailedTransactions) mintWithDifferentPubkeyInRedeemer
    , checkPredicateOptions opts "Fail if receiving address uses a different pubkey hash than redeemer"
        (Test.not assertNoFailedTransactions) mintToAddressWithDifferentStakingPubKey
    , checkPredicateOptions opts "Fail if output datum has wrong askBeacon"
        (Test.not assertNoFailedTransactions) wrongAskBeaconInDatum
    , checkPredicateOptions opts "Fail if output datum has wrong borrower ID"
        (Test.not assertNoFailedTransactions) wrongBorrowerIdInDatum
    , checkPredicateOptions opts "Fail if output datum has loanQuantity <= 0"
        (Test.not assertNoFailedTransactions) negativeQuantity
    , checkPredicateOptions opts "Fail if output datum has loanTerm <= 0"
        (Test.not assertNoFailedTransactions) negativeTerm
    , checkPredicateOptions opts "Fail if output datum has empty collateral list"
        (Test.not assertNoFailedTransactions) emptyCollateral
    , checkPredicateOptions opts "Fail if output datum is not inline"
        (Test.not assertNoFailedTransactions) notInline
    , checkPredicateOptions opts "Fail if receiving address did not sign tx"
        (Test.not assertNoFailedTransactions) receivingAddressDidNotSign
    , checkPredicateOptions opts "Successfully create ask"
        assertNoFailedTransactions askWithStakePubKey
    ]

testTrace :: IO ()
testTrace = runEmulatorTraceIO' def emConfig mintMultipleKindsOfTokens