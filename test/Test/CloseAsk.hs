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

module Test.CloseAsk
(
  tests,
  testTrace
) where

import Prelude (IO)
import Control.Lens hiding (from)
import Control.Monad (void)
import PlutusTx.Prelude
import Plutus.Trace
import Wallet.Emulator.Wallet
import Plutus.Contract.Test as Test
import Test.Tasty
import Ledger.Ada (lovelaceValueOf)
import Data.Default
import Plutus.V2.Ledger.Api
import Ledger.Address

import Test.Common
import CardanoLoans

-------------------------------------------------
-- CloseAsk Scenarios
-------------------------------------------------
closeSingleAsk :: EmulatorTrace ()
closeSingleAsk = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { loanBeaconSym' = beaconPolicySymbol
        , borrowerId' = pubKeyAsToken borrowerPubKey
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
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

  void $ waitUntilSlot 2

  callEndpoint @"close-ask" h1 $
    CloseAskParams
      { closeAskBeaconsBurned = [("Ask",-1)]
      , closeAskBeaconRedeemer = BurnBeaconToken'
      , closeAskBeaconPolicy = beaconPolicy
      , closeAskLoanVal = loanValidator
      , closeAskLoanAddress = addr
      , closeAskSpecificUTxOs = 
          [ ( askDatum
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1
            )
          ]
      }

stakingCredDidNotApprove :: EmulatorTrace ()
stakingCredDidNotApprove = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { loanBeaconSym' = beaconPolicySymbol
        , borrowerId' = pubKeyAsToken borrowerPubKey
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
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

  void $ waitUntilSlot 2

  callEndpoint @"close-ask" h2 $
    CloseAskParams
      { closeAskBeaconsBurned = [("Ask",-1)]
      , closeAskBeaconRedeemer = BurnBeaconToken'
      , closeAskBeaconPolicy = beaconPolicy
      , closeAskLoanVal = loanValidator
      , closeAskLoanAddress = addr
      , closeAskSpecificUTxOs = 
          [ ( askDatum
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1
            )
          ]
      }

notAllBeaconsBurned :: EmulatorTrace ()
notAllBeaconsBurned = do
  h1 <- activateContractWallet (knownWallet 2) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
      askDatum = AskDatum'
        { loanBeaconSym' = beaconPolicySymbol
        , borrowerId' = pubKeyAsToken borrowerPubKey
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
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

  void $ waitUntilSlot 2

  callEndpoint @"close-ask" h1 $
    CloseAskParams
      { closeAskBeaconsBurned = [("Ask",-2)]
      , closeAskBeaconRedeemer = BurnBeaconToken'
      , closeAskBeaconPolicy = beaconPolicy
      , closeAskLoanVal = loanValidator
      , closeAskLoanAddress = addr
      , closeAskSpecificUTxOs = 
          [ ( askDatum
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1
            )
          ]
      }

wrongDatumType :: EmulatorTrace ()
wrongDatumType = do
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
      offerDatum = OfferDatum'
        { loanBeaconSym' = beaconPolicySymbol
        , lenderId' = pubKeyAsToken lenderPubKey
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 12000
        , loanInterest' = unsafeRatio 1 10
        , collateralization' = [(testToken1,unsafeRatio 1 2)]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"offer" h2 $
    OfferParams
      { offerBeaconsMinted = [("Offer",1),(pubKeyAsToken lenderPubKey,1)]
      , offerBeaconRedeemer = MintOfferToken' lenderPubKey
      , offerBeaconPolicy = beaconPolicy
      , offerAddress = addr
      , offerInfo = 
          [ ( Just offerDatum
            , lovelaceValueOf 103_000_000 <> singleton beaconPolicySymbol "Offer" 1
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
            )
          ]
      , offerAsInline = True
      }

  void $ waitUntilSlot 2

  callEndpoint @"close-ask" h2 $
    CloseAskParams
      { closeAskBeaconsBurned = []
      , closeAskBeaconRedeemer = BurnBeaconToken'
      , closeAskBeaconPolicy = beaconPolicy
      , closeAskLoanVal = loanValidator
      , closeAskLoanAddress = addr
      , closeAskSpecificUTxOs = 
          [ ( offerDatum
            , lovelaceValueOf 103_000_000 <> singleton beaconPolicySymbol "Offer" 1
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
            )
          ]
      }

closeWithoutBurning :: EmulatorTrace ()
closeWithoutBurning = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { loanBeaconSym' = beaconPolicySymbol
        , borrowerId' = pubKeyAsToken borrowerPubKey
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
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

  void $ waitUntilSlot 2

  callEndpoint @"close-ask" h1 $
    CloseAskParams
      { closeAskBeaconsBurned = []
      , closeAskBeaconRedeemer = BurnBeaconToken'
      , closeAskBeaconPolicy = beaconPolicy
      , closeAskLoanVal = loanValidator
      , closeAskLoanAddress = addr
      , closeAskSpecificUTxOs = 
          [ ( askDatum
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1
            )
          ]
      }

closeMultipleAsks :: EmulatorTrace ()
closeMultipleAsks = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { loanBeaconSym' = beaconPolicySymbol
        , borrowerId' = pubKeyAsToken borrowerPubKey
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
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

  void $ waitUntilSlot 2

  callEndpoint @"ask" h1 $
    AskParams
      { askBeaconsMinted = [("Ask",1)]
      , askBeaconRedeemer = MintAskToken' borrowerPubKey
      , askBeaconPolicy = beaconPolicy
      , askAddress = addr
      , askInfo = 
          [ ( Just askDatum
            , lovelaceValueOf 2_100_000 <> singleton beaconPolicySymbol "Ask" 1)
          ]
      , askAsInline = True
      }

  void $ waitUntilSlot 4

  callEndpoint @"close-ask" h1 $
    CloseAskParams
      { closeAskBeaconsBurned = [("Ask",-2)]
      , closeAskBeaconRedeemer = BurnBeaconToken'
      , closeAskBeaconPolicy = beaconPolicy
      , closeAskLoanVal = loanValidator
      , closeAskLoanAddress = addr
      , closeAskSpecificUTxOs = 
          [ ( askDatum
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1
            )
          , ( askDatum
            , lovelaceValueOf 2_100_000 <> singleton beaconPolicySymbol "Ask" 1
            )
          ]
      }

-------------------------------------------------
-- Test Function
-------------------------------------------------
tests :: TestTree
tests = do
  let opts = defaultCheckOptions & emulatorConfig .~ emConfig
  testGroup "Close ask(s)"
    [ checkPredicateOptions opts "Fail if staking credential did not approve"
        (Test.not assertNoFailedTransactions) stakingCredDidNotApprove
    , checkPredicateOptions opts "Fail if not all ask beacons burned"
        (Test.not assertNoFailedTransactions) notAllBeaconsBurned
    , checkPredicateOptions opts "Fail if input datum is not an AskDatum"
        (Test.not assertNoFailedTransactions) wrongDatumType
    , checkPredicateOptions opts "Fail if ask beacon from address not burned"
        (Test.not assertNoFailedTransactions) closeWithoutBurning
    , checkPredicateOptions opts "Successfully close single ask"
        assertNoFailedTransactions closeSingleAsk
    , checkPredicateOptions opts "Successfully close multiple asks"
        assertNoFailedTransactions closeMultipleAsks
    ]

testTrace :: IO ()
testTrace = runEmulatorTraceIO' def emConfig closeMultipleAsks