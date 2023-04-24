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

module Test.CloseOffer
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
-- CloseOffer Scenarios
-------------------------------------------------
closeSingleOffer :: EmulatorTrace ()
closeSingleOffer = do
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

  callEndpoint @"close-offer" h2 $
    CloseOfferParams
      { closeOfferBeaconsBurned = [("Offer",-1),(pubKeyAsToken lenderPubKey,-1)]
      , closeOfferBeaconRedeemer = BurnBeaconToken'
      , closeOfferBeaconPolicy = beaconPolicy
      , closeOfferLoanVal = loanValidator
      , closeOfferLoanAddress = addr
      , closeOfferSpecificUTxOs = 
          [ ( offerDatum
            , lovelaceValueOf 103_000_000 <> singleton beaconPolicySymbol "Offer" 1
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
            )
          ]
      }

offerBeaconNotBurned :: EmulatorTrace ()
offerBeaconNotBurned = do
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

  callEndpoint @"close-offer" h2 $
    CloseOfferParams
      { closeOfferBeaconsBurned = [(pubKeyAsToken lenderPubKey,-1)]
      , closeOfferBeaconRedeemer = BurnBeaconToken'
      , closeOfferBeaconPolicy = beaconPolicy
      , closeOfferLoanVal = loanValidator
      , closeOfferLoanAddress = addr
      , closeOfferSpecificUTxOs = 
          [ ( offerDatum
            , lovelaceValueOf 103_000_000 <> singleton beaconPolicySymbol "Offer" 1
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
            )
          ]
      }

lenderIdNotBurned :: EmulatorTrace ()
lenderIdNotBurned = do
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

  callEndpoint @"close-offer" h2 $
    CloseOfferParams
      { closeOfferBeaconsBurned = [("Offer",-1)]
      , closeOfferBeaconRedeemer = BurnBeaconToken'
      , closeOfferBeaconPolicy = beaconPolicy
      , closeOfferLoanVal = loanValidator
      , closeOfferLoanAddress = addr
      , closeOfferSpecificUTxOs = 
          [ ( offerDatum
            , lovelaceValueOf 103_000_000 <> singleton beaconPolicySymbol "Offer" 1
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
            )
          ]
      }

otherBeaconsNotBurned :: EmulatorTrace ()
otherBeaconsNotBurned = do
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

  callEndpoint @"close-offer" h2 $
    CloseOfferParams
      { closeOfferBeaconsBurned = [("Offer",-2),(pubKeyAsToken lenderPubKey,-1)]
      , closeOfferBeaconRedeemer = BurnBeaconToken'
      , closeOfferBeaconPolicy = beaconPolicy
      , closeOfferLoanVal = loanValidator
      , closeOfferLoanAddress = addr
      , closeOfferSpecificUTxOs = 
          [ ( offerDatum
            , lovelaceValueOf 103_000_000 <> singleton beaconPolicySymbol "Offer" 1
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
            )
          ]
      }

offerBeaconPresentButLenderDidNotSign :: EmulatorTrace ()
offerBeaconPresentButLenderDidNotSign = do
  h2 <- activateContractWallet (knownWallet 2) endpoints
  h1 <- activateContractWallet (knownWallet 1) endpoints

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

  callEndpoint @"close-offer" h1 $
    CloseOfferParams
      { closeOfferBeaconsBurned = [("Offer",-1),(pubKeyAsToken lenderPubKey,-1)]
      , closeOfferBeaconRedeemer = BurnBeaconToken'
      , closeOfferBeaconPolicy = beaconPolicy
      , closeOfferLoanVal = loanValidator
      , closeOfferLoanAddress = addr
      , closeOfferSpecificUTxOs = 
          [ ( offerDatum
            , lovelaceValueOf 103_000_000 <> singleton beaconPolicySymbol "Offer" 1
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
            )
          ]
      }

offerBeaconNotPresentButStakeCredDidNotApprove :: EmulatorTrace ()
offerBeaconNotPresentButStakeCredDidNotApprove = do
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
      { offerBeaconsMinted = []
      , offerBeaconRedeemer = MintOfferToken' lenderPubKey
      , offerBeaconPolicy = beaconPolicy
      , offerAddress = addr
      , offerInfo = 
          [ ( Just offerDatum
            , lovelaceValueOf 103_000_000
            )
          ]
      , offerAsInline = True
      }

  void $ waitUntilSlot 2

  callEndpoint @"close-offer" h2 $
    CloseOfferParams
      { closeOfferBeaconsBurned = []
      , closeOfferBeaconRedeemer = BurnBeaconToken'
      , closeOfferBeaconPolicy = beaconPolicy
      , closeOfferLoanVal = loanValidator
      , closeOfferLoanAddress = addr
      , closeOfferSpecificUTxOs = 
          [ ( offerDatum
            , lovelaceValueOf 103_000_000 
            )
          ]
      }

offerBeaconAbsentAndStakeCredApproves :: EmulatorTrace ()
offerBeaconAbsentAndStakeCredApproves = do
  h2 <- activateContractWallet (knownWallet 2) endpoints
  h1 <- activateContractWallet (knownWallet 1) endpoints

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
      { offerBeaconsMinted = []
      , offerBeaconRedeemer = MintOfferToken' lenderPubKey
      , offerBeaconPolicy = beaconPolicy
      , offerAddress = addr
      , offerInfo = 
          [ ( Just offerDatum
            , lovelaceValueOf 103_000_000
            )
          ]
      , offerAsInline = True
      }

  void $ waitUntilSlot 2

  callEndpoint @"close-offer" h1 $
    CloseOfferParams
      { closeOfferBeaconsBurned = []
      , closeOfferBeaconRedeemer = BurnBeaconToken'
      , closeOfferBeaconPolicy = beaconPolicy
      , closeOfferLoanVal = loanValidator
      , closeOfferLoanAddress = addr
      , closeOfferSpecificUTxOs = 
          [ ( offerDatum
            , lovelaceValueOf 103_000_000 
            )
          ]
      }

closeMultipleOffers :: EmulatorTrace ()
closeMultipleOffers = do
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

  callEndpoint @"offer" h2 $
    OfferParams
      { offerBeaconsMinted = [("Offer",1),(pubKeyAsToken lenderPubKey,1)]
      , offerBeaconRedeemer = MintOfferToken' lenderPubKey
      , offerBeaconPolicy = beaconPolicy
      , offerAddress = addr
      , offerInfo = 
          [ ( Just offerDatum
            , lovelaceValueOf 104_000_000 <> singleton beaconPolicySymbol "Offer" 1
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
            )
          ]
      , offerAsInline = True
      }

  void $ waitUntilSlot 4

  callEndpoint @"close-offer" h2 $
    CloseOfferParams
      { closeOfferBeaconsBurned = [("Offer",-2),(pubKeyAsToken lenderPubKey,-2)]
      , closeOfferBeaconRedeemer = BurnBeaconToken'
      , closeOfferBeaconPolicy = beaconPolicy
      , closeOfferLoanVal = loanValidator
      , closeOfferLoanAddress = addr
      , closeOfferSpecificUTxOs = 
          [ ( offerDatum
            , lovelaceValueOf 103_000_000 <> singleton beaconPolicySymbol "Offer" 1
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
            )
          , ( offerDatum
            , lovelaceValueOf 104_000_000 <> singleton beaconPolicySymbol "Offer" 1
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
            )
          ]
      }

wrongDatumType :: EmulatorTrace ()
wrongDatumType = do
  h2 <- activateContractWallet (knownWallet 2) endpoints
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
      offerDatum = AskDatum'
        { loanBeaconSym' = beaconPolicySymbol
        , borrowerId' = pubKeyAsToken lenderPubKey
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
  
  callEndpoint @"offer" h2 $
    OfferParams
      { offerBeaconsMinted = []
      , offerBeaconRedeemer = MintOfferToken' lenderPubKey
      , offerBeaconPolicy = beaconPolicy
      , offerAddress = addr
      , offerInfo = 
          [ ( Just offerDatum
            , lovelaceValueOf 103_000_000
            )
          ]
      , offerAsInline = True
      }

  void $ waitUntilSlot 2

  callEndpoint @"close-offer" h1 $
    CloseOfferParams
      { closeOfferBeaconsBurned = []
      , closeOfferBeaconRedeemer = BurnBeaconToken'
      , closeOfferBeaconPolicy = beaconPolicy
      , closeOfferLoanVal = loanValidator
      , closeOfferLoanAddress = addr
      , closeOfferSpecificUTxOs = 
          [ ( offerDatum
            , lovelaceValueOf 103_000_000 
            )
          ]
      }

-------------------------------------------------
-- Test Function
-------------------------------------------------
tests :: TestTree
tests = do
  let opts = defaultCheckOptions & emulatorConfig .~ emConfig
  testGroup "Close offer(s)"
    [ checkPredicateOptions opts "Fail if offer beacon not burned"
        (Test.not assertNoFailedTransactions) offerBeaconNotBurned
    , checkPredicateOptions opts "Fail if lender ID not burned"
        (Test.not assertNoFailedTransactions) lenderIdNotBurned
    , checkPredicateOptions opts "Fail if not all beacons in inputs burned"
        (Test.not assertNoFailedTransactions) otherBeaconsNotBurned
    , checkPredicateOptions opts "Fail if offer beacon present but lender didn't sign"
        (Test.not assertNoFailedTransactions) offerBeaconPresentButLenderDidNotSign
    , checkPredicateOptions opts "Fail if offer beacon absent but staking cred did not approve"
        (Test.not assertNoFailedTransactions) offerBeaconNotPresentButStakeCredDidNotApprove
    , checkPredicateOptions opts "Fail if input utxo does not have an OfferDatum"
        (Test.not assertNoFailedTransactions) wrongDatumType
    , checkPredicateOptions opts "Successfully close with staking cred when offer beacon is absent"
        assertNoFailedTransactions offerBeaconAbsentAndStakeCredApproves
    , checkPredicateOptions opts "Successfully close single offer"
        assertNoFailedTransactions closeSingleOffer
    , checkPredicateOptions opts "Successfully close multiple offers"
        assertNoFailedTransactions closeMultipleOffers
    ]

testTrace :: IO ()
testTrace = runEmulatorTraceIO' def emConfig wrongDatumType