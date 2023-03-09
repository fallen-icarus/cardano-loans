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

module Test.RepayLoan
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
import Plutus.Script.Utils.V2.Generators (alwaysSucceedValidatorHash,alwaysSucceedPolicy)
import Ledger.TimeSlot

import Test.Common
import CardanoLoans

-------------------------------------------------
-- RepayLoan Scenarios
-------------------------------------------------
makePartialPayment :: EmulatorTrace ()
makePartialPayment = do
  borrowerH <- activateContractWallet (knownWallet 1) endpoints
  lenderH <- activateContractWallet (knownWallet 2) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { askBeacon' = (beaconPolicySymbol,"Ask")
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , collateral' = [testToken1]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"ask" borrowerH $
    AskParams
      { askBeaconsMinted = [("Ask",1)]
      , askBeaconRedeemer = MintAskToken' borrowerPubKey
      , askBeaconPolicy = beaconPolicy
      , askAddress = addr
      , askInfo = 
          [ ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconPolicySymbol "Ask" 1)
          ]
      , askAsInline = True
      }
  
  void $ waitUntilSlot 2

  let lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
      offerDatum = OfferDatum'
        { offerBeacon' = (beaconPolicySymbol,"Offer")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanBacking' = 100_000_000
        , loanInterest' = unsafeRatio 1 10
        , collateralRates' = [(testToken1,unsafeRatio 1 2_000_000)]
        }
  
  callEndpoint @"offer" lenderH $
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

  void $ waitUntilSlot 4

  let exp = slotToBeginPOSIXTime def 14
      activeDatum = ActiveDatum'
        { activeBeacon' = (beaconPolicySymbol,"Active")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanBacking' = 100_000_000
        , loanInterest' = unsafeRatio 1 10
        , collateralRates' = [(testToken1,unsafeRatio 1 2_000_000)]
        , loanExpiration' = exp
        , loanOutstanding' = fromInteger 100_000_000 * (fromInteger 1 + unsafeRatio 1 10)
        }
    
  callEndpoint @"accept" borrowerH $
    AcceptParams
      { acceptBeaconsMinted = [("Offer",-1),("Ask",-1),("Active",1),(pubKeyAsToken borrowerPubKey,1)]
      , acceptBeaconRedeemer = MintActiveToken' borrowerPubKey lenderPubKey
      , acceptBeaconPolicy = beaconPolicy
      , acceptLoanVal = loanValidator
      , acceptLoanAddress = addr
      , acceptSpecificUTxOs =
          [ ( offerDatum
            , lovelaceValueOf 103_000_000 <> singleton beaconPolicySymbol "Offer" 1
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
            )
          , ( askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconPolicySymbol "Ask" 1
            )
          ]
      , acceptChangeAddress = addr
      , acceptChangeOutput =
          [ (Just activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , acceptDatumAsInline = True
      , acceptWithTTL = True
      }

  void $ waitUntilSlot 6

  let amountPaid = fromInteger 22_000_000
      newActiveDatum = activeDatum{loanOutstanding'= loanOutstanding' activeDatum - amountPaid}

  callEndpoint @"repay-loan" borrowerH $
    RepayLoanParams
      { repayLoanBeaconsBurned = []
      , repayLoanBeaconRedeemer = BurnBeaconToken'
      , repayLoanBeaconPolicy = beaconPolicy
      , repayLoanVal = loanValidator
      , repayLoanAddress = addr
      , repayLoanSpecificUTxOs =
          [ ( activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , repayLoanChangeAddress = addr
      , repayLoanChangeOutputs =
          [ (Just newActiveDatum
            , lovelaceValueOf 25_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 40
            )
          ]
      , repayLoanDatumAsInline = True
      , repayLoanWithTTE = True
      , repayLoanOtherMint = []
      , repayLoanOtherMintPolicy = alwaysSucceedPolicy
      }

inputDoesNotHaveActiveDatum :: EmulatorTrace ()
inputDoesNotHaveActiveDatum = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { askBeacon' = (beaconPolicySymbol,"Ask")
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
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

  callEndpoint @"repay-loan" h1 $
    RepayLoanParams
      { repayLoanBeaconsBurned = []
      , repayLoanBeaconRedeemer = BurnBeaconToken'
      , repayLoanBeaconPolicy = beaconPolicy
      , repayLoanVal = loanValidator
      , repayLoanAddress = addr
      , repayLoanSpecificUTxOs =
          [ ( askDatum
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1
            )
          ]
      , repayLoanChangeAddress = addr
      , repayLoanChangeOutputs =
          [ 
          ]
      , repayLoanDatumAsInline = True
      , repayLoanWithTTE = True
      , repayLoanOtherMint = []
      , repayLoanOtherMintPolicy = alwaysSucceedPolicy
      }

txHasMultipleInputsFromAddress :: EmulatorTrace ()
txHasMultipleInputsFromAddress = do
  borrowerH <- activateContractWallet (knownWallet 1) endpoints
  lenderH <- activateContractWallet (knownWallet 2) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { askBeacon' = (beaconPolicySymbol,"Ask")
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , collateral' = [testToken1]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"ask" borrowerH $
    AskParams
      { askBeaconsMinted = [("Ask",1)]
      , askBeaconRedeemer = MintAskToken' borrowerPubKey
      , askBeaconPolicy = beaconPolicy
      , askAddress = addr
      , askInfo = 
          [ ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconPolicySymbol "Ask" 1)
          ]
      , askAsInline = True
      }
  
  void $ waitUntilSlot 2

  let lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
      offerDatum = OfferDatum'
        { offerBeacon' = (beaconPolicySymbol,"Offer")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanBacking' = 100_000_000
        , loanInterest' = unsafeRatio 1 10
        , collateralRates' = [(testToken1,unsafeRatio 1 2_000_000)]
        }
  
  callEndpoint @"offer" lenderH $
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

  void $ waitUntilSlot 4

  let exp = slotToBeginPOSIXTime def 14
      activeDatum = ActiveDatum'
        { activeBeacon' = (beaconPolicySymbol,"Active")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanBacking' = 100_000_000
        , loanInterest' = unsafeRatio 1 10
        , collateralRates' = [(testToken1,unsafeRatio 1 2_000_000)]
        , loanExpiration' = exp
        , loanOutstanding' = fromInteger 100_000_000 * (fromInteger 1 + unsafeRatio 1 10)
        }
    
  callEndpoint @"accept" borrowerH $
    AcceptParams
      { acceptBeaconsMinted = [("Offer",-1),("Ask",-1),("Active",1),(pubKeyAsToken borrowerPubKey,1)]
      , acceptBeaconRedeemer = MintActiveToken' borrowerPubKey lenderPubKey
      , acceptBeaconPolicy = beaconPolicy
      , acceptLoanVal = loanValidator
      , acceptLoanAddress = addr
      , acceptSpecificUTxOs =
          [ ( offerDatum
            , lovelaceValueOf 103_000_000 <> singleton beaconPolicySymbol "Offer" 1
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
            )
          , ( askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconPolicySymbol "Ask" 1
            )
          ]
      , acceptChangeAddress = addr
      , acceptChangeOutput =
          [ (Just activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , acceptDatumAsInline = True
      , acceptWithTTL = True
      }

  void $ waitUntilSlot 6

  callEndpoint @"ask" borrowerH $
    AskParams
      { askBeaconsMinted = []
      , askBeaconRedeemer = MintAskToken' borrowerPubKey
      , askBeaconPolicy = beaconPolicy
      , askAddress = addr
      , askInfo = 
          [ ( Just activeDatum
            , lovelaceValueOf 3_100_000
            )
          ]
      , askAsInline = True
      }

  void $ waitUntilSlot 8

  let amountPaid = fromInteger 22_000_000
      newActiveDatum = activeDatum{loanOutstanding'= loanOutstanding' activeDatum - amountPaid}

  callEndpoint @"repay-loan" borrowerH $
    RepayLoanParams
      { repayLoanBeaconsBurned = []
      , repayLoanBeaconRedeemer = BurnBeaconToken'
      , repayLoanBeaconPolicy = beaconPolicy
      , repayLoanVal = loanValidator
      , repayLoanAddress = addr
      , repayLoanSpecificUTxOs =
          [ ( activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          , ( activeDatum
            , lovelaceValueOf 3_100_000
            )
          ]
      , repayLoanChangeAddress = addr
      , repayLoanChangeOutputs =
          [ (Just newActiveDatum
            , lovelaceValueOf 25_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 40
            )
          ]
      , repayLoanDatumAsInline = True
      , repayLoanWithTTE = True
      , repayLoanOtherMint = []
      , repayLoanOtherMintPolicy = alwaysSucceedPolicy
      }

loanIsExpired :: EmulatorTrace ()
loanIsExpired = do
  borrowerH <- activateContractWallet (knownWallet 1) endpoints
  lenderH <- activateContractWallet (knownWallet 2) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { askBeacon' = (beaconPolicySymbol,"Ask")
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , collateral' = [testToken1]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"ask" borrowerH $
    AskParams
      { askBeaconsMinted = [("Ask",1)]
      , askBeaconRedeemer = MintAskToken' borrowerPubKey
      , askBeaconPolicy = beaconPolicy
      , askAddress = addr
      , askInfo = 
          [ ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconPolicySymbol "Ask" 1)
          ]
      , askAsInline = True
      }
  
  void $ waitUntilSlot 2

  let lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
      offerDatum = OfferDatum'
        { offerBeacon' = (beaconPolicySymbol,"Offer")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanBacking' = 100_000_000
        , loanInterest' = unsafeRatio 1 10
        , collateralRates' = [(testToken1,unsafeRatio 1 2_000_000)]
        }
  
  callEndpoint @"offer" lenderH $
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

  void $ waitUntilSlot 4

  let exp = slotToBeginPOSIXTime def 14
      activeDatum = ActiveDatum'
        { activeBeacon' = (beaconPolicySymbol,"Active")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanBacking' = 100_000_000
        , loanInterest' = unsafeRatio 1 10
        , collateralRates' = [(testToken1,unsafeRatio 1 2_000_000)]
        , loanExpiration' = exp
        , loanOutstanding' = fromInteger 100_000_000 * (fromInteger 1 + unsafeRatio 1 10)
        }
    
  callEndpoint @"accept" borrowerH $
    AcceptParams
      { acceptBeaconsMinted = [("Offer",-1),("Ask",-1),("Active",1),(pubKeyAsToken borrowerPubKey,1)]
      , acceptBeaconRedeemer = MintActiveToken' borrowerPubKey lenderPubKey
      , acceptBeaconPolicy = beaconPolicy
      , acceptLoanVal = loanValidator
      , acceptLoanAddress = addr
      , acceptSpecificUTxOs =
          [ ( offerDatum
            , lovelaceValueOf 103_000_000 <> singleton beaconPolicySymbol "Offer" 1
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
            )
          , ( askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconPolicySymbol "Ask" 1
            )
          ]
      , acceptChangeAddress = addr
      , acceptChangeOutput =
          [ (Just activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , acceptDatumAsInline = True
      , acceptWithTTL = True
      }

  void $ waitUntilSlot 15

  let amountPaid = fromInteger 22_000_000
      newActiveDatum = activeDatum{loanOutstanding'= loanOutstanding' activeDatum - amountPaid}

  callEndpoint @"repay-loan" borrowerH $
    RepayLoanParams
      { repayLoanBeaconsBurned = []
      , repayLoanBeaconRedeemer = BurnBeaconToken'
      , repayLoanBeaconPolicy = beaconPolicy
      , repayLoanVal = loanValidator
      , repayLoanAddress = addr
      , repayLoanSpecificUTxOs =
          [ ( activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , repayLoanChangeAddress = addr
      , repayLoanChangeOutputs =
          [ (Just newActiveDatum
            , lovelaceValueOf 25_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 40
            )
          ]
      , repayLoanDatumAsInline = True
      , repayLoanWithTTE = True
      , repayLoanOtherMint = []
      , repayLoanOtherMintPolicy = alwaysSucceedPolicy
      }

multipleOutputsToAddress :: EmulatorTrace ()
multipleOutputsToAddress = do
  borrowerH <- activateContractWallet (knownWallet 1) endpoints
  lenderH <- activateContractWallet (knownWallet 2) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { askBeacon' = (beaconPolicySymbol,"Ask")
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , collateral' = [testToken1]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"ask" borrowerH $
    AskParams
      { askBeaconsMinted = [("Ask",1)]
      , askBeaconRedeemer = MintAskToken' borrowerPubKey
      , askBeaconPolicy = beaconPolicy
      , askAddress = addr
      , askInfo = 
          [ ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconPolicySymbol "Ask" 1)
          ]
      , askAsInline = True
      }
  
  void $ waitUntilSlot 2

  let lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
      offerDatum = OfferDatum'
        { offerBeacon' = (beaconPolicySymbol,"Offer")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanBacking' = 100_000_000
        , loanInterest' = unsafeRatio 1 10
        , collateralRates' = [(testToken1,unsafeRatio 1 2_000_000)]
        }
  
  callEndpoint @"offer" lenderH $
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

  void $ waitUntilSlot 4

  let exp = slotToBeginPOSIXTime def 14
      activeDatum = ActiveDatum'
        { activeBeacon' = (beaconPolicySymbol,"Active")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanBacking' = 100_000_000
        , loanInterest' = unsafeRatio 1 10
        , collateralRates' = [(testToken1,unsafeRatio 1 2_000_000)]
        , loanExpiration' = exp
        , loanOutstanding' = fromInteger 100_000_000 * (fromInteger 1 + unsafeRatio 1 10)
        }
    
  callEndpoint @"accept" borrowerH $
    AcceptParams
      { acceptBeaconsMinted = [("Offer",-1),("Ask",-1),("Active",1),(pubKeyAsToken borrowerPubKey,1)]
      , acceptBeaconRedeemer = MintActiveToken' borrowerPubKey lenderPubKey
      , acceptBeaconPolicy = beaconPolicy
      , acceptLoanVal = loanValidator
      , acceptLoanAddress = addr
      , acceptSpecificUTxOs =
          [ ( offerDatum
            , lovelaceValueOf 103_000_000 <> singleton beaconPolicySymbol "Offer" 1
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
            )
          , ( askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconPolicySymbol "Ask" 1
            )
          ]
      , acceptChangeAddress = addr
      , acceptChangeOutput =
          [ (Just activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , acceptDatumAsInline = True
      , acceptWithTTL = True
      }

  void $ waitUntilSlot 6

  let amountPaid = fromInteger 22_000_000
      newActiveDatum = activeDatum{loanOutstanding'= loanOutstanding' activeDatum - amountPaid}

  callEndpoint @"repay-loan" borrowerH $
    RepayLoanParams
      { repayLoanBeaconsBurned = []
      , repayLoanBeaconRedeemer = BurnBeaconToken'
      , repayLoanBeaconPolicy = beaconPolicy
      , repayLoanVal = loanValidator
      , repayLoanAddress = addr
      , repayLoanSpecificUTxOs =
          [ ( activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , repayLoanChangeAddress = addr
      , repayLoanChangeOutputs =
          [ (Just newActiveDatum
            , lovelaceValueOf 25_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 40
            )
          , (Just newActiveDatum
            , lovelaceValueOf 3_000_000
            )
          ]
      , repayLoanDatumAsInline = True
      , repayLoanWithTTE = True
      , repayLoanOtherMint = []
      , repayLoanOtherMintPolicy = alwaysSucceedPolicy
      }

noOutputsToAddress :: EmulatorTrace ()
noOutputsToAddress = do
  borrowerH <- activateContractWallet (knownWallet 1) endpoints
  lenderH <- activateContractWallet (knownWallet 2) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { askBeacon' = (beaconPolicySymbol,"Ask")
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , collateral' = [testToken1]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"ask" borrowerH $
    AskParams
      { askBeaconsMinted = [("Ask",1)]
      , askBeaconRedeemer = MintAskToken' borrowerPubKey
      , askBeaconPolicy = beaconPolicy
      , askAddress = addr
      , askInfo = 
          [ ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconPolicySymbol "Ask" 1)
          ]
      , askAsInline = True
      }
  
  void $ waitUntilSlot 2

  let lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
      offerDatum = OfferDatum'
        { offerBeacon' = (beaconPolicySymbol,"Offer")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanBacking' = 100_000_000
        , loanInterest' = unsafeRatio 1 10
        , collateralRates' = [(testToken1,unsafeRatio 1 2_000_000)]
        }
  
  callEndpoint @"offer" lenderH $
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

  void $ waitUntilSlot 4

  let exp = slotToBeginPOSIXTime def 14
      activeDatum = ActiveDatum'
        { activeBeacon' = (beaconPolicySymbol,"Active")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanBacking' = 100_000_000
        , loanInterest' = unsafeRatio 1 10
        , collateralRates' = [(testToken1,unsafeRatio 1 2_000_000)]
        , loanExpiration' = exp
        , loanOutstanding' = fromInteger 100_000_000 * (fromInteger 1 + unsafeRatio 1 10)
        }
    
  callEndpoint @"accept" borrowerH $
    AcceptParams
      { acceptBeaconsMinted = [("Offer",-1),("Ask",-1),("Active",1),(pubKeyAsToken borrowerPubKey,1)]
      , acceptBeaconRedeemer = MintActiveToken' borrowerPubKey lenderPubKey
      , acceptBeaconPolicy = beaconPolicy
      , acceptLoanVal = loanValidator
      , acceptLoanAddress = addr
      , acceptSpecificUTxOs =
          [ ( offerDatum
            , lovelaceValueOf 103_000_000 <> singleton beaconPolicySymbol "Offer" 1
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
            )
          , ( askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconPolicySymbol "Ask" 1
            )
          ]
      , acceptChangeAddress = addr
      , acceptChangeOutput =
          [ (Just activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , acceptDatumAsInline = True
      , acceptWithTTL = True
      }

  void $ waitUntilSlot 6

  let amountPaid = fromInteger 22_000_000
      newActiveDatum = activeDatum{loanOutstanding'= loanOutstanding' activeDatum - amountPaid}

  callEndpoint @"repay-loan" borrowerH $
    RepayLoanParams
      { repayLoanBeaconsBurned = []
      , repayLoanBeaconRedeemer = BurnBeaconToken'
      , repayLoanBeaconPolicy = beaconPolicy
      , repayLoanVal = loanValidator
      , repayLoanAddress = addr
      , repayLoanSpecificUTxOs =
          [ ( activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , repayLoanChangeAddress = addr
      , repayLoanChangeOutputs =
          [ 
          ]
      , repayLoanDatumAsInline = True
      , repayLoanWithTTE = True
      , repayLoanOtherMint = []
      , repayLoanOtherMintPolicy = alwaysSucceedPolicy
      }

outputHasChangedOtherFieldInActiveDatum :: EmulatorTrace ()
outputHasChangedOtherFieldInActiveDatum = do
  borrowerH <- activateContractWallet (knownWallet 1) endpoints
  lenderH <- activateContractWallet (knownWallet 2) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { askBeacon' = (beaconPolicySymbol,"Ask")
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , collateral' = [testToken1]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"ask" borrowerH $
    AskParams
      { askBeaconsMinted = [("Ask",1)]
      , askBeaconRedeemer = MintAskToken' borrowerPubKey
      , askBeaconPolicy = beaconPolicy
      , askAddress = addr
      , askInfo = 
          [ ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconPolicySymbol "Ask" 1)
          ]
      , askAsInline = True
      }
  
  void $ waitUntilSlot 2

  let lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
      offerDatum = OfferDatum'
        { offerBeacon' = (beaconPolicySymbol,"Offer")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanBacking' = 100_000_000
        , loanInterest' = unsafeRatio 1 10
        , collateralRates' = [(testToken1,unsafeRatio 1 2_000_000)]
        }
  
  callEndpoint @"offer" lenderH $
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

  void $ waitUntilSlot 4

  let exp = slotToBeginPOSIXTime def 14
      activeDatum = ActiveDatum'
        { activeBeacon' = (beaconPolicySymbol,"Active")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanBacking' = 100_000_000
        , loanInterest' = unsafeRatio 1 10
        , collateralRates' = [(testToken1,unsafeRatio 1 2_000_000)]
        , loanExpiration' = exp
        , loanOutstanding' = fromInteger 100_000_000 * (fromInteger 1 + unsafeRatio 1 10)
        }
    
  callEndpoint @"accept" borrowerH $
    AcceptParams
      { acceptBeaconsMinted = [("Offer",-1),("Ask",-1),("Active",1),(pubKeyAsToken borrowerPubKey,1)]
      , acceptBeaconRedeemer = MintActiveToken' borrowerPubKey lenderPubKey
      , acceptBeaconPolicy = beaconPolicy
      , acceptLoanVal = loanValidator
      , acceptLoanAddress = addr
      , acceptSpecificUTxOs =
          [ ( offerDatum
            , lovelaceValueOf 103_000_000 <> singleton beaconPolicySymbol "Offer" 1
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
            )
          , ( askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconPolicySymbol "Ask" 1
            )
          ]
      , acceptChangeAddress = addr
      , acceptChangeOutput =
          [ (Just activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , acceptDatumAsInline = True
      , acceptWithTTL = True
      }

  void $ waitUntilSlot 6

  let amountPaid = fromInteger 22_000_000
      newActiveDatum = activeDatum{loanOutstanding'= loanOutstanding' activeDatum - amountPaid}

  callEndpoint @"repay-loan" borrowerH $
    RepayLoanParams
      { repayLoanBeaconsBurned = []
      , repayLoanBeaconRedeemer = BurnBeaconToken'
      , repayLoanBeaconPolicy = beaconPolicy
      , repayLoanVal = loanValidator
      , repayLoanAddress = addr
      , repayLoanSpecificUTxOs =
          [ ( activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , repayLoanChangeAddress = addr
      , repayLoanChangeOutputs =
          [ (Just newActiveDatum{loanPrinciple' = 10}
            , lovelaceValueOf 25_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 40
            )
          ]
      , repayLoanDatumAsInline = True
      , repayLoanWithTTE = True
      , repayLoanOtherMint = []
      , repayLoanOtherMintPolicy = alwaysSucceedPolicy
      }

takeMoreColalteralThanRepaid :: EmulatorTrace ()
takeMoreColalteralThanRepaid = do
  borrowerH <- activateContractWallet (knownWallet 1) endpoints
  lenderH <- activateContractWallet (knownWallet 2) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { askBeacon' = (beaconPolicySymbol,"Ask")
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , collateral' = [testToken1]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"ask" borrowerH $
    AskParams
      { askBeaconsMinted = [("Ask",1)]
      , askBeaconRedeemer = MintAskToken' borrowerPubKey
      , askBeaconPolicy = beaconPolicy
      , askAddress = addr
      , askInfo = 
          [ ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconPolicySymbol "Ask" 1)
          ]
      , askAsInline = True
      }
  
  void $ waitUntilSlot 2

  let lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
      offerDatum = OfferDatum'
        { offerBeacon' = (beaconPolicySymbol,"Offer")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanBacking' = 100_000_000
        , loanInterest' = unsafeRatio 1 10
        , collateralRates' = [(testToken1,unsafeRatio 1 2_000_000)]
        }
  
  callEndpoint @"offer" lenderH $
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

  void $ waitUntilSlot 4

  let exp = slotToBeginPOSIXTime def 14
      activeDatum = ActiveDatum'
        { activeBeacon' = (beaconPolicySymbol,"Active")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanBacking' = 100_000_000
        , loanInterest' = unsafeRatio 1 10
        , collateralRates' = [(testToken1,unsafeRatio 1 2_000_000)]
        , loanExpiration' = exp
        , loanOutstanding' = fromInteger 100_000_000 * (fromInteger 1 + unsafeRatio 1 10)
        }
    
  callEndpoint @"accept" borrowerH $
    AcceptParams
      { acceptBeaconsMinted = [("Offer",-1),("Ask",-1),("Active",1),(pubKeyAsToken borrowerPubKey,1)]
      , acceptBeaconRedeemer = MintActiveToken' borrowerPubKey lenderPubKey
      , acceptBeaconPolicy = beaconPolicy
      , acceptLoanVal = loanValidator
      , acceptLoanAddress = addr
      , acceptSpecificUTxOs =
          [ ( offerDatum
            , lovelaceValueOf 103_000_000 <> singleton beaconPolicySymbol "Offer" 1
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
            )
          , ( askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconPolicySymbol "Ask" 1
            )
          ]
      , acceptChangeAddress = addr
      , acceptChangeOutput =
          [ (Just activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , acceptDatumAsInline = True
      , acceptWithTTL = True
      }

  void $ waitUntilSlot 6

  let amountPaid = fromInteger 22_000_000
      newActiveDatum = activeDatum{loanOutstanding'= loanOutstanding' activeDatum - amountPaid}

  callEndpoint @"repay-loan" borrowerH $
    RepayLoanParams
      { repayLoanBeaconsBurned = []
      , repayLoanBeaconRedeemer = BurnBeaconToken'
      , repayLoanBeaconPolicy = beaconPolicy
      , repayLoanVal = loanValidator
      , repayLoanAddress = addr
      , repayLoanSpecificUTxOs =
          [ ( activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , repayLoanChangeAddress = addr
      , repayLoanChangeOutputs =
          [ (Just newActiveDatum
            , lovelaceValueOf 25_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 39
            )
          ]
      , repayLoanDatumAsInline = True
      , repayLoanWithTTE = True
      , repayLoanOtherMint = []
      , repayLoanOtherMintPolicy = alwaysSucceedPolicy
      }

partialPaymetOutputMissingActiveBeacon :: EmulatorTrace ()
partialPaymetOutputMissingActiveBeacon = do
  borrowerH <- activateContractWallet (knownWallet 1) endpoints
  lenderH <- activateContractWallet (knownWallet 2) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { askBeacon' = (beaconPolicySymbol,"Ask")
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , collateral' = [testToken1]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"ask" borrowerH $
    AskParams
      { askBeaconsMinted = [("Ask",1)]
      , askBeaconRedeemer = MintAskToken' borrowerPubKey
      , askBeaconPolicy = beaconPolicy
      , askAddress = addr
      , askInfo = 
          [ ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconPolicySymbol "Ask" 1)
          ]
      , askAsInline = True
      }
  
  void $ waitUntilSlot 2

  let lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
      offerDatum = OfferDatum'
        { offerBeacon' = (beaconPolicySymbol,"Offer")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanBacking' = 100_000_000
        , loanInterest' = unsafeRatio 1 10
        , collateralRates' = [(testToken1,unsafeRatio 1 2_000_000)]
        }
  
  callEndpoint @"offer" lenderH $
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

  void $ waitUntilSlot 4

  let exp = slotToBeginPOSIXTime def 14
      activeDatum = ActiveDatum'
        { activeBeacon' = (beaconPolicySymbol,"Active")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanBacking' = 100_000_000
        , loanInterest' = unsafeRatio 1 10
        , collateralRates' = [(testToken1,unsafeRatio 1 2_000_000)]
        , loanExpiration' = exp
        , loanOutstanding' = fromInteger 100_000_000 * (fromInteger 1 + unsafeRatio 1 10)
        }
    
  callEndpoint @"accept" borrowerH $
    AcceptParams
      { acceptBeaconsMinted = [("Offer",-1),("Ask",-1),("Active",1),(pubKeyAsToken borrowerPubKey,1)]
      , acceptBeaconRedeemer = MintActiveToken' borrowerPubKey lenderPubKey
      , acceptBeaconPolicy = beaconPolicy
      , acceptLoanVal = loanValidator
      , acceptLoanAddress = addr
      , acceptSpecificUTxOs =
          [ ( offerDatum
            , lovelaceValueOf 103_000_000 <> singleton beaconPolicySymbol "Offer" 1
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
            )
          , ( askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconPolicySymbol "Ask" 1
            )
          ]
      , acceptChangeAddress = addr
      , acceptChangeOutput =
          [ (Just activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , acceptDatumAsInline = True
      , acceptWithTTL = True
      }

  void $ waitUntilSlot 6

  let amountPaid = fromInteger 22_000_000
      newActiveDatum = activeDatum{loanOutstanding'= loanOutstanding' activeDatum - amountPaid}

  callEndpoint @"repay-loan" borrowerH $
    RepayLoanParams
      { repayLoanBeaconsBurned = []
      , repayLoanBeaconRedeemer = BurnBeaconToken'
      , repayLoanBeaconPolicy = beaconPolicy
      , repayLoanVal = loanValidator
      , repayLoanAddress = addr
      , repayLoanSpecificUTxOs =
          [ ( activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , repayLoanChangeAddress = addr
      , repayLoanChangeOutputs =
          [ (Just newActiveDatum
            , lovelaceValueOf 25_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> (uncurry singleton testToken1) 40
            )
          ]
      , repayLoanDatumAsInline = True
      , repayLoanWithTTE = True
      , repayLoanOtherMint = []
      , repayLoanOtherMintPolicy = alwaysSucceedPolicy
      }

partialPaymetOutputMissingLenderID :: EmulatorTrace ()
partialPaymetOutputMissingLenderID = do
  borrowerH <- activateContractWallet (knownWallet 1) endpoints
  lenderH <- activateContractWallet (knownWallet 2) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { askBeacon' = (beaconPolicySymbol,"Ask")
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , collateral' = [testToken1]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"ask" borrowerH $
    AskParams
      { askBeaconsMinted = [("Ask",1)]
      , askBeaconRedeemer = MintAskToken' borrowerPubKey
      , askBeaconPolicy = beaconPolicy
      , askAddress = addr
      , askInfo = 
          [ ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconPolicySymbol "Ask" 1)
          ]
      , askAsInline = True
      }
  
  void $ waitUntilSlot 2

  let lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
      offerDatum = OfferDatum'
        { offerBeacon' = (beaconPolicySymbol,"Offer")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanBacking' = 100_000_000
        , loanInterest' = unsafeRatio 1 10
        , collateralRates' = [(testToken1,unsafeRatio 1 2_000_000)]
        }
  
  callEndpoint @"offer" lenderH $
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

  void $ waitUntilSlot 4

  let exp = slotToBeginPOSIXTime def 14
      activeDatum = ActiveDatum'
        { activeBeacon' = (beaconPolicySymbol,"Active")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanBacking' = 100_000_000
        , loanInterest' = unsafeRatio 1 10
        , collateralRates' = [(testToken1,unsafeRatio 1 2_000_000)]
        , loanExpiration' = exp
        , loanOutstanding' = fromInteger 100_000_000 * (fromInteger 1 + unsafeRatio 1 10)
        }
    
  callEndpoint @"accept" borrowerH $
    AcceptParams
      { acceptBeaconsMinted = [("Offer",-1),("Ask",-1),("Active",1),(pubKeyAsToken borrowerPubKey,1)]
      , acceptBeaconRedeemer = MintActiveToken' borrowerPubKey lenderPubKey
      , acceptBeaconPolicy = beaconPolicy
      , acceptLoanVal = loanValidator
      , acceptLoanAddress = addr
      , acceptSpecificUTxOs =
          [ ( offerDatum
            , lovelaceValueOf 103_000_000 <> singleton beaconPolicySymbol "Offer" 1
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
            )
          , ( askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconPolicySymbol "Ask" 1
            )
          ]
      , acceptChangeAddress = addr
      , acceptChangeOutput =
          [ (Just activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , acceptDatumAsInline = True
      , acceptWithTTL = True
      }

  void $ waitUntilSlot 6

  let amountPaid = fromInteger 22_000_000
      newActiveDatum = activeDatum{loanOutstanding'= loanOutstanding' activeDatum - amountPaid}

  callEndpoint @"repay-loan" borrowerH $
    RepayLoanParams
      { repayLoanBeaconsBurned = []
      , repayLoanBeaconRedeemer = BurnBeaconToken'
      , repayLoanBeaconPolicy = beaconPolicy
      , repayLoanVal = loanValidator
      , repayLoanAddress = addr
      , repayLoanSpecificUTxOs =
          [ ( activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , repayLoanChangeAddress = addr
      , repayLoanChangeOutputs =
          [ (Just newActiveDatum
            , lovelaceValueOf 25_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 40
            )
          ]
      , repayLoanDatumAsInline = True
      , repayLoanWithTTE = True
      , repayLoanOtherMint = []
      , repayLoanOtherMintPolicy = alwaysSucceedPolicy
      }

partialPaymetOutputMissingBorrowerID :: EmulatorTrace ()
partialPaymetOutputMissingBorrowerID = do
  borrowerH <- activateContractWallet (knownWallet 1) endpoints
  lenderH <- activateContractWallet (knownWallet 2) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { askBeacon' = (beaconPolicySymbol,"Ask")
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , collateral' = [testToken1]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"ask" borrowerH $
    AskParams
      { askBeaconsMinted = [("Ask",1)]
      , askBeaconRedeemer = MintAskToken' borrowerPubKey
      , askBeaconPolicy = beaconPolicy
      , askAddress = addr
      , askInfo = 
          [ ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconPolicySymbol "Ask" 1)
          ]
      , askAsInline = True
      }
  
  void $ waitUntilSlot 2

  let lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
      offerDatum = OfferDatum'
        { offerBeacon' = (beaconPolicySymbol,"Offer")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanBacking' = 100_000_000
        , loanInterest' = unsafeRatio 1 10
        , collateralRates' = [(testToken1,unsafeRatio 1 2_000_000)]
        }
  
  callEndpoint @"offer" lenderH $
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

  void $ waitUntilSlot 4

  let exp = slotToBeginPOSIXTime def 14
      activeDatum = ActiveDatum'
        { activeBeacon' = (beaconPolicySymbol,"Active")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanBacking' = 100_000_000
        , loanInterest' = unsafeRatio 1 10
        , collateralRates' = [(testToken1,unsafeRatio 1 2_000_000)]
        , loanExpiration' = exp
        , loanOutstanding' = fromInteger 100_000_000 * (fromInteger 1 + unsafeRatio 1 10)
        }
    
  callEndpoint @"accept" borrowerH $
    AcceptParams
      { acceptBeaconsMinted = [("Offer",-1),("Ask",-1),("Active",1),(pubKeyAsToken borrowerPubKey,1)]
      , acceptBeaconRedeemer = MintActiveToken' borrowerPubKey lenderPubKey
      , acceptBeaconPolicy = beaconPolicy
      , acceptLoanVal = loanValidator
      , acceptLoanAddress = addr
      , acceptSpecificUTxOs =
          [ ( offerDatum
            , lovelaceValueOf 103_000_000 <> singleton beaconPolicySymbol "Offer" 1
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
            )
          , ( askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconPolicySymbol "Ask" 1
            )
          ]
      , acceptChangeAddress = addr
      , acceptChangeOutput =
          [ (Just activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , acceptDatumAsInline = True
      , acceptWithTTL = True
      }

  void $ waitUntilSlot 6

  let amountPaid = fromInteger 22_000_000
      newActiveDatum = activeDatum{loanOutstanding'= loanOutstanding' activeDatum - amountPaid}

  callEndpoint @"repay-loan" borrowerH $
    RepayLoanParams
      { repayLoanBeaconsBurned = []
      , repayLoanBeaconRedeemer = BurnBeaconToken'
      , repayLoanBeaconPolicy = beaconPolicy
      , repayLoanVal = loanValidator
      , repayLoanAddress = addr
      , repayLoanSpecificUTxOs =
          [ ( activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , repayLoanChangeAddress = addr
      , repayLoanChangeOutputs =
          [ (Just newActiveDatum
            , lovelaceValueOf 25_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 40
            )
          ]
      , repayLoanDatumAsInline = True
      , repayLoanWithTTE = True
      , repayLoanOtherMint = []
      , repayLoanOtherMintPolicy = alwaysSucceedPolicy
      }

stakingCredDidNotApprove :: EmulatorTrace ()
stakingCredDidNotApprove = do
  borrowerH <- activateContractWallet (knownWallet 1) endpoints
  lenderH <- activateContractWallet (knownWallet 2) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { askBeacon' = (beaconPolicySymbol,"Ask")
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , collateral' = [testToken1]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"ask" borrowerH $
    AskParams
      { askBeaconsMinted = [("Ask",1)]
      , askBeaconRedeemer = MintAskToken' borrowerPubKey
      , askBeaconPolicy = beaconPolicy
      , askAddress = addr
      , askInfo = 
          [ ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconPolicySymbol "Ask" 1)
          ]
      , askAsInline = True
      }
  
  void $ waitUntilSlot 2

  let lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
      offerDatum = OfferDatum'
        { offerBeacon' = (beaconPolicySymbol,"Offer")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanBacking' = 100_000_000
        , loanInterest' = unsafeRatio 1 10
        , collateralRates' = [(testToken1,unsafeRatio 1 2_000_000)]
        }
  
  callEndpoint @"offer" lenderH $
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

  void $ waitUntilSlot 4

  let exp = slotToBeginPOSIXTime def 14
      activeDatum = ActiveDatum'
        { activeBeacon' = (beaconPolicySymbol,"Active")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanBacking' = 100_000_000
        , loanInterest' = unsafeRatio 1 10
        , collateralRates' = [(testToken1,unsafeRatio 1 2_000_000)]
        , loanExpiration' = exp
        , loanOutstanding' = fromInteger 100_000_000 * (fromInteger 1 + unsafeRatio 1 10)
        }
    
  callEndpoint @"accept" borrowerH $
    AcceptParams
      { acceptBeaconsMinted = [("Offer",-1),("Ask",-1),("Active",1),(pubKeyAsToken borrowerPubKey,1)]
      , acceptBeaconRedeemer = MintActiveToken' borrowerPubKey lenderPubKey
      , acceptBeaconPolicy = beaconPolicy
      , acceptLoanVal = loanValidator
      , acceptLoanAddress = addr
      , acceptSpecificUTxOs =
          [ ( offerDatum
            , lovelaceValueOf 103_000_000 <> singleton beaconPolicySymbol "Offer" 1
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
            )
          , ( askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconPolicySymbol "Ask" 1
            )
          ]
      , acceptChangeAddress = addr
      , acceptChangeOutput =
          [ (Just activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , acceptDatumAsInline = True
      , acceptWithTTL = True
      }

  void $ waitUntilSlot 6

  let amountPaid = fromInteger 22_000_000
      newActiveDatum = activeDatum{loanOutstanding'= loanOutstanding' activeDatum - amountPaid}

  callEndpoint @"repay-loan" lenderH $
    RepayLoanParams
      { repayLoanBeaconsBurned = []
      , repayLoanBeaconRedeemer = BurnBeaconToken'
      , repayLoanBeaconPolicy = beaconPolicy
      , repayLoanVal = loanValidator
      , repayLoanAddress = addr
      , repayLoanSpecificUTxOs =
          [ ( activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , repayLoanChangeAddress = addr
      , repayLoanChangeOutputs =
          [ (Just newActiveDatum
            , lovelaceValueOf 25_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 40
            )
          ]
      , repayLoanDatumAsInline = True
      , repayLoanWithTTE = True
      , repayLoanOtherMint = []
      , repayLoanOtherMintPolicy = alwaysSucceedPolicy
      }

makePartialPaymentOnUnderCollateralizedLoan :: EmulatorTrace ()
makePartialPaymentOnUnderCollateralizedLoan = do
  borrowerH <- activateContractWallet (knownWallet 1) endpoints
  lenderH <- activateContractWallet (knownWallet 2) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { askBeacon' = (beaconPolicySymbol,"Ask")
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , collateral' = [testToken1]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"ask" borrowerH $
    AskParams
      { askBeaconsMinted = [("Ask",1)]
      , askBeaconRedeemer = MintAskToken' borrowerPubKey
      , askBeaconPolicy = beaconPolicy
      , askAddress = addr
      , askInfo = 
          [ ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconPolicySymbol "Ask" 1)
          ]
      , askAsInline = True
      }
  
  void $ waitUntilSlot 2

  let lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
      offerDatum = OfferDatum'
        { offerBeacon' = (beaconPolicySymbol,"Offer")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanBacking' = 20_000_000
        , loanInterest' = unsafeRatio 1 10
        , collateralRates' = [(testToken1,unsafeRatio 1 2_000_000)]
        }
  
  callEndpoint @"offer" lenderH $
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

  void $ waitUntilSlot 4

  let exp = slotToBeginPOSIXTime def 14
      activeDatum = ActiveDatum'
        { activeBeacon' = (beaconPolicySymbol,"Active")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanBacking' = 20_000_000
        , loanInterest' = unsafeRatio 1 10
        , collateralRates' = [(testToken1,unsafeRatio 1 2_000_000)]
        , loanExpiration' = exp
        , loanOutstanding' = fromInteger 100_000_000 * (fromInteger 1 + unsafeRatio 1 10)
        }
    
  callEndpoint @"accept" borrowerH $
    AcceptParams
      { acceptBeaconsMinted = [("Offer",-1),("Ask",-1),("Active",1),(pubKeyAsToken borrowerPubKey,1)]
      , acceptBeaconRedeemer = MintActiveToken' borrowerPubKey lenderPubKey
      , acceptBeaconPolicy = beaconPolicy
      , acceptLoanVal = loanValidator
      , acceptLoanAddress = addr
      , acceptSpecificUTxOs =
          [ ( offerDatum
            , lovelaceValueOf 103_000_000 <> singleton beaconPolicySymbol "Offer" 1
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
            )
          , ( askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconPolicySymbol "Ask" 1
            )
          ]
      , acceptChangeAddress = addr
      , acceptChangeOutput =
          [ (Just activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 10
            )
          ]
      , acceptDatumAsInline = True
      , acceptWithTTL = True
      }

  void $ waitUntilSlot 6

  let amountPaid = fromInteger 22_000_000
      newActiveDatum = activeDatum{loanOutstanding'= loanOutstanding' activeDatum - amountPaid}

  callEndpoint @"repay-loan" borrowerH $
    RepayLoanParams
      { repayLoanBeaconsBurned = []
      , repayLoanBeaconRedeemer = BurnBeaconToken'
      , repayLoanBeaconPolicy = beaconPolicy
      , repayLoanVal = loanValidator
      , repayLoanAddress = addr
      , repayLoanSpecificUTxOs =
          [ ( activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 10
            )
          ]
      , repayLoanChangeAddress = addr
      , repayLoanChangeOutputs =
          [ (Just newActiveDatum
            , lovelaceValueOf 25_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
            )
          ]
      , repayLoanDatumAsInline = True
      , repayLoanWithTTE = True
      , repayLoanOtherMint = []
      , repayLoanOtherMintPolicy = alwaysSucceedPolicy
      }

fullyRepayLoan :: EmulatorTrace ()
fullyRepayLoan = do
  borrowerH <- activateContractWallet (knownWallet 1) endpoints
  lenderH <- activateContractWallet (knownWallet 2) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { askBeacon' = (beaconPolicySymbol,"Ask")
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , collateral' = [testToken1]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"ask" borrowerH $
    AskParams
      { askBeaconsMinted = [("Ask",1)]
      , askBeaconRedeemer = MintAskToken' borrowerPubKey
      , askBeaconPolicy = beaconPolicy
      , askAddress = addr
      , askInfo = 
          [ ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconPolicySymbol "Ask" 1)
          ]
      , askAsInline = True
      }
  
  void $ waitUntilSlot 2

  let lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
      offerDatum = OfferDatum'
        { offerBeacon' = (beaconPolicySymbol,"Offer")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanBacking' = 100_000_000
        , loanInterest' = unsafeRatio 1 10
        , collateralRates' = [(testToken1,unsafeRatio 1 2_000_000)]
        }
  
  callEndpoint @"offer" lenderH $
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

  void $ waitUntilSlot 4

  let exp = slotToBeginPOSIXTime def 14
      activeDatum = ActiveDatum'
        { activeBeacon' = (beaconPolicySymbol,"Active")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanBacking' = 100_000_000
        , loanInterest' = unsafeRatio 1 10
        , collateralRates' = [(testToken1,unsafeRatio 1 2_000_000)]
        , loanExpiration' = exp
        , loanOutstanding' = fromInteger 100_000_000 * (fromInteger 1 + unsafeRatio 1 10)
        }
    
  callEndpoint @"accept" borrowerH $
    AcceptParams
      { acceptBeaconsMinted = [("Offer",-1),("Ask",-1),("Active",1),(pubKeyAsToken borrowerPubKey,1)]
      , acceptBeaconRedeemer = MintActiveToken' borrowerPubKey lenderPubKey
      , acceptBeaconPolicy = beaconPolicy
      , acceptLoanVal = loanValidator
      , acceptLoanAddress = addr
      , acceptSpecificUTxOs =
          [ ( offerDatum
            , lovelaceValueOf 103_000_000 <> singleton beaconPolicySymbol "Offer" 1
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
            )
          , ( askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconPolicySymbol "Ask" 1
            )
          ]
      , acceptChangeAddress = addr
      , acceptChangeOutput =
          [ (Just activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , acceptDatumAsInline = True
      , acceptWithTTL = True
      }

  void $ waitUntilSlot 6

  let amountPaid = fromInteger 110_000_000
      newActiveDatum = activeDatum{loanOutstanding'= loanOutstanding' activeDatum - amountPaid}

  callEndpoint @"repay-loan" borrowerH $
    RepayLoanParams
      { repayLoanBeaconsBurned = [(pubKeyAsToken borrowerPubKey,-1)]
      , repayLoanBeaconRedeemer = BurnBeaconToken'
      , repayLoanBeaconPolicy = beaconPolicy
      , repayLoanVal = loanValidator
      , repayLoanAddress = addr
      , repayLoanSpecificUTxOs =
          [ ( activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , repayLoanChangeAddress = addr
      , repayLoanChangeOutputs =
          [ (Just newActiveDatum
            , lovelaceValueOf 113_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
            )
          ]
      , repayLoanDatumAsInline = True
      , repayLoanWithTTE = True
      , repayLoanOtherMint = []
      , repayLoanOtherMintPolicy = alwaysSucceedPolicy
      }

borrowerIdNotBurned :: EmulatorTrace ()
borrowerIdNotBurned = do
  borrowerH <- activateContractWallet (knownWallet 1) endpoints
  lenderH <- activateContractWallet (knownWallet 2) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { askBeacon' = (beaconPolicySymbol,"Ask")
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , collateral' = [testToken1]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"ask" borrowerH $
    AskParams
      { askBeaconsMinted = [("Ask",1)]
      , askBeaconRedeemer = MintAskToken' borrowerPubKey
      , askBeaconPolicy = beaconPolicy
      , askAddress = addr
      , askInfo = 
          [ ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconPolicySymbol "Ask" 1)
          ]
      , askAsInline = True
      }
  
  void $ waitUntilSlot 2

  let lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
      offerDatum = OfferDatum'
        { offerBeacon' = (beaconPolicySymbol,"Offer")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanBacking' = 100_000_000
        , loanInterest' = unsafeRatio 1 10
        , collateralRates' = [(testToken1,unsafeRatio 1 2_000_000)]
        }
  
  callEndpoint @"offer" lenderH $
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

  void $ waitUntilSlot 4

  let exp = slotToBeginPOSIXTime def 14
      activeDatum = ActiveDatum'
        { activeBeacon' = (beaconPolicySymbol,"Active")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanBacking' = 100_000_000
        , loanInterest' = unsafeRatio 1 10
        , collateralRates' = [(testToken1,unsafeRatio 1 2_000_000)]
        , loanExpiration' = exp
        , loanOutstanding' = fromInteger 100_000_000 * (fromInteger 1 + unsafeRatio 1 10)
        }
    
  callEndpoint @"accept" borrowerH $
    AcceptParams
      { acceptBeaconsMinted = [("Offer",-1),("Ask",-1),("Active",1),(pubKeyAsToken borrowerPubKey,1)]
      , acceptBeaconRedeemer = MintActiveToken' borrowerPubKey lenderPubKey
      , acceptBeaconPolicy = beaconPolicy
      , acceptLoanVal = loanValidator
      , acceptLoanAddress = addr
      , acceptSpecificUTxOs =
          [ ( offerDatum
            , lovelaceValueOf 103_000_000 <> singleton beaconPolicySymbol "Offer" 1
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
            )
          , ( askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconPolicySymbol "Ask" 1
            )
          ]
      , acceptChangeAddress = addr
      , acceptChangeOutput =
          [ (Just activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , acceptDatumAsInline = True
      , acceptWithTTL = True
      }

  void $ waitUntilSlot 6

  let amountPaid = fromInteger 110_000_000
      newActiveDatum = activeDatum{loanOutstanding'= loanOutstanding' activeDatum - amountPaid}

  callEndpoint @"repay-loan" borrowerH $
    RepayLoanParams
      { repayLoanBeaconsBurned = []
      , repayLoanBeaconRedeemer = BurnBeaconToken'
      , repayLoanBeaconPolicy = beaconPolicy
      , repayLoanVal = loanValidator
      , repayLoanAddress = addr
      , repayLoanSpecificUTxOs =
          [ ( activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , repayLoanChangeAddress = addr
      , repayLoanChangeOutputs =
          [ (Just newActiveDatum
            , lovelaceValueOf 113_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
            )
          ]
      , repayLoanDatumAsInline = True
      , repayLoanWithTTE = True
      , repayLoanOtherMint = []
      , repayLoanOtherMintPolicy = alwaysSucceedPolicy
      }

fullPaymentOutputMissingActiveBeacon :: EmulatorTrace ()
fullPaymentOutputMissingActiveBeacon = do
  borrowerH <- activateContractWallet (knownWallet 1) endpoints
  lenderH <- activateContractWallet (knownWallet 2) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { askBeacon' = (beaconPolicySymbol,"Ask")
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , collateral' = [testToken1]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"ask" borrowerH $
    AskParams
      { askBeaconsMinted = [("Ask",1)]
      , askBeaconRedeemer = MintAskToken' borrowerPubKey
      , askBeaconPolicy = beaconPolicy
      , askAddress = addr
      , askInfo = 
          [ ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconPolicySymbol "Ask" 1)
          ]
      , askAsInline = True
      }
  
  void $ waitUntilSlot 2

  let lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
      offerDatum = OfferDatum'
        { offerBeacon' = (beaconPolicySymbol,"Offer")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanBacking' = 100_000_000
        , loanInterest' = unsafeRatio 1 10
        , collateralRates' = [(testToken1,unsafeRatio 1 2_000_000)]
        }
  
  callEndpoint @"offer" lenderH $
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

  void $ waitUntilSlot 4

  let exp = slotToBeginPOSIXTime def 14
      activeDatum = ActiveDatum'
        { activeBeacon' = (beaconPolicySymbol,"Active")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanBacking' = 100_000_000
        , loanInterest' = unsafeRatio 1 10
        , collateralRates' = [(testToken1,unsafeRatio 1 2_000_000)]
        , loanExpiration' = exp
        , loanOutstanding' = fromInteger 100_000_000 * (fromInteger 1 + unsafeRatio 1 10)
        }
    
  callEndpoint @"accept" borrowerH $
    AcceptParams
      { acceptBeaconsMinted = [("Offer",-1),("Ask",-1),("Active",1),(pubKeyAsToken borrowerPubKey,1)]
      , acceptBeaconRedeemer = MintActiveToken' borrowerPubKey lenderPubKey
      , acceptBeaconPolicy = beaconPolicy
      , acceptLoanVal = loanValidator
      , acceptLoanAddress = addr
      , acceptSpecificUTxOs =
          [ ( offerDatum
            , lovelaceValueOf 103_000_000 <> singleton beaconPolicySymbol "Offer" 1
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
            )
          , ( askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconPolicySymbol "Ask" 1
            )
          ]
      , acceptChangeAddress = addr
      , acceptChangeOutput =
          [ (Just activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , acceptDatumAsInline = True
      , acceptWithTTL = True
      }

  void $ waitUntilSlot 6

  let amountPaid = fromInteger 110_000_000
      newActiveDatum = activeDatum{loanOutstanding'= loanOutstanding' activeDatum - amountPaid}

  callEndpoint @"repay-loan" borrowerH $
    RepayLoanParams
      { repayLoanBeaconsBurned = [(pubKeyAsToken borrowerPubKey,-1)]
      , repayLoanBeaconRedeemer = BurnBeaconToken'
      , repayLoanBeaconPolicy = beaconPolicy
      , repayLoanVal = loanValidator
      , repayLoanAddress = addr
      , repayLoanSpecificUTxOs =
          [ ( activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , repayLoanChangeAddress = addr
      , repayLoanChangeOutputs =
          [ (Just newActiveDatum
            , lovelaceValueOf 113_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
            )
          ]
      , repayLoanDatumAsInline = True
      , repayLoanWithTTE = True
      , repayLoanOtherMint = []
      , repayLoanOtherMintPolicy = alwaysSucceedPolicy
      }

fullPaymentOutputMissingLenderId :: EmulatorTrace ()
fullPaymentOutputMissingLenderId = do
  borrowerH <- activateContractWallet (knownWallet 1) endpoints
  lenderH <- activateContractWallet (knownWallet 2) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { askBeacon' = (beaconPolicySymbol,"Ask")
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , collateral' = [testToken1]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"ask" borrowerH $
    AskParams
      { askBeaconsMinted = [("Ask",1)]
      , askBeaconRedeemer = MintAskToken' borrowerPubKey
      , askBeaconPolicy = beaconPolicy
      , askAddress = addr
      , askInfo = 
          [ ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconPolicySymbol "Ask" 1)
          ]
      , askAsInline = True
      }
  
  void $ waitUntilSlot 2

  let lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
      offerDatum = OfferDatum'
        { offerBeacon' = (beaconPolicySymbol,"Offer")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanBacking' = 100_000_000
        , loanInterest' = unsafeRatio 1 10
        , collateralRates' = [(testToken1,unsafeRatio 1 2_000_000)]
        }
  
  callEndpoint @"offer" lenderH $
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

  void $ waitUntilSlot 4

  let exp = slotToBeginPOSIXTime def 14
      activeDatum = ActiveDatum'
        { activeBeacon' = (beaconPolicySymbol,"Active")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanBacking' = 100_000_000
        , loanInterest' = unsafeRatio 1 10
        , collateralRates' = [(testToken1,unsafeRatio 1 2_000_000)]
        , loanExpiration' = exp
        , loanOutstanding' = fromInteger 100_000_000 * (fromInteger 1 + unsafeRatio 1 10)
        }
    
  callEndpoint @"accept" borrowerH $
    AcceptParams
      { acceptBeaconsMinted = [("Offer",-1),("Ask",-1),("Active",1),(pubKeyAsToken borrowerPubKey,1)]
      , acceptBeaconRedeemer = MintActiveToken' borrowerPubKey lenderPubKey
      , acceptBeaconPolicy = beaconPolicy
      , acceptLoanVal = loanValidator
      , acceptLoanAddress = addr
      , acceptSpecificUTxOs =
          [ ( offerDatum
            , lovelaceValueOf 103_000_000 <> singleton beaconPolicySymbol "Offer" 1
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
            )
          , ( askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconPolicySymbol "Ask" 1
            )
          ]
      , acceptChangeAddress = addr
      , acceptChangeOutput =
          [ (Just activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , acceptDatumAsInline = True
      , acceptWithTTL = True
      }

  void $ waitUntilSlot 6

  let amountPaid = fromInteger 110_000_000
      newActiveDatum = activeDatum{loanOutstanding'= loanOutstanding' activeDatum - amountPaid}

  callEndpoint @"repay-loan" borrowerH $
    RepayLoanParams
      { repayLoanBeaconsBurned = [(pubKeyAsToken borrowerPubKey,-1)]
      , repayLoanBeaconRedeemer = BurnBeaconToken'
      , repayLoanBeaconPolicy = beaconPolicy
      , repayLoanVal = loanValidator
      , repayLoanAddress = addr
      , repayLoanSpecificUTxOs =
          [ ( activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , repayLoanChangeAddress = addr
      , repayLoanChangeOutputs =
          [ (Just newActiveDatum
            , lovelaceValueOf 113_000_000
           <> singleton beaconPolicySymbol "Active" 1
            )
          ]
      , repayLoanDatumAsInline = True
      , repayLoanWithTTE = True
      , repayLoanOtherMint = []
      , repayLoanOtherMintPolicy = alwaysSucceedPolicy
      }

fullyRepayOverCollateralizedLoan :: EmulatorTrace ()
fullyRepayOverCollateralizedLoan = do
  borrowerH <- activateContractWallet (knownWallet 1) endpoints
  lenderH <- activateContractWallet (knownWallet 2) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { askBeacon' = (beaconPolicySymbol,"Ask")
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , collateral' = [testToken1]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"ask" borrowerH $
    AskParams
      { askBeaconsMinted = [("Ask",1)]
      , askBeaconRedeemer = MintAskToken' borrowerPubKey
      , askBeaconPolicy = beaconPolicy
      , askAddress = addr
      , askInfo = 
          [ ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconPolicySymbol "Ask" 1)
          ]
      , askAsInline = True
      }
  
  void $ waitUntilSlot 2

  let lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
      offerDatum = OfferDatum'
        { offerBeacon' = (beaconPolicySymbol,"Offer")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanBacking' = 150_000_000
        , loanInterest' = unsafeRatio 1 10
        , collateralRates' = [(testToken1,unsafeRatio 1 2_000_000)]
        }
  
  callEndpoint @"offer" lenderH $
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

  void $ waitUntilSlot 4

  let exp = slotToBeginPOSIXTime def 14
      activeDatum = ActiveDatum'
        { activeBeacon' = (beaconPolicySymbol,"Active")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanBacking' = 150_000_000
        , loanInterest' = unsafeRatio 1 10
        , collateralRates' = [(testToken1,unsafeRatio 1 2_000_000)]
        , loanExpiration' = exp
        , loanOutstanding' = fromInteger 100_000_000 * (fromInteger 1 + unsafeRatio 1 10)
        }
    
  callEndpoint @"accept" borrowerH $
    AcceptParams
      { acceptBeaconsMinted = [("Offer",-1),("Ask",-1),("Active",1),(pubKeyAsToken borrowerPubKey,1)]
      , acceptBeaconRedeemer = MintActiveToken' borrowerPubKey lenderPubKey
      , acceptBeaconPolicy = beaconPolicy
      , acceptLoanVal = loanValidator
      , acceptLoanAddress = addr
      , acceptSpecificUTxOs =
          [ ( offerDatum
            , lovelaceValueOf 103_000_000 <> singleton beaconPolicySymbol "Offer" 1
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
            )
          , ( askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconPolicySymbol "Ask" 1
            )
          ]
      , acceptChangeAddress = addr
      , acceptChangeOutput =
          [ (Just activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 75
            )
          ]
      , acceptDatumAsInline = True
      , acceptWithTTL = True
      }

  void $ waitUntilSlot 6

  let amountPaid = fromInteger 110_000_000
      newActiveDatum = activeDatum{loanOutstanding'= loanOutstanding' activeDatum - amountPaid}

  callEndpoint @"repay-loan" borrowerH $
    RepayLoanParams
      { repayLoanBeaconsBurned = [(pubKeyAsToken borrowerPubKey,-1)]
      , repayLoanBeaconRedeemer = BurnBeaconToken'
      , repayLoanBeaconPolicy = beaconPolicy
      , repayLoanVal = loanValidator
      , repayLoanAddress = addr
      , repayLoanSpecificUTxOs =
          [ ( activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 75
            )
          ]
      , repayLoanChangeAddress = addr
      , repayLoanChangeOutputs =
          [ (Just newActiveDatum
            , lovelaceValueOf 113_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
            )
          ]
      , repayLoanDatumAsInline = True
      , repayLoanWithTTE = True
      , repayLoanOtherMint = []
      , repayLoanOtherMintPolicy = alwaysSucceedPolicy
      }

outputDatumNotInline :: EmulatorTrace ()
outputDatumNotInline = do
  borrowerH <- activateContractWallet (knownWallet 1) endpoints
  lenderH <- activateContractWallet (knownWallet 2) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { askBeacon' = (beaconPolicySymbol,"Ask")
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , collateral' = [testToken1]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"ask" borrowerH $
    AskParams
      { askBeaconsMinted = [("Ask",1)]
      , askBeaconRedeemer = MintAskToken' borrowerPubKey
      , askBeaconPolicy = beaconPolicy
      , askAddress = addr
      , askInfo = 
          [ ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconPolicySymbol "Ask" 1)
          ]
      , askAsInline = True
      }
  
  void $ waitUntilSlot 2

  let lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
      offerDatum = OfferDatum'
        { offerBeacon' = (beaconPolicySymbol,"Offer")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanBacking' = 100_000_000
        , loanInterest' = unsafeRatio 1 10
        , collateralRates' = [(testToken1,unsafeRatio 1 2_000_000)]
        }
  
  callEndpoint @"offer" lenderH $
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

  void $ waitUntilSlot 4

  let exp = slotToBeginPOSIXTime def 14
      activeDatum = ActiveDatum'
        { activeBeacon' = (beaconPolicySymbol,"Active")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanBacking' = 100_000_000
        , loanInterest' = unsafeRatio 1 10
        , collateralRates' = [(testToken1,unsafeRatio 1 2_000_000)]
        , loanExpiration' = exp
        , loanOutstanding' = fromInteger 100_000_000 * (fromInteger 1 + unsafeRatio 1 10)
        }
    
  callEndpoint @"accept" borrowerH $
    AcceptParams
      { acceptBeaconsMinted = [("Offer",-1),("Ask",-1),("Active",1),(pubKeyAsToken borrowerPubKey,1)]
      , acceptBeaconRedeemer = MintActiveToken' borrowerPubKey lenderPubKey
      , acceptBeaconPolicy = beaconPolicy
      , acceptLoanVal = loanValidator
      , acceptLoanAddress = addr
      , acceptSpecificUTxOs =
          [ ( offerDatum
            , lovelaceValueOf 103_000_000 <> singleton beaconPolicySymbol "Offer" 1
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
            )
          , ( askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconPolicySymbol "Ask" 1
            )
          ]
      , acceptChangeAddress = addr
      , acceptChangeOutput =
          [ (Just activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , acceptDatumAsInline = True
      , acceptWithTTL = True
      }

  void $ waitUntilSlot 6

  let amountPaid = fromInteger 22_000_000
      newActiveDatum = activeDatum{loanOutstanding'= loanOutstanding' activeDatum - amountPaid}

  callEndpoint @"repay-loan" borrowerH $
    RepayLoanParams
      { repayLoanBeaconsBurned = []
      , repayLoanBeaconRedeemer = BurnBeaconToken'
      , repayLoanBeaconPolicy = beaconPolicy
      , repayLoanVal = loanValidator
      , repayLoanAddress = addr
      , repayLoanSpecificUTxOs =
          [ ( activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , repayLoanChangeAddress = addr
      , repayLoanChangeOutputs =
          [ (Just newActiveDatum
            , lovelaceValueOf 25_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 40
            )
          ]
      , repayLoanDatumAsInline = False
      , repayLoanWithTTE = True
      , repayLoanOtherMint = []
      , repayLoanOtherMintPolicy = alwaysSucceedPolicy
      }

repayWithoutTTE :: EmulatorTrace ()
repayWithoutTTE = do
  borrowerH <- activateContractWallet (knownWallet 1) endpoints
  lenderH <- activateContractWallet (knownWallet 2) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { askBeacon' = (beaconPolicySymbol,"Ask")
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , collateral' = [testToken1]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"ask" borrowerH $
    AskParams
      { askBeaconsMinted = [("Ask",1)]
      , askBeaconRedeemer = MintAskToken' borrowerPubKey
      , askBeaconPolicy = beaconPolicy
      , askAddress = addr
      , askInfo = 
          [ ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconPolicySymbol "Ask" 1)
          ]
      , askAsInline = True
      }
  
  void $ waitUntilSlot 2

  let lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
      offerDatum = OfferDatum'
        { offerBeacon' = (beaconPolicySymbol,"Offer")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanBacking' = 100_000_000
        , loanInterest' = unsafeRatio 1 10
        , collateralRates' = [(testToken1,unsafeRatio 1 2_000_000)]
        }
  
  callEndpoint @"offer" lenderH $
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

  void $ waitUntilSlot 4

  let exp = slotToBeginPOSIXTime def 14
      activeDatum = ActiveDatum'
        { activeBeacon' = (beaconPolicySymbol,"Active")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanBacking' = 100_000_000
        , loanInterest' = unsafeRatio 1 10
        , collateralRates' = [(testToken1,unsafeRatio 1 2_000_000)]
        , loanExpiration' = exp
        , loanOutstanding' = fromInteger 100_000_000 * (fromInteger 1 + unsafeRatio 1 10)
        }
    
  callEndpoint @"accept" borrowerH $
    AcceptParams
      { acceptBeaconsMinted = [("Offer",-1),("Ask",-1),("Active",1),(pubKeyAsToken borrowerPubKey,1)]
      , acceptBeaconRedeemer = MintActiveToken' borrowerPubKey lenderPubKey
      , acceptBeaconPolicy = beaconPolicy
      , acceptLoanVal = loanValidator
      , acceptLoanAddress = addr
      , acceptSpecificUTxOs =
          [ ( offerDatum
            , lovelaceValueOf 103_000_000 <> singleton beaconPolicySymbol "Offer" 1
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
            )
          , ( askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconPolicySymbol "Ask" 1
            )
          ]
      , acceptChangeAddress = addr
      , acceptChangeOutput =
          [ (Just activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , acceptDatumAsInline = True
      , acceptWithTTL = True
      }

  void $ waitUntilSlot 6

  let amountPaid = fromInteger 22_000_000
      newActiveDatum = activeDatum{loanOutstanding'= loanOutstanding' activeDatum - amountPaid}

  callEndpoint @"repay-loan" borrowerH $
    RepayLoanParams
      { repayLoanBeaconsBurned = []
      , repayLoanBeaconRedeemer = BurnBeaconToken'
      , repayLoanBeaconPolicy = beaconPolicy
      , repayLoanVal = loanValidator
      , repayLoanAddress = addr
      , repayLoanSpecificUTxOs =
          [ ( activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , repayLoanChangeAddress = addr
      , repayLoanChangeOutputs =
          [ (Just newActiveDatum
            , lovelaceValueOf 25_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 40
            )
          ]
      , repayLoanDatumAsInline = True
      , repayLoanWithTTE = False
      , repayLoanOtherMint = []
      , repayLoanOtherMintPolicy = alwaysSucceedPolicy
      }

otherBeaconsInInputs :: EmulatorTrace ()
otherBeaconsInInputs = do
  borrowerH <- activateContractWallet (knownWallet 1) endpoints
  lenderH <- activateContractWallet (knownWallet 2) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { askBeacon' = (beaconPolicySymbol,"Ask")
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , collateral' = [testToken1]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"ask" borrowerH $
    AskParams
      { askBeaconsMinted = [("Ask",1)]
      , askBeaconRedeemer = MintAskToken' borrowerPubKey
      , askBeaconPolicy = beaconPolicy
      , askAddress = addr
      , askInfo = 
          [ ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconPolicySymbol "Ask" 1)
          ]
      , askAsInline = True
      }
  
  void $ waitUntilSlot 2

  let lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
      offerDatum = OfferDatum'
        { offerBeacon' = (beaconPolicySymbol,"Offer")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanBacking' = 100_000_000
        , loanInterest' = unsafeRatio 1 10
        , collateralRates' = [(testToken1,unsafeRatio 1 2_000_000)]
        }
  
  callEndpoint @"offer" lenderH $
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

  void $ waitUntilSlot 4

  let exp = slotToBeginPOSIXTime def 14
      activeDatum = ActiveDatum'
        { activeBeacon' = (beaconPolicySymbol,"Active")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanBacking' = 100_000_000
        , loanInterest' = unsafeRatio 1 10
        , collateralRates' = [(testToken1,unsafeRatio 1 2_000_000)]
        , loanExpiration' = exp
        , loanOutstanding' = fromInteger 100_000_000 * (fromInteger 1 + unsafeRatio 1 10)
        }
    
  callEndpoint @"accept" borrowerH $
    AcceptParams
      { acceptBeaconsMinted = [("Offer",-1),("Ask",-1),("Active",1),(pubKeyAsToken borrowerPubKey,1)]
      , acceptBeaconRedeemer = MintActiveToken' borrowerPubKey lenderPubKey
      , acceptBeaconPolicy = beaconPolicy
      , acceptLoanVal = loanValidator
      , acceptLoanAddress = addr
      , acceptSpecificUTxOs =
          [ ( offerDatum
            , lovelaceValueOf 103_000_000 <> singleton beaconPolicySymbol "Offer" 1
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
            )
          , ( askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconPolicySymbol "Ask" 1
            )
          ]
      , acceptChangeAddress = addr
      , acceptChangeOutput =
          [ (Just activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , acceptDatumAsInline = True
      , acceptWithTTL = True
      }

  void $ waitUntilSlot 6

  let amountPaid = fromInteger 22_000_000
      newActiveDatum = activeDatum{loanOutstanding'= loanOutstanding' activeDatum - amountPaid}

  callEndpoint @"repay-loan" lenderH $
    RepayLoanParams
      { repayLoanBeaconsBurned = [("Ask",-1)]
      , repayLoanBeaconRedeemer = BurnBeaconToken'
      , repayLoanBeaconPolicy = beaconPolicy
      , repayLoanVal = loanValidator
      , repayLoanAddress = addr
      , repayLoanSpecificUTxOs =
          [ ( activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , repayLoanChangeAddress = addr
      , repayLoanChangeOutputs =
          [ (Just newActiveDatum
            , lovelaceValueOf 25_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 40
            )
          ]
      , repayLoanDatumAsInline = True
      , repayLoanWithTTE = True
      , repayLoanOtherMint = []
      , repayLoanOtherMintPolicy = alwaysSucceedPolicy
      }

mintOtherToken :: EmulatorTrace ()
mintOtherToken = do
  borrowerH <- activateContractWallet (knownWallet 1) endpoints
  lenderH <- activateContractWallet (knownWallet 2) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { askBeacon' = (beaconPolicySymbol,"Ask")
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , collateral' = [testToken1]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"ask" borrowerH $
    AskParams
      { askBeaconsMinted = [("Ask",1)]
      , askBeaconRedeemer = MintAskToken' borrowerPubKey
      , askBeaconPolicy = beaconPolicy
      , askAddress = addr
      , askInfo = 
          [ ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconPolicySymbol "Ask" 1)
          ]
      , askAsInline = True
      }
  
  void $ waitUntilSlot 2

  let lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
      offerDatum = OfferDatum'
        { offerBeacon' = (beaconPolicySymbol,"Offer")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanBacking' = 100_000_000
        , loanInterest' = unsafeRatio 1 10
        , collateralRates' = [(testToken1,unsafeRatio 1 2_000_000)]
        }
  
  callEndpoint @"offer" lenderH $
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

  void $ waitUntilSlot 4

  let exp = slotToBeginPOSIXTime def 14
      activeDatum = ActiveDatum'
        { activeBeacon' = (beaconPolicySymbol,"Active")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanBacking' = 100_000_000
        , loanInterest' = unsafeRatio 1 10
        , collateralRates' = [(testToken1,unsafeRatio 1 2_000_000)]
        , loanExpiration' = exp
        , loanOutstanding' = fromInteger 100_000_000 * (fromInteger 1 + unsafeRatio 1 10)
        }
    
  callEndpoint @"accept" borrowerH $
    AcceptParams
      { acceptBeaconsMinted = [("Offer",-1),("Ask",-1),("Active",1),(pubKeyAsToken borrowerPubKey,1)]
      , acceptBeaconRedeemer = MintActiveToken' borrowerPubKey lenderPubKey
      , acceptBeaconPolicy = beaconPolicy
      , acceptLoanVal = loanValidator
      , acceptLoanAddress = addr
      , acceptSpecificUTxOs =
          [ ( offerDatum
            , lovelaceValueOf 103_000_000 <> singleton beaconPolicySymbol "Offer" 1
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
            )
          , ( askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconPolicySymbol "Ask" 1
            )
          ]
      , acceptChangeAddress = addr
      , acceptChangeOutput =
          [ (Just activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , acceptDatumAsInline = True
      , acceptWithTTL = True
      }

  void $ waitUntilSlot 6

  let amountPaid = fromInteger 110_000_000
      newActiveDatum = activeDatum{loanOutstanding'= loanOutstanding' activeDatum - amountPaid}

  callEndpoint @"repay-loan" borrowerH $
    RepayLoanParams
      { repayLoanBeaconsBurned = [(pubKeyAsToken borrowerPubKey,-1)]
      , repayLoanBeaconRedeemer = BurnBeaconToken'
      , repayLoanBeaconPolicy = beaconPolicy
      , repayLoanVal = loanValidator
      , repayLoanAddress = addr
      , repayLoanSpecificUTxOs =
          [ ( activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , repayLoanChangeAddress = addr
      , repayLoanChangeOutputs =
          [ (Just newActiveDatum
            , lovelaceValueOf 113_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
            )
          ]
      , repayLoanDatumAsInline = True
      , repayLoanWithTTE = True
      , repayLoanOtherMint = [("Other",1)]
      , repayLoanOtherMintPolicy = alwaysSucceedPolicy
      }

claimInvalidActiveUTxOAsAddressOwner :: EmulatorTrace ()
claimInvalidActiveUTxOAsAddressOwner = do
  borrowerH <- activateContractWallet (knownWallet 1) endpoints
  lenderH <- activateContractWallet (knownWallet 2) endpoints

  let exp = slotToBeginPOSIXTime def 14
      lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
      borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
      activeDatum = ActiveDatum'
        { activeBeacon' = (beaconPolicySymbol,"Active")
        , lenderId' = (beaconPolicySymbol,"pubKeyAsToken lenderPubKey")
        , borrowerId' = (beaconPolicySymbol,"pubKeyAsToken borrowerPubKey")
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanBacking' = 100_000_000
        , loanInterest' = unsafeRatio 1 10
        , collateralRates' = [(testToken1,unsafeRatio 1 2_000_000)]
        , loanExpiration' = exp
        , loanOutstanding' = fromInteger 100_000_000 * (fromInteger 1 + unsafeRatio 1 10)
        }
    
  callEndpoint @"accept" borrowerH $
    AcceptParams
      { acceptBeaconsMinted = []
      , acceptBeaconRedeemer = MintActiveToken' borrowerPubKey lenderPubKey
      , acceptBeaconPolicy = beaconPolicy
      , acceptLoanVal = loanValidator
      , acceptLoanAddress = addr
      , acceptSpecificUTxOs =
          [ 
          ]
      , acceptChangeAddress = addr
      , acceptChangeOutput =
          [ (Just activeDatum
            , lovelaceValueOf 3_000_000
           <> (uncurry singleton testToken1) 50
            )
          ]
      , acceptDatumAsInline = True
      , acceptWithTTL = True
      }

  void $ waitUntilSlot 6

  let amountPaid = fromInteger 110_000_000
      newActiveDatum = activeDatum{loanOutstanding'= loanOutstanding' activeDatum - amountPaid}

  callEndpoint @"repay-loan" borrowerH $
    RepayLoanParams
      { repayLoanBeaconsBurned = []
      , repayLoanBeaconRedeemer = BurnBeaconToken'
      , repayLoanBeaconPolicy = beaconPolicy
      , repayLoanVal = loanValidator
      , repayLoanAddress = addr
      , repayLoanSpecificUTxOs =
          [ ( activeDatum
            , lovelaceValueOf 3_000_000
           <> (uncurry singleton testToken1) 50
            )
          ]
      , repayLoanChangeAddress = addr
      , repayLoanChangeOutputs =
          [
          ]
      , repayLoanDatumAsInline = True
      , repayLoanWithTTE = True
      , repayLoanOtherMint = []
      , repayLoanOtherMintPolicy = alwaysSucceedPolicy
      }

claimInvalidActiveUTxOAsNonAddressOwner :: EmulatorTrace ()
claimInvalidActiveUTxOAsNonAddressOwner = do
  borrowerH <- activateContractWallet (knownWallet 1) endpoints
  lenderH <- activateContractWallet (knownWallet 2) endpoints

  let exp = slotToBeginPOSIXTime def 14
      lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
      borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
      activeDatum = ActiveDatum'
        { activeBeacon' = (beaconPolicySymbol,"Active")
        , lenderId' = (beaconPolicySymbol,"pubKeyAsToken lenderPubKey")
        , borrowerId' = (beaconPolicySymbol,"pubKeyAsToken borrowerPubKey")
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanBacking' = 100_000_000
        , loanInterest' = unsafeRatio 1 10
        , collateralRates' = [(testToken1,unsafeRatio 1 2_000_000)]
        , loanExpiration' = exp
        , loanOutstanding' = fromInteger 100_000_000 * (fromInteger 1 + unsafeRatio 1 10)
        }
    
  callEndpoint @"accept" borrowerH $
    AcceptParams
      { acceptBeaconsMinted = []
      , acceptBeaconRedeemer = MintActiveToken' borrowerPubKey lenderPubKey
      , acceptBeaconPolicy = beaconPolicy
      , acceptLoanVal = loanValidator
      , acceptLoanAddress = addr
      , acceptSpecificUTxOs =
          [ 
          ]
      , acceptChangeAddress = addr
      , acceptChangeOutput =
          [ (Just activeDatum
            , lovelaceValueOf 3_000_000
           <> (uncurry singleton testToken1) 50
            )
          ]
      , acceptDatumAsInline = True
      , acceptWithTTL = True
      }

  void $ waitUntilSlot 6

  let amountPaid = fromInteger 110_000_000
      newActiveDatum = activeDatum{loanOutstanding'= loanOutstanding' activeDatum - amountPaid}

  callEndpoint @"repay-loan" lenderH $
    RepayLoanParams
      { repayLoanBeaconsBurned = []
      , repayLoanBeaconRedeemer = BurnBeaconToken'
      , repayLoanBeaconPolicy = beaconPolicy
      , repayLoanVal = loanValidator
      , repayLoanAddress = addr
      , repayLoanSpecificUTxOs =
          [ ( activeDatum
            , lovelaceValueOf 3_000_000
           <> (uncurry singleton testToken1) 50
            )
          ]
      , repayLoanChangeAddress = addr
      , repayLoanChangeOutputs =
          [
          ]
      , repayLoanDatumAsInline = True
      , repayLoanWithTTE = True
      , repayLoanOtherMint = []
      , repayLoanOtherMintPolicy = alwaysSucceedPolicy
      }

repayALoanWithMultipleCollateralAssets :: EmulatorTrace ()
repayALoanWithMultipleCollateralAssets = do
  borrowerH <- activateContractWallet (knownWallet 1) endpoints
  lenderH <- activateContractWallet (knownWallet 2) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { askBeacon' = (beaconPolicySymbol,"Ask")
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , collateral' = [testToken1,testToken2]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"ask" borrowerH $
    AskParams
      { askBeaconsMinted = [("Ask",1)]
      , askBeaconRedeemer = MintAskToken' borrowerPubKey
      , askBeaconPolicy = beaconPolicy
      , askAddress = addr
      , askInfo = 
          [ ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconPolicySymbol "Ask" 1)
          ]
      , askAsInline = True
      }
  
  void $ waitUntilSlot 2

  let lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
      offerDatum = OfferDatum'
        { offerBeacon' = (beaconPolicySymbol,"Offer")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanBacking' = 100_000_000
        , loanInterest' = unsafeRatio 1 10
        , collateralRates' = 
            [ (testToken1,unsafeRatio 1 2_000_000)
            , (testToken2, unsafeRatio 1 1_000_000)
            ]
        }
  
  callEndpoint @"offer" lenderH $
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

  void $ waitUntilSlot 4

  let exp = slotToBeginPOSIXTime def 14
      activeDatum = ActiveDatum'
        { activeBeacon' = (beaconPolicySymbol,"Active")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanBacking' = 100_000_000
        , loanInterest' = unsafeRatio 1 10
        , collateralRates' = 
            [ (testToken1,unsafeRatio 1 2_000_000)
            , (testToken2,unsafeRatio 1 1_000_000)
            ]
        , loanExpiration' = exp
        , loanOutstanding' = fromInteger 100_000_000 * (fromInteger 1 + unsafeRatio 1 10)
        }
    
  callEndpoint @"accept" borrowerH $
    AcceptParams
      { acceptBeaconsMinted = [("Offer",-1),("Ask",-1),("Active",1),(pubKeyAsToken borrowerPubKey,1)]
      , acceptBeaconRedeemer = MintActiveToken' borrowerPubKey lenderPubKey
      , acceptBeaconPolicy = beaconPolicy
      , acceptLoanVal = loanValidator
      , acceptLoanAddress = addr
      , acceptSpecificUTxOs =
          [ ( offerDatum
            , lovelaceValueOf 103_000_000 <> singleton beaconPolicySymbol "Offer" 1
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
            )
          , ( askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconPolicySymbol "Ask" 1
            )
          ]
      , acceptChangeAddress = addr
      , acceptChangeOutput =
          [ (Just activeDatum
            , lovelaceValueOf 3_200_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 40
           <> (uncurry singleton testToken2) 20
            )
          ]
      , acceptDatumAsInline = True
      , acceptWithTTL = True
      }

  void $ waitUntilSlot 6

  let amountPaid = fromInteger 110_000_000
      newActiveDatum = activeDatum{loanOutstanding'= loanOutstanding' activeDatum - amountPaid}

  callEndpoint @"repay-loan" borrowerH $
    RepayLoanParams
      { repayLoanBeaconsBurned = [(pubKeyAsToken borrowerPubKey,-1)]
      , repayLoanBeaconRedeemer = BurnBeaconToken'
      , repayLoanBeaconPolicy = beaconPolicy
      , repayLoanVal = loanValidator
      , repayLoanAddress = addr
      , repayLoanSpecificUTxOs =
          [ ( activeDatum
            , lovelaceValueOf 3_200_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 40
           <> (uncurry singleton testToken2) 20
            )
          ]
      , repayLoanChangeAddress = addr
      , repayLoanChangeOutputs =
          [ (Just newActiveDatum
            , lovelaceValueOf 113_200_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
            )
          ]
      , repayLoanDatumAsInline = True
      , repayLoanWithTTE = True
      , repayLoanOtherMint = []
      , repayLoanOtherMintPolicy = alwaysSucceedPolicy
      }

stealFundsAfterFullyRepaid :: EmulatorTrace ()
stealFundsAfterFullyRepaid = do
  borrowerH <- activateContractWallet (knownWallet 1) endpoints
  lenderH <- activateContractWallet (knownWallet 2) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { askBeacon' = (beaconPolicySymbol,"Ask")
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , collateral' = [testToken1]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"ask" borrowerH $
    AskParams
      { askBeaconsMinted = [("Ask",1)]
      , askBeaconRedeemer = MintAskToken' borrowerPubKey
      , askBeaconPolicy = beaconPolicy
      , askAddress = addr
      , askInfo = 
          [ ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconPolicySymbol "Ask" 1)
          ]
      , askAsInline = True
      }
  
  void $ waitUntilSlot 2

  let lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
      offerDatum = OfferDatum'
        { offerBeacon' = (beaconPolicySymbol,"Offer")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanBacking' = 100_000_000
        , loanInterest' = unsafeRatio 1 10
        , collateralRates' = [(testToken1,unsafeRatio 1 2_000_000)]
        }
  
  callEndpoint @"offer" lenderH $
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

  void $ waitUntilSlot 4

  let exp = slotToBeginPOSIXTime def 14
      activeDatum = ActiveDatum'
        { activeBeacon' = (beaconPolicySymbol,"Active")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanBacking' = 100_000_000
        , loanInterest' = unsafeRatio 1 10
        , collateralRates' = [(testToken1,unsafeRatio 1 2_000_000)]
        , loanExpiration' = exp
        , loanOutstanding' = fromInteger 100_000_000 * (fromInteger 1 + unsafeRatio 1 10)
        }
    
  callEndpoint @"accept" borrowerH $
    AcceptParams
      { acceptBeaconsMinted = [("Offer",-1),("Ask",-1),("Active",1),(pubKeyAsToken borrowerPubKey,1)]
      , acceptBeaconRedeemer = MintActiveToken' borrowerPubKey lenderPubKey
      , acceptBeaconPolicy = beaconPolicy
      , acceptLoanVal = loanValidator
      , acceptLoanAddress = addr
      , acceptSpecificUTxOs =
          [ ( offerDatum
            , lovelaceValueOf 103_000_000 <> singleton beaconPolicySymbol "Offer" 1
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
            )
          , ( askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconPolicySymbol "Ask" 1
            )
          ]
      , acceptChangeAddress = addr
      , acceptChangeOutput =
          [ (Just activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , acceptDatumAsInline = True
      , acceptWithTTL = True
      }

  void $ waitUntilSlot 6

  let amountPaid = fromInteger 110_000_000
      newActiveDatum = activeDatum{loanOutstanding'= loanOutstanding' activeDatum - amountPaid}

  callEndpoint @"repay-loan" borrowerH $
    RepayLoanParams
      { repayLoanBeaconsBurned = [(pubKeyAsToken borrowerPubKey,-1)]
      , repayLoanBeaconRedeemer = BurnBeaconToken'
      , repayLoanBeaconPolicy = beaconPolicy
      , repayLoanVal = loanValidator
      , repayLoanAddress = addr
      , repayLoanSpecificUTxOs =
          [ ( activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , repayLoanChangeAddress = addr
      , repayLoanChangeOutputs =
          [ (Just newActiveDatum
            , lovelaceValueOf 113_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
            )
          ]
      , repayLoanDatumAsInline = True
      , repayLoanWithTTE = True
      , repayLoanOtherMint = []
      , repayLoanOtherMintPolicy = alwaysSucceedPolicy
      }

  void $ waitUntilSlot 8

  let amountPaid' = fromInteger (-110_000_000)
      newActiveDatum' = activeDatum{loanOutstanding'= loanOutstanding' newActiveDatum - amountPaid'}

  callEndpoint @"repay-loan" borrowerH $
    RepayLoanParams
      { repayLoanBeaconsBurned = []
      , repayLoanBeaconRedeemer = BurnBeaconToken'
      , repayLoanBeaconPolicy = beaconPolicy
      , repayLoanVal = loanValidator
      , repayLoanAddress = addr
      , repayLoanSpecificUTxOs =
          [ ( newActiveDatum
            , lovelaceValueOf 113_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
            )
          ]
      , repayLoanChangeAddress = addr
      , repayLoanChangeOutputs =
          [ (Just newActiveDatum'
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , repayLoanDatumAsInline = True
      , repayLoanWithTTE = True
      , repayLoanOtherMint = []
      , repayLoanOtherMintPolicy = alwaysSucceedPolicy
      }

depositMoreAfterFullyRepaid :: EmulatorTrace ()
depositMoreAfterFullyRepaid = do
  borrowerH <- activateContractWallet (knownWallet 1) endpoints
  lenderH <- activateContractWallet (knownWallet 2) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { askBeacon' = (beaconPolicySymbol,"Ask")
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , collateral' = [testToken1]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"ask" borrowerH $
    AskParams
      { askBeaconsMinted = [("Ask",1)]
      , askBeaconRedeemer = MintAskToken' borrowerPubKey
      , askBeaconPolicy = beaconPolicy
      , askAddress = addr
      , askInfo = 
          [ ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconPolicySymbol "Ask" 1)
          ]
      , askAsInline = True
      }
  
  void $ waitUntilSlot 2

  let lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
      offerDatum = OfferDatum'
        { offerBeacon' = (beaconPolicySymbol,"Offer")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanBacking' = 100_000_000
        , loanInterest' = unsafeRatio 1 10
        , collateralRates' = [(testToken1,unsafeRatio 1 2_000_000)]
        }
  
  callEndpoint @"offer" lenderH $
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

  void $ waitUntilSlot 4

  let exp = slotToBeginPOSIXTime def 14
      activeDatum = ActiveDatum'
        { activeBeacon' = (beaconPolicySymbol,"Active")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanBacking' = 100_000_000
        , loanInterest' = unsafeRatio 1 10
        , collateralRates' = [(testToken1,unsafeRatio 1 2_000_000)]
        , loanExpiration' = exp
        , loanOutstanding' = fromInteger 100_000_000 * (fromInteger 1 + unsafeRatio 1 10)
        }
    
  callEndpoint @"accept" borrowerH $
    AcceptParams
      { acceptBeaconsMinted = [("Offer",-1),("Ask",-1),("Active",1),(pubKeyAsToken borrowerPubKey,1)]
      , acceptBeaconRedeemer = MintActiveToken' borrowerPubKey lenderPubKey
      , acceptBeaconPolicy = beaconPolicy
      , acceptLoanVal = loanValidator
      , acceptLoanAddress = addr
      , acceptSpecificUTxOs =
          [ ( offerDatum
            , lovelaceValueOf 103_000_000 <> singleton beaconPolicySymbol "Offer" 1
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
            )
          , ( askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconPolicySymbol "Ask" 1
            )
          ]
      , acceptChangeAddress = addr
      , acceptChangeOutput =
          [ (Just activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , acceptDatumAsInline = True
      , acceptWithTTL = True
      }

  void $ waitUntilSlot 6

  let amountPaid = fromInteger 110_000_000
      newActiveDatum = activeDatum{loanOutstanding'= loanOutstanding' activeDatum - amountPaid}

  callEndpoint @"repay-loan" borrowerH $
    RepayLoanParams
      { repayLoanBeaconsBurned = [(pubKeyAsToken borrowerPubKey,-1)]
      , repayLoanBeaconRedeemer = BurnBeaconToken'
      , repayLoanBeaconPolicy = beaconPolicy
      , repayLoanVal = loanValidator
      , repayLoanAddress = addr
      , repayLoanSpecificUTxOs =
          [ ( activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , repayLoanChangeAddress = addr
      , repayLoanChangeOutputs =
          [ (Just newActiveDatum
            , lovelaceValueOf 113_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
            )
          ]
      , repayLoanDatumAsInline = True
      , repayLoanWithTTE = True
      , repayLoanOtherMint = []
      , repayLoanOtherMintPolicy = alwaysSucceedPolicy
      }

  void $ waitUntilSlot 8

  let amountPaid' = fromInteger (110_000_000)
      newActiveDatum' = activeDatum{loanOutstanding'= loanOutstanding' newActiveDatum - amountPaid'}

  callEndpoint @"repay-loan" borrowerH $
    RepayLoanParams
      { repayLoanBeaconsBurned = []
      , repayLoanBeaconRedeemer = BurnBeaconToken'
      , repayLoanBeaconPolicy = beaconPolicy
      , repayLoanVal = loanValidator
      , repayLoanAddress = addr
      , repayLoanSpecificUTxOs =
          [ ( newActiveDatum
            , lovelaceValueOf 113_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
            )
          ]
      , repayLoanChangeAddress = addr
      , repayLoanChangeOutputs =
          [ (Just newActiveDatum'
            , lovelaceValueOf 223_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
            )
          ]
      , repayLoanDatumAsInline = True
      , repayLoanWithTTE = True
      , repayLoanOtherMint = []
      , repayLoanOtherMintPolicy = alwaysSucceedPolicy
      }

-------------------------------------------------
-- Test Function
-------------------------------------------------
tests :: TestTree
tests = do
  let opts = defaultCheckOptions & emulatorConfig .~ emConfig
  testGroup "Repay Loan"
    [ checkPredicateOptions opts "Fail if input datum is not an ActiveDatum"
        (Test.not assertNoFailedTransactions) inputDoesNotHaveActiveDatum
    , checkPredicateOptions opts "Fail if multiple utxos spent from address in tx"
        (Test.not assertNoFailedTransactions) txHasMultipleInputsFromAddress
    , checkPredicateOptions opts "Fail if loan is expired"
        (Test.not assertNoFailedTransactions) loanIsExpired
    , checkPredicateOptions opts "Fail if multiple outputs to address"
        (Test.not assertNoFailedTransactions) multipleOutputsToAddress
    , checkPredicateOptions opts "Fail if no outputs to address"
        (Test.not assertNoFailedTransactions) noOutputsToAddress
    , checkPredicateOptions opts "Fail if output changed another field in ActiveDatum"
        (Test.not assertNoFailedTransactions) outputHasChangedOtherFieldInActiveDatum
    , checkPredicateOptions opts "Fail if taking more collateral than loan repaid"
        (Test.not assertNoFailedTransactions) takeMoreColalteralThanRepaid
    , checkPredicateOptions opts "Fail if partial payment output is missing active beacon"
        (Test.not assertNoFailedTransactions) partialPaymetOutputMissingActiveBeacon
    , checkPredicateOptions opts "Fail if partial payment output is missing lender ID"
        (Test.not assertNoFailedTransactions) partialPaymetOutputMissingLenderID
    , checkPredicateOptions opts "Fail if partial payment output is missing borrower ID"
        (Test.not assertNoFailedTransactions) partialPaymetOutputMissingBorrowerID
    , checkPredicateOptions opts "Fail if staking credential did not approve"
        (Test.not assertNoFailedTransactions) stakingCredDidNotApprove
    , checkPredicateOptions opts "Fail if borrower ID not burned when loan fully paid off"
        (Test.not assertNoFailedTransactions) borrowerIdNotBurned
    , checkPredicateOptions opts "Fail if full payment output is missing active beacon"
        (Test.not assertNoFailedTransactions) fullPaymentOutputMissingActiveBeacon
    , checkPredicateOptions opts "Fail if full payment output is missing lender ID"
        (Test.not assertNoFailedTransactions) fullPaymentOutputMissingLenderId
    , checkPredicateOptions opts "Fail if output datum not inline"
        (Test.not assertNoFailedTransactions) outputDatumNotInline
    , checkPredicateOptions opts "Fail if TTE not specified"
        (Test.not assertNoFailedTransactions) repayWithoutTTE
    , checkPredicateOptions opts "Fail if other phase beacons in tx inputs"
        (Test.not assertNoFailedTransactions) otherBeaconsInInputs
    , checkPredicateOptions opts "Fail if other tokens minted/burned when paying off loan"
        (Test.not assertNoFailedTransactions) mintOtherToken
    , checkPredicateOptions opts "Fail if non-address-owner tries reclaiming invalid active utxo"
        (Test.not assertNoFailedTransactions) claimInvalidActiveUTxOAsNonAddressOwner
    , checkPredicateOptions opts "Cannot take back funds once loan is fully repaid"
        (Test.not assertNoFailedTransactions) stealFundsAfterFullyRepaid
    , checkPredicateOptions opts "Cannot deposit more after loan is fully repaid"
        (Test.not assertNoFailedTransactions) depositMoreAfterFullyRepaid
      
      -- Success Checks
    , checkPredicateOptions opts "Successfully make partial payment on fully collateralized loan"
        assertNoFailedTransactions makePartialPayment
    , checkPredicateOptions opts "Successfully repay loan in full"
        assertNoFailedTransactions fullyRepayLoan
    , checkPredicateOptions opts "Successfully reclaim all collateral when fully repaying over-collateralized loan"
        assertNoFailedTransactions fullyRepayOverCollateralizedLoan
    , checkPredicateOptions opts "Successfully make partial payment on under-collateralized loan"
        assertNoFailedTransactions makePartialPaymentOnUnderCollateralizedLoan
    , checkPredicateOptions opts "Successfully claim invalid active utxo as address owner"
        assertNoFailedTransactions claimInvalidActiveUTxOAsAddressOwner
    , checkPredicateOptions opts "Successfully repay a loan using multiple collateral assets"
        assertNoFailedTransactions repayALoanWithMultipleCollateralAssets
    ]

testTrace :: IO ()
testTrace = runEmulatorTraceIO' def emConfig depositMoreAfterFullyRepaid