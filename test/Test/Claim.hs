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

module Test.Claim
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
import Plutus.Script.Utils.V2.Generators (alwaysSucceedPolicy)
import Ledger.TimeSlot

import Test.Common
import CardanoLoans

-------------------------------------------------
-- Claim Scenarios
-------------------------------------------------
claimExpiredLoan :: EmulatorTrace ()
claimExpiredLoan = do
  borrowerH <- activateContractWallet (knownWallet 1) endpoints
  lenderH <- activateContractWallet (knownWallet 2) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { loanBeaconSym' = beaconPolicySymbol
        , borrowerId' = pubKeyAsToken borrowerPubKey
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
        { loanBeaconSym' = beaconPolicySymbol
        , lenderId' = pubKeyAsToken lenderPubKey
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanInterest' = unsafeRatio 1 10
        , collateralization' = [(testToken1,unsafeRatio 1 2_000_000)]
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
        { loanBeaconSym' = beaconPolicySymbol
        , lenderId' = pubKeyAsToken lenderPubKey
        , borrowerId' = pubKeyAsToken borrowerPubKey
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanInterest' = unsafeRatio 1 10
        , collateralization' = [(testToken1,unsafeRatio 1 2_000_000)]
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
           <> (uncurry singleton testToken1) 50
            )
          ]
      , repayLoanDatumAsInline = True
      , repayLoanWithTTE = True
      , repayLoanOtherMint = []
      , repayLoanOtherMintPolicy = alwaysSucceedPolicy
      }

  void $ waitUntilSlot 15

  callEndpoint @"claim-loan" lenderH $
    ClaimLoanParams
      { claimLoanBeaconsBurned = 
          [ ("Active",-1)
          , (pubKeyAsToken lenderPubKey,-1)
          , (pubKeyAsToken borrowerPubKey,-1)
          ]
      , claimLoanBeaconRedeemer = BurnBeaconToken'
      , claimLoanBeaconPolicy = beaconPolicy
      , claimLoanVal = loanValidator
      , claimLoanAddress = addr
      , claimLoanSpecificUTxOs =
          [ ( newActiveDatum
            , lovelaceValueOf 25_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , claimLoanWithTTL = True
      , claimLoanOtherMint = []
      , claimLoanOtherMintPolicy = alwaysSucceedPolicy
      }

otherBeaconsInInput :: EmulatorTrace ()
otherBeaconsInInput = do
  borrowerH <- activateContractWallet (knownWallet 1) endpoints
  lenderH <- activateContractWallet (knownWallet 2) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { loanBeaconSym' = beaconPolicySymbol
        , borrowerId' = pubKeyAsToken borrowerPubKey
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
        { loanBeaconSym' = beaconPolicySymbol
        , lenderId' = pubKeyAsToken lenderPubKey
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanInterest' = unsafeRatio 1 10
        , collateralization' = [(testToken1,unsafeRatio 1 2_000_000)]
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
        { loanBeaconSym' = beaconPolicySymbol
        , lenderId' = pubKeyAsToken lenderPubKey
        , borrowerId' = pubKeyAsToken borrowerPubKey
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanInterest' = unsafeRatio 1 10
        , collateralization' = [(testToken1,unsafeRatio 1 2_000_000)]
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
           <> (uncurry singleton testToken1) 50
            )
          ]
      , repayLoanDatumAsInline = True
      , repayLoanWithTTE = True
      , repayLoanOtherMint = []
      , repayLoanOtherMintPolicy = alwaysSucceedPolicy
      }

  void $ waitUntilSlot 15

  callEndpoint @"claim-loan" lenderH $
    ClaimLoanParams
      { claimLoanBeaconsBurned = 
          [ ("Active",-1)
          , ("Ask",-1)
          , (pubKeyAsToken lenderPubKey,-1)
          , (pubKeyAsToken borrowerPubKey,-1)
          ]
      , claimLoanBeaconRedeemer = BurnBeaconToken'
      , claimLoanBeaconPolicy = beaconPolicy
      , claimLoanVal = loanValidator
      , claimLoanAddress = addr
      , claimLoanSpecificUTxOs =
          [ ( newActiveDatum
            , lovelaceValueOf 25_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , claimLoanWithTTL = True
      , claimLoanOtherMint = []
      , claimLoanOtherMintPolicy = alwaysSucceedPolicy
      }

claimActiveLoan :: EmulatorTrace ()
claimActiveLoan = do
  borrowerH <- activateContractWallet (knownWallet 1) endpoints
  lenderH <- activateContractWallet (knownWallet 2) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { loanBeaconSym' = beaconPolicySymbol
        , borrowerId' = pubKeyAsToken borrowerPubKey
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
        { loanBeaconSym' = beaconPolicySymbol
        , lenderId' = pubKeyAsToken lenderPubKey
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanInterest' = unsafeRatio 1 10
        , collateralization' = [(testToken1,unsafeRatio 1 2_000_000)]
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
        { loanBeaconSym' = beaconPolicySymbol
        , lenderId' = pubKeyAsToken lenderPubKey
        , borrowerId' = pubKeyAsToken borrowerPubKey
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanInterest' = unsafeRatio 1 10
        , collateralization' = [(testToken1,unsafeRatio 1 2_000_000)]
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
           <> (uncurry singleton testToken1) 50
            )
          ]
      , repayLoanDatumAsInline = True
      , repayLoanWithTTE = True
      , repayLoanOtherMint = []
      , repayLoanOtherMintPolicy = alwaysSucceedPolicy
      }

  void $ waitUntilSlot 8

  callEndpoint @"claim-loan" lenderH $
    ClaimLoanParams
      { claimLoanBeaconsBurned = 
          [ ("Active",-1)
          , (pubKeyAsToken lenderPubKey,-1)
          , (pubKeyAsToken borrowerPubKey,-1)
          ]
      , claimLoanBeaconRedeemer = BurnBeaconToken'
      , claimLoanBeaconPolicy = beaconPolicy
      , claimLoanVal = loanValidator
      , claimLoanAddress = addr
      , claimLoanSpecificUTxOs =
          [ ( newActiveDatum
            , lovelaceValueOf 25_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , claimLoanWithTTL = True
      , claimLoanOtherMint = []
      , claimLoanOtherMintPolicy = alwaysSucceedPolicy
      }

activeBeaconNotBurned :: EmulatorTrace ()
activeBeaconNotBurned = do
  borrowerH <- activateContractWallet (knownWallet 1) endpoints
  lenderH <- activateContractWallet (knownWallet 2) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { loanBeaconSym' = beaconPolicySymbol
        , borrowerId' = pubKeyAsToken borrowerPubKey
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
        { loanBeaconSym' = beaconPolicySymbol
        , lenderId' = pubKeyAsToken lenderPubKey
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanInterest' = unsafeRatio 1 10
        , collateralization' = [(testToken1,unsafeRatio 1 2_000_000)]
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
        { loanBeaconSym' = beaconPolicySymbol
        , lenderId' = pubKeyAsToken lenderPubKey
        , borrowerId' = pubKeyAsToken borrowerPubKey
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanInterest' = unsafeRatio 1 10
        , collateralization' = [(testToken1,unsafeRatio 1 2_000_000)]
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
           <> (uncurry singleton testToken1) 50
            )
          ]
      , repayLoanDatumAsInline = True
      , repayLoanWithTTE = True
      , repayLoanOtherMint = []
      , repayLoanOtherMintPolicy = alwaysSucceedPolicy
      }

  void $ waitUntilSlot 15

  callEndpoint @"claim-loan" lenderH $
    ClaimLoanParams
      { claimLoanBeaconsBurned = 
          [ (pubKeyAsToken lenderPubKey,-1)
          , (pubKeyAsToken borrowerPubKey,-1)
          ]
      , claimLoanBeaconRedeemer = BurnBeaconToken'
      , claimLoanBeaconPolicy = beaconPolicy
      , claimLoanVal = loanValidator
      , claimLoanAddress = addr
      , claimLoanSpecificUTxOs =
          [ ( newActiveDatum
            , lovelaceValueOf 25_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , claimLoanWithTTL = True
      , claimLoanOtherMint = []
      , claimLoanOtherMintPolicy = alwaysSucceedPolicy
      }

lenderIdNotBurned :: EmulatorTrace ()
lenderIdNotBurned = do
  borrowerH <- activateContractWallet (knownWallet 1) endpoints
  lenderH <- activateContractWallet (knownWallet 2) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { loanBeaconSym' = beaconPolicySymbol
        , borrowerId' = pubKeyAsToken borrowerPubKey
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
        { loanBeaconSym' = beaconPolicySymbol
        , lenderId' = pubKeyAsToken lenderPubKey
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanInterest' = unsafeRatio 1 10
        , collateralization' = [(testToken1,unsafeRatio 1 2_000_000)]
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
        { loanBeaconSym' = beaconPolicySymbol
        , lenderId' = pubKeyAsToken lenderPubKey
        , borrowerId' = pubKeyAsToken borrowerPubKey
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanInterest' = unsafeRatio 1 10
        , collateralization' = [(testToken1,unsafeRatio 1 2_000_000)]
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
           <> (uncurry singleton testToken1) 50
            )
          ]
      , repayLoanDatumAsInline = True
      , repayLoanWithTTE = True
      , repayLoanOtherMint = []
      , repayLoanOtherMintPolicy = alwaysSucceedPolicy
      }

  void $ waitUntilSlot 15

  callEndpoint @"claim-loan" lenderH $
    ClaimLoanParams
      { claimLoanBeaconsBurned = 
          [ ("Active",-1)
          , (pubKeyAsToken borrowerPubKey,-1)
          ]
      , claimLoanBeaconRedeemer = BurnBeaconToken'
      , claimLoanBeaconPolicy = beaconPolicy
      , claimLoanVal = loanValidator
      , claimLoanAddress = addr
      , claimLoanSpecificUTxOs =
          [ ( newActiveDatum
            , lovelaceValueOf 25_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , claimLoanWithTTL = True
      , claimLoanOtherMint = []
      , claimLoanOtherMintPolicy = alwaysSucceedPolicy
      }

borrowerIdNotBurned :: EmulatorTrace ()
borrowerIdNotBurned = do
  borrowerH <- activateContractWallet (knownWallet 1) endpoints
  lenderH <- activateContractWallet (knownWallet 2) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { loanBeaconSym' = beaconPolicySymbol
        , borrowerId' = pubKeyAsToken borrowerPubKey
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
        { loanBeaconSym' = beaconPolicySymbol
        , lenderId' = pubKeyAsToken lenderPubKey
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanInterest' = unsafeRatio 1 10
        , collateralization' = [(testToken1,unsafeRatio 1 2_000_000)]
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
        { loanBeaconSym' = beaconPolicySymbol
        , lenderId' = pubKeyAsToken lenderPubKey
        , borrowerId' = pubKeyAsToken borrowerPubKey
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanInterest' = unsafeRatio 1 10
        , collateralization' = [(testToken1,unsafeRatio 1 2_000_000)]
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
           <> (uncurry singleton testToken1) 50
            )
          ]
      , repayLoanDatumAsInline = True
      , repayLoanWithTTE = True
      , repayLoanOtherMint = []
      , repayLoanOtherMintPolicy = alwaysSucceedPolicy
      }

  void $ waitUntilSlot 15

  callEndpoint @"claim-loan" lenderH $
    ClaimLoanParams
      { claimLoanBeaconsBurned = 
          [ ("Active",-1)
          , (pubKeyAsToken lenderPubKey,-1)
          ]
      , claimLoanBeaconRedeemer = BurnBeaconToken'
      , claimLoanBeaconPolicy = beaconPolicy
      , claimLoanVal = loanValidator
      , claimLoanAddress = addr
      , claimLoanSpecificUTxOs =
          [ ( newActiveDatum
            , lovelaceValueOf 25_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , claimLoanWithTTL = True
      , claimLoanOtherMint = []
      , claimLoanOtherMintPolicy = alwaysSucceedPolicy
      }

lenderDidNotSign :: EmulatorTrace ()
lenderDidNotSign = do
  borrowerH <- activateContractWallet (knownWallet 1) endpoints
  lenderH <- activateContractWallet (knownWallet 2) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { loanBeaconSym' = beaconPolicySymbol
        , borrowerId' = pubKeyAsToken borrowerPubKey
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
        { loanBeaconSym' = beaconPolicySymbol
        , lenderId' = pubKeyAsToken lenderPubKey
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanInterest' = unsafeRatio 1 10
        , collateralization' = [(testToken1,unsafeRatio 1 2_000_000)]
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
        { loanBeaconSym' = beaconPolicySymbol
        , lenderId' = pubKeyAsToken lenderPubKey
        , borrowerId' = pubKeyAsToken borrowerPubKey
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanInterest' = unsafeRatio 1 10
        , collateralization' = [(testToken1,unsafeRatio 1 2_000_000)]
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
           <> (uncurry singleton testToken1) 50
            )
          ]
      , repayLoanDatumAsInline = True
      , repayLoanWithTTE = True
      , repayLoanOtherMint = []
      , repayLoanOtherMintPolicy = alwaysSucceedPolicy
      }

  void $ waitUntilSlot 15

  callEndpoint @"claim-loan" borrowerH $
    ClaimLoanParams
      { claimLoanBeaconsBurned = 
          [ ("Active",-1)
          , (pubKeyAsToken lenderPubKey,-1)
          , (pubKeyAsToken borrowerPubKey,-1)
          ]
      , claimLoanBeaconRedeemer = BurnBeaconToken'
      , claimLoanBeaconPolicy = beaconPolicy
      , claimLoanVal = loanValidator
      , claimLoanAddress = addr
      , claimLoanSpecificUTxOs =
          [ ( newActiveDatum
            , lovelaceValueOf 25_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , claimLoanWithTTL = True
      , claimLoanOtherMint = []
      , claimLoanOtherMintPolicy = alwaysSucceedPolicy
      }

otherTokensMinted :: EmulatorTrace ()
otherTokensMinted = do
  borrowerH <- activateContractWallet (knownWallet 1) endpoints
  lenderH <- activateContractWallet (knownWallet 2) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { loanBeaconSym' = beaconPolicySymbol
        , borrowerId' = pubKeyAsToken borrowerPubKey
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
        { loanBeaconSym' = beaconPolicySymbol
        , lenderId' = pubKeyAsToken lenderPubKey
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanInterest' = unsafeRatio 1 10
        , collateralization' = [(testToken1,unsafeRatio 1 2_000_000)]
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
        { loanBeaconSym' = beaconPolicySymbol
        , lenderId' = pubKeyAsToken lenderPubKey
        , borrowerId' = pubKeyAsToken borrowerPubKey
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanInterest' = unsafeRatio 1 10
        , collateralization' = [(testToken1,unsafeRatio 1 2_000_000)]
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
           <> (uncurry singleton testToken1) 50
            )
          ]
      , repayLoanDatumAsInline = True
      , repayLoanWithTTE = True
      , repayLoanOtherMint = []
      , repayLoanOtherMintPolicy = alwaysSucceedPolicy
      }

  void $ waitUntilSlot 15

  callEndpoint @"claim-loan" lenderH $
    ClaimLoanParams
      { claimLoanBeaconsBurned = 
          [ ("Active",-1)
          , (pubKeyAsToken lenderPubKey,-1)
          , (pubKeyAsToken borrowerPubKey,-1)
          ]
      , claimLoanBeaconRedeemer = BurnBeaconToken'
      , claimLoanBeaconPolicy = beaconPolicy
      , claimLoanVal = loanValidator
      , claimLoanAddress = addr
      , claimLoanSpecificUTxOs =
          [ ( newActiveDatum
            , lovelaceValueOf 25_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , claimLoanWithTTL = True
      , claimLoanOtherMint = [("other",1)]
      , claimLoanOtherMintPolicy = alwaysSucceedPolicy
      }

claimFullyPaidLoanEarly :: EmulatorTrace ()
claimFullyPaidLoanEarly = do
  borrowerH <- activateContractWallet (knownWallet 1) endpoints
  lenderH <- activateContractWallet (knownWallet 2) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { loanBeaconSym' = beaconPolicySymbol
        , borrowerId' = pubKeyAsToken borrowerPubKey
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
        { loanBeaconSym' = beaconPolicySymbol
        , lenderId' = pubKeyAsToken lenderPubKey
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanInterest' = unsafeRatio 1 10
        , collateralization' = [(testToken1,unsafeRatio 1 2_000_000)]
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
        { loanBeaconSym' = beaconPolicySymbol
        , lenderId' = pubKeyAsToken lenderPubKey
        , borrowerId' = pubKeyAsToken borrowerPubKey
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanInterest' = unsafeRatio 1 10
        , collateralization' = [(testToken1,unsafeRatio 1 2_000_000)]
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

  callEndpoint @"claim-loan" lenderH $
    ClaimLoanParams
      { claimLoanBeaconsBurned = 
          [ ("Active",-1)
          , (pubKeyAsToken lenderPubKey,-1)
          ]
      , claimLoanBeaconRedeemer = BurnBeaconToken'
      , claimLoanBeaconPolicy = beaconPolicy
      , claimLoanVal = loanValidator
      , claimLoanAddress = addr
      , claimLoanSpecificUTxOs =
          [ ( newActiveDatum
            , lovelaceValueOf 113_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
            )
          ]
      , claimLoanWithTTL = True
      , claimLoanOtherMint = []
      , claimLoanOtherMintPolicy = alwaysSucceedPolicy
      }

inputDoesNotHaveActiveDatum :: EmulatorTrace ()
inputDoesNotHaveActiveDatum = do
  borrowerH <- activateContractWallet (knownWallet 1) endpoints
  lenderH <- activateContractWallet (knownWallet 2) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { loanBeaconSym' = beaconPolicySymbol
        , borrowerId' = pubKeyAsToken borrowerPubKey
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

  void $ waitUntilSlot 4

  callEndpoint @"claim-loan" lenderH $
    ClaimLoanParams
      { claimLoanBeaconsBurned = 
          [
          ]
      , claimLoanBeaconRedeemer = BurnBeaconToken'
      , claimLoanBeaconPolicy = beaconPolicy
      , claimLoanVal = loanValidator
      , claimLoanAddress = addr
      , claimLoanSpecificUTxOs =
          [ ( askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconPolicySymbol "Ask" 1
            )
          ]
      , claimLoanWithTTL = True
      , claimLoanOtherMint = []
      , claimLoanOtherMintPolicy = alwaysSucceedPolicy
      }

noActiveBeaconPresent :: EmulatorTrace ()
noActiveBeaconPresent = do
  borrowerH <- activateContractWallet (knownWallet 1) endpoints
  lenderH <- activateContractWallet (knownWallet 2) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { loanBeaconSym' = beaconPolicySymbol
        , borrowerId' = pubKeyAsToken borrowerPubKey
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
        { loanBeaconSym' = beaconPolicySymbol
        , lenderId' = pubKeyAsToken lenderPubKey
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanInterest' = unsafeRatio 1 10
        , collateralization' = [(testToken1,unsafeRatio 1 2_000_000)]
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
        { loanBeaconSym' = beaconPolicySymbol
        , lenderId' = pubKeyAsToken lenderPubKey
        , borrowerId' = pubKeyAsToken borrowerPubKey
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , loanInterest' = unsafeRatio 1 10
        , collateralization' = [(testToken1,unsafeRatio 1 2_000_000)]
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
          [ 
          ]
      , repayLoanChangeAddress = addr
      , repayLoanChangeOutputs =
          [ ( Just newActiveDatum
            , lovelaceValueOf 25_000_000
            )
          ]
      , repayLoanDatumAsInline = True
      , repayLoanWithTTE = True
      , repayLoanOtherMint = []
      , repayLoanOtherMintPolicy = alwaysSucceedPolicy
      }

  void $ waitUntilSlot 15

  callEndpoint @"claim-loan" lenderH $
    ClaimLoanParams
      { claimLoanBeaconsBurned = 
          [ 
          ]
      , claimLoanBeaconRedeemer = BurnBeaconToken'
      , claimLoanBeaconPolicy = beaconPolicy
      , claimLoanVal = loanValidator
      , claimLoanAddress = addr
      , claimLoanSpecificUTxOs =
          [ ( newActiveDatum
            , lovelaceValueOf 25_000_000
            )
          ]
      , claimLoanWithTTL = True
      , claimLoanOtherMint = [("other",1),("other2",2)]
      , claimLoanOtherMintPolicy = alwaysSucceedPolicy
      }

-------------------------------------------------
-- Test Function
-------------------------------------------------
tests :: TestTree
tests = do
  let opts = defaultCheckOptions & emulatorConfig .~ emConfig
  testGroup "Claim Loan"
    [ checkPredicateOptions opts "Fail if other phase beacons in tx inputs"
        (Test.not assertNoFailedTransactions) otherBeaconsInInput
    , checkPredicateOptions opts "Fail if loan is not expired"
        (Test.not assertNoFailedTransactions) claimActiveLoan
    , checkPredicateOptions opts "Fail if active beacon not burned"
        (Test.not assertNoFailedTransactions) activeBeaconNotBurned
    , checkPredicateOptions opts "Fail if lender ID not burned"
        (Test.not assertNoFailedTransactions) lenderIdNotBurned
    , checkPredicateOptions opts "Fail if borrower ID not burned"
        (Test.not assertNoFailedTransactions) borrowerIdNotBurned
    , checkPredicateOptions opts "Fail if other tokens minted or burned in the tx"
        (Test.not assertNoFailedTransactions) otherTokensMinted
    , checkPredicateOptions opts "Fail if lender did not sign"
        (Test.not assertNoFailedTransactions) lenderDidNotSign
    , checkPredicateOptions opts "Fail if input utxo doesn't have ActiveDatum"
        (Test.not assertNoFailedTransactions) inputDoesNotHaveActiveDatum
    , checkPredicateOptions opts "Fail if input utxo is missing active beacon"
        (Test.not assertNoFailedTransactions) noActiveBeaconPresent
    
      -- Success Checks
    , checkPredicateOptions opts "Successfully claim paid loan early"
        assertNoFailedTransactions claimFullyPaidLoanEarly
    , checkPredicateOptions opts "Successfully claim expired loan"
        assertNoFailedTransactions claimExpiredLoan
    ]

testTrace :: IO ()
testTrace = runEmulatorTraceIO' def emConfig claimFullyPaidLoanEarly