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

module Test.AcceptOffer
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
import Ledger.TimeSlot

import Test.Common
import CardanoLoans

-------------------------------------------------
-- Accept Scenarios
-------------------------------------------------
acceptALoanOffer :: EmulatorTrace ()
acceptALoanOffer = do
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

askTokenNotBurned :: EmulatorTrace ()
askTokenNotBurned = do
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
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1)
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
      { acceptBeaconsMinted = [("Offer",-1),("Active",1),(pubKeyAsToken borrowerPubKey,1)]
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
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1
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

offerTokenNotBurned :: EmulatorTrace ()
offerTokenNotBurned = do
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
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1)
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
      { acceptBeaconsMinted = [("Ask",-1),("Active",1),(pubKeyAsToken borrowerPubKey,1)]
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
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1
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

activeTokenNotMinted :: EmulatorTrace ()
activeTokenNotMinted = do
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
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1)
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
      { acceptBeaconsMinted = [("Offer",-1),("Ask",-1),(pubKeyAsToken borrowerPubKey,1)]
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
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1
            )
          ]
      , acceptChangeAddress = addr
      , acceptChangeOutput =
          [ (Just activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , acceptDatumAsInline = True
      , acceptWithTTL = True
      }

multipleActiveTokensMinted :: EmulatorTrace ()
multipleActiveTokensMinted = do
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
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1)
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
      { acceptBeaconsMinted = [("Offer",-1),("Ask",-1),("Active",2),(pubKeyAsToken borrowerPubKey,1)]
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
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1
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

activeTokenHasWrongName :: EmulatorTrace ()
activeTokenHasWrongName = do
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
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1)
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
      { acceptBeaconsMinted = [("Offer",-1),("Ask",-1),("Ative",1),(pubKeyAsToken borrowerPubKey,1)]
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
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1
            )
          ]
      , acceptChangeAddress = addr
      , acceptChangeOutput =
          [ (Just activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Ative" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , acceptDatumAsInline = True
      , acceptWithTTL = True
      }
  
borrowerIdNotMinted :: EmulatorTrace ()
borrowerIdNotMinted = do
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
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1)
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
      { acceptBeaconsMinted = [("Offer",-1),("Ask",-1),("Active",1)]
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
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1
            )
          ]
      , acceptChangeAddress = addr
      , acceptChangeOutput =
          [ (Just activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , acceptDatumAsInline = True
      , acceptWithTTL = True
      }

multipleBorrowerIdsMinted :: EmulatorTrace ()
multipleBorrowerIdsMinted = do
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
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1)
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
      { acceptBeaconsMinted = [("Offer",-1),("Ask",-1),("Active",1),(pubKeyAsToken borrowerPubKey,2)]
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
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1
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

wrongBorrowerIdName :: EmulatorTrace ()
wrongBorrowerIdName = do
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
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1)
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
      { acceptBeaconsMinted = [("Offer",-1),("Ask",-1),("Active",1),(pubKeyAsToken lenderPubKey,1)]
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
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1
            )
          ]
      , acceptChangeAddress = addr
      , acceptChangeOutput =
          [ (Just activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , acceptDatumAsInline = True
      , acceptWithTTL = True
      }

beaconsMintedToPubKeyAddress :: EmulatorTrace ()
beaconsMintedToPubKeyAddress = do
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
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1)
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
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1
            )
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

beaconsMintedToAddressWithoutStakingCred :: EmulatorTrace ()
beaconsMintedToAddressWithoutStakingCred = do
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
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1)
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
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1
            )
          ]
      , acceptChangeAddress = addr{addressStakingCredential=Nothing}
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

beaconsMintedToDappAddressWithDifferentStakePubKey :: EmulatorTrace ()
beaconsMintedToDappAddressWithDifferentStakePubKey = do
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
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1)
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
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1
            )
          ]
      , acceptChangeAddress = 
          addr{addressStakingCredential= Just $ StakingHash 
                                              $ PubKeyCredential 
                                              $ unPaymentPubKeyHash lenderPubKey}
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

activeBeaconStoredSeparately :: EmulatorTrace ()
activeBeaconStoredSeparately = do
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
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1)
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
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1
            )
          ]
      , acceptChangeAddress = addr
      , acceptChangeOutput =
          [ (Just activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> (uncurry singleton testToken1) 50
            )
          , ( Just activeDatum
            , lovelaceValueOf 2_000_000
           <> singleton beaconPolicySymbol "Active" 1
            )
          ]
      , acceptDatumAsInline = True
      , acceptWithTTL = True
      }

lenderIdStoredSeparately :: EmulatorTrace ()
lenderIdStoredSeparately = do
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
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1)
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
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1
            )
          ]
      , acceptChangeAddress = addr
      , acceptChangeOutput =
          [ (Just activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , acceptDatumAsInline = True
      , acceptWithTTL = True
      }

borrowerIdStoredSeparately :: EmulatorTrace ()
borrowerIdStoredSeparately = do
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
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1)
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
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1
            )
          ]
      , acceptChangeAddress = addr
      , acceptChangeOutput =
          [ (Just activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , acceptDatumAsInline = True
      , acceptWithTTL = True
      }

stakingCredDoesNotApprove :: EmulatorTrace ()
stakingCredDoesNotApprove = do
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
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1)
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
    
  callEndpoint @"accept" lenderH $
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
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1
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

moreThanTwoInputs :: EmulatorTrace ()
moreThanTwoInputs = do
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
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1)
          ]
      , askAsInline = True
      }

  void $ waitUntilSlot 2

  callEndpoint @"ask" borrowerH $
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
  
  void $ waitUntilSlot 3

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
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1
            )
          , ( askDatum
            , lovelaceValueOf 2_100_000 <> singleton beaconPolicySymbol "Ask" 1
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

onlyAskInput :: EmulatorTrace ()
onlyAskInput = do
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
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1)
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
      { acceptBeaconsMinted = [("Ask",-1),("Active",1),(pubKeyAsToken borrowerPubKey,1)]
      , acceptBeaconRedeemer = MintActiveToken' borrowerPubKey lenderPubKey
      , acceptBeaconPolicy = beaconPolicy
      , acceptLoanVal = loanValidator
      , acceptLoanAddress = addr
      , acceptSpecificUTxOs =
          [ ( askDatum
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1
            )
          ]
      , acceptChangeAddress = addr
      , acceptChangeOutput =
          [ (Just activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , acceptDatumAsInline = True
      , acceptWithTTL = True
      }

twoAskInputs :: EmulatorTrace ()
twoAskInputs = do
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
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1)
          ]
      , askAsInline = True
      }

  void $ waitUntilSlot 2

  callEndpoint @"ask" borrowerH $
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
  
  void $ waitUntilSlot 3

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
      { acceptBeaconsMinted = [("Ask",-2),("Active",1),(pubKeyAsToken borrowerPubKey,1)]
      , acceptBeaconRedeemer = MintActiveToken' borrowerPubKey lenderPubKey
      , acceptBeaconPolicy = beaconPolicy
      , acceptLoanVal = loanValidator
      , acceptLoanAddress = addr
      , acceptSpecificUTxOs =
          [ ( askDatum
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1
            )
          , ( askDatum
            , lovelaceValueOf 2_100_000 <> singleton beaconPolicySymbol "Ask" 1
            )
          ]
      , acceptChangeAddress = addr
      , acceptChangeOutput =
          [ (Just activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , acceptDatumAsInline = True
      , acceptWithTTL = True
      }

missingOfferBeacon :: EmulatorTrace ()
missingOfferBeacon = do
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
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1)
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
      { acceptBeaconsMinted = [("Ask",-1),("Active",1),(pubKeyAsToken borrowerPubKey,1)]
      , acceptBeaconRedeemer = MintActiveToken' borrowerPubKey lenderPubKey
      , acceptBeaconPolicy = beaconPolicy
      , acceptLoanVal = loanValidator
      , acceptLoanAddress = addr
      , acceptSpecificUTxOs =
          [ ( offerDatum
            , lovelaceValueOf 103_000_000
            )
          , ( askDatum
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol "Ask" 1
            )
          ]
      , acceptChangeAddress = addr
      , acceptChangeOutput =
          [ (Just activeDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , acceptDatumAsInline = True
      , acceptWithTTL = True
      }

missingAskBeacon :: EmulatorTrace ()
missingAskBeacon = do
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
      { askBeaconsMinted = []
      , askBeaconRedeemer = MintAskToken' borrowerPubKey
      , askBeaconPolicy = beaconPolicy
      , askAddress = addr
      , askInfo = 
          [ ( Just askDatum
            , lovelaceValueOf 2_000_000)
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
      { acceptBeaconsMinted = [("Offer",-1),("Active",1),(pubKeyAsToken borrowerPubKey,1)]
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
            , lovelaceValueOf 2_000_000
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

differentLoanAssets :: EmulatorTrace ()
differentLoanAssets = do
  borrowerH <- activateContractWallet (knownWallet 1) endpoints
  lenderH <- activateContractWallet (knownWallet 2) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { askBeacon' = (beaconPolicySymbol,"Ask")
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = testToken2
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

differentLoanPrinciples :: EmulatorTrace ()
differentLoanPrinciples = do
  borrowerH <- activateContractWallet (knownWallet 1) endpoints
  lenderH <- activateContractWallet (knownWallet 2) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { askBeacon' = (beaconPolicySymbol,"Ask")
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 10_000_000
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

differentLoanTerms :: EmulatorTrace ()
differentLoanTerms = do
  borrowerH <- activateContractWallet (knownWallet 1) endpoints
  lenderH <- activateContractWallet (knownWallet 2) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { askBeacon' = (beaconPolicySymbol,"Ask")
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 1000
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

differentCollaterals :: EmulatorTrace ()
differentCollaterals = do
  borrowerH <- activateContractWallet (knownWallet 1) endpoints
  lenderH <- activateContractWallet (knownWallet 2) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
      askDatum = AskDatum'
        { askBeacon' = (beaconPolicySymbol,"Ask")
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 10000
        , collateral' = [testToken2]
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

otherAskBeacons :: EmulatorTrace ()
otherAskBeacons = do
  borrowerH <- activateContractWallet (knownWallet 2) endpoints
  lenderH <- activateContractWallet (knownWallet 1) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
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
                           $ knownWallet 2)
  
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

  let lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
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
      { acceptBeaconsMinted = [("Offer",-1),("Ask",-2),("Active",1),(pubKeyAsToken borrowerPubKey,1)]
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

otherOfferBeacons :: EmulatorTrace ()
otherOfferBeacons = do
  borrowerH <- activateContractWallet (knownWallet 2) endpoints
  lenderH <- activateContractWallet (knownWallet 1) endpoints

  let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
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
                           $ knownWallet 2)
  
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

  let lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
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
      { acceptBeaconsMinted = [("Offer",-2),("Ask",-1),("Active",1),(pubKeyAsToken borrowerPubKey,1)]
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

noOutputToLoanAddress :: EmulatorTrace ()
noOutputToLoanAddress = do
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
          [ 
          ]
      , acceptDatumAsInline = True
      , acceptWithTTL = True
      }

additionalLoanAddressOutput :: EmulatorTrace ()
additionalLoanAddressOutput = do
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
          , (Just activeDatum
            , lovelaceValueOf 2_000_000
            )
          ]
      , acceptDatumAsInline = True
      , acceptWithTTL = True
      }

wrongDatumActiveBeacon :: EmulatorTrace ()
wrongDatumActiveBeacon = do
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
        { activeBeacon' = (beaconPolicySymbol,"Ative")
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

wrongDatumLenderId :: EmulatorTrace ()
wrongDatumLenderId = do
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
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
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

wrongDatumBorrowerId :: EmulatorTrace ()
wrongDatumBorrowerId = do
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
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
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

wrongDatumLoanAsset :: EmulatorTrace ()
wrongDatumLoanAsset = do
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
        , loanAsset' = testToken1
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

wrongDatumLoanPrinciple :: EmulatorTrace ()
wrongDatumLoanPrinciple = do
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
        , loanPrinciple' = 10_000_000
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

wrongDatumLoanTerm :: EmulatorTrace ()
wrongDatumLoanTerm = do
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
        , loanTerm' = 100000
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

wrongDatumLoanInterest :: EmulatorTrace ()
wrongDatumLoanInterest = do
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
        , loanInterest' = unsafeRatio 1 11
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

wrongDatumLoanBacking :: EmulatorTrace ()
wrongDatumLoanBacking = do
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
        , loanBacking' = 10_000_000
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

wrongDatumCollateralRates :: EmulatorTrace ()
wrongDatumCollateralRates = do
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
        , collateralRates' = [(testToken1,unsafeRatio 2 2_000_000)]
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

wrongDatumExpiration :: EmulatorTrace ()
wrongDatumExpiration = do
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

  let exp = slotToBeginPOSIXTime def 16
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

wrongDatumOutstanding :: EmulatorTrace ()
wrongDatumOutstanding = do
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
        , loanOutstanding' = fromInteger 100_000_000
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

datumNotInline :: EmulatorTrace ()
datumNotInline = do
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
      , acceptDatumAsInline = False
      , acceptWithTTL = True
      }

outputDatumNotActiveDatum :: EmulatorTrace ()
outputDatumNotActiveDatum = do
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
          [ (Just offerDatum
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

downPaymentNotMet :: EmulatorTrace ()
downPaymentNotMet = do
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
           <> (uncurry singleton testToken1) 49
            )
          ]
      , acceptDatumAsInline = True
      , acceptWithTTL = True
      }

takeMoreAfterMeetingBacking :: EmulatorTrace ()
takeMoreAfterMeetingBacking = do
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
            , lovelaceValueOf 2_900_000
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
           <> singleton beaconPolicySymbol "Active" 1
           <> (uncurry singleton testToken1) 50
            )
          ]
      , acceptDatumAsInline = True
      , acceptWithTTL = True
      }

acceptWithoutTTL :: EmulatorTrace ()
acceptWithoutTTL = do
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
      , acceptWithTTL = False
      }

acceptUnderCollateralizedLoan :: EmulatorTrace ()
acceptUnderCollateralizedLoan = do
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
        , loanBacking' = 50_000_000
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
        , loanBacking' = 50_000_000
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
           <> (uncurry singleton testToken1) 25
            )
          ]
      , acceptDatumAsInline = True
      , acceptWithTTL = True
      }

acceptOverCollateralizedLoan :: EmulatorTrace ()
acceptOverCollateralizedLoan = do
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

acceptALoanOfferWithMultipleCollateralAssets :: EmulatorTrace ()
acceptALoanOfferWithMultipleCollateralAssets = do
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

-- Minimum: 2.844600 ADA
-- minAcceptUTxOTest :: EmulatorTrace ()
-- minAcceptUTxOTest = do
--   borrowerH <- activateContractWallet (knownWallet 1) endpoints
--   lenderH <- activateContractWallet (knownWallet 2) endpoints

--   let borrowerPubKey = mockWalletPaymentPubKeyHash $ knownWallet 1
--       askDatum = AskDatum'
--         { askBeacon' = (beaconPolicySymbol,"Ask")
--         , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
--         , loanAsset' = (adaSymbol,adaToken)
--         , loanPrinciple' = 100_000_000
--         , loanTerm' = 10000
--         , collateral' = [testToken1]
--         }
--       addr = Address (ScriptCredential loanValidatorHash)
--                      (Just $ StakingHash
--                            $ PubKeyCredential
--                            $ unPaymentPubKeyHash
--                            $ mockWalletPaymentPubKeyHash
--                            $ knownWallet 1)
  
--   callEndpoint @"ask" borrowerH $
--     AskParams
--       { askBeaconsMinted = [("Ask",1)]
--       , askBeaconRedeemer = MintAskToken' borrowerPubKey
--       , askBeaconPolicy = beaconPolicy
--       , askAddress = addr
--       , askInfo = 
--           [ ( Just askDatum
--             , lovelaceValueOf 3_000_000 <> singleton beaconPolicySymbol "Ask" 1)
--           ]
--       , askAsInline = True
--       }
  
--   void $ waitUntilSlot 2

--   let lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
--       offerDatum = OfferDatum'
--         { offerBeacon' = (beaconPolicySymbol,"Offer")
--         , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
--         , loanAsset' = (adaSymbol,adaToken)
--         , loanPrinciple' = 100_000_000
--         , loanTerm' = 10000
--         , loanBacking' = 100_000_000
--         , loanInterest' = unsafeRatio 1 10
--         , collateralRates' = [(testToken1,unsafeRatio 1 2_000_000)]
--         }
  
--   callEndpoint @"offer" lenderH $
--     OfferParams
--       { offerBeaconsMinted = [("Offer",1),(pubKeyAsToken lenderPubKey,1)]
--       , offerBeaconRedeemer = MintOfferToken' lenderPubKey
--       , offerBeaconPolicy = beaconPolicy
--       , offerAddress = addr
--       , offerInfo = 
--           [ ( Just offerDatum
--             , lovelaceValueOf 103_000_000 <> singleton beaconPolicySymbol "Offer" 1
--            <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
--             )
--           ]
--       , offerAsInline = True
--       }

--   void $ waitUntilSlot 4

--   let exp = slotToBeginPOSIXTime def 14
--       activeDatum = ActiveDatum'
--         { activeBeacon' = (beaconPolicySymbol,"Active")
--         , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
--         , borrowerId' = (beaconPolicySymbol,pubKeyAsToken borrowerPubKey)
--         , loanAsset' = (adaSymbol,adaToken)
--         , loanPrinciple' = 100_000_000
--         , loanTerm' = 10000
--         , loanBacking' = 100_000_000
--         , loanInterest' = unsafeRatio 1 10
--         , collateralRates' = [(testToken1,unsafeRatio 1 2_000_000)]
--         , loanExpiration' = exp
--         , loanOutstanding' = fromInteger 100_000_000 * (fromInteger 1 + unsafeRatio 1 10)
--         }
    
--   callEndpoint @"accept" borrowerH $
--     AcceptParams
--       { acceptBeaconsMinted = [("Offer",-1),("Ask",-1),("Active",1),(pubKeyAsToken borrowerPubKey,1)]
--       , acceptBeaconRedeemer = MintActiveToken' borrowerPubKey lenderPubKey
--       , acceptBeaconPolicy = beaconPolicy
--       , acceptLoanVal = loanValidator
--       , acceptLoanAddress = addr
--       , acceptSpecificUTxOs =
--           [ ( offerDatum
--             , lovelaceValueOf 103_000_000 <> singleton beaconPolicySymbol "Offer" 1
--            <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
--             )
--           , ( askDatum
--             , lovelaceValueOf 3_000_000 <> singleton beaconPolicySymbol "Ask" 1
--             )
--           ]
--       , acceptChangeAddress = addr
--       , acceptChangeOutput =
--           [ (Just activeDatum
--             , lovelaceValueOf 2_000_000
--            <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
--            <> singleton beaconPolicySymbol (pubKeyAsToken borrowerPubKey) 1
--            <> singleton beaconPolicySymbol "Active" 1
--            <> (uncurry singleton testToken1) 50
--             )
--           ]
--       , acceptDatumAsInline = True
--       , acceptWithTTL = True
--       }

-------------------------------------------------
-- Test Function
-------------------------------------------------
tests :: TestTree
tests = do
  let opts = defaultCheckOptions & emulatorConfig .~ emConfig
  testGroup "Accept an offer"
    [ -- MintActiveToken Checks
      checkPredicateOptions opts "Fail if ask token not burned"
        (Test.not assertNoFailedTransactions) askTokenNotBurned
    , checkPredicateOptions opts "Fail if offer token not burned"
        (Test.not assertNoFailedTransactions) offerTokenNotBurned
    , checkPredicateOptions opts "Fail if active token not minted"
        (Test.not assertNoFailedTransactions) activeTokenNotMinted
    , checkPredicateOptions opts "Fail if multiple active tokens minted"
        (Test.not assertNoFailedTransactions) multipleActiveTokensMinted
    , checkPredicateOptions opts "Fail if active token does not have token name 'Active'"
        (Test.not assertNoFailedTransactions) activeTokenHasWrongName
    , checkPredicateOptions opts "Fail if borrower ID not minted"
        (Test.not assertNoFailedTransactions) borrowerIdNotMinted
    , checkPredicateOptions opts "Fail if multiple borrower IDs minted"
        (Test.not assertNoFailedTransactions) multipleBorrowerIdsMinted
    , checkPredicateOptions opts "Fail if borrower ID has wrong token name"
        (Test.not assertNoFailedTransactions) wrongBorrowerIdName
    , checkPredicateOptions opts "Fail if beacons minted to pubkey address"
        (Test.not assertNoFailedTransactions) beaconsMintedToPubKeyAddress
    , checkPredicateOptions opts "Fail if beacons minted to dapp address without a staking credential"
        (Test.not assertNoFailedTransactions) beaconsMintedToAddressWithoutStakingCred
    , checkPredicateOptions opts "Fail if beacons minted to dapp address with different stake pubkey"
        (Test.not assertNoFailedTransactions) beaconsMintedToDappAddressWithDifferentStakePubKey
    , checkPredicateOptions opts "Fail if active beacon stored separately"
        (Test.not assertNoFailedTransactions) activeBeaconStoredSeparately
    , checkPredicateOptions opts "Fail if lender ID stored separately"
        (Test.not assertNoFailedTransactions) lenderIdStoredSeparately
    , checkPredicateOptions opts "Fail if borrower ID stored separately"
        (Test.not assertNoFailedTransactions) borrowerIdStoredSeparately

      -- AcceptOffer Checks
    , checkPredicateOptions opts "Fail if staking credential did not approve"
        (Test.not assertNoFailedTransactions) stakingCredDoesNotApprove
    , checkPredicateOptions opts "Fail if more than two inputs from loan address"
        (Test.not assertNoFailedTransactions) moreThanTwoInputs
    , checkPredicateOptions opts "Fail if only one input from loan address."
        (Test.not assertNoFailedTransactions) onlyAskInput
    , checkPredicateOptions opts "Fail if both inputs are the same phase"
        (Test.not assertNoFailedTransactions) twoAskInputs
    , checkPredicateOptions opts "Fail if offer input does not have an offer beacon"
        (Test.not assertNoFailedTransactions) missingOfferBeacon
    , checkPredicateOptions opts "Fail if ask input does not have an ask beacon"
        (Test.not assertNoFailedTransactions) missingAskBeacon
    , checkPredicateOptions opts "Fail if input loanAssets are different"
        (Test.not assertNoFailedTransactions) differentLoanAssets
    , checkPredicateOptions opts "Fail if input loanPrinciples are different"
        (Test.not assertNoFailedTransactions) differentLoanPrinciples
    , checkPredicateOptions opts "Fail if input loanTerms are different"
        (Test.not assertNoFailedTransactions) differentLoanTerms
    , checkPredicateOptions opts "Fail if input collateral are different"
        (Test.not assertNoFailedTransactions) differentCollaterals
    , checkPredicateOptions opts "Fail if any other ask beacon inputs in tx"
        (Test.not assertNoFailedTransactions) otherAskBeacons
    , checkPredicateOptions opts "Fail if any other offer beacon inputs in tx"
        (Test.not assertNoFailedTransactions) otherOfferBeacons
    , checkPredicateOptions opts "Fail if no output to loan address"
        (Test.not assertNoFailedTransactions) noOutputToLoanAddress
    , checkPredicateOptions opts "Fail if additional outputs to loan address"
        (Test.not assertNoFailedTransactions) additionalLoanAddressOutput
    , checkPredicateOptions opts "Fail if output datum has wrong activeBeacon"
        (Test.not assertNoFailedTransactions) wrongDatumActiveBeacon
    , checkPredicateOptions opts "Fail if output datum has wrong lenderId"
        (Test.not assertNoFailedTransactions) wrongDatumLenderId
    , checkPredicateOptions opts "Fail if output datum has wrong borrowerId"
        (Test.not assertNoFailedTransactions) wrongDatumBorrowerId
    , checkPredicateOptions opts "Fail if output datum has wrong loanAsset"
        (Test.not assertNoFailedTransactions) wrongDatumLoanAsset
    , checkPredicateOptions opts "Fail if output datum has wrong loanPrinciple"
        (Test.not assertNoFailedTransactions) wrongDatumLoanPrinciple
    , checkPredicateOptions opts "Fail if output datum has wrong loanTerm"
        (Test.not assertNoFailedTransactions) wrongDatumLoanTerm
    , checkPredicateOptions opts "Fail if output datum has wrong loanInterest"
        (Test.not assertNoFailedTransactions) wrongDatumLoanInterest
    , checkPredicateOptions opts "Fail if output datum has wrong loanBacking"
        (Test.not assertNoFailedTransactions) wrongDatumLoanBacking
    , checkPredicateOptions opts "Fail if output datum has wrong collateralRates"
        (Test.not assertNoFailedTransactions) wrongDatumCollateralRates
    , checkPredicateOptions opts "Fail if output datum has wrong loanExpiration"
        (Test.not assertNoFailedTransactions) wrongDatumExpiration
    , checkPredicateOptions opts "Fail if output datum has wrong loanOutstanding"
        (Test.not assertNoFailedTransactions) wrongDatumOutstanding
    , checkPredicateOptions opts "Fail if output datum is not inline"
        (Test.not assertNoFailedTransactions) datumNotInline
    , checkPredicateOptions opts "Fail if output datum is not ActiveDatum"
        (Test.not assertNoFailedTransactions) outputDatumNotActiveDatum
    , checkPredicateOptions opts "Fail if collateral value deposited does not match loanBacking"
        (Test.not assertNoFailedTransactions) downPaymentNotMet
    , checkPredicateOptions opts "Fail if TTL not specified"
        (Test.not assertNoFailedTransactions) acceptWithoutTTL

      -- Success Tests
    , checkPredicateOptions opts "Successfully accept fully collateralized loan"
        assertNoFailedTransactions acceptALoanOffer
    , checkPredicateOptions opts "Successfully accept under collateralized loan"
        assertNoFailedTransactions acceptUnderCollateralizedLoan
    , checkPredicateOptions opts "Successfully accept over collateralized loan"
        assertNoFailedTransactions acceptOverCollateralizedLoan
    , checkPredicateOptions opts "Successfully accept loan using multiple collateral assets"
        assertNoFailedTransactions acceptALoanOfferWithMultipleCollateralAssets
    ]

testTrace :: IO ()
testTrace = runEmulatorTraceIO' def emConfig acceptALoanOfferWithMultipleCollateralAssets