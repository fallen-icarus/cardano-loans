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

module Test.MakePayment
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
import Plutus.Script.Utils.Ada (lovelaceValueOf)
import Data.Default
import Plutus.V2.Ledger.Api
import Ledger.Address
import Plutus.Script.Utils.V2.Generators (alwaysSucceedValidatorHash)
import Cardano.Node.Emulator.TimeSlot

import Test.Common
import CardanoLoans

-------------------------------------------------
-- Make Payment Scenarios
-------------------------------------------------
successfullyMakeSinglePartialPayment :: DappScripts -> EmulatorTrace ()
successfullyMakeSinglePartialPayment ts@DappScripts{..} = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let refAddr = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unValidatorScript spendingValidator
      , createReferenceScriptAddress = refAddr
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOSpendRef
      }

  void $ waitUntilSlot 2

  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unMintingPolicyScript beaconPolicy
      , createReferenceScriptAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOMintRef
      }

  void $ waitUntilSlot 4

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  spendRef <- txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      borrowerToken = credentialAsToken borrowerCred
      askDatum = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanTerm = 10000
        , collateral = [testToken1]
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  callEndpoint @"create-ask" h1 $
    CreateAskParams
      { createAskBeaconsMinted = [("Ask",1)]
      , createAskBeaconRedeemer = MintAskBeacon borrowerCred
      , createAskLoanAddress = loanAddr
      , createAskUTxOs = 
          [ ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          ]
      , createAskAsInline = True
      , createAskScripts = ts
      , createAskWithRefScript = True
      , createAskRefScript = mintRef
      , createAskRefAddress = refAddr
      }

  void $ waitUntilSlot 6

  let lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      lenderAddr = Address lenderCred Nothing
      offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
  
  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum
            , lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 8

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum
  offer1 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum

  let exp = slotToBeginPOSIXTime def 8
      loanId1 = txOutRefToLoanId offer1
      activeDatum = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , nextCheckpoints = []
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000
        , loanExpiration = exp + 10000
        , loanOutstanding = fromInteger 100_000_000
        , loanId = loanId1
        }

  callEndpoint @"accept-offer" h1 $ 
    AcceptOfferParams
      { acceptOfferBeaconsMinted = 
          [ [("Ask",-1),("Offer",-1),(lenderToken,-1),(loanId1,2),("Active",1),(borrowerToken,1)]
          ]
      , acceptOfferBeaconRedeemers = [ MintActiveBeacon borrowerCred [(ask1,offer1)] ]
      , acceptOfferLoanAddresses = [loanAddr]
      , acceptOfferLoanRedeemers = [ AcceptOffer ]
      , acceptOfferUTxOs = [[ask1,offer1]]
      , acceptOfferCollateralAddresses = [loanAddr]
      , acceptOfferCollateralUTxOs = 
          [ [ ( Just activeDatum
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 100
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId1 1
              )
            ]
          ]
      , acceptOfferCollateralAsInline = True
      , acceptOfferLenderAddresses = [lenderAddr]
      , acceptOfferLenderUTxOs = 
          [ [ ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId1 1
              )
            ]
          ]
      , acceptOfferLenderAsInline = True
      , acceptOfferWithTTL = True
      , acceptOfferScripts = ts
      , acceptOfferWithRefScripts = True
      , acceptOfferSpendRefScript = spendRef
      , acceptOfferMintRefScript = mintRef
      , acceptOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 10

  active1 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 100
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId1 1
                                 )
  let newActiveDatum = activeDatum{ loanOutstanding = 
                                      loanOutstanding activeDatum - fromInteger 50_000_000 
                                  }

  callEndpoint @"make-payment" h1 $ 
    MakePaymentParams
      { makePaymentBeaconsMinted = []
      , makePaymentBeaconRedeemer = BurnBeacons
      , makePaymentLoanAddress = loanAddr
      , makePaymentLoanRedeemer = MakePayment
      , makePaymentUTxOs = [active1]
      , makePaymentCollateralAddress = loanAddr
      , makePaymentCollateralUTxOs = 
          [ ( Just newActiveDatum
            , lovelaceValueOf 3_000_000
           <> (uncurry singleton testToken1) 50
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol borrowerToken 1
           <> singleton beaconCurrencySymbol loanId1 1
            )
          ]
      , makePaymentCollateralAsInline = True
      , makePaymentLenderAddresses = [lenderAddr]
      , makePaymentLenderUTxOs = 
          [ [ ( Just $ PaymentDatum (beaconCurrencySymbol,loanId1)
              , lovelaceValueOf 50_000_000
              )
            ]
          ]
      , makePaymentLenderAsInline = True
      , makePaymentWithTTE = True
      , makePaymentScripts = ts
      , makePaymentWithRefScripts = True
      , makePaymentSpendRefScript = spendRef
      , makePaymentMintRefScript = mintRef
      , makePaymentRefAddress = refAddr
      }

successfullyMakeMultiplePartialPayments :: DappScripts -> EmulatorTrace ()
successfullyMakeMultiplePartialPayments ts@DappScripts{..} = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let refAddr = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unValidatorScript spendingValidator
      , createReferenceScriptAddress = refAddr
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOSpendRef
      }

  void $ waitUntilSlot 2

  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unMintingPolicyScript beaconPolicy
      , createReferenceScriptAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOMintRef
      }

  void $ waitUntilSlot 4

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  spendRef <- txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      borrowerToken = credentialAsToken borrowerCred
      askDatum1 = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanTerm = 10000
        , collateral = [testToken1]
        }
      askDatum2 = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 50_000_000
        , loanTerm = 10000
        , collateral = [testToken1]
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  callEndpoint @"create-ask" h1 $
    CreateAskParams
      { createAskBeaconsMinted = [("Ask",2)]
      , createAskBeaconRedeemer = MintAskBeacon borrowerCred
      , createAskLoanAddress = loanAddr
      , createAskUTxOs = 
          [ ( Just askDatum1
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum2
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          ]
      , createAskAsInline = True
      , createAskScripts = ts
      , createAskWithRefScript = True
      , createAskRefScript = mintRef
      , createAskRefAddress = refAddr
      }

  void $ waitUntilSlot 6

  let lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      lenderAddr = Address lenderCred Nothing
      offerDatum1 = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
      offerDatum2 = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 50_000_000
        , loanCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
  
  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum1
            , lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 8

  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum2
            , lovelaceValueOf 55_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 10

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum1
  ask2 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum2
  offer1 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum1
  offer2 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 55_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum2

  let exp = slotToBeginPOSIXTime def 10
      loanId1 = txOutRefToLoanId offer1
      loanId2 = txOutRefToLoanId offer2
      activeDatum1 = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , nextCheckpoints = []
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000
        , loanExpiration = exp + 10000
        , loanOutstanding = fromInteger 100_000_000
        , loanId = loanId1
        }
      activeDatum2 = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 50_000_000
        , nextCheckpoints = []
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000
        , loanExpiration = exp + 10000
        , loanOutstanding = fromInteger 50_000_000
        , loanId = loanId2
        }

  callEndpoint @"accept-offer" h1 $ 
    AcceptOfferParams
      { acceptOfferBeaconsMinted = 
          [ [ ("Ask",-2)
            , ("Offer",-2)
            , (lenderToken,-2)
            , (loanId1,2)
            , (loanId2,2)
            , ("Active",2)
            , (borrowerToken,2)
            ]
          ]
      , acceptOfferBeaconRedeemers = 
          [ MintActiveBeacon borrowerCred [(ask1,offer1),(ask2,offer2)] ]
      , acceptOfferLoanAddresses = [loanAddr]
      , acceptOfferLoanRedeemers = [ AcceptOffer ]
      , acceptOfferUTxOs = [[ask1,offer1,ask2,offer2]]
      , acceptOfferCollateralAddresses = [loanAddr]
      , acceptOfferCollateralUTxOs = 
          [ [ ( Just activeDatum1
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 100
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId1 1
              )
            , ( Just activeDatum2
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 50
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId2 1
              )
            ]
          ]
      , acceptOfferCollateralAsInline = True
      , acceptOfferLenderAddresses = [lenderAddr]
      , acceptOfferLenderUTxOs = 
          [ [ ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId1 1
              )
            , ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId2 1
              )
            ]
          ]
      , acceptOfferLenderAsInline = True
      , acceptOfferWithTTL = True
      , acceptOfferScripts = ts
      , acceptOfferWithRefScripts = True
      , acceptOfferSpendRefScript = spendRef
      , acceptOfferMintRefScript = mintRef
      , acceptOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 12

  active1 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 100
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId1 1
                                 )
  active2 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 50
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId2 1
                                 )

  let newActiveDatum1 = activeDatum1{ loanOutstanding = 
                                        loanOutstanding activeDatum1 - fromInteger 50_000_000 
                                    }
  let newActiveDatum2 = activeDatum2{ loanOutstanding = 
                                        loanOutstanding activeDatum2 - fromInteger 25_000_000 
                                    }

  callEndpoint @"make-payment" h1 $ 
    MakePaymentParams
      { makePaymentBeaconsMinted = []
      , makePaymentBeaconRedeemer = BurnBeacons
      , makePaymentLoanAddress = loanAddr
      , makePaymentLoanRedeemer = MakePayment
      , makePaymentUTxOs = [active1,active2]
      , makePaymentCollateralAddress = loanAddr
      , makePaymentCollateralUTxOs = 
          [ ( Just newActiveDatum1
            , lovelaceValueOf 3_000_000
           <> (uncurry singleton testToken1) 50
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol borrowerToken 1
           <> singleton beaconCurrencySymbol loanId1 1
            )
          , ( Just newActiveDatum2
            , lovelaceValueOf 3_000_000
           <> (uncurry singleton testToken1) 25
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol borrowerToken 1
           <> singleton beaconCurrencySymbol loanId2 1
            )
          ]
      , makePaymentCollateralAsInline = True
      , makePaymentLenderAddresses = [lenderAddr]
      , makePaymentLenderUTxOs = 
          [ [ ( Just $ PaymentDatum (beaconCurrencySymbol,loanId1)
              , lovelaceValueOf 50_000_000
              )
            , ( Just $ PaymentDatum (beaconCurrencySymbol,loanId2)
              , lovelaceValueOf 25_000_000
              )
            ]
          ]
      , makePaymentLenderAsInline = True
      , makePaymentWithTTE = True
      , makePaymentScripts = ts
      , makePaymentWithRefScripts = True
      , makePaymentSpendRefScript = spendRef
      , makePaymentMintRefScript = mintRef
      , makePaymentRefAddress = refAddr
      }

successfullyMakeSingleFullPayment :: DappScripts -> EmulatorTrace ()
successfullyMakeSingleFullPayment ts@DappScripts{..} = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let refAddr = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unValidatorScript spendingValidator
      , createReferenceScriptAddress = refAddr
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOSpendRef
      }

  void $ waitUntilSlot 2

  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unMintingPolicyScript beaconPolicy
      , createReferenceScriptAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOMintRef
      }

  void $ waitUntilSlot 4

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  spendRef <- txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      borrowerToken = credentialAsToken borrowerCred
      askDatum = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanTerm = 10000
        , collateral = [testToken1]
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  callEndpoint @"create-ask" h1 $
    CreateAskParams
      { createAskBeaconsMinted = [("Ask",1)]
      , createAskBeaconRedeemer = MintAskBeacon borrowerCred
      , createAskLoanAddress = loanAddr
      , createAskUTxOs = 
          [ ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          ]
      , createAskAsInline = True
      , createAskScripts = ts
      , createAskWithRefScript = True
      , createAskRefScript = mintRef
      , createAskRefAddress = refAddr
      }

  void $ waitUntilSlot 6

  let lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      lenderAddr = Address lenderCred Nothing
      offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
  
  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum
            , lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 8

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum
  offer1 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum

  let exp = slotToBeginPOSIXTime def 8
      loanId1 = txOutRefToLoanId offer1
      activeDatum = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , nextCheckpoints = []
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000
        , loanExpiration = exp + 10000
        , loanOutstanding = fromInteger 100_000_000
        , loanId = loanId1
        }

  callEndpoint @"accept-offer" h1 $ 
    AcceptOfferParams
      { acceptOfferBeaconsMinted = 
          [ [("Ask",-1),("Offer",-1),(lenderToken,-1),(loanId1,2),("Active",1),(borrowerToken,1)]
          ]
      , acceptOfferBeaconRedeemers = [ MintActiveBeacon borrowerCred [(ask1,offer1)] ]
      , acceptOfferLoanAddresses = [loanAddr]
      , acceptOfferLoanRedeemers = [ AcceptOffer ]
      , acceptOfferUTxOs = [[ask1,offer1]]
      , acceptOfferCollateralAddresses = [loanAddr]
      , acceptOfferCollateralUTxOs = 
          [ [ ( Just activeDatum
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 100
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId1 1
              )
            ]
          ]
      , acceptOfferCollateralAsInline = True
      , acceptOfferLenderAddresses = [lenderAddr]
      , acceptOfferLenderUTxOs = 
          [ [ ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId1 1
              )
            ]
          ]
      , acceptOfferLenderAsInline = True
      , acceptOfferWithTTL = True
      , acceptOfferScripts = ts
      , acceptOfferWithRefScripts = True
      , acceptOfferSpendRefScript = spendRef
      , acceptOfferMintRefScript = mintRef
      , acceptOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 10

  active1 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 100
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId1 1
                                 )
  let newActiveDatum = activeDatum{ loanOutstanding = 
                                      loanOutstanding activeDatum - fromInteger 100_000_000 
                                  }

  callEndpoint @"make-payment" h1 $ 
    MakePaymentParams
      { makePaymentBeaconsMinted = [(borrowerToken,-1)]
      , makePaymentBeaconRedeemer = BurnBeacons
      , makePaymentLoanAddress = loanAddr
      , makePaymentLoanRedeemer = MakePayment
      , makePaymentUTxOs = [active1]
      , makePaymentCollateralAddress = loanAddr
      , makePaymentCollateralUTxOs = 
          [ ( Just newActiveDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol loanId1 1
            )
          ]
      , makePaymentCollateralAsInline = True
      , makePaymentLenderAddresses = [lenderAddr]
      , makePaymentLenderUTxOs = 
          [ [ ( Just $ PaymentDatum (beaconCurrencySymbol,loanId1)
              , lovelaceValueOf 100_000_000
              )
            ]
          ]
      , makePaymentLenderAsInline = True
      , makePaymentWithTTE = True
      , makePaymentScripts = ts
      , makePaymentWithRefScripts = True
      , makePaymentSpendRefScript = spendRef
      , makePaymentMintRefScript = mintRef
      , makePaymentRefAddress = refAddr
      }

successfullyMakeMultipleFullPayments :: DappScripts -> EmulatorTrace ()
successfullyMakeMultipleFullPayments ts@DappScripts{..} = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let refAddr = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unValidatorScript spendingValidator
      , createReferenceScriptAddress = refAddr
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOSpendRef
      }

  void $ waitUntilSlot 2

  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unMintingPolicyScript beaconPolicy
      , createReferenceScriptAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOMintRef
      }

  void $ waitUntilSlot 4

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  spendRef <- txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      borrowerToken = credentialAsToken borrowerCred
      askDatum1 = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanTerm = 10000
        , collateral = [testToken1]
        }
      askDatum2 = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 50_000_000
        , loanTerm = 10000
        , collateral = [testToken1]
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  callEndpoint @"create-ask" h1 $
    CreateAskParams
      { createAskBeaconsMinted = [("Ask",2)]
      , createAskBeaconRedeemer = MintAskBeacon borrowerCred
      , createAskLoanAddress = loanAddr
      , createAskUTxOs = 
          [ ( Just askDatum1
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum2
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          ]
      , createAskAsInline = True
      , createAskScripts = ts
      , createAskWithRefScript = True
      , createAskRefScript = mintRef
      , createAskRefAddress = refAddr
      }

  void $ waitUntilSlot 6

  let lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      lenderAddr = Address lenderCred Nothing
      offerDatum1 = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
      offerDatum2 = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 50_000_000
        , loanCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
  
  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum1
            , lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 8

  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum2
            , lovelaceValueOf 55_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 10

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum1
  ask2 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum2
  offer1 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum1
  offer2 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 55_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum2

  let exp = slotToBeginPOSIXTime def 10
      loanId1 = txOutRefToLoanId offer1
      loanId2 = txOutRefToLoanId offer2
      activeDatum1 = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , nextCheckpoints = []
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000
        , loanExpiration = exp + 10000
        , loanOutstanding = fromInteger 100_000_000
        , loanId = loanId1
        }
      activeDatum2 = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 50_000_000
        , nextCheckpoints = []
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000
        , loanExpiration = exp + 10000
        , loanOutstanding = fromInteger 50_000_000
        , loanId = loanId2
        }

  callEndpoint @"accept-offer" h1 $ 
    AcceptOfferParams
      { acceptOfferBeaconsMinted = 
          [ [ ("Ask",-2)
            , ("Offer",-2)
            , (lenderToken,-2)
            , (loanId1,2)
            , (loanId2,2)
            , ("Active",2)
            , (borrowerToken,2)
            ]
          ]
      , acceptOfferBeaconRedeemers = 
          [ MintActiveBeacon borrowerCred [(ask1,offer1),(ask2,offer2)] ]
      , acceptOfferLoanAddresses = [loanAddr]
      , acceptOfferLoanRedeemers = [ AcceptOffer ]
      , acceptOfferUTxOs = [[ask1,offer1,ask2,offer2]]
      , acceptOfferCollateralAddresses = [loanAddr]
      , acceptOfferCollateralUTxOs = 
          [ [ ( Just activeDatum1
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 100
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId1 1
              )
            , ( Just activeDatum2
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 50
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId2 1
              )
            ]
          ]
      , acceptOfferCollateralAsInline = True
      , acceptOfferLenderAddresses = [lenderAddr]
      , acceptOfferLenderUTxOs = 
          [ [ ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId1 1
              )
            , ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId2 1
              )
            ]
          ]
      , acceptOfferLenderAsInline = True
      , acceptOfferWithTTL = True
      , acceptOfferScripts = ts
      , acceptOfferWithRefScripts = True
      , acceptOfferSpendRefScript = spendRef
      , acceptOfferMintRefScript = mintRef
      , acceptOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 12

  active1 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 100
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId1 1
                                 )
  active2 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 50
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId2 1
                                 )

  let newActiveDatum1 = activeDatum1{ loanOutstanding = 
                                        loanOutstanding activeDatum1 - fromInteger 100_000_000 
                                    }
  let newActiveDatum2 = activeDatum2{ loanOutstanding = 
                                        loanOutstanding activeDatum2 - fromInteger 50_000_000 
                                    }

  callEndpoint @"make-payment" h1 $ 
    MakePaymentParams
      { makePaymentBeaconsMinted = [(borrowerToken,-2)]
      , makePaymentBeaconRedeemer = BurnBeacons
      , makePaymentLoanAddress = loanAddr
      , makePaymentLoanRedeemer = MakePayment
      , makePaymentUTxOs = [active1,active2]
      , makePaymentCollateralAddress = loanAddr
      , makePaymentCollateralUTxOs = 
          [ ( Just newActiveDatum1
            , lovelaceValueOf 3_000_000
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol loanId1 1
            )
          , ( Just newActiveDatum2
            , lovelaceValueOf 3_000_000
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol loanId2 1
            )
          ]
      , makePaymentCollateralAsInline = True
      , makePaymentLenderAddresses = [lenderAddr]
      , makePaymentLenderUTxOs = 
          [ [ ( Just $ PaymentDatum (beaconCurrencySymbol,loanId1)
              , lovelaceValueOf 100_000_000
              )
            , ( Just $ PaymentDatum (beaconCurrencySymbol,loanId2)
              , lovelaceValueOf 50_000_000
              )
            ]
          ]
      , makePaymentLenderAsInline = True
      , makePaymentWithTTE = True
      , makePaymentScripts = ts
      , makePaymentWithRefScripts = True
      , makePaymentSpendRefScript = spendRef
      , makePaymentMintRefScript = mintRef
      , makePaymentRefAddress = refAddr
      }

successfullyMixPartialAndFullPayments :: DappScripts -> EmulatorTrace ()
successfullyMixPartialAndFullPayments ts@DappScripts{..} = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let refAddr = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unValidatorScript spendingValidator
      , createReferenceScriptAddress = refAddr
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOSpendRef
      }

  void $ waitUntilSlot 2

  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unMintingPolicyScript beaconPolicy
      , createReferenceScriptAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOMintRef
      }

  void $ waitUntilSlot 4

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  spendRef <- txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      borrowerToken = credentialAsToken borrowerCred
      askDatum1 = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanTerm = 10000
        , collateral = [testToken1]
        }
      askDatum2 = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 50_000_000
        , loanTerm = 10000
        , collateral = [testToken1]
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  callEndpoint @"create-ask" h1 $
    CreateAskParams
      { createAskBeaconsMinted = [("Ask",2)]
      , createAskBeaconRedeemer = MintAskBeacon borrowerCred
      , createAskLoanAddress = loanAddr
      , createAskUTxOs = 
          [ ( Just askDatum1
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum2
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          ]
      , createAskAsInline = True
      , createAskScripts = ts
      , createAskWithRefScript = True
      , createAskRefScript = mintRef
      , createAskRefAddress = refAddr
      }

  void $ waitUntilSlot 6

  let lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      lenderAddr = Address lenderCred Nothing
      offerDatum1 = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
      offerDatum2 = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 50_000_000
        , loanCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
  
  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum1
            , lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 8

  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum2
            , lovelaceValueOf 55_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 10

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum1
  ask2 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum2
  offer1 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum1
  offer2 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 55_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum2

  let exp = slotToBeginPOSIXTime def 10
      loanId1 = txOutRefToLoanId offer1
      loanId2 = txOutRefToLoanId offer2
      activeDatum1 = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , nextCheckpoints = []
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000
        , loanExpiration = exp + 10000
        , loanOutstanding = fromInteger 100_000_000
        , loanId = loanId1
        }
      activeDatum2 = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 50_000_000
        , nextCheckpoints = []
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000
        , loanExpiration = exp + 10000
        , loanOutstanding = fromInteger 50_000_000
        , loanId = loanId2
        }

  callEndpoint @"accept-offer" h1 $ 
    AcceptOfferParams
      { acceptOfferBeaconsMinted = 
          [ [ ("Ask",-2)
            , ("Offer",-2)
            , (lenderToken,-2)
            , (loanId1,2)
            , (loanId2,2)
            , ("Active",2)
            , (borrowerToken,2)
            ]
          ]
      , acceptOfferBeaconRedeemers = 
          [ MintActiveBeacon borrowerCred [(ask1,offer1),(ask2,offer2)] ]
      , acceptOfferLoanAddresses = [loanAddr]
      , acceptOfferLoanRedeemers = [ AcceptOffer ]
      , acceptOfferUTxOs = [[ask1,offer1,ask2,offer2]]
      , acceptOfferCollateralAddresses = [loanAddr]
      , acceptOfferCollateralUTxOs = 
          [ [ ( Just activeDatum1
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 100
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId1 1
              )
            , ( Just activeDatum2
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 50
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId2 1
              )
            ]
          ]
      , acceptOfferCollateralAsInline = True
      , acceptOfferLenderAddresses = [lenderAddr]
      , acceptOfferLenderUTxOs = 
          [ [ ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId1 1
              )
            , ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId2 1
              )
            ]
          ]
      , acceptOfferLenderAsInline = True
      , acceptOfferWithTTL = True
      , acceptOfferScripts = ts
      , acceptOfferWithRefScripts = True
      , acceptOfferSpendRefScript = spendRef
      , acceptOfferMintRefScript = mintRef
      , acceptOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 12

  active1 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 100
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId1 1
                                 )
  active2 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 50
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId2 1
                                 )

  let newActiveDatum1 = activeDatum1{ loanOutstanding = 
                                        loanOutstanding activeDatum1 - fromInteger 100_000_000 
                                    }
  let newActiveDatum2 = activeDatum2{ loanOutstanding = 
                                        loanOutstanding activeDatum2 - fromInteger 25_000_000 
                                    }

  callEndpoint @"make-payment" h1 $ 
    MakePaymentParams
      { makePaymentBeaconsMinted = [(borrowerToken,-1)]
      , makePaymentBeaconRedeemer = BurnBeacons
      , makePaymentLoanAddress = loanAddr
      , makePaymentLoanRedeemer = MakePayment
      , makePaymentUTxOs = [active1,active2]
      , makePaymentCollateralAddress = loanAddr
      , makePaymentCollateralUTxOs = 
          [ ( Just newActiveDatum1
            , lovelaceValueOf 3_000_000
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol loanId1 1
            )
          , ( Just newActiveDatum2
            , lovelaceValueOf 3_000_000
           <> (uncurry singleton testToken1) 25
           <> singleton beaconCurrencySymbol borrowerToken 1
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol loanId2 1
            )
          ]
      , makePaymentCollateralAsInline = True
      , makePaymentLenderAddresses = [lenderAddr]
      , makePaymentLenderUTxOs = 
          [ [ ( Just $ PaymentDatum (beaconCurrencySymbol,loanId1)
              , lovelaceValueOf 100_000_000
              )
            , ( Just $ PaymentDatum (beaconCurrencySymbol,loanId2)
              , lovelaceValueOf 25_000_000
              )
            ]
          ]
      , makePaymentLenderAsInline = True
      , makePaymentWithTTE = True
      , makePaymentScripts = ts
      , makePaymentWithRefScripts = True
      , makePaymentSpendRefScript = spendRef
      , makePaymentMintRefScript = mintRef
      , makePaymentRefAddress = refAddr
      }

borrowerDidNotApprove :: DappScripts -> EmulatorTrace ()
borrowerDidNotApprove ts@DappScripts{..} = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let refAddr = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unValidatorScript spendingValidator
      , createReferenceScriptAddress = refAddr
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOSpendRef
      }

  void $ waitUntilSlot 2

  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unMintingPolicyScript beaconPolicy
      , createReferenceScriptAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOMintRef
      }

  void $ waitUntilSlot 4

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  spendRef <- txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      borrowerToken = credentialAsToken borrowerCred
      askDatum = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanTerm = 10000
        , collateral = [testToken1]
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  callEndpoint @"create-ask" h1 $
    CreateAskParams
      { createAskBeaconsMinted = [("Ask",1)]
      , createAskBeaconRedeemer = MintAskBeacon borrowerCred
      , createAskLoanAddress = loanAddr
      , createAskUTxOs = 
          [ ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          ]
      , createAskAsInline = True
      , createAskScripts = ts
      , createAskWithRefScript = True
      , createAskRefScript = mintRef
      , createAskRefAddress = refAddr
      }

  void $ waitUntilSlot 6

  let lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      lenderAddr = Address lenderCred Nothing
      offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
  
  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum
            , lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 8

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum
  offer1 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum

  let exp = slotToBeginPOSIXTime def 8
      loanId1 = txOutRefToLoanId offer1
      activeDatum = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , nextCheckpoints = []
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000
        , loanExpiration = exp + 10000
        , loanOutstanding = fromInteger 100_000_000
        , loanId = loanId1
        }

  callEndpoint @"accept-offer" h1 $ 
    AcceptOfferParams
      { acceptOfferBeaconsMinted = 
          [ [("Ask",-1),("Offer",-1),(lenderToken,-1),(loanId1,2),("Active",1),(borrowerToken,1)]
          ]
      , acceptOfferBeaconRedeemers = [ MintActiveBeacon borrowerCred [(ask1,offer1)] ]
      , acceptOfferLoanAddresses = [loanAddr]
      , acceptOfferLoanRedeemers = [ AcceptOffer ]
      , acceptOfferUTxOs = [[ask1,offer1]]
      , acceptOfferCollateralAddresses = [loanAddr]
      , acceptOfferCollateralUTxOs = 
          [ [ ( Just activeDatum
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 100
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId1 1
              )
            ]
          ]
      , acceptOfferCollateralAsInline = True
      , acceptOfferLenderAddresses = [lenderAddr]
      , acceptOfferLenderUTxOs = 
          [ [ ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId1 1
              )
            ]
          ]
      , acceptOfferLenderAsInline = True
      , acceptOfferWithTTL = True
      , acceptOfferScripts = ts
      , acceptOfferWithRefScripts = True
      , acceptOfferSpendRefScript = spendRef
      , acceptOfferMintRefScript = mintRef
      , acceptOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 10

  active1 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 100
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId1 1
                                 )
  let newActiveDatum = activeDatum{ loanOutstanding = 
                                      loanOutstanding activeDatum - fromInteger 50_000_000 
                                  }

  callEndpoint @"make-payment" h2 $ 
    MakePaymentParams
      { makePaymentBeaconsMinted = []
      , makePaymentBeaconRedeemer = BurnBeacons
      , makePaymentLoanAddress = loanAddr
      , makePaymentLoanRedeemer = MakePayment
      , makePaymentUTxOs = [active1]
      , makePaymentCollateralAddress = loanAddr
      , makePaymentCollateralUTxOs = 
          [ ( Just newActiveDatum
            , lovelaceValueOf 3_000_000
           <> (uncurry singleton testToken1) 50
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol borrowerToken 1
           <> singleton beaconCurrencySymbol loanId1 1
            )
          ]
      , makePaymentCollateralAsInline = True
      , makePaymentLenderAddresses = [lenderAddr]
      , makePaymentLenderUTxOs = 
          [ [ ( Just $ PaymentDatum (beaconCurrencySymbol,loanId1)
              , lovelaceValueOf 50_000_000
              )
            ]
          ]
      , makePaymentLenderAsInline = True
      , makePaymentWithTTE = True
      , makePaymentScripts = ts
      , makePaymentWithRefScripts = True
      , makePaymentSpendRefScript = spendRef
      , makePaymentMintRefScript = mintRef
      , makePaymentRefAddress = refAddr
      }

successfullySpendInvalidActiveUTxO :: DappScripts -> EmulatorTrace ()
successfullySpendInvalidActiveUTxO ts@DappScripts{..} = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let refAddr = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unValidatorScript spendingValidator
      , createReferenceScriptAddress = refAddr
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOSpendRef
      }

  void $ waitUntilSlot 2

  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unMintingPolicyScript beaconPolicy
      , createReferenceScriptAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOMintRef
      }

  void $ waitUntilSlot 4

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  spendRef <- txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      borrowerToken = credentialAsToken borrowerCred
      askDatum = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanTerm = 10000
        , collateral = [testToken1]
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  callEndpoint @"create-ask" h1 $
    CreateAskParams
      { createAskBeaconsMinted = [("Ask",1)]
      , createAskBeaconRedeemer = MintAskBeacon borrowerCred
      , createAskLoanAddress = loanAddr
      , createAskUTxOs = 
          [ ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          ]
      , createAskAsInline = True
      , createAskScripts = ts
      , createAskWithRefScript = True
      , createAskRefScript = mintRef
      , createAskRefAddress = refAddr
      }

  void $ waitUntilSlot 6

  let lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      lenderAddr = Address lenderCred Nothing
      offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
  
  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum
            , lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 8

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum
  offer1 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum

  let exp = slotToBeginPOSIXTime def 8
      loanId1 = txOutRefToLoanId offer1
      activeDatum = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , nextCheckpoints = []
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000
        , loanExpiration = exp + 10000
        , loanOutstanding = fromInteger 100_000_000
        , loanId = loanId1
        }

  callEndpoint @"accept-offer" h1 $ 
    AcceptOfferParams
      { acceptOfferBeaconsMinted = 
          [ []
          ]
      , acceptOfferBeaconRedeemers = [ MintActiveBeacon borrowerCred [(ask1,offer1)] ]
      , acceptOfferLoanAddresses = [loanAddr]
      , acceptOfferLoanRedeemers = [ AcceptOffer ]
      , acceptOfferUTxOs = [[]]
      , acceptOfferCollateralAddresses = [loanAddr]
      , acceptOfferCollateralUTxOs = 
          [ [ ( Just activeDatum
              , lovelaceValueOf 3_000_001
              )
            ]
          ]
      , acceptOfferCollateralAsInline = True
      , acceptOfferLenderAddresses = [lenderAddr]
      , acceptOfferLenderUTxOs = 
          [ []
          ]
      , acceptOfferLenderAsInline = True
      , acceptOfferWithTTL = True
      , acceptOfferScripts = ts
      , acceptOfferWithRefScripts = True
      , acceptOfferSpendRefScript = spendRef
      , acceptOfferMintRefScript = mintRef
      , acceptOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 10

  active1 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_001
                                 )

  callEndpoint @"make-payment" h1 $ 
    MakePaymentParams
      { makePaymentBeaconsMinted = []
      , makePaymentBeaconRedeemer = BurnBeacons
      , makePaymentLoanAddress = loanAddr
      , makePaymentLoanRedeemer = MakePayment
      , makePaymentUTxOs = [active1]
      , makePaymentCollateralAddress = loanAddr
      , makePaymentCollateralUTxOs = 
          []
      , makePaymentCollateralAsInline = True
      , makePaymentLenderAddresses = [lenderAddr]
      , makePaymentLenderUTxOs = 
          [ []
          ]
      , makePaymentLenderAsInline = True
      , makePaymentWithTTE = True
      , makePaymentScripts = ts
      , makePaymentWithRefScripts = True
      , makePaymentSpendRefScript = spendRef
      , makePaymentMintRefScript = mintRef
      , makePaymentRefAddress = refAddr
      }

onlyLoanIsExpired :: DappScripts -> EmulatorTrace ()
onlyLoanIsExpired ts@DappScripts{..} = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let refAddr = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unValidatorScript spendingValidator
      , createReferenceScriptAddress = refAddr
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOSpendRef
      }

  void $ waitUntilSlot 2

  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unMintingPolicyScript beaconPolicy
      , createReferenceScriptAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOMintRef
      }

  void $ waitUntilSlot 4

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  spendRef <- txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      borrowerToken = credentialAsToken borrowerCred
      askDatum = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanTerm = 10000
        , collateral = [testToken1]
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  callEndpoint @"create-ask" h1 $
    CreateAskParams
      { createAskBeaconsMinted = [("Ask",1)]
      , createAskBeaconRedeemer = MintAskBeacon borrowerCred
      , createAskLoanAddress = loanAddr
      , createAskUTxOs = 
          [ ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          ]
      , createAskAsInline = True
      , createAskScripts = ts
      , createAskWithRefScript = True
      , createAskRefScript = mintRef
      , createAskRefAddress = refAddr
      }

  void $ waitUntilSlot 6

  let lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      lenderAddr = Address lenderCred Nothing
      offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
  
  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum
            , lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 8

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum
  offer1 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum

  let exp = slotToBeginPOSIXTime def 8
      loanId1 = txOutRefToLoanId offer1
      activeDatum = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , nextCheckpoints = []
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000
        , loanExpiration = exp + 10000
        , loanOutstanding = fromInteger 100_000_000
        , loanId = loanId1
        }

  callEndpoint @"accept-offer" h1 $ 
    AcceptOfferParams
      { acceptOfferBeaconsMinted = 
          [ [("Ask",-1),("Offer",-1),(lenderToken,-1),(loanId1,2),("Active",1),(borrowerToken,1)]
          ]
      , acceptOfferBeaconRedeemers = [ MintActiveBeacon borrowerCred [(ask1,offer1)] ]
      , acceptOfferLoanAddresses = [loanAddr]
      , acceptOfferLoanRedeemers = [ AcceptOffer ]
      , acceptOfferUTxOs = [[ask1,offer1]]
      , acceptOfferCollateralAddresses = [loanAddr]
      , acceptOfferCollateralUTxOs = 
          [ [ ( Just activeDatum
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 100
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId1 1
              )
            ]
          ]
      , acceptOfferCollateralAsInline = True
      , acceptOfferLenderAddresses = [lenderAddr]
      , acceptOfferLenderUTxOs = 
          [ [ ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId1 1
              )
            ]
          ]
      , acceptOfferLenderAsInline = True
      , acceptOfferWithTTL = True
      , acceptOfferScripts = ts
      , acceptOfferWithRefScripts = True
      , acceptOfferSpendRefScript = spendRef
      , acceptOfferMintRefScript = mintRef
      , acceptOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 20

  active1 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 100
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId1 1
                                 )
  let newActiveDatum = activeDatum{ loanOutstanding = 
                                      loanOutstanding activeDatum - fromInteger 50_000_000 
                                  }

  callEndpoint @"make-payment" h1 $ 
    MakePaymentParams
      { makePaymentBeaconsMinted = []
      , makePaymentBeaconRedeemer = BurnBeacons
      , makePaymentLoanAddress = loanAddr
      , makePaymentLoanRedeemer = MakePayment
      , makePaymentUTxOs = [active1]
      , makePaymentCollateralAddress = loanAddr
      , makePaymentCollateralUTxOs = 
          [ ( Just newActiveDatum
            , lovelaceValueOf 3_000_000
           <> (uncurry singleton testToken1) 50
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol borrowerToken 1
           <> singleton beaconCurrencySymbol loanId1 1
            )
          ]
      , makePaymentCollateralAsInline = True
      , makePaymentLenderAddresses = [lenderAddr]
      , makePaymentLenderUTxOs = 
          [ [ ( Just $ PaymentDatum (beaconCurrencySymbol,loanId1)
              , lovelaceValueOf 50_000_000
              )
            ]
          ]
      , makePaymentLenderAsInline = True
      , makePaymentWithTTE = True
      , makePaymentScripts = ts
      , makePaymentWithRefScripts = True
      , makePaymentSpendRefScript = spendRef
      , makePaymentMintRefScript = mintRef
      , makePaymentRefAddress = refAddr
      }

atLeastOneLoanIsExpired :: DappScripts -> EmulatorTrace ()
atLeastOneLoanIsExpired ts@DappScripts{..} = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let refAddr = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unValidatorScript spendingValidator
      , createReferenceScriptAddress = refAddr
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOSpendRef
      }

  void $ waitUntilSlot 2

  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unMintingPolicyScript beaconPolicy
      , createReferenceScriptAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOMintRef
      }

  void $ waitUntilSlot 4

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  spendRef <- txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      borrowerToken = credentialAsToken borrowerCred
      askDatum1 = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanTerm = 10000
        , collateral = [testToken1]
        }
      askDatum2 = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 50_000_000
        , loanTerm = 50000
        , collateral = [testToken1]
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  callEndpoint @"create-ask" h1 $
    CreateAskParams
      { createAskBeaconsMinted = [("Ask",2)]
      , createAskBeaconRedeemer = MintAskBeacon borrowerCred
      , createAskLoanAddress = loanAddr
      , createAskUTxOs = 
          [ ( Just askDatum1
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum2
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          ]
      , createAskAsInline = True
      , createAskScripts = ts
      , createAskWithRefScript = True
      , createAskRefScript = mintRef
      , createAskRefAddress = refAddr
      }

  void $ waitUntilSlot 6

  let lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      lenderAddr = Address lenderCred Nothing
      offerDatum1 = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
      offerDatum2 = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 50_000_000
        , loanCheckpoints = []
        , loanTerm = 50000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
  
  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum1
            , lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 8

  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum2
            , lovelaceValueOf 55_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 10

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum1
  ask2 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum2
  offer1 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum1
  offer2 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 55_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum2

  let exp = slotToBeginPOSIXTime def 10
      loanId1 = txOutRefToLoanId offer1
      loanId2 = txOutRefToLoanId offer2
      activeDatum1 = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , nextCheckpoints = []
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000
        , loanExpiration = exp + 10000
        , loanOutstanding = fromInteger 100_000_000
        , loanId = loanId1
        }
      activeDatum2 = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 50_000_000
        , nextCheckpoints = []
        , pastCheckpoints = []
        , loanTerm = 50000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 50000 + 10000
        , loanExpiration = exp + 50000
        , loanOutstanding = fromInteger 50_000_000
        , loanId = loanId2
        }

  callEndpoint @"accept-offer" h1 $ 
    AcceptOfferParams
      { acceptOfferBeaconsMinted = 
          [ [ ("Ask",-2)
            , ("Offer",-2)
            , (lenderToken,-2)
            , (loanId1,2)
            , (loanId2,2)
            , ("Active",2)
            , (borrowerToken,2)
            ]
          ]
      , acceptOfferBeaconRedeemers = 
          [ MintActiveBeacon borrowerCred [(ask1,offer1),(ask2,offer2)] ]
      , acceptOfferLoanAddresses = [loanAddr]
      , acceptOfferLoanRedeemers = [ AcceptOffer ]
      , acceptOfferUTxOs = [[ask1,offer1,ask2,offer2]]
      , acceptOfferCollateralAddresses = [loanAddr]
      , acceptOfferCollateralUTxOs = 
          [ [ ( Just activeDatum1
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 100
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId1 1
              )
            , ( Just activeDatum2
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 50
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId2 1
              )
            ]
          ]
      , acceptOfferCollateralAsInline = True
      , acceptOfferLenderAddresses = [lenderAddr]
      , acceptOfferLenderUTxOs = 
          [ [ ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId1 1
              )
            , ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId2 1
              )
            ]
          ]
      , acceptOfferLenderAsInline = True
      , acceptOfferWithTTL = True
      , acceptOfferScripts = ts
      , acceptOfferWithRefScripts = True
      , acceptOfferSpendRefScript = spendRef
      , acceptOfferMintRefScript = mintRef
      , acceptOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 25

  active1 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 100
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId1 1
                                 )
  active2 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 50
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId2 1
                                 )

  let newActiveDatum1 = activeDatum1{ loanOutstanding = 
                                        loanOutstanding activeDatum1 - fromInteger 50_000_000 
                                    }
  let newActiveDatum2 = activeDatum2{ loanOutstanding = 
                                        loanOutstanding activeDatum2 - fromInteger 25_000_000 
                                    }

  callEndpoint @"make-payment" h1 $ 
    MakePaymentParams
      { makePaymentBeaconsMinted = []
      , makePaymentBeaconRedeemer = BurnBeacons
      , makePaymentLoanAddress = loanAddr
      , makePaymentLoanRedeemer = MakePayment
      , makePaymentUTxOs = [active1,active2]
      , makePaymentCollateralAddress = loanAddr
      , makePaymentCollateralUTxOs = 
          [ ( Just newActiveDatum1
            , lovelaceValueOf 3_000_000
           <> (uncurry singleton testToken1) 50
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol borrowerToken 1
           <> singleton beaconCurrencySymbol loanId1 1
            )
          , ( Just newActiveDatum2
            , lovelaceValueOf 3_000_000
           <> (uncurry singleton testToken1) 25
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol borrowerToken 1
           <> singleton beaconCurrencySymbol loanId2 1
            )
          ]
      , makePaymentCollateralAsInline = True
      , makePaymentLenderAddresses = [lenderAddr]
      , makePaymentLenderUTxOs = 
          [ [ ( Just $ PaymentDatum (beaconCurrencySymbol,loanId1)
              , lovelaceValueOf 50_000_000
              )
            , ( Just $ PaymentDatum (beaconCurrencySymbol,loanId2)
              , lovelaceValueOf 25_000_000
              )
            ]
          ]
      , makePaymentLenderAsInline = True
      , makePaymentWithTTE = True
      , makePaymentScripts = ts
      , makePaymentWithRefScripts = True
      , makePaymentSpendRefScript = spendRef
      , makePaymentMintRefScript = mintRef
      , makePaymentRefAddress = refAddr
      }

onlyLoanNextCheckpointHasPassed :: DappScripts -> EmulatorTrace ()
onlyLoanNextCheckpointHasPassed ts@DappScripts{..} = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let refAddr = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unValidatorScript spendingValidator
      , createReferenceScriptAddress = refAddr
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOSpendRef
      }

  void $ waitUntilSlot 2

  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unMintingPolicyScript beaconPolicy
      , createReferenceScriptAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOMintRef
      }

  void $ waitUntilSlot 4

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  spendRef <- txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      borrowerToken = credentialAsToken borrowerCred
      askDatum = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanTerm = 10000
        , collateral = [testToken1]
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  callEndpoint @"create-ask" h1 $
    CreateAskParams
      { createAskBeaconsMinted = [("Ask",1)]
      , createAskBeaconRedeemer = MintAskBeacon borrowerCred
      , createAskLoanAddress = loanAddr
      , createAskUTxOs = 
          [ ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          ]
      , createAskAsInline = True
      , createAskScripts = ts
      , createAskWithRefScript = True
      , createAskRefScript = mintRef
      , createAskRefAddress = refAddr
      }

  void $ waitUntilSlot 6

  let lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      lenderAddr = Address lenderCred Nothing
      offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = [10]
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
  
  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum
            , lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 8

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum
  offer1 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum

  let exp = slotToBeginPOSIXTime def 8
      loanId1 = txOutRefToLoanId offer1
      activeDatum = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , nextCheckpoints = [exp+10]
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000
        , loanExpiration = exp + 10000
        , loanOutstanding = fromInteger 100_000_000
        , loanId = loanId1
        }

  callEndpoint @"accept-offer" h1 $ 
    AcceptOfferParams
      { acceptOfferBeaconsMinted = 
          [ [("Ask",-1),("Offer",-1),(lenderToken,-1),(loanId1,2),("Active",1),(borrowerToken,1)]
          ]
      , acceptOfferBeaconRedeemers = [ MintActiveBeacon borrowerCred [(ask1,offer1)] ]
      , acceptOfferLoanAddresses = [loanAddr]
      , acceptOfferLoanRedeemers = [ AcceptOffer ]
      , acceptOfferUTxOs = [[ask1,offer1]]
      , acceptOfferCollateralAddresses = [loanAddr]
      , acceptOfferCollateralUTxOs = 
          [ [ ( Just activeDatum
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 100
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId1 1
              )
            ]
          ]
      , acceptOfferCollateralAsInline = True
      , acceptOfferLenderAddresses = [lenderAddr]
      , acceptOfferLenderUTxOs = 
          [ [ ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId1 1
              )
            ]
          ]
      , acceptOfferLenderAsInline = True
      , acceptOfferWithTTL = True
      , acceptOfferScripts = ts
      , acceptOfferWithRefScripts = True
      , acceptOfferSpendRefScript = spendRef
      , acceptOfferMintRefScript = mintRef
      , acceptOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 10

  active1 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 100
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId1 1
                                 )
  let newActiveDatum = activeDatum{ loanOutstanding = 
                                      loanOutstanding activeDatum - fromInteger 50_000_000 
                                  }

  callEndpoint @"make-payment" h1 $ 
    MakePaymentParams
      { makePaymentBeaconsMinted = []
      , makePaymentBeaconRedeemer = BurnBeacons
      , makePaymentLoanAddress = loanAddr
      , makePaymentLoanRedeemer = MakePayment
      , makePaymentUTxOs = [active1]
      , makePaymentCollateralAddress = loanAddr
      , makePaymentCollateralUTxOs = 
          [ ( Just newActiveDatum
            , lovelaceValueOf 3_000_000
           <> (uncurry singleton testToken1) 50
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol borrowerToken 1
           <> singleton beaconCurrencySymbol loanId1 1
            )
          ]
      , makePaymentCollateralAsInline = True
      , makePaymentLenderAddresses = [lenderAddr]
      , makePaymentLenderUTxOs = 
          [ [ ( Just $ PaymentDatum (beaconCurrencySymbol,loanId1)
              , lovelaceValueOf 50_000_000
              )
            ]
          ]
      , makePaymentLenderAsInline = True
      , makePaymentWithTTE = True
      , makePaymentScripts = ts
      , makePaymentWithRefScripts = True
      , makePaymentSpendRefScript = spendRef
      , makePaymentMintRefScript = mintRef
      , makePaymentRefAddress = refAddr
      }

atLeastOneLoansNextCheckpointHasPassed :: DappScripts -> EmulatorTrace ()
atLeastOneLoansNextCheckpointHasPassed ts@DappScripts{..} = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let refAddr = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unValidatorScript spendingValidator
      , createReferenceScriptAddress = refAddr
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOSpendRef
      }

  void $ waitUntilSlot 2

  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unMintingPolicyScript beaconPolicy
      , createReferenceScriptAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOMintRef
      }

  void $ waitUntilSlot 4

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  spendRef <- txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      borrowerToken = credentialAsToken borrowerCred
      askDatum1 = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanTerm = 10000
        , collateral = [testToken1]
        }
      askDatum2 = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 50_000_000
        , loanTerm = 10000
        , collateral = [testToken1]
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  callEndpoint @"create-ask" h1 $
    CreateAskParams
      { createAskBeaconsMinted = [("Ask",2)]
      , createAskBeaconRedeemer = MintAskBeacon borrowerCred
      , createAskLoanAddress = loanAddr
      , createAskUTxOs = 
          [ ( Just askDatum1
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum2
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          ]
      , createAskAsInline = True
      , createAskScripts = ts
      , createAskWithRefScript = True
      , createAskRefScript = mintRef
      , createAskRefAddress = refAddr
      }

  void $ waitUntilSlot 6

  let lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      lenderAddr = Address lenderCred Nothing
      offerDatum1 = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = [10]
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
      offerDatum2 = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 50_000_000
        , loanCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
  
  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum1
            , lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 8

  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum2
            , lovelaceValueOf 55_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 10

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum1
  ask2 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum2
  offer1 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum1
  offer2 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 55_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum2

  let exp = slotToBeginPOSIXTime def 10
      loanId1 = txOutRefToLoanId offer1
      loanId2 = txOutRefToLoanId offer2
      activeDatum1 = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , nextCheckpoints = [exp+10]
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000
        , loanExpiration = exp + 10000
        , loanOutstanding = fromInteger 100_000_000
        , loanId = loanId1
        }
      activeDatum2 = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 50_000_000
        , nextCheckpoints = []
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000
        , loanExpiration = exp + 10000
        , loanOutstanding = fromInteger 50_000_000
        , loanId = loanId2
        }

  callEndpoint @"accept-offer" h1 $ 
    AcceptOfferParams
      { acceptOfferBeaconsMinted = 
          [ [ ("Ask",-2)
            , ("Offer",-2)
            , (lenderToken,-2)
            , (loanId1,2)
            , (loanId2,2)
            , ("Active",2)
            , (borrowerToken,2)
            ]
          ]
      , acceptOfferBeaconRedeemers = 
          [ MintActiveBeacon borrowerCred [(ask1,offer1),(ask2,offer2)] ]
      , acceptOfferLoanAddresses = [loanAddr]
      , acceptOfferLoanRedeemers = [ AcceptOffer ]
      , acceptOfferUTxOs = [[ask1,offer1,ask2,offer2]]
      , acceptOfferCollateralAddresses = [loanAddr]
      , acceptOfferCollateralUTxOs = 
          [ [ ( Just activeDatum1
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 100
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId1 1
              )
            , ( Just activeDatum2
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 50
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId2 1
              )
            ]
          ]
      , acceptOfferCollateralAsInline = True
      , acceptOfferLenderAddresses = [lenderAddr]
      , acceptOfferLenderUTxOs = 
          [ [ ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId1 1
              )
            , ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId2 1
              )
            ]
          ]
      , acceptOfferLenderAsInline = True
      , acceptOfferWithTTL = True
      , acceptOfferScripts = ts
      , acceptOfferWithRefScripts = True
      , acceptOfferSpendRefScript = spendRef
      , acceptOfferMintRefScript = mintRef
      , acceptOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 12

  active1 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 100
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId1 1
                                 )
  active2 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 50
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId2 1
                                 )

  let newActiveDatum1 = activeDatum1{ loanOutstanding = 
                                        loanOutstanding activeDatum1 - fromInteger 50_000_000 
                                    }
  let newActiveDatum2 = activeDatum2{ loanOutstanding = 
                                        loanOutstanding activeDatum2 - fromInteger 25_000_000 
                                    }

  callEndpoint @"make-payment" h1 $ 
    MakePaymentParams
      { makePaymentBeaconsMinted = []
      , makePaymentBeaconRedeemer = BurnBeacons
      , makePaymentLoanAddress = loanAddr
      , makePaymentLoanRedeemer = MakePayment
      , makePaymentUTxOs = [active1,active2]
      , makePaymentCollateralAddress = loanAddr
      , makePaymentCollateralUTxOs = 
          [ ( Just newActiveDatum1
            , lovelaceValueOf 3_000_000
           <> (uncurry singleton testToken1) 50
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol borrowerToken 1
           <> singleton beaconCurrencySymbol loanId1 1
            )
          , ( Just newActiveDatum2
            , lovelaceValueOf 3_000_000
           <> (uncurry singleton testToken1) 25
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol borrowerToken 1
           <> singleton beaconCurrencySymbol loanId2 1
            )
          ]
      , makePaymentCollateralAsInline = True
      , makePaymentLenderAddresses = [lenderAddr]
      , makePaymentLenderUTxOs = 
          [ [ ( Just $ PaymentDatum (beaconCurrencySymbol,loanId1)
              , lovelaceValueOf 50_000_000
              )
            , ( Just $ PaymentDatum (beaconCurrencySymbol,loanId2)
              , lovelaceValueOf 25_000_000
              )
            ]
          ]
      , makePaymentLenderAsInline = True
      , makePaymentWithTTE = True
      , makePaymentScripts = ts
      , makePaymentWithRefScripts = True
      , makePaymentSpendRefScript = spendRef
      , makePaymentMintRefScript = mintRef
      , makePaymentRefAddress = refAddr
      }

onlyPartialPaymentCollateralDatumIsWrong :: DappScripts -> EmulatorTrace ()
onlyPartialPaymentCollateralDatumIsWrong ts@DappScripts{..} = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let refAddr = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unValidatorScript spendingValidator
      , createReferenceScriptAddress = refAddr
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOSpendRef
      }

  void $ waitUntilSlot 2

  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unMintingPolicyScript beaconPolicy
      , createReferenceScriptAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOMintRef
      }

  void $ waitUntilSlot 4

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  spendRef <- txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      borrowerToken = credentialAsToken borrowerCred
      askDatum = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanTerm = 10000
        , collateral = [testToken1]
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  callEndpoint @"create-ask" h1 $
    CreateAskParams
      { createAskBeaconsMinted = [("Ask",1)]
      , createAskBeaconRedeemer = MintAskBeacon borrowerCred
      , createAskLoanAddress = loanAddr
      , createAskUTxOs = 
          [ ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          ]
      , createAskAsInline = True
      , createAskScripts = ts
      , createAskWithRefScript = True
      , createAskRefScript = mintRef
      , createAskRefAddress = refAddr
      }

  void $ waitUntilSlot 6

  let lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      lenderAddr = Address lenderCred Nothing
      offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
  
  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum
            , lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 8

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum
  offer1 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum

  let exp = slotToBeginPOSIXTime def 8
      loanId1 = txOutRefToLoanId offer1
      activeDatum = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , nextCheckpoints = []
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000
        , loanExpiration = exp + 10000
        , loanOutstanding = fromInteger 100_000_000
        , loanId = loanId1
        }

  callEndpoint @"accept-offer" h1 $ 
    AcceptOfferParams
      { acceptOfferBeaconsMinted = 
          [ [("Ask",-1),("Offer",-1),(lenderToken,-1),(loanId1,2),("Active",1),(borrowerToken,1)]
          ]
      , acceptOfferBeaconRedeemers = [ MintActiveBeacon borrowerCred [(ask1,offer1)] ]
      , acceptOfferLoanAddresses = [loanAddr]
      , acceptOfferLoanRedeemers = [ AcceptOffer ]
      , acceptOfferUTxOs = [[ask1,offer1]]
      , acceptOfferCollateralAddresses = [loanAddr]
      , acceptOfferCollateralUTxOs = 
          [ [ ( Just activeDatum
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 100
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId1 1
              )
            ]
          ]
      , acceptOfferCollateralAsInline = True
      , acceptOfferLenderAddresses = [lenderAddr]
      , acceptOfferLenderUTxOs = 
          [ [ ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId1 1
              )
            ]
          ]
      , acceptOfferLenderAsInline = True
      , acceptOfferWithTTL = True
      , acceptOfferScripts = ts
      , acceptOfferWithRefScripts = True
      , acceptOfferSpendRefScript = spendRef
      , acceptOfferMintRefScript = mintRef
      , acceptOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 10

  active1 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 100
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId1 1
                                 )
  let newActiveDatum = activeDatum{ loanOutstanding = 
                                      loanOutstanding activeDatum - fromInteger 40_000_000 
                                  }

  callEndpoint @"make-payment" h1 $ 
    MakePaymentParams
      { makePaymentBeaconsMinted = []
      , makePaymentBeaconRedeemer = BurnBeacons
      , makePaymentLoanAddress = loanAddr
      , makePaymentLoanRedeemer = MakePayment
      , makePaymentUTxOs = [active1]
      , makePaymentCollateralAddress = loanAddr
      , makePaymentCollateralUTxOs = 
          [ ( Just newActiveDatum
            , lovelaceValueOf 3_000_000
           <> (uncurry singleton testToken1) 50
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol borrowerToken 1
           <> singleton beaconCurrencySymbol loanId1 1
            )
          ]
      , makePaymentCollateralAsInline = True
      , makePaymentLenderAddresses = [lenderAddr]
      , makePaymentLenderUTxOs = 
          [ [ ( Just $ PaymentDatum (beaconCurrencySymbol,loanId1)
              , lovelaceValueOf 50_000_000
              )
            ]
          ]
      , makePaymentLenderAsInline = True
      , makePaymentWithTTE = True
      , makePaymentScripts = ts
      , makePaymentWithRefScripts = True
      , makePaymentSpendRefScript = spendRef
      , makePaymentMintRefScript = mintRef
      , makePaymentRefAddress = refAddr
      }

onlyFullPaymentCollateralDatumIsWrong :: DappScripts -> EmulatorTrace ()
onlyFullPaymentCollateralDatumIsWrong ts@DappScripts{..} = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let refAddr = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unValidatorScript spendingValidator
      , createReferenceScriptAddress = refAddr
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOSpendRef
      }

  void $ waitUntilSlot 2

  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unMintingPolicyScript beaconPolicy
      , createReferenceScriptAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOMintRef
      }

  void $ waitUntilSlot 4

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  spendRef <- txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      borrowerToken = credentialAsToken borrowerCred
      askDatum = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanTerm = 10000
        , collateral = [testToken1]
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  callEndpoint @"create-ask" h1 $
    CreateAskParams
      { createAskBeaconsMinted = [("Ask",1)]
      , createAskBeaconRedeemer = MintAskBeacon borrowerCred
      , createAskLoanAddress = loanAddr
      , createAskUTxOs = 
          [ ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          ]
      , createAskAsInline = True
      , createAskScripts = ts
      , createAskWithRefScript = True
      , createAskRefScript = mintRef
      , createAskRefAddress = refAddr
      }

  void $ waitUntilSlot 6

  let lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      lenderAddr = Address lenderCred Nothing
      offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
  
  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum
            , lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 8

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum
  offer1 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum

  let exp = slotToBeginPOSIXTime def 8
      loanId1 = txOutRefToLoanId offer1
      activeDatum = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , nextCheckpoints = []
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000
        , loanExpiration = exp + 10000
        , loanOutstanding = fromInteger 100_000_000
        , loanId = loanId1
        }

  callEndpoint @"accept-offer" h1 $ 
    AcceptOfferParams
      { acceptOfferBeaconsMinted = 
          [ [("Ask",-1),("Offer",-1),(lenderToken,-1),(loanId1,2),("Active",1),(borrowerToken,1)]
          ]
      , acceptOfferBeaconRedeemers = [ MintActiveBeacon borrowerCred [(ask1,offer1)] ]
      , acceptOfferLoanAddresses = [loanAddr]
      , acceptOfferLoanRedeemers = [ AcceptOffer ]
      , acceptOfferUTxOs = [[ask1,offer1]]
      , acceptOfferCollateralAddresses = [loanAddr]
      , acceptOfferCollateralUTxOs = 
          [ [ ( Just activeDatum
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 100
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId1 1
              )
            ]
          ]
      , acceptOfferCollateralAsInline = True
      , acceptOfferLenderAddresses = [lenderAddr]
      , acceptOfferLenderUTxOs = 
          [ [ ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId1 1
              )
            ]
          ]
      , acceptOfferLenderAsInline = True
      , acceptOfferWithTTL = True
      , acceptOfferScripts = ts
      , acceptOfferWithRefScripts = True
      , acceptOfferSpendRefScript = spendRef
      , acceptOfferMintRefScript = mintRef
      , acceptOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 10

  active1 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 100
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId1 1
                                 )
  let newActiveDatum = activeDatum{ loanOutstanding = 
                                      loanOutstanding activeDatum - fromInteger 50_000_000 
                                  }

  callEndpoint @"make-payment" h1 $ 
    MakePaymentParams
      { makePaymentBeaconsMinted = [(borrowerToken,-1)]
      , makePaymentBeaconRedeemer = BurnBeacons
      , makePaymentLoanAddress = loanAddr
      , makePaymentLoanRedeemer = MakePayment
      , makePaymentUTxOs = [active1]
      , makePaymentCollateralAddress = loanAddr
      , makePaymentCollateralUTxOs = 
          [ ( Just newActiveDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol loanId1 1
            )
          ]
      , makePaymentCollateralAsInline = True
      , makePaymentLenderAddresses = [lenderAddr]
      , makePaymentLenderUTxOs = 
          [ [ ( Just $ PaymentDatum (beaconCurrencySymbol,loanId1)
              , lovelaceValueOf 100_000_000
              )
            ]
          ]
      , makePaymentLenderAsInline = True
      , makePaymentWithTTE = True
      , makePaymentScripts = ts
      , makePaymentWithRefScripts = True
      , makePaymentSpendRefScript = spendRef
      , makePaymentMintRefScript = mintRef
      , makePaymentRefAddress = refAddr
      }

atLeastOneCollateralDatumIsWrong :: DappScripts -> EmulatorTrace ()
atLeastOneCollateralDatumIsWrong ts@DappScripts{..} = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let refAddr = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unValidatorScript spendingValidator
      , createReferenceScriptAddress = refAddr
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOSpendRef
      }

  void $ waitUntilSlot 2

  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unMintingPolicyScript beaconPolicy
      , createReferenceScriptAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOMintRef
      }

  void $ waitUntilSlot 4

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  spendRef <- txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      borrowerToken = credentialAsToken borrowerCred
      askDatum1 = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanTerm = 10000
        , collateral = [testToken1]
        }
      askDatum2 = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 50_000_000
        , loanTerm = 10000
        , collateral = [testToken1]
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  callEndpoint @"create-ask" h1 $
    CreateAskParams
      { createAskBeaconsMinted = [("Ask",2)]
      , createAskBeaconRedeemer = MintAskBeacon borrowerCred
      , createAskLoanAddress = loanAddr
      , createAskUTxOs = 
          [ ( Just askDatum1
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum2
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          ]
      , createAskAsInline = True
      , createAskScripts = ts
      , createAskWithRefScript = True
      , createAskRefScript = mintRef
      , createAskRefAddress = refAddr
      }

  void $ waitUntilSlot 6

  let lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      lenderAddr = Address lenderCred Nothing
      offerDatum1 = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
      offerDatum2 = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 50_000_000
        , loanCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
  
  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum1
            , lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 8

  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum2
            , lovelaceValueOf 55_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 10

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum1
  ask2 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum2
  offer1 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum1
  offer2 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 55_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum2

  let exp = slotToBeginPOSIXTime def 10
      loanId1 = txOutRefToLoanId offer1
      loanId2 = txOutRefToLoanId offer2
      activeDatum1 = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , nextCheckpoints = []
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000
        , loanExpiration = exp + 10000
        , loanOutstanding = fromInteger 100_000_000
        , loanId = loanId1
        }
      activeDatum2 = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 50_000_000
        , nextCheckpoints = []
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000
        , loanExpiration = exp + 10000
        , loanOutstanding = fromInteger 50_000_000
        , loanId = loanId2
        }

  callEndpoint @"accept-offer" h1 $ 
    AcceptOfferParams
      { acceptOfferBeaconsMinted = 
          [ [ ("Ask",-2)
            , ("Offer",-2)
            , (lenderToken,-2)
            , (loanId1,2)
            , (loanId2,2)
            , ("Active",2)
            , (borrowerToken,2)
            ]
          ]
      , acceptOfferBeaconRedeemers = 
          [ MintActiveBeacon borrowerCred [(ask1,offer1),(ask2,offer2)] ]
      , acceptOfferLoanAddresses = [loanAddr]
      , acceptOfferLoanRedeemers = [ AcceptOffer ]
      , acceptOfferUTxOs = [[ask1,offer1,ask2,offer2]]
      , acceptOfferCollateralAddresses = [loanAddr]
      , acceptOfferCollateralUTxOs = 
          [ [ ( Just activeDatum1
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 100
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId1 1
              )
            , ( Just activeDatum2
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 50
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId2 1
              )
            ]
          ]
      , acceptOfferCollateralAsInline = True
      , acceptOfferLenderAddresses = [lenderAddr]
      , acceptOfferLenderUTxOs = 
          [ [ ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId1 1
              )
            , ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId2 1
              )
            ]
          ]
      , acceptOfferLenderAsInline = True
      , acceptOfferWithTTL = True
      , acceptOfferScripts = ts
      , acceptOfferWithRefScripts = True
      , acceptOfferSpendRefScript = spendRef
      , acceptOfferMintRefScript = mintRef
      , acceptOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 12

  active1 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 100
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId1 1
                                 )
  active2 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 50
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId2 1
                                 )

  let newActiveDatum1 = activeDatum1{ loanOutstanding = 
                                        loanOutstanding activeDatum1 - fromInteger 50_000_000 
                                    }
  let newActiveDatum2 = activeDatum2{ loanOutstanding = 
                                        loanOutstanding activeDatum2 - fromInteger 23_000_000 
                                    }

  callEndpoint @"make-payment" h1 $ 
    MakePaymentParams
      { makePaymentBeaconsMinted = []
      , makePaymentBeaconRedeemer = BurnBeacons
      , makePaymentLoanAddress = loanAddr
      , makePaymentLoanRedeemer = MakePayment
      , makePaymentUTxOs = [active1,active2]
      , makePaymentCollateralAddress = loanAddr
      , makePaymentCollateralUTxOs = 
          [ ( Just newActiveDatum1
            , lovelaceValueOf 3_000_000
           <> (uncurry singleton testToken1) 50
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol borrowerToken 1
           <> singleton beaconCurrencySymbol loanId1 1
            )
          , ( Just newActiveDatum2
            , lovelaceValueOf 3_000_000
           <> (uncurry singleton testToken1) 25
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol borrowerToken 1
           <> singleton beaconCurrencySymbol loanId2 1
            )
          ]
      , makePaymentCollateralAsInline = True
      , makePaymentLenderAddresses = [lenderAddr]
      , makePaymentLenderUTxOs = 
          [ [ ( Just $ PaymentDatum (beaconCurrencySymbol,loanId1)
              , lovelaceValueOf 50_000_000
              )
            , ( Just $ PaymentDatum (beaconCurrencySymbol,loanId2)
              , lovelaceValueOf 25_000_000
              )
            ]
          ]
      , makePaymentLenderAsInline = True
      , makePaymentWithTTE = True
      , makePaymentScripts = ts
      , makePaymentWithRefScripts = True
      , makePaymentSpendRefScript = spendRef
      , makePaymentMintRefScript = mintRef
      , makePaymentRefAddress = refAddr
      }

onlyFullPaymentBorrowerIdWithdrawn :: DappScripts -> EmulatorTrace ()
onlyFullPaymentBorrowerIdWithdrawn ts@DappScripts{..} = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let refAddr = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unValidatorScript spendingValidator
      , createReferenceScriptAddress = refAddr
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOSpendRef
      }

  void $ waitUntilSlot 2

  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unMintingPolicyScript beaconPolicy
      , createReferenceScriptAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOMintRef
      }

  void $ waitUntilSlot 4

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  spendRef <- txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      borrowerToken = credentialAsToken borrowerCred
      askDatum = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanTerm = 10000
        , collateral = [testToken1]
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  callEndpoint @"create-ask" h1 $
    CreateAskParams
      { createAskBeaconsMinted = [("Ask",1)]
      , createAskBeaconRedeemer = MintAskBeacon borrowerCred
      , createAskLoanAddress = loanAddr
      , createAskUTxOs = 
          [ ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          ]
      , createAskAsInline = True
      , createAskScripts = ts
      , createAskWithRefScript = True
      , createAskRefScript = mintRef
      , createAskRefAddress = refAddr
      }

  void $ waitUntilSlot 6

  let lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      lenderAddr = Address lenderCred Nothing
      offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
  
  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum
            , lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 8

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum
  offer1 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum

  let exp = slotToBeginPOSIXTime def 8
      loanId1 = txOutRefToLoanId offer1
      activeDatum = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , nextCheckpoints = []
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000
        , loanExpiration = exp + 10000
        , loanOutstanding = fromInteger 100_000_000
        , loanId = loanId1
        }

  callEndpoint @"accept-offer" h1 $ 
    AcceptOfferParams
      { acceptOfferBeaconsMinted = 
          [ [("Ask",-1),("Offer",-1),(lenderToken,-1),(loanId1,2),("Active",1),(borrowerToken,1)]
          ]
      , acceptOfferBeaconRedeemers = [ MintActiveBeacon borrowerCred [(ask1,offer1)] ]
      , acceptOfferLoanAddresses = [loanAddr]
      , acceptOfferLoanRedeemers = [ AcceptOffer ]
      , acceptOfferUTxOs = [[ask1,offer1]]
      , acceptOfferCollateralAddresses = [loanAddr]
      , acceptOfferCollateralUTxOs = 
          [ [ ( Just activeDatum
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 100
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId1 1
              )
            ]
          ]
      , acceptOfferCollateralAsInline = True
      , acceptOfferLenderAddresses = [lenderAddr]
      , acceptOfferLenderUTxOs = 
          [ [ ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId1 1
              )
            ]
          ]
      , acceptOfferLenderAsInline = True
      , acceptOfferWithTTL = True
      , acceptOfferScripts = ts
      , acceptOfferWithRefScripts = True
      , acceptOfferSpendRefScript = spendRef
      , acceptOfferMintRefScript = mintRef
      , acceptOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 10

  active1 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 100
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId1 1
                                 )
  let newActiveDatum = activeDatum{ loanOutstanding = 
                                      loanOutstanding activeDatum - fromInteger 100_000_000 
                                  }

  callEndpoint @"make-payment" h1 $ 
    MakePaymentParams
      { makePaymentBeaconsMinted = []
      , makePaymentBeaconRedeemer = BurnBeacons
      , makePaymentLoanAddress = loanAddr
      , makePaymentLoanRedeemer = MakePayment
      , makePaymentUTxOs = [active1]
      , makePaymentCollateralAddress = loanAddr
      , makePaymentCollateralUTxOs = 
          [ ( Just newActiveDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol loanId1 1
            )
          ]
      , makePaymentCollateralAsInline = True
      , makePaymentLenderAddresses = [lenderAddr]
      , makePaymentLenderUTxOs = 
          [ [ ( Just $ PaymentDatum (beaconCurrencySymbol,loanId1)
              , lovelaceValueOf 100_000_000
              )
            ]
          ]
      , makePaymentLenderAsInline = True
      , makePaymentWithTTE = True
      , makePaymentScripts = ts
      , makePaymentWithRefScripts = True
      , makePaymentSpendRefScript = spendRef
      , makePaymentMintRefScript = mintRef
      , makePaymentRefAddress = refAddr
      }

atLeastOneBorrowerIdWithdrawn :: DappScripts -> EmulatorTrace ()
atLeastOneBorrowerIdWithdrawn ts@DappScripts{..} = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let refAddr = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unValidatorScript spendingValidator
      , createReferenceScriptAddress = refAddr
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOSpendRef
      }

  void $ waitUntilSlot 2

  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unMintingPolicyScript beaconPolicy
      , createReferenceScriptAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOMintRef
      }

  void $ waitUntilSlot 4

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  spendRef <- txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      borrowerToken = credentialAsToken borrowerCred
      askDatum1 = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanTerm = 10000
        , collateral = [testToken1]
        }
      askDatum2 = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 50_000_000
        , loanTerm = 10000
        , collateral = [testToken1]
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  callEndpoint @"create-ask" h1 $
    CreateAskParams
      { createAskBeaconsMinted = [("Ask",2)]
      , createAskBeaconRedeemer = MintAskBeacon borrowerCred
      , createAskLoanAddress = loanAddr
      , createAskUTxOs = 
          [ ( Just askDatum1
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum2
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          ]
      , createAskAsInline = True
      , createAskScripts = ts
      , createAskWithRefScript = True
      , createAskRefScript = mintRef
      , createAskRefAddress = refAddr
      }

  void $ waitUntilSlot 6

  let lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      lenderAddr = Address lenderCred Nothing
      offerDatum1 = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
      offerDatum2 = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 50_000_000
        , loanCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
  
  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum1
            , lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 8

  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum2
            , lovelaceValueOf 55_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 10

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum1
  ask2 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum2
  offer1 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum1
  offer2 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 55_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum2

  let exp = slotToBeginPOSIXTime def 10
      loanId1 = txOutRefToLoanId offer1
      loanId2 = txOutRefToLoanId offer2
      activeDatum1 = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , nextCheckpoints = []
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000
        , loanExpiration = exp + 10000
        , loanOutstanding = fromInteger 100_000_000
        , loanId = loanId1
        }
      activeDatum2 = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 50_000_000
        , nextCheckpoints = []
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000
        , loanExpiration = exp + 10000
        , loanOutstanding = fromInteger 50_000_000
        , loanId = loanId2
        }

  callEndpoint @"accept-offer" h1 $ 
    AcceptOfferParams
      { acceptOfferBeaconsMinted = 
          [ [ ("Ask",-2)
            , ("Offer",-2)
            , (lenderToken,-2)
            , (loanId1,2)
            , (loanId2,2)
            , ("Active",2)
            , (borrowerToken,2)
            ]
          ]
      , acceptOfferBeaconRedeemers = 
          [ MintActiveBeacon borrowerCred [(ask1,offer1),(ask2,offer2)] ]
      , acceptOfferLoanAddresses = [loanAddr]
      , acceptOfferLoanRedeemers = [ AcceptOffer ]
      , acceptOfferUTxOs = [[ask1,offer1,ask2,offer2]]
      , acceptOfferCollateralAddresses = [loanAddr]
      , acceptOfferCollateralUTxOs = 
          [ [ ( Just activeDatum1
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 100
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId1 1
              )
            , ( Just activeDatum2
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 50
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId2 1
              )
            ]
          ]
      , acceptOfferCollateralAsInline = True
      , acceptOfferLenderAddresses = [lenderAddr]
      , acceptOfferLenderUTxOs = 
          [ [ ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId1 1
              )
            , ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId2 1
              )
            ]
          ]
      , acceptOfferLenderAsInline = True
      , acceptOfferWithTTL = True
      , acceptOfferScripts = ts
      , acceptOfferWithRefScripts = True
      , acceptOfferSpendRefScript = spendRef
      , acceptOfferMintRefScript = mintRef
      , acceptOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 12

  active1 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 100
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId1 1
                                 )
  active2 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 50
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId2 1
                                 )

  let newActiveDatum1 = activeDatum1{ loanOutstanding = 
                                        loanOutstanding activeDatum1 - fromInteger 100_000_000 
                                    }
  let newActiveDatum2 = activeDatum2{ loanOutstanding = 
                                        loanOutstanding activeDatum2 - fromInteger 50_000_000 
                                    }

  callEndpoint @"make-payment" h1 $ 
    MakePaymentParams
      { makePaymentBeaconsMinted = [(borrowerToken,-1)]
      , makePaymentBeaconRedeemer = BurnBeacons
      , makePaymentLoanAddress = loanAddr
      , makePaymentLoanRedeemer = MakePayment
      , makePaymentUTxOs = [active1,active2]
      , makePaymentCollateralAddress = loanAddr
      , makePaymentCollateralUTxOs = 
          [ ( Just newActiveDatum1
            , lovelaceValueOf 3_000_000
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol loanId1 1
            )
          , ( Just newActiveDatum2
            , lovelaceValueOf 3_000_000
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol loanId2 1
            )
          ]
      , makePaymentCollateralAsInline = True
      , makePaymentLenderAddresses = [lenderAddr]
      , makePaymentLenderUTxOs = 
          [ [ ( Just $ PaymentDatum (beaconCurrencySymbol,loanId1)
              , lovelaceValueOf 100_000_000
              )
            , ( Just $ PaymentDatum (beaconCurrencySymbol,loanId2)
              , lovelaceValueOf 50_000_000
              )
            ]
          ]
      , makePaymentLenderAsInline = True
      , makePaymentWithTTE = True
      , makePaymentScripts = ts
      , makePaymentWithRefScripts = True
      , makePaymentSpendRefScript = spendRef
      , makePaymentMintRefScript = mintRef
      , makePaymentRefAddress = refAddr
      }

onlyFullPaymentCollateralOutputStillHasBorrowerId :: DappScripts -> EmulatorTrace ()
onlyFullPaymentCollateralOutputStillHasBorrowerId ts@DappScripts{..} = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let refAddr = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unValidatorScript spendingValidator
      , createReferenceScriptAddress = refAddr
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOSpendRef
      }

  void $ waitUntilSlot 2

  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unMintingPolicyScript beaconPolicy
      , createReferenceScriptAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOMintRef
      }

  void $ waitUntilSlot 4

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  spendRef <- txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      borrowerToken = credentialAsToken borrowerCred
      askDatum = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanTerm = 10000
        , collateral = [testToken1]
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  callEndpoint @"create-ask" h1 $
    CreateAskParams
      { createAskBeaconsMinted = [("Ask",1)]
      , createAskBeaconRedeemer = MintAskBeacon borrowerCred
      , createAskLoanAddress = loanAddr
      , createAskUTxOs = 
          [ ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          ]
      , createAskAsInline = True
      , createAskScripts = ts
      , createAskWithRefScript = True
      , createAskRefScript = mintRef
      , createAskRefAddress = refAddr
      }

  void $ waitUntilSlot 6

  let lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      lenderAddr = Address lenderCred Nothing
      offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
  
  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum
            , lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 8

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum
  offer1 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum

  let exp = slotToBeginPOSIXTime def 8
      loanId1 = txOutRefToLoanId offer1
      activeDatum = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , nextCheckpoints = []
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000
        , loanExpiration = exp + 10000
        , loanOutstanding = fromInteger 100_000_000
        , loanId = loanId1
        }

  callEndpoint @"accept-offer" h1 $ 
    AcceptOfferParams
      { acceptOfferBeaconsMinted = 
          [ [("Ask",-1),("Offer",-1),(lenderToken,-1),(loanId1,2),("Active",1),(borrowerToken,1)]
          ]
      , acceptOfferBeaconRedeemers = [ MintActiveBeacon borrowerCred [(ask1,offer1)] ]
      , acceptOfferLoanAddresses = [loanAddr]
      , acceptOfferLoanRedeemers = [ AcceptOffer ]
      , acceptOfferUTxOs = [[ask1,offer1]]
      , acceptOfferCollateralAddresses = [loanAddr]
      , acceptOfferCollateralUTxOs = 
          [ [ ( Just activeDatum
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 100
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId1 1
              )
            ]
          ]
      , acceptOfferCollateralAsInline = True
      , acceptOfferLenderAddresses = [lenderAddr]
      , acceptOfferLenderUTxOs = 
          [ [ ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId1 1
              )
            ]
          ]
      , acceptOfferLenderAsInline = True
      , acceptOfferWithTTL = True
      , acceptOfferScripts = ts
      , acceptOfferWithRefScripts = True
      , acceptOfferSpendRefScript = spendRef
      , acceptOfferMintRefScript = mintRef
      , acceptOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 10

  active1 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 100
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId1 1
                                 )
  let newActiveDatum = activeDatum{ loanOutstanding = 
                                      loanOutstanding activeDatum - fromInteger 100_000_000 
                                  }

  callEndpoint @"make-payment" h1 $ 
    MakePaymentParams
      { makePaymentBeaconsMinted = []
      , makePaymentBeaconRedeemer = BurnBeacons
      , makePaymentLoanAddress = loanAddr
      , makePaymentLoanRedeemer = MakePayment
      , makePaymentUTxOs = [active1]
      , makePaymentCollateralAddress = loanAddr
      , makePaymentCollateralUTxOs = 
          [ ( Just newActiveDatum
            , lovelaceValueOf 3_000_000
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol borrowerToken 1
           <> singleton beaconCurrencySymbol loanId1 1
            )
          ]
      , makePaymentCollateralAsInline = True
      , makePaymentLenderAddresses = [lenderAddr]
      , makePaymentLenderUTxOs = 
          [ [ ( Just $ PaymentDatum (beaconCurrencySymbol,loanId1)
              , lovelaceValueOf 100_000_000
              )
            ]
          ]
      , makePaymentLenderAsInline = True
      , makePaymentWithTTE = True
      , makePaymentScripts = ts
      , makePaymentWithRefScripts = True
      , makePaymentSpendRefScript = spendRef
      , makePaymentMintRefScript = mintRef
      , makePaymentRefAddress = refAddr
      }

atLeastOneFullPaymentCollateralOutputStillHasBorrowerId :: DappScripts -> EmulatorTrace ()
atLeastOneFullPaymentCollateralOutputStillHasBorrowerId ts@DappScripts{..} = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let refAddr = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unValidatorScript spendingValidator
      , createReferenceScriptAddress = refAddr
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOSpendRef
      }

  void $ waitUntilSlot 2

  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unMintingPolicyScript beaconPolicy
      , createReferenceScriptAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOMintRef
      }

  void $ waitUntilSlot 4

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  spendRef <- txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      borrowerToken = credentialAsToken borrowerCred
      askDatum1 = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanTerm = 10000
        , collateral = [testToken1]
        }
      askDatum2 = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 50_000_000
        , loanTerm = 10000
        , collateral = [testToken1]
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  callEndpoint @"create-ask" h1 $
    CreateAskParams
      { createAskBeaconsMinted = [("Ask",2)]
      , createAskBeaconRedeemer = MintAskBeacon borrowerCred
      , createAskLoanAddress = loanAddr
      , createAskUTxOs = 
          [ ( Just askDatum1
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum2
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          ]
      , createAskAsInline = True
      , createAskScripts = ts
      , createAskWithRefScript = True
      , createAskRefScript = mintRef
      , createAskRefAddress = refAddr
      }

  void $ waitUntilSlot 6

  let lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      lenderAddr = Address lenderCred Nothing
      offerDatum1 = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
      offerDatum2 = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 50_000_000
        , loanCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
  
  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum1
            , lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 8

  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum2
            , lovelaceValueOf 55_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 10

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum1
  ask2 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum2
  offer1 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum1
  offer2 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 55_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum2

  let exp = slotToBeginPOSIXTime def 10
      loanId1 = txOutRefToLoanId offer1
      loanId2 = txOutRefToLoanId offer2
      activeDatum1 = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , nextCheckpoints = []
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000
        , loanExpiration = exp + 10000
        , loanOutstanding = fromInteger 100_000_000
        , loanId = loanId1
        }
      activeDatum2 = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 50_000_000
        , nextCheckpoints = []
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000
        , loanExpiration = exp + 10000
        , loanOutstanding = fromInteger 50_000_000
        , loanId = loanId2
        }

  callEndpoint @"accept-offer" h1 $ 
    AcceptOfferParams
      { acceptOfferBeaconsMinted = 
          [ [ ("Ask",-2)
            , ("Offer",-2)
            , (lenderToken,-2)
            , (loanId1,2)
            , (loanId2,2)
            , ("Active",2)
            , (borrowerToken,2)
            ]
          ]
      , acceptOfferBeaconRedeemers = 
          [ MintActiveBeacon borrowerCred [(ask1,offer1),(ask2,offer2)] ]
      , acceptOfferLoanAddresses = [loanAddr]
      , acceptOfferLoanRedeemers = [ AcceptOffer ]
      , acceptOfferUTxOs = [[ask1,offer1,ask2,offer2]]
      , acceptOfferCollateralAddresses = [loanAddr]
      , acceptOfferCollateralUTxOs = 
          [ [ ( Just activeDatum1
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 100
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId1 1
              )
            , ( Just activeDatum2
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 50
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId2 1
              )
            ]
          ]
      , acceptOfferCollateralAsInline = True
      , acceptOfferLenderAddresses = [lenderAddr]
      , acceptOfferLenderUTxOs = 
          [ [ ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId1 1
              )
            , ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId2 1
              )
            ]
          ]
      , acceptOfferLenderAsInline = True
      , acceptOfferWithTTL = True
      , acceptOfferScripts = ts
      , acceptOfferWithRefScripts = True
      , acceptOfferSpendRefScript = spendRef
      , acceptOfferMintRefScript = mintRef
      , acceptOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 12

  active1 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 100
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId1 1
                                 )
  active2 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 50
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId2 1
                                 )

  let newActiveDatum1 = activeDatum1{ loanOutstanding = 
                                        loanOutstanding activeDatum1 - fromInteger 100_000_000 
                                    }
  let newActiveDatum2 = activeDatum2{ loanOutstanding = 
                                        loanOutstanding activeDatum2 - fromInteger 50_000_000 
                                    }

  callEndpoint @"make-payment" h1 $ 
    MakePaymentParams
      { makePaymentBeaconsMinted = [(borrowerToken,-1)]
      , makePaymentBeaconRedeemer = BurnBeacons
      , makePaymentLoanAddress = loanAddr
      , makePaymentLoanRedeemer = MakePayment
      , makePaymentUTxOs = [active1,active2]
      , makePaymentCollateralAddress = loanAddr
      , makePaymentCollateralUTxOs = 
          [ ( Just newActiveDatum1
            , lovelaceValueOf 3_000_000
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol borrowerToken 1
           <> singleton beaconCurrencySymbol loanId1 1
            )
          , ( Just newActiveDatum2
            , lovelaceValueOf 3_000_000
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol loanId2 1
            )
          ]
      , makePaymentCollateralAsInline = True
      , makePaymentLenderAddresses = [lenderAddr]
      , makePaymentLenderUTxOs = 
          [ [ ( Just $ PaymentDatum (beaconCurrencySymbol,loanId1)
              , lovelaceValueOf 100_000_000
              )
            , ( Just $ PaymentDatum (beaconCurrencySymbol,loanId2)
              , lovelaceValueOf 50_000_000
              )
            ]
          ]
      , makePaymentLenderAsInline = True
      , makePaymentWithTTE = True
      , makePaymentScripts = ts
      , makePaymentWithRefScripts = True
      , makePaymentSpendRefScript = spendRef
      , makePaymentMintRefScript = mintRef
      , makePaymentRefAddress = refAddr
      }

mixedFullPaymentBorrowerIdWithdrawnInsteadOfBurned :: DappScripts -> EmulatorTrace ()
mixedFullPaymentBorrowerIdWithdrawnInsteadOfBurned ts@DappScripts{..} = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let refAddr = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unValidatorScript spendingValidator
      , createReferenceScriptAddress = refAddr
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOSpendRef
      }

  void $ waitUntilSlot 2

  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unMintingPolicyScript beaconPolicy
      , createReferenceScriptAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOMintRef
      }

  void $ waitUntilSlot 4

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  spendRef <- txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      borrowerToken = credentialAsToken borrowerCred
      askDatum1 = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanTerm = 10000
        , collateral = [testToken1]
        }
      askDatum2 = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 50_000_000
        , loanTerm = 10000
        , collateral = [testToken1]
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  callEndpoint @"create-ask" h1 $
    CreateAskParams
      { createAskBeaconsMinted = [("Ask",2)]
      , createAskBeaconRedeemer = MintAskBeacon borrowerCred
      , createAskLoanAddress = loanAddr
      , createAskUTxOs = 
          [ ( Just askDatum1
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum2
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          ]
      , createAskAsInline = True
      , createAskScripts = ts
      , createAskWithRefScript = True
      , createAskRefScript = mintRef
      , createAskRefAddress = refAddr
      }

  void $ waitUntilSlot 6

  let lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      lenderAddr = Address lenderCred Nothing
      offerDatum1 = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
      offerDatum2 = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 50_000_000
        , loanCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
  
  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum1
            , lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 8

  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum2
            , lovelaceValueOf 55_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 10

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum1
  ask2 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum2
  offer1 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum1
  offer2 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 55_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum2

  let exp = slotToBeginPOSIXTime def 10
      loanId1 = txOutRefToLoanId offer1
      loanId2 = txOutRefToLoanId offer2
      activeDatum1 = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , nextCheckpoints = []
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000
        , loanExpiration = exp + 10000
        , loanOutstanding = fromInteger 100_000_000
        , loanId = loanId1
        }
      activeDatum2 = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 50_000_000
        , nextCheckpoints = []
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000
        , loanExpiration = exp + 10000
        , loanOutstanding = fromInteger 50_000_000
        , loanId = loanId2
        }

  callEndpoint @"accept-offer" h1 $ 
    AcceptOfferParams
      { acceptOfferBeaconsMinted = 
          [ [ ("Ask",-2)
            , ("Offer",-2)
            , (lenderToken,-2)
            , (loanId1,2)
            , (loanId2,2)
            , ("Active",2)
            , (borrowerToken,2)
            ]
          ]
      , acceptOfferBeaconRedeemers = 
          [ MintActiveBeacon borrowerCred [(ask1,offer1),(ask2,offer2)] ]
      , acceptOfferLoanAddresses = [loanAddr]
      , acceptOfferLoanRedeemers = [ AcceptOffer ]
      , acceptOfferUTxOs = [[ask1,offer1,ask2,offer2]]
      , acceptOfferCollateralAddresses = [loanAddr]
      , acceptOfferCollateralUTxOs = 
          [ [ ( Just activeDatum1
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 100
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId1 1
              )
            , ( Just activeDatum2
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 50
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId2 1
              )
            ]
          ]
      , acceptOfferCollateralAsInline = True
      , acceptOfferLenderAddresses = [lenderAddr]
      , acceptOfferLenderUTxOs = 
          [ [ ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId1 1
              )
            , ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId2 1
              )
            ]
          ]
      , acceptOfferLenderAsInline = True
      , acceptOfferWithTTL = True
      , acceptOfferScripts = ts
      , acceptOfferWithRefScripts = True
      , acceptOfferSpendRefScript = spendRef
      , acceptOfferMintRefScript = mintRef
      , acceptOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 12

  active1 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 100
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId1 1
                                 )
  active2 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 50
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId2 1
                                 )

  let newActiveDatum1 = activeDatum1{ loanOutstanding = 
                                        loanOutstanding activeDatum1 - fromInteger 100_000_000 
                                    }
  let newActiveDatum2 = activeDatum2{ loanOutstanding = 
                                        loanOutstanding activeDatum2 - fromInteger 25_000_000 
                                    }

  callEndpoint @"make-payment" h1 $ 
    MakePaymentParams
      { makePaymentBeaconsMinted = []
      , makePaymentBeaconRedeemer = BurnBeacons
      , makePaymentLoanAddress = loanAddr
      , makePaymentLoanRedeemer = MakePayment
      , makePaymentUTxOs = [active1,active2]
      , makePaymentCollateralAddress = loanAddr
      , makePaymentCollateralUTxOs = 
          [ ( Just newActiveDatum1
            , lovelaceValueOf 3_000_000
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol loanId1 1
            )
          , ( Just newActiveDatum2
            , lovelaceValueOf 3_000_000
           <> (uncurry singleton testToken1) 25
           <> singleton beaconCurrencySymbol borrowerToken 1
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol loanId2 1
            )
          ]
      , makePaymentCollateralAsInline = True
      , makePaymentLenderAddresses = [lenderAddr]
      , makePaymentLenderUTxOs = 
          [ [ ( Just $ PaymentDatum (beaconCurrencySymbol,loanId1)
              , lovelaceValueOf 100_000_000
              )
            , ( Just $ PaymentDatum (beaconCurrencySymbol,loanId2)
              , lovelaceValueOf 25_000_000
              )
            ]
          ]
      , makePaymentLenderAsInline = True
      , makePaymentWithTTE = True
      , makePaymentScripts = ts
      , makePaymentWithRefScripts = True
      , makePaymentSpendRefScript = spendRef
      , makePaymentMintRefScript = mintRef
      , makePaymentRefAddress = refAddr
      }

mixedFullPaymentBorrowerIdGroupedWithPartialCollateralOutput :: DappScripts -> EmulatorTrace ()
mixedFullPaymentBorrowerIdGroupedWithPartialCollateralOutput ts@DappScripts{..} = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let refAddr = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unValidatorScript spendingValidator
      , createReferenceScriptAddress = refAddr
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOSpendRef
      }

  void $ waitUntilSlot 2

  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unMintingPolicyScript beaconPolicy
      , createReferenceScriptAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOMintRef
      }

  void $ waitUntilSlot 4

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  spendRef <- txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      borrowerToken = credentialAsToken borrowerCred
      askDatum1 = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanTerm = 10000
        , collateral = [testToken1]
        }
      askDatum2 = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 50_000_000
        , loanTerm = 10000
        , collateral = [testToken1]
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  callEndpoint @"create-ask" h1 $
    CreateAskParams
      { createAskBeaconsMinted = [("Ask",2)]
      , createAskBeaconRedeemer = MintAskBeacon borrowerCred
      , createAskLoanAddress = loanAddr
      , createAskUTxOs = 
          [ ( Just askDatum1
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum2
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          ]
      , createAskAsInline = True
      , createAskScripts = ts
      , createAskWithRefScript = True
      , createAskRefScript = mintRef
      , createAskRefAddress = refAddr
      }

  void $ waitUntilSlot 6

  let lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      lenderAddr = Address lenderCred Nothing
      offerDatum1 = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
      offerDatum2 = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 50_000_000
        , loanCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
  
  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum1
            , lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 8

  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum2
            , lovelaceValueOf 55_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 10

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum1
  ask2 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum2
  offer1 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum1
  offer2 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 55_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum2

  let exp = slotToBeginPOSIXTime def 10
      loanId1 = txOutRefToLoanId offer1
      loanId2 = txOutRefToLoanId offer2
      activeDatum1 = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , nextCheckpoints = []
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000
        , loanExpiration = exp + 10000
        , loanOutstanding = fromInteger 100_000_000
        , loanId = loanId1
        }
      activeDatum2 = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 50_000_000
        , nextCheckpoints = []
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000
        , loanExpiration = exp + 10000
        , loanOutstanding = fromInteger 50_000_000
        , loanId = loanId2
        }

  callEndpoint @"accept-offer" h1 $ 
    AcceptOfferParams
      { acceptOfferBeaconsMinted = 
          [ [ ("Ask",-2)
            , ("Offer",-2)
            , (lenderToken,-2)
            , (loanId1,2)
            , (loanId2,2)
            , ("Active",2)
            , (borrowerToken,2)
            ]
          ]
      , acceptOfferBeaconRedeemers = 
          [ MintActiveBeacon borrowerCred [(ask1,offer1),(ask2,offer2)] ]
      , acceptOfferLoanAddresses = [loanAddr]
      , acceptOfferLoanRedeemers = [ AcceptOffer ]
      , acceptOfferUTxOs = [[ask1,offer1,ask2,offer2]]
      , acceptOfferCollateralAddresses = [loanAddr]
      , acceptOfferCollateralUTxOs = 
          [ [ ( Just activeDatum1
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 100
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId1 1
              )
            , ( Just activeDatum2
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 50
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId2 1
              )
            ]
          ]
      , acceptOfferCollateralAsInline = True
      , acceptOfferLenderAddresses = [lenderAddr]
      , acceptOfferLenderUTxOs = 
          [ [ ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId1 1
              )
            , ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId2 1
              )
            ]
          ]
      , acceptOfferLenderAsInline = True
      , acceptOfferWithTTL = True
      , acceptOfferScripts = ts
      , acceptOfferWithRefScripts = True
      , acceptOfferSpendRefScript = spendRef
      , acceptOfferMintRefScript = mintRef
      , acceptOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 12

  active1 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 100
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId1 1
                                 )
  active2 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 50
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId2 1
                                 )

  let newActiveDatum1 = activeDatum1{ loanOutstanding = 
                                        loanOutstanding activeDatum1 - fromInteger 100_000_000 
                                    }
  let newActiveDatum2 = activeDatum2{ loanOutstanding = 
                                        loanOutstanding activeDatum2 - fromInteger 25_000_000 
                                    }

  callEndpoint @"make-payment" h1 $ 
    MakePaymentParams
      { makePaymentBeaconsMinted = []
      , makePaymentBeaconRedeemer = BurnBeacons
      , makePaymentLoanAddress = loanAddr
      , makePaymentLoanRedeemer = MakePayment
      , makePaymentUTxOs = [active1,active2]
      , makePaymentCollateralAddress = loanAddr
      , makePaymentCollateralUTxOs = 
          [ ( Just newActiveDatum1
            , lovelaceValueOf 3_000_000
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol loanId1 1
            )
          , ( Just newActiveDatum2
            , lovelaceValueOf 3_000_000
           <> (uncurry singleton testToken1) 25
           <> singleton beaconCurrencySymbol borrowerToken 2
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol loanId2 1
            )
          ]
      , makePaymentCollateralAsInline = True
      , makePaymentLenderAddresses = [lenderAddr]
      , makePaymentLenderUTxOs = 
          [ [ ( Just $ PaymentDatum (beaconCurrencySymbol,loanId1)
              , lovelaceValueOf 100_000_000
              )
            , ( Just $ PaymentDatum (beaconCurrencySymbol,loanId2)
              , lovelaceValueOf 25_000_000
              )
            ]
          ]
      , makePaymentLenderAsInline = True
      , makePaymentWithTTE = True
      , makePaymentScripts = ts
      , makePaymentWithRefScripts = True
      , makePaymentSpendRefScript = spendRef
      , makePaymentMintRefScript = mintRef
      , makePaymentRefAddress = refAddr
      }

tooMuchCollateralTakenDuringOnlyPartialPayment :: DappScripts -> EmulatorTrace ()
tooMuchCollateralTakenDuringOnlyPartialPayment ts@DappScripts{..} = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let refAddr = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unValidatorScript spendingValidator
      , createReferenceScriptAddress = refAddr
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOSpendRef
      }

  void $ waitUntilSlot 2

  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unMintingPolicyScript beaconPolicy
      , createReferenceScriptAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOMintRef
      }

  void $ waitUntilSlot 4

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  spendRef <- txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      borrowerToken = credentialAsToken borrowerCred
      askDatum = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanTerm = 10000
        , collateral = [testToken1]
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  callEndpoint @"create-ask" h1 $
    CreateAskParams
      { createAskBeaconsMinted = [("Ask",1)]
      , createAskBeaconRedeemer = MintAskBeacon borrowerCred
      , createAskLoanAddress = loanAddr
      , createAskUTxOs = 
          [ ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          ]
      , createAskAsInline = True
      , createAskScripts = ts
      , createAskWithRefScript = True
      , createAskRefScript = mintRef
      , createAskRefAddress = refAddr
      }

  void $ waitUntilSlot 6

  let lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      lenderAddr = Address lenderCred Nothing
      offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
  
  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum
            , lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 8

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum
  offer1 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum

  let exp = slotToBeginPOSIXTime def 8
      loanId1 = txOutRefToLoanId offer1
      activeDatum = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , nextCheckpoints = []
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000
        , loanExpiration = exp + 10000
        , loanOutstanding = fromInteger 100_000_000
        , loanId = loanId1
        }

  callEndpoint @"accept-offer" h1 $ 
    AcceptOfferParams
      { acceptOfferBeaconsMinted = 
          [ [("Ask",-1),("Offer",-1),(lenderToken,-1),(loanId1,2),("Active",1),(borrowerToken,1)]
          ]
      , acceptOfferBeaconRedeemers = [ MintActiveBeacon borrowerCred [(ask1,offer1)] ]
      , acceptOfferLoanAddresses = [loanAddr]
      , acceptOfferLoanRedeemers = [ AcceptOffer ]
      , acceptOfferUTxOs = [[ask1,offer1]]
      , acceptOfferCollateralAddresses = [loanAddr]
      , acceptOfferCollateralUTxOs = 
          [ [ ( Just activeDatum
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 100
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId1 1
              )
            ]
          ]
      , acceptOfferCollateralAsInline = True
      , acceptOfferLenderAddresses = [lenderAddr]
      , acceptOfferLenderUTxOs = 
          [ [ ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId1 1
              )
            ]
          ]
      , acceptOfferLenderAsInline = True
      , acceptOfferWithTTL = True
      , acceptOfferScripts = ts
      , acceptOfferWithRefScripts = True
      , acceptOfferSpendRefScript = spendRef
      , acceptOfferMintRefScript = mintRef
      , acceptOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 10

  active1 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 100
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId1 1
                                 )
  let newActiveDatum = activeDatum{ loanOutstanding = 
                                      loanOutstanding activeDatum - fromInteger 50_000_000 
                                  }

  callEndpoint @"make-payment" h1 $ 
    MakePaymentParams
      { makePaymentBeaconsMinted = []
      , makePaymentBeaconRedeemer = BurnBeacons
      , makePaymentLoanAddress = loanAddr
      , makePaymentLoanRedeemer = MakePayment
      , makePaymentUTxOs = [active1]
      , makePaymentCollateralAddress = loanAddr
      , makePaymentCollateralUTxOs = 
          [ ( Just newActiveDatum
            , lovelaceValueOf 3_000_000
           <> (uncurry singleton testToken1) 49
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol borrowerToken 1
           <> singleton beaconCurrencySymbol loanId1 1
            )
          ]
      , makePaymentCollateralAsInline = True
      , makePaymentLenderAddresses = [lenderAddr]
      , makePaymentLenderUTxOs = 
          [ [ ( Just $ PaymentDatum (beaconCurrencySymbol,loanId1)
              , lovelaceValueOf 50_000_000
              )
            ]
          ]
      , makePaymentLenderAsInline = True
      , makePaymentWithTTE = True
      , makePaymentScripts = ts
      , makePaymentWithRefScripts = True
      , makePaymentSpendRefScript = spendRef
      , makePaymentMintRefScript = mintRef
      , makePaymentRefAddress = refAddr
      }

tooMuchCollateralTakenDuringAtLeastOnePartialPayment :: DappScripts -> EmulatorTrace ()
tooMuchCollateralTakenDuringAtLeastOnePartialPayment ts@DappScripts{..} = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let refAddr = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unValidatorScript spendingValidator
      , createReferenceScriptAddress = refAddr
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOSpendRef
      }

  void $ waitUntilSlot 2

  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unMintingPolicyScript beaconPolicy
      , createReferenceScriptAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOMintRef
      }

  void $ waitUntilSlot 4

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  spendRef <- txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      borrowerToken = credentialAsToken borrowerCred
      askDatum1 = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanTerm = 10000
        , collateral = [testToken1]
        }
      askDatum2 = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 50_000_000
        , loanTerm = 10000
        , collateral = [testToken1]
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  callEndpoint @"create-ask" h1 $
    CreateAskParams
      { createAskBeaconsMinted = [("Ask",2)]
      , createAskBeaconRedeemer = MintAskBeacon borrowerCred
      , createAskLoanAddress = loanAddr
      , createAskUTxOs = 
          [ ( Just askDatum1
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum2
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          ]
      , createAskAsInline = True
      , createAskScripts = ts
      , createAskWithRefScript = True
      , createAskRefScript = mintRef
      , createAskRefAddress = refAddr
      }

  void $ waitUntilSlot 6

  let lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      lenderAddr = Address lenderCred Nothing
      offerDatum1 = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
      offerDatum2 = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 50_000_000
        , loanCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
  
  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum1
            , lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 8

  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum2
            , lovelaceValueOf 55_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 10

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum1
  ask2 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum2
  offer1 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum1
  offer2 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 55_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum2

  let exp = slotToBeginPOSIXTime def 10
      loanId1 = txOutRefToLoanId offer1
      loanId2 = txOutRefToLoanId offer2
      activeDatum1 = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , nextCheckpoints = []
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000
        , loanExpiration = exp + 10000
        , loanOutstanding = fromInteger 100_000_000
        , loanId = loanId1
        }
      activeDatum2 = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 50_000_000
        , nextCheckpoints = []
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000
        , loanExpiration = exp + 10000
        , loanOutstanding = fromInteger 50_000_000
        , loanId = loanId2
        }

  callEndpoint @"accept-offer" h1 $ 
    AcceptOfferParams
      { acceptOfferBeaconsMinted = 
          [ [ ("Ask",-2)
            , ("Offer",-2)
            , (lenderToken,-2)
            , (loanId1,2)
            , (loanId2,2)
            , ("Active",2)
            , (borrowerToken,2)
            ]
          ]
      , acceptOfferBeaconRedeemers = 
          [ MintActiveBeacon borrowerCred [(ask1,offer1),(ask2,offer2)] ]
      , acceptOfferLoanAddresses = [loanAddr]
      , acceptOfferLoanRedeemers = [ AcceptOffer ]
      , acceptOfferUTxOs = [[ask1,offer1,ask2,offer2]]
      , acceptOfferCollateralAddresses = [loanAddr]
      , acceptOfferCollateralUTxOs = 
          [ [ ( Just activeDatum1
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 100
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId1 1
              )
            , ( Just activeDatum2
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 50
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId2 1
              )
            ]
          ]
      , acceptOfferCollateralAsInline = True
      , acceptOfferLenderAddresses = [lenderAddr]
      , acceptOfferLenderUTxOs = 
          [ [ ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId1 1
              )
            , ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId2 1
              )
            ]
          ]
      , acceptOfferLenderAsInline = True
      , acceptOfferWithTTL = True
      , acceptOfferScripts = ts
      , acceptOfferWithRefScripts = True
      , acceptOfferSpendRefScript = spendRef
      , acceptOfferMintRefScript = mintRef
      , acceptOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 12

  active1 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 100
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId1 1
                                 )
  active2 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 50
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId2 1
                                 )

  let newActiveDatum1 = activeDatum1{ loanOutstanding = 
                                        loanOutstanding activeDatum1 - fromInteger 50_000_000 
                                    }
  let newActiveDatum2 = activeDatum2{ loanOutstanding = 
                                        loanOutstanding activeDatum2 - fromInteger 25_000_000 
                                    }

  callEndpoint @"make-payment" h1 $ 
    MakePaymentParams
      { makePaymentBeaconsMinted = []
      , makePaymentBeaconRedeemer = BurnBeacons
      , makePaymentLoanAddress = loanAddr
      , makePaymentLoanRedeemer = MakePayment
      , makePaymentUTxOs = [active1,active2]
      , makePaymentCollateralAddress = loanAddr
      , makePaymentCollateralUTxOs = 
          [ ( Just newActiveDatum1
            , lovelaceValueOf 3_000_000
           <> (uncurry singleton testToken1) 49
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol borrowerToken 1
           <> singleton beaconCurrencySymbol loanId1 1
            )
          , ( Just newActiveDatum2
            , lovelaceValueOf 3_000_000
           <> (uncurry singleton testToken1) 25
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol borrowerToken 1
           <> singleton beaconCurrencySymbol loanId2 1
            )
          ]
      , makePaymentCollateralAsInline = True
      , makePaymentLenderAddresses = [lenderAddr]
      , makePaymentLenderUTxOs = 
          [ [ ( Just $ PaymentDatum (beaconCurrencySymbol,loanId1)
              , lovelaceValueOf 50_000_000
              )
            , ( Just $ PaymentDatum (beaconCurrencySymbol,loanId2)
              , lovelaceValueOf 25_000_000
              )
            ]
          ]
      , makePaymentLenderAsInline = True
      , makePaymentWithTTE = True
      , makePaymentScripts = ts
      , makePaymentWithRefScripts = True
      , makePaymentSpendRefScript = spendRef
      , makePaymentMintRefScript = mintRef
      , makePaymentRefAddress = refAddr
      }

successfullyMakeSinglePartialPaymentOnLoanWithMixedCollateral :: DappScripts -> EmulatorTrace ()
successfullyMakeSinglePartialPaymentOnLoanWithMixedCollateral ts@DappScripts{..} = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let refAddr = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unValidatorScript spendingValidator
      , createReferenceScriptAddress = refAddr
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOSpendRef
      }

  void $ waitUntilSlot 2

  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unMintingPolicyScript beaconPolicy
      , createReferenceScriptAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOMintRef
      }

  void $ waitUntilSlot 4

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  spendRef <- txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      borrowerToken = credentialAsToken borrowerCred
      askDatum = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanTerm = 10000
        , collateral = [testToken1,testToken2]
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  callEndpoint @"create-ask" h1 $
    CreateAskParams
      { createAskBeaconsMinted = [("Ask",1)]
      , createAskBeaconRedeemer = MintAskBeacon borrowerCred
      , createAskLoanAddress = loanAddr
      , createAskUTxOs = 
          [ ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          ]
      , createAskAsInline = True
      , createAskScripts = ts
      , createAskWithRefScript = True
      , createAskRefScript = mintRef
      , createAskRefAddress = refAddr
      }

  void $ waitUntilSlot 6

  let lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      lenderAddr = Address lenderCred Nothing
      offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = 
            [ (testToken1,unsafeRatio 1 1_000_000)
            , (testToken2,unsafeRatio 2 1_000_000)
            ]
        , claimPeriod = 10000
        }
  
  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum
            , lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 8

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum
  offer1 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum

  let exp = slotToBeginPOSIXTime def 8
      loanId1 = txOutRefToLoanId offer1
      activeDatum = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , nextCheckpoints = []
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = 
            [ (testToken1,unsafeRatio 1 1_000_000)
            , (testToken2, unsafeRatio 2 1_000_000)
            ]
        , claimExpiration = exp + 10000 + 10000
        , loanExpiration = exp + 10000
        , loanOutstanding = fromInteger 100_000_000
        , loanId = loanId1
        }

  callEndpoint @"accept-offer" h1 $ 
    AcceptOfferParams
      { acceptOfferBeaconsMinted = 
          [ [("Ask",-1),("Offer",-1),(lenderToken,-1),(loanId1,2),("Active",1),(borrowerToken,1)]
          ]
      , acceptOfferBeaconRedeemers = [ MintActiveBeacon borrowerCred [(ask1,offer1)] ]
      , acceptOfferLoanAddresses = [loanAddr]
      , acceptOfferLoanRedeemers = [ AcceptOffer ]
      , acceptOfferUTxOs = [[ask1,offer1]]
      , acceptOfferCollateralAddresses = [loanAddr]
      , acceptOfferCollateralUTxOs = 
          [ [ ( Just activeDatum
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 50
             <> (uncurry singleton testToken2) 100
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId1 1
              )
            ]
          ]
      , acceptOfferCollateralAsInline = True
      , acceptOfferLenderAddresses = [lenderAddr]
      , acceptOfferLenderUTxOs = 
          [ [ ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId1 1
              )
            ]
          ]
      , acceptOfferLenderAsInline = True
      , acceptOfferWithTTL = True
      , acceptOfferScripts = ts
      , acceptOfferWithRefScripts = True
      , acceptOfferSpendRefScript = spendRef
      , acceptOfferMintRefScript = mintRef
      , acceptOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 10

  active1 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 50
                                <> (uncurry singleton testToken2) 100
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId1 1
                                 )
  let newActiveDatum = activeDatum{ loanOutstanding = 
                                      loanOutstanding activeDatum - fromInteger 50_000_000 
                                  }

  callEndpoint @"make-payment" h1 $ 
    MakePaymentParams
      { makePaymentBeaconsMinted = []
      , makePaymentBeaconRedeemer = BurnBeacons
      , makePaymentLoanAddress = loanAddr
      , makePaymentLoanRedeemer = MakePayment
      , makePaymentUTxOs = [active1]
      , makePaymentCollateralAddress = loanAddr
      , makePaymentCollateralUTxOs = 
          [ ( Just newActiveDatum
            , lovelaceValueOf 3_000_000
           <> (uncurry singleton testToken1) 50
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol borrowerToken 1
           <> singleton beaconCurrencySymbol loanId1 1
            )
          ]
      , makePaymentCollateralAsInline = True
      , makePaymentLenderAddresses = [lenderAddr]
      , makePaymentLenderUTxOs = 
          [ [ ( Just $ PaymentDatum (beaconCurrencySymbol,loanId1)
              , lovelaceValueOf 50_000_000
              )
            ]
          ]
      , makePaymentLenderAsInline = True
      , makePaymentWithTTE = True
      , makePaymentScripts = ts
      , makePaymentWithRefScripts = True
      , makePaymentSpendRefScript = spendRef
      , makePaymentMintRefScript = mintRef
      , makePaymentRefAddress = refAddr
      }

successfullySwapOutCollateral :: DappScripts -> EmulatorTrace ()
successfullySwapOutCollateral ts@DappScripts{..} = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let refAddr = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unValidatorScript spendingValidator
      , createReferenceScriptAddress = refAddr
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOSpendRef
      }

  void $ waitUntilSlot 2

  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unMintingPolicyScript beaconPolicy
      , createReferenceScriptAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOMintRef
      }

  void $ waitUntilSlot 4

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  spendRef <- txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      borrowerToken = credentialAsToken borrowerCred
      askDatum = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanTerm = 10000
        , collateral = [testToken1,testToken2]
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  callEndpoint @"create-ask" h1 $
    CreateAskParams
      { createAskBeaconsMinted = [("Ask",1)]
      , createAskBeaconRedeemer = MintAskBeacon borrowerCred
      , createAskLoanAddress = loanAddr
      , createAskUTxOs = 
          [ ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          ]
      , createAskAsInline = True
      , createAskScripts = ts
      , createAskWithRefScript = True
      , createAskRefScript = mintRef
      , createAskRefAddress = refAddr
      }

  void $ waitUntilSlot 6

  let lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      lenderAddr = Address lenderCred Nothing
      offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = 
            [ (testToken1,unsafeRatio 1 1_000_000)
            , (testToken2,unsafeRatio 2 1_000_000)
            ]
        , claimPeriod = 10000
        }
  
  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum
            , lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 8

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum
  offer1 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum

  let exp = slotToBeginPOSIXTime def 8
      loanId1 = txOutRefToLoanId offer1
      activeDatum = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , nextCheckpoints = []
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = 
            [ (testToken1,unsafeRatio 1 1_000_000)
            , (testToken2, unsafeRatio 2 1_000_000)
            ]
        , claimExpiration = exp + 10000 + 10000
        , loanExpiration = exp + 10000
        , loanOutstanding = fromInteger 100_000_000
        , loanId = loanId1
        }

  callEndpoint @"accept-offer" h1 $ 
    AcceptOfferParams
      { acceptOfferBeaconsMinted = 
          [ [("Ask",-1),("Offer",-1),(lenderToken,-1),(loanId1,2),("Active",1),(borrowerToken,1)]
          ]
      , acceptOfferBeaconRedeemers = [ MintActiveBeacon borrowerCred [(ask1,offer1)] ]
      , acceptOfferLoanAddresses = [loanAddr]
      , acceptOfferLoanRedeemers = [ AcceptOffer ]
      , acceptOfferUTxOs = [[ask1,offer1]]
      , acceptOfferCollateralAddresses = [loanAddr]
      , acceptOfferCollateralUTxOs = 
          [ [ ( Just activeDatum
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 50
             <> (uncurry singleton testToken2) 100
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId1 1
              )
            ]
          ]
      , acceptOfferCollateralAsInline = True
      , acceptOfferLenderAddresses = [lenderAddr]
      , acceptOfferLenderUTxOs = 
          [ [ ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId1 1
              )
            ]
          ]
      , acceptOfferLenderAsInline = True
      , acceptOfferWithTTL = True
      , acceptOfferScripts = ts
      , acceptOfferWithRefScripts = True
      , acceptOfferSpendRefScript = spendRef
      , acceptOfferMintRefScript = mintRef
      , acceptOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 10

  active1 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 50
                                <> (uncurry singleton testToken2) 100
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId1 1
                                 )
  let newActiveDatum = activeDatum{ loanOutstanding = 
                                      loanOutstanding activeDatum - fromInteger 25_000_000 
                                  }

  callEndpoint @"make-payment" h1 $ 
    MakePaymentParams
      { makePaymentBeaconsMinted = []
      , makePaymentBeaconRedeemer = BurnBeacons
      , makePaymentLoanAddress = loanAddr
      , makePaymentLoanRedeemer = MakePayment
      , makePaymentUTxOs = [active1]
      , makePaymentCollateralAddress = loanAddr
      , makePaymentCollateralUTxOs = 
          [ ( Just newActiveDatum
            , lovelaceValueOf 3_000_000
           <> (uncurry singleton testToken1) 75
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol borrowerToken 1
           <> singleton beaconCurrencySymbol loanId1 1
            )
          ]
      , makePaymentCollateralAsInline = True
      , makePaymentLenderAddresses = [lenderAddr]
      , makePaymentLenderUTxOs = 
          [ [ ( Just $ PaymentDatum (beaconCurrencySymbol,loanId1)
              , lovelaceValueOf 25_000_000
              )
            ]
          ]
      , makePaymentLenderAsInline = True
      , makePaymentWithTTE = True
      , makePaymentScripts = ts
      , makePaymentWithRefScripts = True
      , makePaymentSpendRefScript = spendRef
      , makePaymentMintRefScript = mintRef
      , makePaymentRefAddress = refAddr
      }

invalidHereAfterNotSpecified :: DappScripts -> EmulatorTrace ()
invalidHereAfterNotSpecified ts@DappScripts{..} = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let refAddr = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unValidatorScript spendingValidator
      , createReferenceScriptAddress = refAddr
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOSpendRef
      }

  void $ waitUntilSlot 2

  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unMintingPolicyScript beaconPolicy
      , createReferenceScriptAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOMintRef
      }

  void $ waitUntilSlot 4

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  spendRef <- txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      borrowerToken = credentialAsToken borrowerCred
      askDatum = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanTerm = 10000
        , collateral = [testToken1]
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  callEndpoint @"create-ask" h1 $
    CreateAskParams
      { createAskBeaconsMinted = [("Ask",1)]
      , createAskBeaconRedeemer = MintAskBeacon borrowerCred
      , createAskLoanAddress = loanAddr
      , createAskUTxOs = 
          [ ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          ]
      , createAskAsInline = True
      , createAskScripts = ts
      , createAskWithRefScript = True
      , createAskRefScript = mintRef
      , createAskRefAddress = refAddr
      }

  void $ waitUntilSlot 6

  let lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      lenderAddr = Address lenderCred Nothing
      offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
  
  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum
            , lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 8

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum
  offer1 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum

  let exp = slotToBeginPOSIXTime def 8
      loanId1 = txOutRefToLoanId offer1
      activeDatum = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , nextCheckpoints = []
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000
        , loanExpiration = exp + 10000
        , loanOutstanding = fromInteger 100_000_000
        , loanId = loanId1
        }

  callEndpoint @"accept-offer" h1 $ 
    AcceptOfferParams
      { acceptOfferBeaconsMinted = 
          [ [("Ask",-1),("Offer",-1),(lenderToken,-1),(loanId1,2),("Active",1),(borrowerToken,1)]
          ]
      , acceptOfferBeaconRedeemers = [ MintActiveBeacon borrowerCred [(ask1,offer1)] ]
      , acceptOfferLoanAddresses = [loanAddr]
      , acceptOfferLoanRedeemers = [ AcceptOffer ]
      , acceptOfferUTxOs = [[ask1,offer1]]
      , acceptOfferCollateralAddresses = [loanAddr]
      , acceptOfferCollateralUTxOs = 
          [ [ ( Just activeDatum
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 100
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId1 1
              )
            ]
          ]
      , acceptOfferCollateralAsInline = True
      , acceptOfferLenderAddresses = [lenderAddr]
      , acceptOfferLenderUTxOs = 
          [ [ ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId1 1
              )
            ]
          ]
      , acceptOfferLenderAsInline = True
      , acceptOfferWithTTL = True
      , acceptOfferScripts = ts
      , acceptOfferWithRefScripts = True
      , acceptOfferSpendRefScript = spendRef
      , acceptOfferMintRefScript = mintRef
      , acceptOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 10

  active1 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 100
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId1 1
                                 )
  let newActiveDatum = activeDatum{ loanOutstanding = 
                                      loanOutstanding activeDatum - fromInteger 50_000_000 
                                  }

  callEndpoint @"make-payment" h1 $ 
    MakePaymentParams
      { makePaymentBeaconsMinted = []
      , makePaymentBeaconRedeemer = BurnBeacons
      , makePaymentLoanAddress = loanAddr
      , makePaymentLoanRedeemer = MakePayment
      , makePaymentUTxOs = [active1]
      , makePaymentCollateralAddress = loanAddr
      , makePaymentCollateralUTxOs = 
          [ ( Just newActiveDatum
            , lovelaceValueOf 3_000_000
           <> (uncurry singleton testToken1) 50
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol borrowerToken 1
           <> singleton beaconCurrencySymbol loanId1 1
            )
          ]
      , makePaymentCollateralAsInline = True
      , makePaymentLenderAddresses = [lenderAddr]
      , makePaymentLenderUTxOs = 
          [ [ ( Just $ PaymentDatum (beaconCurrencySymbol,loanId1)
              , lovelaceValueOf 50_000_000
              )
            ]
          ]
      , makePaymentLenderAsInline = True
      , makePaymentWithTTE = False
      , makePaymentScripts = ts
      , makePaymentWithRefScripts = True
      , makePaymentSpendRefScript = spendRef
      , makePaymentMintRefScript = mintRef
      , makePaymentRefAddress = refAddr
      }

successfullyMakePaymentAfterRollover :: DappScripts -> EmulatorTrace ()
successfullyMakePaymentAfterRollover ts@DappScripts{..} = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let refAddr = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unValidatorScript spendingValidator
      , createReferenceScriptAddress = refAddr
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOSpendRef
      }

  void $ waitUntilSlot 2

  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unMintingPolicyScript beaconPolicy
      , createReferenceScriptAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOMintRef
      }

  void $ waitUntilSlot 4

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  spendRef <- txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      borrowerToken = credentialAsToken borrowerCred
      askDatum = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanTerm = 10000
        , collateral = [testToken1]
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  callEndpoint @"create-ask" h1 $
    CreateAskParams
      { createAskBeaconsMinted = [("Ask",1)]
      , createAskBeaconRedeemer = MintAskBeacon borrowerCred
      , createAskLoanAddress = loanAddr
      , createAskUTxOs = 
          [ ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          ]
      , createAskAsInline = True
      , createAskScripts = ts
      , createAskWithRefScript = True
      , createAskRefScript = mintRef
      , createAskRefAddress = refAddr
      }

  void $ waitUntilSlot 6

  let lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      lenderAddr = Address lenderCred Nothing
      offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = [10]
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 1
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
  
  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum
            , lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 8

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum
  offer1 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum

  let exp = slotToBeginPOSIXTime def 8
      loanId1 = txOutRefToLoanId offer1
      activeDatum = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , nextCheckpoints = [exp + 10]
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 1
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000
        , loanExpiration = exp + 10000
        , loanOutstanding = fromInteger 100_000_000
        , loanId = loanId1
        }

  callEndpoint @"accept-offer" h1 $ 
    AcceptOfferParams
      { acceptOfferBeaconsMinted = 
          [ [("Ask",-1),("Offer",-1),(lenderToken,-1),(loanId1,2),("Active",1),(borrowerToken,1)]
          ]
      , acceptOfferBeaconRedeemers = [ MintActiveBeacon borrowerCred [(ask1,offer1)] ]
      , acceptOfferLoanAddresses = [loanAddr]
      , acceptOfferLoanRedeemers = [ AcceptOffer ]
      , acceptOfferUTxOs = [[ask1,offer1]]
      , acceptOfferCollateralAddresses = [loanAddr]
      , acceptOfferCollateralUTxOs = 
          [ [ ( Just activeDatum
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 100
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId1 1
              )
            ]
          ]
      , acceptOfferCollateralAsInline = True
      , acceptOfferLenderAddresses = [lenderAddr]
      , acceptOfferLenderUTxOs = 
          [ [ ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId1 1
              )
            ]
          ]
      , acceptOfferLenderAsInline = True
      , acceptOfferWithTTL = True
      , acceptOfferScripts = ts
      , acceptOfferWithRefScripts = True
      , acceptOfferSpendRefScript = spendRef
      , acceptOfferMintRefScript = mintRef
      , acceptOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 10

  active1 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 100
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId1 1
                                 )
  let newActiveDatum = 
        activeDatum
          { loanOutstanding = loanOutstanding activeDatum * (fromInteger 1 + loanInterest activeDatum) 
          , nextCheckpoints = []
          , pastCheckpoints = [exp + 10]
          }

  callEndpoint @"rollover" h1 $
    RolloverParams
      { rolloverLoanAddress = loanAddr
      , rolloverRedeemer = Rollover
      , rolloverInputs = [active1]
      , rolloverOutputAddress = loanAddr
      , rolloverOutputs = 
          [ ( Just newActiveDatum
            , lovelaceValueOf 3_000_000
           <> (uncurry singleton testToken1) 100
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol borrowerToken 1
           <> singleton beaconCurrencySymbol loanId1 1
            )
          ]
      , rolloverOutputsAsInline = True
      , rolloverWithTTE = True
      , rolloverScripts = ts
      , rolloverWithRefScripts = True
      , rolloverSpendRefScript = spendRef
      , rolloverMintRefScript = mintRef
      , rolloverRefAddress = refAddr
      } 

  void $ waitUntilSlot 12

  newActive1 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                   <> (uncurry singleton testToken1) 100
                                   <> singleton beaconCurrencySymbol "Active" 1
                                   <> singleton beaconCurrencySymbol borrowerToken 1
                                   <> singleton beaconCurrencySymbol loanId1 1
                                   )

  let newActiveDatum' = 
        newActiveDatum{loanOutstanding = loanOutstanding newActiveDatum - fromInteger 50_000_000}

  callEndpoint @"make-payment" h1 $ 
    MakePaymentParams
      { makePaymentBeaconsMinted = []
      , makePaymentBeaconRedeemer = BurnBeacons
      , makePaymentLoanAddress = loanAddr
      , makePaymentLoanRedeemer = MakePayment
      , makePaymentUTxOs = [newActive1]
      , makePaymentCollateralAddress = loanAddr
      , makePaymentCollateralUTxOs = 
          [ ( Just newActiveDatum'
            , lovelaceValueOf 3_000_000
           <> (uncurry singleton testToken1) 75
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol borrowerToken 1
           <> singleton beaconCurrencySymbol loanId1 1
            )
          ]
      , makePaymentCollateralAsInline = True
      , makePaymentLenderAddresses = [lenderAddr]
      , makePaymentLenderUTxOs = 
          [ [ ( Just $ PaymentDatum (beaconCurrencySymbol,loanId1)
              , lovelaceValueOf 50_000_000
              )
            ]
          ]
      , makePaymentLenderAsInline = True
      , makePaymentWithTTE = True
      , makePaymentScripts = ts
      , makePaymentWithRefScripts = True
      , makePaymentSpendRefScript = spendRef
      , makePaymentMintRefScript = mintRef
      , makePaymentRefAddress = refAddr
      }

benchFullPayments :: DappScripts -> EmulatorTrace ()
benchFullPayments ts@DappScripts{..} = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let refAddr = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unValidatorScript spendingValidator
      , createReferenceScriptAddress = refAddr
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOSpendRef
      }

  void $ waitUntilSlot 2

  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unMintingPolicyScript beaconPolicy
      , createReferenceScriptAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOMintRef
      }

  void $ waitUntilSlot 4

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  spendRef <- txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      borrowerToken = credentialAsToken borrowerCred
      askDatum1 = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanTerm = 10000
        , collateral = [testToken1]
        }
      askDatum2 = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 50_000_000
        , loanTerm = 10000
        , collateral = [testToken1]
        }
      askDatum3 = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 110_000_000
        , loanTerm = 10000
        , collateral = [testToken1]
        }
      askDatum4 = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 55_000_000
        , loanTerm = 10000
        , collateral = [testToken1]
        }
      askDatum5 = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 20_000_000
        , loanTerm = 10000
        , collateral = [testToken1]
        }
      askDatum6 = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 30_000_000
        , loanTerm = 10000
        , collateral = [testToken1]
        }
      askDatum7 = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 40_000_000
        , loanTerm = 10000
        , collateral = [testToken1]
        }
      askDatum8 = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 45_000_000
        , loanTerm = 10000
        , collateral = [testToken1]
        }
      askDatum9 = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 10_000_000
        , loanTerm = 10000
        , collateral = [testToken1]
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  callEndpoint @"create-ask" h1 $
    CreateAskParams
      { createAskBeaconsMinted = [("Ask",9)]
      , createAskBeaconRedeemer = MintAskBeacon borrowerCred
      , createAskLoanAddress = loanAddr
      , createAskUTxOs = 
          [ ( Just askDatum1
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum2
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum3
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum4
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum5
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum6
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum7
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum8
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum9
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          ]
      , createAskAsInline = True
      , createAskScripts = ts
      , createAskWithRefScript = True
      , createAskRefScript = mintRef
      , createAskRefAddress = refAddr
      }

  void $ waitUntilSlot 6

  let lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      lenderAddr = Address lenderCred Nothing
      offerDatum1 = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
      offerDatum2 = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 50_000_000
        , loanCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
      offerDatum3 = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 110_000_000
        , loanCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
      offerDatum4 = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 55_000_000
        , loanCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
      offerDatum5 = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 20_000_000
        , loanCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
      offerDatum6 = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 30_000_000
        , loanCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
      offerDatum7 = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 40_000_000
        , loanCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
      offerDatum8 = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 45_000_000
        , loanCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
      offerDatum9 = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 10_000_000
        , loanCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
  
  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum1
            , lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 8

  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum2
            , lovelaceValueOf 55_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 10

  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum3
            , lovelaceValueOf 115_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 12

  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum4
            , lovelaceValueOf 60_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 14

  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum5
            , lovelaceValueOf 25_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 16

  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum6
            , lovelaceValueOf 35_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 18

  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum7
            , lovelaceValueOf 45_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 20

  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum8
            , lovelaceValueOf 50_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 22

  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum9
            , lovelaceValueOf 15_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 24

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum1
  ask2 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum2
  ask3 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum3
  ask4 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum4
  ask5 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum5
  ask6 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum6
  ask7 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum7
  ask8 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum8
  ask9 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum9
  offer1 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum1
  offer2 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 55_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum2
  
  offer3 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 115_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum3
  offer4 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 60_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum4
  offer5 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 25_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum5
  
  offer6 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 35_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum6
  offer7 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 45_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum7
  offer8 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 50_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum8
  offer9 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 15_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum9

  let exp = slotToBeginPOSIXTime def 24
      loanId1 = txOutRefToLoanId offer1
      loanId2 = txOutRefToLoanId offer2
      loanId3 = txOutRefToLoanId offer3
      loanId4 = txOutRefToLoanId offer4
      loanId5 = txOutRefToLoanId offer5
      loanId6 = txOutRefToLoanId offer6
      loanId7 = txOutRefToLoanId offer7
      loanId8 = txOutRefToLoanId offer8
      loanId9 = txOutRefToLoanId offer9
      activeDatum1 = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , nextCheckpoints = []
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000
        , loanExpiration = exp + 10000
        , loanOutstanding = fromInteger 100_000_000
        , loanId = loanId1
        }
      activeDatum2 = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 50_000_000
        , nextCheckpoints = []
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000
        , loanExpiration = exp + 10000
        , loanOutstanding = fromInteger 50_000_000
        , loanId = loanId2
        }
      activeDatum3 = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 110_000_000
        , nextCheckpoints = []
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000
        , loanExpiration = exp + 10000
        , loanOutstanding = fromInteger 110_000_000
        , loanId = loanId3
        }
      activeDatum4 = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 55_000_000
        , nextCheckpoints = []
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000
        , loanExpiration = exp + 10000
        , loanOutstanding = fromInteger 55_000_000
        , loanId = loanId4
        }
      activeDatum5 = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 20_000_000
        , nextCheckpoints = []
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000
        , loanExpiration = exp + 10000
        , loanOutstanding = fromInteger 20_000_000
        , loanId = loanId5
        }
      activeDatum6 = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 30_000_000
        , nextCheckpoints = []
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000
        , loanExpiration = exp + 10000
        , loanOutstanding = fromInteger 30_000_000
        , loanId = loanId6
        }
      activeDatum7 = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 40_000_000
        , nextCheckpoints = []
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000 + 2000
        , loanExpiration = exp + 10000 + 2000
        , loanOutstanding = fromInteger 40_000_000
        , loanId = loanId7
        }
      activeDatum8 = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 45_000_000
        , nextCheckpoints = []
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000 + 2000
        , loanExpiration = exp + 10000 + 2000
        , loanOutstanding = fromInteger 45_000_000
        , loanId = loanId8
        }
      activeDatum9 = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 10_000_000
        , nextCheckpoints = []
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000 + 2000
        , loanExpiration = exp + 10000 + 2000
        , loanOutstanding = fromInteger 10_000_000
        , loanId = loanId9
        }

  callEndpoint @"accept-offer" h1 $ 
    AcceptOfferParams
      { acceptOfferBeaconsMinted = 
          [ [ ("Ask",-6)
            , ("Offer",-6)
            , (lenderToken,-6)
            , (loanId1,2)
            , (loanId2,2)
            , (loanId3,2)
            , (loanId4,2)
            , (loanId5,2)
            , (loanId6,2)
            , ("Active",6)
            , (borrowerToken,6)
            ]
          ]
      , acceptOfferBeaconRedeemers = 
          [ MintActiveBeacon borrowerCred 
              [ (ask1,offer1)
              , (ask2,offer2)
              , (ask3,offer3)
              , (ask4,offer4)
              , (ask5,offer5)
              , (ask6,offer6)
              ]
          ]
      , acceptOfferLoanAddresses = [loanAddr]
      , acceptOfferLoanRedeemers = [ AcceptOffer ]
      , acceptOfferUTxOs = 
          [ [ ask1
            , offer1
            , ask2
            , offer2
            , ask3
            , offer3
            , ask4
            , offer4
            , ask5
            , offer5
            , ask6
            , offer6
            ]
          ]
      , acceptOfferCollateralAddresses = [loanAddr]
      , acceptOfferCollateralUTxOs = 
          [ [ ( Just activeDatum1
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 100
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId1 1
              )
            , ( Just activeDatum2
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 50
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId2 1
              )
            , ( Just activeDatum3
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 110
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId3 1
              )
            , ( Just activeDatum4
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 55
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId4 1
              )
            , ( Just activeDatum5
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 20
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId5 1
              )
            , ( Just activeDatum6
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 30
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId6 1
              )
            ]
          ]
      , acceptOfferCollateralAsInline = True
      , acceptOfferLenderAddresses = [lenderAddr]
      , acceptOfferLenderUTxOs = 
          [ [ ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId1 1
              )
            , ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId2 1
              )
            , ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId3 1
              )
            , ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId4 1
              )
            , ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId5 1
              )
            , ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId6 1
              )
            ]
          ]
      , acceptOfferLenderAsInline = True
      , acceptOfferWithTTL = True
      , acceptOfferScripts = ts
      , acceptOfferWithRefScripts = True
      , acceptOfferSpendRefScript = spendRef
      , acceptOfferMintRefScript = mintRef
      , acceptOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 26

  callEndpoint @"accept-offer" h1 $ 
    AcceptOfferParams
      { acceptOfferBeaconsMinted = 
          [ [ ("Ask",-3)
            , ("Offer",-3)
            , (lenderToken,-3)
            , (loanId7,2)
            , (loanId8,2)
            , (loanId9,2)
            , ("Active",3)
            , (borrowerToken,3)
            ]
          ]
      , acceptOfferBeaconRedeemers = 
          [ MintActiveBeacon borrowerCred 
              [ (ask7,offer7)
              , (ask8,offer8)
              , (ask9,offer9)
              ]
          ]
      , acceptOfferLoanAddresses = [loanAddr]
      , acceptOfferLoanRedeemers = [ AcceptOffer ]
      , acceptOfferUTxOs = 
          [ [ ask7
            , offer7
            , ask8
            , offer8
            , ask9
            , offer9
            ]
          ]
      , acceptOfferCollateralAddresses = [loanAddr]
      , acceptOfferCollateralUTxOs = 
          [ [ ( Just activeDatum7
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 40
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId7 1
              )
            , ( Just activeDatum8
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 45
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId8 1
              )
            , ( Just activeDatum9
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 10
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId9 1
              )
            ]
          ]
      , acceptOfferCollateralAsInline = True
      , acceptOfferLenderAddresses = [lenderAddr]
      , acceptOfferLenderUTxOs = 
          [ [ ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId7 1
              )
            , ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId8 1
              )
            , ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId9 1
              )
            ]
          ]
      , acceptOfferLenderAsInline = True
      , acceptOfferWithTTL = True
      , acceptOfferScripts = ts
      , acceptOfferWithRefScripts = True
      , acceptOfferSpendRefScript = spendRef
      , acceptOfferMintRefScript = mintRef
      , acceptOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 28

  active1 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 100
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId1 1
                                 )
  active2 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 50
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId2 1
                                 )
  active3 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 110
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId3 1
                                 )
  active4 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 55
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId4 1
                                 )
  active5 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 20
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId5 1
                                 )
  active6 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 30
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId6 1
                                 )
  active7 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 40
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId7 1
                                 )
  active8 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 45
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId8 1
                                 )
  active9 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 10
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId9 1
                                 )

  let newActiveDatum1 = 
        activeDatum1{loanOutstanding = loanOutstanding activeDatum1 - fromInteger 100_000_000 }
      newActiveDatum2 = 
        activeDatum2{loanOutstanding = loanOutstanding activeDatum2 - fromInteger 50_000_000 }
      newActiveDatum3 = 
        activeDatum3{loanOutstanding = loanOutstanding activeDatum3 - fromInteger 110_000_000 }
      newActiveDatum4 = 
        activeDatum4{loanOutstanding = loanOutstanding activeDatum4 - fromInteger 55_000_000 }
      newActiveDatum5 = 
        activeDatum5{loanOutstanding = loanOutstanding activeDatum5 - fromInteger 20_000_000 }
      newActiveDatum6 = 
        activeDatum6{loanOutstanding = loanOutstanding activeDatum6 - fromInteger 30_000_000 }
      newActiveDatum7 = 
        activeDatum7{loanOutstanding = loanOutstanding activeDatum7 - fromInteger 40_000_000 }
      newActiveDatum8 = 
        activeDatum8{loanOutstanding = loanOutstanding activeDatum8 - fromInteger 45_000_000 }
      newActiveDatum9 = 
        activeDatum9{loanOutstanding = loanOutstanding activeDatum9 - fromInteger 10_000_000 }

  callEndpoint @"make-payment" h1 $ 
    MakePaymentParams
      { makePaymentBeaconsMinted = [(borrowerToken,-9)]
      , makePaymentBeaconRedeemer = BurnBeacons
      , makePaymentLoanAddress = loanAddr
      , makePaymentLoanRedeemer = MakePayment
      , makePaymentUTxOs = 
          [ active1
          , active2
          , active3
          , active4
          , active5
          , active6
          , active7
          , active8
          , active9
          ]
      , makePaymentCollateralAddress = loanAddr
      , makePaymentCollateralUTxOs = 
          [ ( Just newActiveDatum1
            , lovelaceValueOf 3_000_000
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol loanId1 1
            )
          , ( Just newActiveDatum2
            , lovelaceValueOf 3_000_000
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol loanId2 1
            )
          , ( Just newActiveDatum3
            , lovelaceValueOf 3_000_000
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol loanId3 1
            )
          , ( Just newActiveDatum4
            , lovelaceValueOf 3_000_000
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol loanId4 1
            )
          , ( Just newActiveDatum5
            , lovelaceValueOf 3_000_000
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol loanId5 1
            )
          , ( Just newActiveDatum6
            , lovelaceValueOf 3_000_000
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol loanId6 1
            )
          , ( Just newActiveDatum7
            , lovelaceValueOf 3_000_000
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol loanId7 1
            )
          , ( Just newActiveDatum8
            , lovelaceValueOf 3_000_000
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol loanId8 1
            )
          , ( Just newActiveDatum9
            , lovelaceValueOf 3_000_000
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol loanId9 1
            )
          ]
      , makePaymentCollateralAsInline = True
      , makePaymentLenderAddresses = [lenderAddr]
      , makePaymentLenderUTxOs = 
          [ [ ( Just $ PaymentDatum (beaconCurrencySymbol,loanId1)
              , lovelaceValueOf 100_000_000
              )
            , ( Just $ PaymentDatum (beaconCurrencySymbol,loanId2)
              , lovelaceValueOf 50_000_000
              )
            , ( Just $ PaymentDatum (beaconCurrencySymbol,loanId3)
              , lovelaceValueOf 110_000_000
              )
            , ( Just $ PaymentDatum (beaconCurrencySymbol,loanId4)
              , lovelaceValueOf 55_000_000
              )
            , ( Just $ PaymentDatum (beaconCurrencySymbol,loanId5)
              , lovelaceValueOf 20_000_000
              )
            , ( Just $ PaymentDatum (beaconCurrencySymbol,loanId6)
              , lovelaceValueOf 30_000_000
              )
            , ( Just $ PaymentDatum (beaconCurrencySymbol,loanId7)
              , lovelaceValueOf 40_000_000
              )
            , ( Just $ PaymentDatum (beaconCurrencySymbol,loanId8)
              , lovelaceValueOf 45_000_000
              )
            , ( Just $ PaymentDatum (beaconCurrencySymbol,loanId9)
              , lovelaceValueOf 10_000_000
              )
            ]
          ]
      , makePaymentLenderAsInline = True
      , makePaymentWithTTE = True
      , makePaymentScripts = ts
      , makePaymentWithRefScripts = True
      , makePaymentSpendRefScript = spendRef
      , makePaymentMintRefScript = mintRef
      , makePaymentRefAddress = refAddr
      }

benchPartialPayments :: DappScripts -> EmulatorTrace ()
benchPartialPayments ts@DappScripts{..} = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let refAddr = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unValidatorScript spendingValidator
      , createReferenceScriptAddress = refAddr
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOSpendRef
      }

  void $ waitUntilSlot 2

  callEndpoint @"create-reference-script" h1 $
    CreateReferenceScriptParams
      { createReferenceScriptScript = unMintingPolicyScript beaconPolicy
      , createReferenceScriptAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
      , createReferenceScriptUTxO = lovelaceValueOf minUTxOMintRef
      }

  void $ waitUntilSlot 4

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  spendRef <- txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      borrowerToken = credentialAsToken borrowerCred
      askDatum1 = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanTerm = 10000
        , collateral = [testToken1]
        }
      askDatum2 = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 50_000_000
        , loanTerm = 10000
        , collateral = [testToken1]
        }
      askDatum3 = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 110_000_000
        , loanTerm = 10000
        , collateral = [testToken1]
        }
      askDatum4 = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 55_000_000
        , loanTerm = 10000
        , collateral = [testToken1]
        }
      askDatum5 = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 20_000_000
        , loanTerm = 10000
        , collateral = [testToken1]
        }
      askDatum6 = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 30_000_000
        , loanTerm = 10000
        , collateral = [testToken1]
        }
      askDatum7 = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 40_000_000
        , loanTerm = 10000
        , collateral = [testToken1]
        }
      askDatum8 = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 45_000_000
        , loanTerm = 10000
        , collateral = [testToken1]
        }
      askDatum9 = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 10_000_000
        , loanTerm = 10000
        , collateral = [testToken1]
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  callEndpoint @"create-ask" h1 $
    CreateAskParams
      { createAskBeaconsMinted = [("Ask",9)]
      , createAskBeaconRedeemer = MintAskBeacon borrowerCred
      , createAskLoanAddress = loanAddr
      , createAskUTxOs = 
          [ ( Just askDatum1
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum2
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum3
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum4
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum5
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum6
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum7
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum8
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum9
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          ]
      , createAskAsInline = True
      , createAskScripts = ts
      , createAskWithRefScript = True
      , createAskRefScript = mintRef
      , createAskRefAddress = refAddr
      }

  void $ waitUntilSlot 6

  let lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      lenderAddr = Address lenderCred Nothing
      offerDatum1 = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
      offerDatum2 = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 50_000_000
        , loanCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
      offerDatum3 = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 110_000_000
        , loanCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
      offerDatum4 = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 55_000_000
        , loanCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
      offerDatum5 = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 20_000_000
        , loanCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
      offerDatum6 = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 30_000_000
        , loanCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
      offerDatum7 = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 40_000_000
        , loanCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
      offerDatum8 = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 45_000_000
        , loanCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
      offerDatum9 = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 10_000_000
        , loanCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
  
  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum1
            , lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 8

  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum2
            , lovelaceValueOf 55_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 10

  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum3
            , lovelaceValueOf 115_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 12

  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum4
            , lovelaceValueOf 60_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 14

  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum5
            , lovelaceValueOf 25_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 16

  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum6
            , lovelaceValueOf 35_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 18

  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum7
            , lovelaceValueOf 45_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 20

  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum8
            , lovelaceValueOf 50_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 22

  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum9
            , lovelaceValueOf 15_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 24

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum1
  ask2 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum2
  ask3 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum3
  ask4 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum4
  ask5 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum5
  ask6 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum6
  ask7 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum7
  ask8 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum8
  ask9 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum9
  offer1 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum1
  offer2 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 55_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum2
  
  offer3 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 115_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum3
  offer4 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 60_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum4
  offer5 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 25_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum5
  
  offer6 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 35_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum6
  offer7 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 45_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum7
  offer8 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 50_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum8
  offer9 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 15_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum9

  let exp = slotToBeginPOSIXTime def 24
      loanId1 = txOutRefToLoanId offer1
      loanId2 = txOutRefToLoanId offer2
      loanId3 = txOutRefToLoanId offer3
      loanId4 = txOutRefToLoanId offer4
      loanId5 = txOutRefToLoanId offer5
      loanId6 = txOutRefToLoanId offer6
      loanId7 = txOutRefToLoanId offer7
      loanId8 = txOutRefToLoanId offer8
      loanId9 = txOutRefToLoanId offer9
      activeDatum1 = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , nextCheckpoints = []
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000
        , loanExpiration = exp + 10000
        , loanOutstanding = fromInteger 100_000_000
        , loanId = loanId1
        }
      activeDatum2 = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 50_000_000
        , nextCheckpoints = []
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000
        , loanExpiration = exp + 10000
        , loanOutstanding = fromInteger 50_000_000
        , loanId = loanId2
        }
      activeDatum3 = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 110_000_000
        , nextCheckpoints = []
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000
        , loanExpiration = exp + 10000
        , loanOutstanding = fromInteger 110_000_000
        , loanId = loanId3
        }
      activeDatum4 = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 55_000_000
        , nextCheckpoints = []
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000
        , loanExpiration = exp + 10000
        , loanOutstanding = fromInteger 55_000_000
        , loanId = loanId4
        }
      activeDatum5 = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 20_000_000
        , nextCheckpoints = []
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000
        , loanExpiration = exp + 10000
        , loanOutstanding = fromInteger 20_000_000
        , loanId = loanId5
        }
      activeDatum6 = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 30_000_000
        , nextCheckpoints = []
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000
        , loanExpiration = exp + 10000
        , loanOutstanding = fromInteger 30_000_000
        , loanId = loanId6
        }
      activeDatum7 = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 40_000_000
        , nextCheckpoints = []
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000 + 2000
        , loanExpiration = exp + 10000 + 2000
        , loanOutstanding = fromInteger 40_000_000
        , loanId = loanId7
        }
      activeDatum8 = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 45_000_000
        , nextCheckpoints = []
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000 + 2000
        , loanExpiration = exp + 10000 + 2000
        , loanOutstanding = fromInteger 45_000_000
        , loanId = loanId8
        }
      activeDatum9 = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 10_000_000
        , nextCheckpoints = []
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000 + 2000
        , loanExpiration = exp + 10000 + 2000
        , loanOutstanding = fromInteger 10_000_000
        , loanId = loanId9
        }

  callEndpoint @"accept-offer" h1 $ 
    AcceptOfferParams
      { acceptOfferBeaconsMinted = 
          [ [ ("Ask",-6)
            , ("Offer",-6)
            , (lenderToken,-6)
            , (loanId1,2)
            , (loanId2,2)
            , (loanId3,2)
            , (loanId4,2)
            , (loanId5,2)
            , (loanId6,2)
            , ("Active",6)
            , (borrowerToken,6)
            ]
          ]
      , acceptOfferBeaconRedeemers = 
          [ MintActiveBeacon borrowerCred 
              [ (ask1,offer1)
              , (ask2,offer2)
              , (ask3,offer3)
              , (ask4,offer4)
              , (ask5,offer5)
              , (ask6,offer6)
              ]
          ]
      , acceptOfferLoanAddresses = [loanAddr]
      , acceptOfferLoanRedeemers = [ AcceptOffer ]
      , acceptOfferUTxOs = 
          [ [ ask1
            , offer1
            , ask2
            , offer2
            , ask3
            , offer3
            , ask4
            , offer4
            , ask5
            , offer5
            , ask6
            , offer6
            ]
          ]
      , acceptOfferCollateralAddresses = [loanAddr]
      , acceptOfferCollateralUTxOs = 
          [ [ ( Just activeDatum1
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 100
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId1 1
              )
            , ( Just activeDatum2
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 50
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId2 1
              )
            , ( Just activeDatum3
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 110
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId3 1
              )
            , ( Just activeDatum4
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 55
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId4 1
              )
            , ( Just activeDatum5
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 20
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId5 1
              )
            , ( Just activeDatum6
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 30
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId6 1
              )
            ]
          ]
      , acceptOfferCollateralAsInline = True
      , acceptOfferLenderAddresses = [lenderAddr]
      , acceptOfferLenderUTxOs = 
          [ [ ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId1 1
              )
            , ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId2 1
              )
            , ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId3 1
              )
            , ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId4 1
              )
            , ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId5 1
              )
            , ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId6 1
              )
            ]
          ]
      , acceptOfferLenderAsInline = True
      , acceptOfferWithTTL = True
      , acceptOfferScripts = ts
      , acceptOfferWithRefScripts = True
      , acceptOfferSpendRefScript = spendRef
      , acceptOfferMintRefScript = mintRef
      , acceptOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 26

  callEndpoint @"accept-offer" h1 $ 
    AcceptOfferParams
      { acceptOfferBeaconsMinted = 
          [ [ ("Ask",-3)
            , ("Offer",-3)
            , (lenderToken,-3)
            , (loanId7,2)
            , (loanId8,2)
            , (loanId9,2)
            , ("Active",3)
            , (borrowerToken,3)
            ]
          ]
      , acceptOfferBeaconRedeemers = 
          [ MintActiveBeacon borrowerCred 
              [ (ask7,offer7)
              , (ask8,offer8)
              , (ask9,offer9)
              ]
          ]
      , acceptOfferLoanAddresses = [loanAddr]
      , acceptOfferLoanRedeemers = [ AcceptOffer ]
      , acceptOfferUTxOs = 
          [ [ ask7
            , offer7
            , ask8
            , offer8
            , ask9
            , offer9
            ]
          ]
      , acceptOfferCollateralAddresses = [loanAddr]
      , acceptOfferCollateralUTxOs = 
          [ [ ( Just activeDatum7
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 40
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId7 1
              )
            , ( Just activeDatum8
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 45
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId8 1
              )
            , ( Just activeDatum9
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 10
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId9 1
              )
            ]
          ]
      , acceptOfferCollateralAsInline = True
      , acceptOfferLenderAddresses = [lenderAddr]
      , acceptOfferLenderUTxOs = 
          [ [ ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId7 1
              )
            , ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId8 1
              )
            , ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId9 1
              )
            ]
          ]
      , acceptOfferLenderAsInline = True
      , acceptOfferWithTTL = True
      , acceptOfferScripts = ts
      , acceptOfferWithRefScripts = True
      , acceptOfferSpendRefScript = spendRef
      , acceptOfferMintRefScript = mintRef
      , acceptOfferRefAddress = refAddr
      }

  void $ waitUntilSlot 28

  active1 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 100
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId1 1
                                 )
  active2 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 50
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId2 1
                                 )
  active3 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 110
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId3 1
                                 )
  active4 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 55
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId4 1
                                 )
  active5 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 20
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId5 1
                                 )
  active6 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 30
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId6 1
                                 )
  active7 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 40
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId7 1
                                 )
  active8 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 45
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId8 1
                                 )
  active9 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 10
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId9 1
                                 )

  let newActiveDatum1 = 
        activeDatum1{loanOutstanding = loanOutstanding activeDatum1 - fromInteger 50_000_000 }
      newActiveDatum2 = 
        activeDatum2{loanOutstanding = loanOutstanding activeDatum2 - fromInteger 25_000_000 }
      newActiveDatum3 = 
        activeDatum3{loanOutstanding = loanOutstanding activeDatum3 - fromInteger 55_000_000 }
      newActiveDatum4 = 
        activeDatum4{loanOutstanding = loanOutstanding activeDatum4 - fromInteger 27_500_000 }
      newActiveDatum5 = 
        activeDatum5{loanOutstanding = loanOutstanding activeDatum5 - fromInteger 10_000_000 }
      newActiveDatum6 = 
        activeDatum6{loanOutstanding = loanOutstanding activeDatum6 - fromInteger 15_000_000 }
      newActiveDatum7 = 
        activeDatum7{loanOutstanding = loanOutstanding activeDatum7 - fromInteger 20_000_000 }
      newActiveDatum8 = 
        activeDatum8{loanOutstanding = loanOutstanding activeDatum8 - fromInteger 22_500_000 }
      newActiveDatum9 = 
        activeDatum9{loanOutstanding = loanOutstanding activeDatum9 - fromInteger 5_000_000 }

  callEndpoint @"make-payment" h1 $ 
    MakePaymentParams
      { makePaymentBeaconsMinted = []
      , makePaymentBeaconRedeemer = BurnBeacons
      , makePaymentLoanAddress = loanAddr
      , makePaymentLoanRedeemer = MakePayment
      , makePaymentUTxOs = 
          [ active1
          , active2
          , active3
          , active4
          , active5
          , active6
          -- , active7
          -- , active8
          -- , active9
          ]
      , makePaymentCollateralAddress = loanAddr
      , makePaymentCollateralUTxOs = 
          [ ( Just newActiveDatum1
            , lovelaceValueOf 3_000_000
           <> (uncurry singleton testToken1) 50
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol borrowerToken 1
           <> singleton beaconCurrencySymbol loanId1 1
            )
          , ( Just newActiveDatum2
            , lovelaceValueOf 3_000_000
           <> (uncurry singleton testToken1) 25
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol borrowerToken 1
           <> singleton beaconCurrencySymbol loanId2 1
            )
          , ( Just newActiveDatum3
            , lovelaceValueOf 3_000_000
           <> (uncurry singleton testToken1) 55
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol borrowerToken 1
           <> singleton beaconCurrencySymbol loanId3 1
            )
          , ( Just newActiveDatum4
            , lovelaceValueOf 3_000_000
           <> (uncurry singleton testToken1) 28
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol borrowerToken 1
           <> singleton beaconCurrencySymbol loanId4 1
            )
          , ( Just newActiveDatum5
            , lovelaceValueOf 3_000_000
           <> (uncurry singleton testToken1) 10
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol borrowerToken 1
           <> singleton beaconCurrencySymbol loanId5 1
            )
          , ( Just newActiveDatum6
            , lovelaceValueOf 3_000_000
           <> (uncurry singleton testToken1) 15
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol borrowerToken 1
           <> singleton beaconCurrencySymbol loanId6 1
            )
          -- , ( Just newActiveDatum7
          --   , lovelaceValueOf 3_000_000
          --  <> (uncurry singleton testToken1) 20
          --  <> singleton beaconCurrencySymbol "Active" 1
          --  <> singleton beaconCurrencySymbol borrowerToken 1
          --  <> singleton beaconCurrencySymbol loanId7 1
          --   )
          -- , ( Just newActiveDatum8
          --   , lovelaceValueOf 3_000_000
          --  <> (uncurry singleton testToken1) 23
          --  <> singleton beaconCurrencySymbol "Active" 1
          --  <> singleton beaconCurrencySymbol borrowerToken 1
          --  <> singleton beaconCurrencySymbol loanId8 1
          --   )
          -- , ( Just newActiveDatum9
          --   , lovelaceValueOf 3_000_000
          --  <> (uncurry singleton testToken1) 5
          --  <> singleton beaconCurrencySymbol "Active" 1
          --  <> singleton beaconCurrencySymbol borrowerToken 1
          --  <> singleton beaconCurrencySymbol loanId9 1
          --   )
          ]
      , makePaymentCollateralAsInline = True
      , makePaymentLenderAddresses = [lenderAddr]
      , makePaymentLenderUTxOs = 
          [ [ ( Just $ PaymentDatum (beaconCurrencySymbol,loanId1)
              , lovelaceValueOf 50_000_000
              )
            , ( Just $ PaymentDatum (beaconCurrencySymbol,loanId2)
              , lovelaceValueOf 25_000_000
              )
            , ( Just $ PaymentDatum (beaconCurrencySymbol,loanId3)
              , lovelaceValueOf 55_000_000
              )
            , ( Just $ PaymentDatum (beaconCurrencySymbol,loanId4)
              , lovelaceValueOf 27_500_000
              )
            , ( Just $ PaymentDatum (beaconCurrencySymbol,loanId5)
              , lovelaceValueOf 10_000_000
              )
            , ( Just $ PaymentDatum (beaconCurrencySymbol,loanId6)
              , lovelaceValueOf 15_000_000
              )
            -- , ( Just $ PaymentDatum (beaconCurrencySymbol,loanId7)
            --   , lovelaceValueOf 20_000_000
            --   )
            -- , ( Just $ PaymentDatum (beaconCurrencySymbol,loanId8)
            --   , lovelaceValueOf 22_500_000
            --   )
            -- , ( Just $ PaymentDatum (beaconCurrencySymbol,loanId9)
            --   , lovelaceValueOf 5_000_000
            --   )
            ]
          ]
      , makePaymentLenderAsInline = True
      , makePaymentWithTTE = True
      , makePaymentScripts = ts
      , makePaymentWithRefScripts = True
      , makePaymentSpendRefScript = spendRef
      , makePaymentMintRefScript = mintRef
      , makePaymentRefAddress = refAddr
      }

-------------------------------------------------
-- Test Function
-------------------------------------------------
tests :: DappScripts -> TestTree
tests ts = do
  let opts = defaultCheckOptions & emulatorConfig .~ emConfig
  testGroup "Make Payment(s)"
    [ checkPredicateOptions opts "Successfully make single partial payment"
        assertNoFailedTransactions (successfullyMakeSinglePartialPayment ts)
    , checkPredicateOptions opts "Successfully make multiple partial payments"
        assertNoFailedTransactions (successfullyMakeMultiplePartialPayments ts)
    , checkPredicateOptions opts "Successfully make single full payment"
        assertNoFailedTransactions (successfullyMakeSingleFullPayment ts)
    , checkPredicateOptions opts "Successfully make multiple full payments"
        assertNoFailedTransactions (successfullyMakeMultipleFullPayments ts)
    , checkPredicateOptions opts "Successfully mix partial and full payments"
        assertNoFailedTransactions (successfullyMixPartialAndFullPayments ts)
    , checkPredicateOptions opts "Successfully make payment after rollover"
        assertNoFailedTransactions (successfullyMakePaymentAfterRollover ts)
    , checkPredicateOptions opts "Fail if borrower did not approve"
        (Test.not assertNoFailedTransactions) (borrowerDidNotApprove ts)
    , checkPredicateOptions opts "Successfully spend invalid Active UTxO"
        assertNoFailedTransactions (successfullySpendInvalidActiveUTxO ts)
    , checkPredicateOptions opts "Fail if only loan is expired"
        (Test.not assertNoFailedTransactions) (onlyLoanIsExpired ts)
    , checkPredicateOptions opts "Fail if at least one loan is expired"
        (Test.not assertNoFailedTransactions) (atLeastOneLoanIsExpired ts)
    , checkPredicateOptions opts "Fail if only loan's next checkpoint has passed"
        (Test.not assertNoFailedTransactions) (onlyLoanNextCheckpointHasPassed ts)
    , checkPredicateOptions opts "Fail if at least one loan's next checkpoint has passed"
        (Test.not assertNoFailedTransactions) (atLeastOneLoansNextCheckpointHasPassed ts)
    , checkPredicateOptions opts "Fail if only partial payment collateral datum is wrong"
        (Test.not assertNoFailedTransactions) (onlyPartialPaymentCollateralDatumIsWrong ts)
    , checkPredicateOptions opts "Fail if only full payment collateral datum is wrong"
        (Test.not assertNoFailedTransactions) (onlyFullPaymentCollateralDatumIsWrong ts)
    , checkPredicateOptions opts "Fail if at least one collateral datum is wrong"
        (Test.not assertNoFailedTransactions) (atLeastOneCollateralDatumIsWrong ts)
    , checkPredicateOptions opts "Fail if only full payment BorrowerID withdrawn"
        (Test.not assertNoFailedTransactions) (onlyFullPaymentBorrowerIdWithdrawn ts)
    , checkPredicateOptions opts "Fail if at least one full payment BorrowerID withdrawn"
        (Test.not assertNoFailedTransactions) (atLeastOneBorrowerIdWithdrawn ts)
    , checkPredicateOptions opts "Fail if only full payment collateral output still has BorrowerID"
        (Test.not assertNoFailedTransactions) (onlyFullPaymentCollateralOutputStillHasBorrowerId ts)
    , checkPredicateOptions opts "Fail if at least one full payment collateral output still has BorrowerID"
        (Test.not assertNoFailedTransactions) (atLeastOneFullPaymentCollateralOutputStillHasBorrowerId ts)
    , checkPredicateOptions opts "Fail if mixed full payment BorrowerID withdrawn"
        (Test.not assertNoFailedTransactions) (mixedFullPaymentBorrowerIdWithdrawnInsteadOfBurned ts)
    , checkPredicateOptions opts "Fail if mixed full payment BorrowerID grouped with partial collateral output"
        (Test.not assertNoFailedTransactions) (mixedFullPaymentBorrowerIdGroupedWithPartialCollateralOutput ts)
    , checkPredicateOptions opts "Fail if too much collateral taken during only partial payment"
        (Test.not assertNoFailedTransactions) (tooMuchCollateralTakenDuringOnlyPartialPayment ts)
    , checkPredicateOptions opts "Fail if too much collateral taken during at least one partial payment"
        (Test.not assertNoFailedTransactions) (tooMuchCollateralTakenDuringAtLeastOnePartialPayment ts)
    , checkPredicateOptions opts "Successfully make partial payment on loan with mixed collateral"
        assertNoFailedTransactions (successfullyMakeSinglePartialPaymentOnLoanWithMixedCollateral ts)
    , checkPredicateOptions opts "Successfully swap out collateral assets"
        assertNoFailedTransactions (successfullySwapOutCollateral ts)
    , checkPredicateOptions opts "Fail if invalid-hereafter not specified"
        (Test.not assertNoFailedTransactions) (invalidHereAfterNotSpecified ts)
    ]

testTrace :: DappScripts -> IO ()
testTrace = runEmulatorTraceIO' def emConfig . benchPartialPayments