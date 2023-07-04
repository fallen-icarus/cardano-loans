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

module Test.ClaimLost
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
-- Claim Lost Scenarios
-------------------------------------------------
successfullyClaimSingleLostCollateral :: DappScripts -> EmulatorTrace ()
successfullyClaimSingleLostCollateral ts@DappScripts{..} = do
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

  void $ waitUntilSlot 30

  active1 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 100
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId1 1
                                 )

  callEndpoint @"claim-lost" h1 $
    ClaimLostParams
      { claimLostBeaconsBurned = [("Active",-1),(borrowerToken,-1),(loanId1,-1)]
      , claimLostBeaconRedeemer = BurnBeacons
      , claimLostLoanAddress = loanAddr
      , claimLostRedeemer = UnlockLostCollateral
      , claimLostInputs = [active1]
      , claimLostWithTTL = True
      , claimLostScripts = ts
      , claimLostWithRefScripts = True
      , claimLostSpendRefScript = spendRef
      , claimLostMintRefScript = mintRef
      , claimLostRefAddress = refAddr
      }

claimPeriodDidNotPassForActiveLoan :: DappScripts -> EmulatorTrace ()
claimPeriodDidNotPassForActiveLoan ts@DappScripts{..} = do
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

  void $ waitUntilSlot 15

  active1 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 100
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId1 1
                                 )

  callEndpoint @"claim-lost" h1 $
    ClaimLostParams
      { claimLostBeaconsBurned = [("Active",-1),(borrowerToken,-1),(loanId1,-1)]
      , claimLostBeaconRedeemer = BurnBeacons
      , claimLostLoanAddress = loanAddr
      , claimLostRedeemer = UnlockLostCollateral
      , claimLostInputs = [active1]
      , claimLostWithTTL = True
      , claimLostScripts = ts
      , claimLostWithRefScripts = True
      , claimLostSpendRefScript = spendRef
      , claimLostMintRefScript = mintRef
      , claimLostRefAddress = refAddr
      }

successfullyCleanUpFinishedLoanEarly :: DappScripts -> EmulatorTrace ()
successfullyCleanUpFinishedLoanEarly ts@DappScripts{..} = do
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

  void $ waitUntilSlot 12

  finished1 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol loanId1 1
                                 )

  callEndpoint @"claim-lost" h1 $
    ClaimLostParams
      { claimLostBeaconsBurned = [("Active",-1),(loanId1,-1)]
      , claimLostBeaconRedeemer = BurnBeacons
      , claimLostLoanAddress = loanAddr
      , claimLostRedeemer = UnlockLostCollateral
      , claimLostInputs = [finished1]
      , claimLostWithTTL = True
      , claimLostScripts = ts
      , claimLostWithRefScripts = True
      , claimLostSpendRefScript = spendRef
      , claimLostMintRefScript = mintRef
      , claimLostRefAddress = refAddr
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

  void $ waitUntilSlot 12

  finished1 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol loanId1 1
                                 )

  callEndpoint @"claim-lost" h2 $
    ClaimLostParams
      { claimLostBeaconsBurned = [("Active",-1),(loanId1,-1)]
      , claimLostBeaconRedeemer = BurnBeacons
      , claimLostLoanAddress = loanAddr
      , claimLostRedeemer = UnlockLostCollateral
      , claimLostInputs = [finished1]
      , claimLostWithTTL = True
      , claimLostScripts = ts
      , claimLostWithRefScripts = True
      , claimLostSpendRefScript = spendRef
      , claimLostMintRefScript = mintRef
      , claimLostRefAddress = refAddr
      }

inputMissingActiveBeacon :: DappScripts -> EmulatorTrace ()
inputMissingActiveBeacon ts@DappScripts{..} = do
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
        , nextCheckpoints = [exp + 10]
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
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 100
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

  active1 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 100
                                 )

  callEndpoint @"claim-lost" h1 $
    ClaimLostParams
      { claimLostBeaconsBurned = []
      , claimLostBeaconRedeemer = BurnBeacons
      , claimLostLoanAddress = loanAddr
      , claimLostRedeemer = UnlockLostCollateral
      , claimLostInputs = [active1]
      , claimLostWithTTL = True
      , claimLostScripts = ts
      , claimLostWithRefScripts = True
      , claimLostSpendRefScript = spendRef
      , claimLostMintRefScript = mintRef
      , claimLostRefAddress = refAddr
      }

notAllActiveBeaconsBurned :: DappScripts -> EmulatorTrace ()
notAllActiveBeaconsBurned ts@DappScripts{..} = do
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

  void $ waitUntilSlot 12

  finished1 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol loanId1 1
                                 )

  callEndpoint @"claim-lost" h1 $
    ClaimLostParams
      { claimLostBeaconsBurned = [(loanId1,-1)]
      , claimLostBeaconRedeemer = BurnBeacons
      , claimLostLoanAddress = loanAddr
      , claimLostRedeemer = UnlockLostCollateral
      , claimLostInputs = [finished1]
      , claimLostWithTTL = True
      , claimLostScripts = ts
      , claimLostWithRefScripts = True
      , claimLostSpendRefScript = spendRef
      , claimLostMintRefScript = mintRef
      , claimLostRefAddress = refAddr
      }

loanIdNotBurned :: DappScripts -> EmulatorTrace ()
loanIdNotBurned ts@DappScripts{..} = do
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

  void $ waitUntilSlot 12

  finished1 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol loanId1 1
                                 )

  callEndpoint @"claim-lost" h1 $
    ClaimLostParams
      { claimLostBeaconsBurned = [("Active",-1)]
      , claimLostBeaconRedeemer = BurnBeacons
      , claimLostLoanAddress = loanAddr
      , claimLostRedeemer = UnlockLostCollateral
      , claimLostInputs = [finished1]
      , claimLostWithTTL = True
      , claimLostScripts = ts
      , claimLostWithRefScripts = True
      , claimLostSpendRefScript = spendRef
      , claimLostMintRefScript = mintRef
      , claimLostRefAddress = refAddr
      }

borrowerIdNotBurned :: DappScripts -> EmulatorTrace ()
borrowerIdNotBurned ts@DappScripts{..} = do
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

  void $ waitUntilSlot 30

  active1 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 100
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId1 1
                                 )

  callEndpoint @"claim-lost" h1 $
    ClaimLostParams
      { claimLostBeaconsBurned = [("Active",-1),(loanId1,-1)]
      , claimLostBeaconRedeemer = BurnBeacons
      , claimLostLoanAddress = loanAddr
      , claimLostRedeemer = UnlockLostCollateral
      , claimLostInputs = [active1]
      , claimLostWithTTL = True
      , claimLostScripts = ts
      , claimLostWithRefScripts = True
      , claimLostSpendRefScript = spendRef
      , claimLostMintRefScript = mintRef
      , claimLostRefAddress = refAddr
      }

-------------------------------------------------
-- Test Function
-------------------------------------------------
tests :: DappScripts -> TestTree
tests ts = do
  let opts = defaultCheckOptions & emulatorConfig .~ emConfig
  testGroup "Claim Lost Collateral"
    [ checkPredicateOptions opts "Successfully claim lost collateral"
        assertNoFailedTransactions (successfullyClaimSingleLostCollateral ts)
    , checkPredicateOptions opts "Successfully clean up finished loan early"
        assertNoFailedTransactions (successfullyCleanUpFinishedLoanEarly ts)
    , checkPredicateOptions opts "Fail if claim period did not pass for active loan"
        (Test.not assertNoFailedTransactions) (claimPeriodDidNotPassForActiveLoan ts)
    , checkPredicateOptions opts "Fail if address owner did not approve"
        (Test.not assertNoFailedTransactions) (borrowerDidNotApprove ts)
    , checkPredicateOptions opts "Fail if input is missing Active beacon"
        (Test.not assertNoFailedTransactions) (inputMissingActiveBeacon ts)
    , checkPredicateOptions opts "Fail if not all Active beacons burned"
        (Test.not assertNoFailedTransactions) (notAllActiveBeaconsBurned ts)
    , checkPredicateOptions opts "Fail if not all LoanIDs burned"
        (Test.not assertNoFailedTransactions) (loanIdNotBurned ts)
    , checkPredicateOptions opts "Fail if not all BorrowerIDs burned"
        (Test.not assertNoFailedTransactions) (borrowerIdNotBurned ts)
    ]

testTrace :: DappScripts -> IO ()
testTrace = runEmulatorTraceIO' def emConfig . inputMissingActiveBeacon