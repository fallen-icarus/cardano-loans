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

module Test.Rollover
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
-- Rollover Scenarios
-------------------------------------------------
successfullyRolloverSingleLoan :: DappScripts -> EmulatorTrace ()
successfullyRolloverSingleLoan ts@DappScripts{..} = do
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

successfullyRolloverMultipleLoans :: DappScripts -> EmulatorTrace ()
successfullyRolloverMultipleLoans ts@DappScripts{..} = do
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
        , loanCheckpoints = [20]
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
        , nextCheckpoints = [exp+20]
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

  void $ waitUntilSlot 10

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
  let newActiveDatum1 = activeDatum1
        { loanOutstanding = loanOutstanding activeDatum1 * (fromInteger 1 + loanInterest activeDatum1) 
        , nextCheckpoints = []
        , pastCheckpoints = [exp + 10]
        }
      newActiveDatum2 = activeDatum2
        { loanOutstanding = loanOutstanding activeDatum2 * (fromInteger 1 + loanInterest activeDatum2) 
        , nextCheckpoints = []
        , pastCheckpoints = [exp + 20]
        }

  callEndpoint @"rollover" h1 $
    RolloverParams
      { rolloverLoanAddress = loanAddr
      , rolloverRedeemer = Rollover
      , rolloverInputs = [active1,active2]
      , rolloverOutputAddress = loanAddr
      , rolloverOutputs = 
          [ ( Just newActiveDatum1
            , lovelaceValueOf 3_000_000
           <> (uncurry singleton testToken1) 100
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol borrowerToken 1
           <> singleton beaconCurrencySymbol loanId1 1
            )
          , ( Just newActiveDatum2
            , lovelaceValueOf 3_000_000
           <> (uncurry singleton testToken1) 50
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol borrowerToken 1
           <> singleton beaconCurrencySymbol loanId2 1
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

  callEndpoint @"rollover" h2 $
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

  void $ waitUntilSlot 25

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

interestNotApplied :: DappScripts -> EmulatorTrace ()
interestNotApplied ts@DappScripts{..} = do
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
          { nextCheckpoints = []
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

checkpointsNotUpdated :: DappScripts -> EmulatorTrace ()
checkpointsNotUpdated ts@DappScripts{..} = do
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
          , nextCheckpoints = [10]
          , pastCheckpoints = []
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

utxoValueChanged :: DappScripts -> EmulatorTrace ()
utxoValueChanged ts@DappScripts{..} = do
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
           <> (uncurry singleton testToken1) 50
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

outputToWrongAddress :: DappScripts -> EmulatorTrace ()
outputToWrongAddress ts@DappScripts{..} = do
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
      , rolloverOutputAddress = Address lenderCred Nothing
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

-------------------------------------------------
-- Test Function
-------------------------------------------------
tests :: DappScripts -> TestTree
tests ts = do
  let opts = defaultCheckOptions & emulatorConfig .~ emConfig
  testGroup "Make Payment(s)"
    [ checkPredicateOptions opts "Successfully rollover a single loan"
        assertNoFailedTransactions (successfullyRolloverSingleLoan ts)
    , checkPredicateOptions opts "Successfully rollover multiple loans"
        assertNoFailedTransactions (successfullyRolloverMultipleLoans ts)
    , checkPredicateOptions opts "Fail if borrower did not approve"
        (Test.not assertNoFailedTransactions) (borrowerDidNotApprove ts)
    , checkPredicateOptions opts "Fail if input is missing Active beacon"
        (Test.not assertNoFailedTransactions) (inputMissingActiveBeacon ts)
    , checkPredicateOptions opts "Fail if only loan input is expired"
        (Test.not assertNoFailedTransactions) (onlyLoanIsExpired ts)
    , checkPredicateOptions opts "Fail if interest not applied"
        (Test.not assertNoFailedTransactions) (interestNotApplied ts)
    , checkPredicateOptions opts "Fail if checkpoints not updated"
        (Test.not assertNoFailedTransactions) (checkpointsNotUpdated ts)
    , checkPredicateOptions opts "Fail if collateral UTxO value changed"
        (Test.not assertNoFailedTransactions) (utxoValueChanged ts)
    , checkPredicateOptions opts "Fail if collateral UTxO locked at wrong address"
        (Test.not assertNoFailedTransactions) (outputToWrongAddress ts)
    ]

testTrace :: DappScripts -> IO ()
testTrace = runEmulatorTraceIO' def emConfig . outputToWrongAddress