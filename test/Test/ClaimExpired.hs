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

module Test.ClaimExpired
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
-- Claim Expired Scenarios
-------------------------------------------------
successfullyClaimSingleExpiredLoan :: DappScripts -> EmulatorTrace ()
successfullyClaimSingleExpiredLoan ts@DappScripts{..} = do
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

  callEndpoint @"claim-expired" h2 $
    ClaimExpiredParams
      { claimExpiredBeaconsBurned = [("Active",-1),(borrowerToken,-1),(loanId1,-2)]
      , claimExpiredBeaconRedeemer = BurnBeacons
      , claimExpiredLoanAddress = loanAddr
      , claimExpiredRedeemer = ClaimExpired
      , claimExpiredInputs = [active1]
      , claimExpiredWithTTL = True
      , claimExpiredScripts = ts
      , claimExpiredWithRefScripts = True
      , claimExpiredSpendRefScript = spendRef
      , claimExpiredMintRefScript = mintRef
      , claimExpiredRefAddress = refAddr
      }

successfullyClaimMultipleExpiredLoans :: DappScripts -> EmulatorTrace ()
successfullyClaimMultipleExpiredLoans ts@DappScripts{..} = do
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

  void $ waitUntilSlot 22

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

  callEndpoint @"claim-expired" h2 $
    ClaimExpiredParams
      { claimExpiredBeaconsBurned = 
          [ ("Active",-2)
          , (borrowerToken,-2)
          , (loanId1,-2)
          , (loanId2,-2)
          ]
      , claimExpiredBeaconRedeemer = BurnBeacons
      , claimExpiredLoanAddress = loanAddr
      , claimExpiredRedeemer = ClaimExpired
      , claimExpiredInputs = [active1,active2]
      , claimExpiredWithTTL = True
      , claimExpiredScripts = ts
      , claimExpiredWithRefScripts = True
      , claimExpiredSpendRefScript = spendRef
      , claimExpiredMintRefScript = mintRef
      , claimExpiredRefAddress = refAddr
      }

datumIsNotActiveDatum :: DappScripts -> EmulatorTrace ()
datumIsNotActiveDatum ts@DappScripts{..} = do
  h1 <- activateContractWallet (knownWallet 1) endpoints

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

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      askDatum = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = credentialAsToken borrowerCred
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanTerm = 12000
        , collateral = [testToken1]
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  spendRef <- txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef
  
  callEndpoint @"create-ask" h1 $
    CreateAskParams
      { createAskBeaconsMinted = []
      , createAskBeaconRedeemer = MintAskBeacon borrowerCred
      , createAskLoanAddress = loanAddr
      , createAskUTxOs = 
          [ ( Just askDatum
            , lovelaceValueOf 3_000_000
            )
          ]
      , createAskAsInline = True
      , createAskScripts = ts
      , createAskWithRefScript = True
      , createAskRefScript = mintRef
      , createAskRefAddress = refAddr
      }

  void $ waitUntilSlot 6

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000)
            askDatum

  callEndpoint @"claim-expired" h1 $
    ClaimExpiredParams
      { claimExpiredBeaconsBurned = 
          []
      , claimExpiredBeaconRedeemer = BurnBeacons
      , claimExpiredLoanAddress = loanAddr
      , claimExpiredRedeemer = ClaimExpired
      , claimExpiredInputs = [ask1]
      , claimExpiredWithTTL = True
      , claimExpiredScripts = ts
      , claimExpiredWithRefScripts = True
      , claimExpiredSpendRefScript = spendRef
      , claimExpiredMintRefScript = mintRef
      , claimExpiredRefAddress = refAddr
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

  void $ waitUntilSlot 20

  active1 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 100
                                 )

  callEndpoint @"claim-expired" h2 $
    ClaimExpiredParams
      { claimExpiredBeaconsBurned = []
      , claimExpiredBeaconRedeemer = BurnBeacons
      , claimExpiredLoanAddress = loanAddr
      , claimExpiredRedeemer = ClaimExpired
      , claimExpiredInputs = [active1]
      , claimExpiredWithTTL = True
      , claimExpiredScripts = ts
      , claimExpiredWithRefScripts = True
      , claimExpiredSpendRefScript = spendRef
      , claimExpiredMintRefScript = mintRef
      , claimExpiredRefAddress = refAddr
      }

successfullyClaimFullyPaidLoan :: DappScripts -> EmulatorTrace ()
successfullyClaimFullyPaidLoan ts@DappScripts{..} = do
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

  callEndpoint @"claim-expired" h2 $
    ClaimExpiredParams
      { claimExpiredBeaconsBurned = [("Active",-1),(loanId1,-2)]
      , claimExpiredBeaconRedeemer = BurnBeacons
      , claimExpiredLoanAddress = loanAddr
      , claimExpiredRedeemer = ClaimExpired
      , claimExpiredInputs = [finished1]
      , claimExpiredWithTTL = True
      , claimExpiredScripts = ts
      , claimExpiredWithRefScripts = True
      , claimExpiredSpendRefScript = spendRef
      , claimExpiredMintRefScript = mintRef
      , claimExpiredRefAddress = refAddr
      }

loanStillActive :: DappScripts -> EmulatorTrace ()
loanStillActive ts@DappScripts{..} = do
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

  callEndpoint @"claim-expired" h2 $
    ClaimExpiredParams
      { claimExpiredBeaconsBurned = [("Active",-1),(borrowerToken,-1),(loanId1,-2)]
      , claimExpiredBeaconRedeemer = BurnBeacons
      , claimExpiredLoanAddress = loanAddr
      , claimExpiredRedeemer = ClaimExpired
      , claimExpiredInputs = [active1]
      , claimExpiredWithTTL = True
      , claimExpiredScripts = ts
      , claimExpiredWithRefScripts = True
      , claimExpiredSpendRefScript = spendRef
      , claimExpiredMintRefScript = mintRef
      , claimExpiredRefAddress = refAddr
      }

twoLoanIdsNotBurnedForOnlyLoan :: DappScripts -> EmulatorTrace ()
twoLoanIdsNotBurnedForOnlyLoan ts@DappScripts{..} = do
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

  callEndpoint @"claim-expired" h2 $
    ClaimExpiredParams
      { claimExpiredBeaconsBurned = [("Active",-1),(borrowerToken,-1),(loanId1,-1)]
      , claimExpiredBeaconRedeemer = BurnBeacons
      , claimExpiredLoanAddress = loanAddr
      , claimExpiredRedeemer = ClaimExpired
      , claimExpiredInputs = [active1]
      , claimExpiredWithTTL = True
      , claimExpiredScripts = ts
      , claimExpiredWithRefScripts = True
      , claimExpiredSpendRefScript = spendRef
      , claimExpiredMintRefScript = mintRef
      , claimExpiredRefAddress = refAddr
      }

twoLoanIdsNotBurnedForAtLeastOneLoan :: DappScripts -> EmulatorTrace ()
twoLoanIdsNotBurnedForAtLeastOneLoan ts@DappScripts{..} = do
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

  void $ waitUntilSlot 22

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

  callEndpoint @"claim-expired" h2 $
    ClaimExpiredParams
      { claimExpiredBeaconsBurned = 
          [ ("Active",-2)
          , (borrowerToken,-2)
          , (loanId1,-1)
          , (loanId2,-2)
          ]
      , claimExpiredBeaconRedeemer = BurnBeacons
      , claimExpiredLoanAddress = loanAddr
      , claimExpiredRedeemer = ClaimExpired
      , claimExpiredInputs = [active1,active2]
      , claimExpiredWithTTL = True
      , claimExpiredScripts = ts
      , claimExpiredWithRefScripts = True
      , claimExpiredSpendRefScript = spendRef
      , claimExpiredMintRefScript = mintRef
      , claimExpiredRefAddress = refAddr
      }

onlyActiveBeaconNotBurned :: DappScripts -> EmulatorTrace ()
onlyActiveBeaconNotBurned ts@DappScripts{..} = do
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

  callEndpoint @"claim-expired" h2 $
    ClaimExpiredParams
      { claimExpiredBeaconsBurned = [(borrowerToken,-1),(loanId1,-2)]
      , claimExpiredBeaconRedeemer = BurnBeacons
      , claimExpiredLoanAddress = loanAddr
      , claimExpiredRedeemer = ClaimExpired
      , claimExpiredInputs = [active1]
      , claimExpiredWithTTL = True
      , claimExpiredScripts = ts
      , claimExpiredWithRefScripts = True
      , claimExpiredSpendRefScript = spendRef
      , claimExpiredMintRefScript = mintRef
      , claimExpiredRefAddress = refAddr
      }

onlyBorrowerIdNotBurned :: DappScripts -> EmulatorTrace ()
onlyBorrowerIdNotBurned ts@DappScripts{..} = do
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

  callEndpoint @"claim-expired" h2 $
    ClaimExpiredParams
      { claimExpiredBeaconsBurned = [("Active",-1),(loanId1,-2)]
      , claimExpiredBeaconRedeemer = BurnBeacons
      , claimExpiredLoanAddress = loanAddr
      , claimExpiredRedeemer = ClaimExpired
      , claimExpiredInputs = [active1]
      , claimExpiredWithTTL = True
      , claimExpiredScripts = ts
      , claimExpiredWithRefScripts = True
      , claimExpiredSpendRefScript = spendRef
      , claimExpiredMintRefScript = mintRef
      , claimExpiredRefAddress = refAddr
      }

atLeastOneBorrowerIdNotBurned :: DappScripts -> EmulatorTrace ()
atLeastOneBorrowerIdNotBurned ts@DappScripts{..} = do
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

  void $ waitUntilSlot 22

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

  callEndpoint @"claim-expired" h2 $
    ClaimExpiredParams
      { claimExpiredBeaconsBurned = 
          [ ("Active",-2)
          , (borrowerToken,-1)
          , (loanId1,-2)
          , (loanId2,-2)
          ]
      , claimExpiredBeaconRedeemer = BurnBeacons
      , claimExpiredLoanAddress = loanAddr
      , claimExpiredRedeemer = ClaimExpired
      , claimExpiredInputs = [active1,active2]
      , claimExpiredWithTTL = True
      , claimExpiredScripts = ts
      , claimExpiredWithRefScripts = True
      , claimExpiredSpendRefScript = spendRef
      , claimExpiredMintRefScript = mintRef
      , claimExpiredRefAddress = refAddr
      }

atLeastOneActiveBeaconNotBurned :: DappScripts -> EmulatorTrace ()
atLeastOneActiveBeaconNotBurned ts@DappScripts{..} = do
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

  void $ waitUntilSlot 22

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

  callEndpoint @"claim-expired" h2 $
    ClaimExpiredParams
      { claimExpiredBeaconsBurned = 
          [ ("Active",-1)
          , (borrowerToken,-2)
          , (loanId1,-2)
          , (loanId2,-2)
          ]
      , claimExpiredBeaconRedeemer = BurnBeacons
      , claimExpiredLoanAddress = loanAddr
      , claimExpiredRedeemer = ClaimExpired
      , claimExpiredInputs = [active1,active2]
      , claimExpiredWithTTL = True
      , claimExpiredScripts = ts
      , claimExpiredWithRefScripts = True
      , claimExpiredSpendRefScript = spendRef
      , claimExpiredMintRefScript = mintRef
      , claimExpiredRefAddress = refAddr
      }

ttlNotSpecified :: DappScripts -> EmulatorTrace ()
ttlNotSpecified ts@DappScripts{..} = do
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

  callEndpoint @"claim-expired" h2 $
    ClaimExpiredParams
      { claimExpiredBeaconsBurned = [("Active",-1),(borrowerToken,-1),(loanId1,-2)]
      , claimExpiredBeaconRedeemer = BurnBeacons
      , claimExpiredLoanAddress = loanAddr
      , claimExpiredRedeemer = ClaimExpired
      , claimExpiredInputs = [active1]
      , claimExpiredWithTTL = False
      , claimExpiredScripts = ts
      , claimExpiredWithRefScripts = True
      , claimExpiredSpendRefScript = spendRef
      , claimExpiredMintRefScript = mintRef
      , claimExpiredRefAddress = refAddr
      }

-------------------------------------------------
-- Test Function
-------------------------------------------------
tests :: DappScripts -> TestTree
tests ts = do
  let opts = defaultCheckOptions & emulatorConfig .~ emConfig
  testGroup "Claim Expired Loan(s)"
    [ checkPredicateOptions opts "Successfully claim single expired loan"
        assertNoFailedTransactions (successfullyClaimSingleExpiredLoan ts)
    , checkPredicateOptions opts "Successfully claim multiple expired loans"
        assertNoFailedTransactions (successfullyClaimMultipleExpiredLoans ts)
    , checkPredicateOptions opts "Successfully claim fully paid loan"
        assertNoFailedTransactions (successfullyClaimFullyPaidLoan ts)
    , checkPredicateOptions opts "Fail if datum is not an ActiveDatum"
        (Test.not assertNoFailedTransactions) (datumIsNotActiveDatum ts)
    , checkPredicateOptions opts "Fail if input is missing Active beacon"
        (Test.not assertNoFailedTransactions) (inputMissingActiveBeacon ts)
    , checkPredicateOptions opts "Fail if loan is still active"
        (Test.not assertNoFailedTransactions) (loanStillActive ts)
    , checkPredicateOptions opts "Fail if two LoanIDs not burned for only loan"
        (Test.not assertNoFailedTransactions) (twoLoanIdsNotBurnedForOnlyLoan ts)
    , checkPredicateOptions opts "Fail if two LoanIDs not burned for at least one loan"
        (Test.not assertNoFailedTransactions) (twoLoanIdsNotBurnedForAtLeastOneLoan ts)
    , checkPredicateOptions opts "Fail if only Active beacon not burned"
        (Test.not assertNoFailedTransactions) (onlyActiveBeaconNotBurned ts)
    , checkPredicateOptions opts "Fail if only BorrowerID not burned"
        (Test.not assertNoFailedTransactions) (onlyBorrowerIdNotBurned ts)
    , checkPredicateOptions opts "Fail if at least one BorrowerID not burned"
        (Test.not assertNoFailedTransactions) (atLeastOneBorrowerIdNotBurned ts)
    , checkPredicateOptions opts "Fail if at least one Active beacon not burned"
        (Test.not assertNoFailedTransactions) (atLeastOneActiveBeaconNotBurned ts)
    , checkPredicateOptions opts "Fail if invalid-before not specified"
        (Test.not assertNoFailedTransactions) (ttlNotSpecified ts)
    ]

testTrace :: DappScripts -> IO ()
testTrace = runEmulatorTraceIO' def emConfig . twoLoanIdsNotBurnedForOnlyLoan