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

module Test.UpdateAddress
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
-- Update Address Scenarios
-------------------------------------------------
successfullyUpdateAddress :: DappScripts -> EmulatorTrace ()
successfullyUpdateAddress ts@DappScripts{..} = do
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

  
  let newAddr = lenderAddr{addressStakingCredential = Just $ StakingHash lenderCred}
      newActiveDatum = activeDatum{ lenderAddress = newAddr }

  callEndpoint @"update-address" h2 $
    UpdateAddressParams
      { updateAddressLoanAddress = loanAddr
      , updateAddressRedeemer = UpdateLenderAddress newAddr
      , updateAddressInputs = [active1]
      , updateAddressKeyAddresses = [lenderAddr]
      , updateAddressLoanIds = [loanId1]
      , updateAddressIncludeKeyNFT = True
      , updateAddressOutputAddress = loanAddr
      , updateAddressOutputs = 
          [ ( Just newActiveDatum
            , lovelaceValueOf 3_000_000
           <> (uncurry singleton testToken1) 100
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol borrowerToken 1
           <> singleton beaconCurrencySymbol loanId1 1
            )
          ]
      , updateAddressAsInline = True
      , updateAddressScripts = ts
      , updateAddressWithRefScripts = True
      , updateAddressSpendRefScript = spendRef
      , updateAddressMintRefScript = mintRef
      , updateAddressRefAddress = refAddr
      } 

inputMissingLoanId :: DappScripts -> EmulatorTrace ()
inputMissingLoanId ts@DappScripts{..} = do
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
  
  let newAddr = lenderAddr{addressStakingCredential = Just $ StakingHash lenderCred}
      newActiveDatum = activeDatum{ lenderAddress = newAddr }

  callEndpoint @"update-address" h2 $
    UpdateAddressParams
      { updateAddressLoanAddress = loanAddr
      , updateAddressRedeemer = UpdateLenderAddress newAddr
      , updateAddressInputs = [active1]
      , updateAddressKeyAddresses = [lenderAddr]
      , updateAddressLoanIds = [loanId1]
      , updateAddressIncludeKeyNFT = False
      , updateAddressOutputAddress = loanAddr
      , updateAddressOutputs = 
          [ ( Just newActiveDatum
            , lovelaceValueOf 3_000_000
           <> (uncurry singleton testToken1) 100
            )
          ]
      , updateAddressAsInline = True
      , updateAddressScripts = ts
      , updateAddressWithRefScripts = True
      , updateAddressSpendRefScript = spendRef
      , updateAddressMintRefScript = mintRef
      , updateAddressRefAddress = refAddr
      } 

missingKeyNft :: DappScripts -> EmulatorTrace ()
missingKeyNft ts@DappScripts{..} = do
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

  
  let newAddr = lenderAddr{addressStakingCredential = Just $ StakingHash lenderCred}
      newActiveDatum = activeDatum{ lenderAddress = newAddr }

  callEndpoint @"update-address" h1 $
    UpdateAddressParams
      { updateAddressLoanAddress = loanAddr
      , updateAddressRedeemer = UpdateLenderAddress newAddr
      , updateAddressInputs = [active1]
      , updateAddressKeyAddresses = [lenderAddr]
      , updateAddressLoanIds = [loanId1]
      , updateAddressIncludeKeyNFT = False
      , updateAddressOutputAddress = loanAddr
      , updateAddressOutputs = 
          [ ( Just newActiveDatum
            , lovelaceValueOf 3_000_000
           <> (uncurry singleton testToken1) 100
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol borrowerToken 1
           <> singleton beaconCurrencySymbol loanId1 1
            )
          ]
      , updateAddressAsInline = True
      , updateAddressScripts = ts
      , updateAddressWithRefScripts = True
      , updateAddressSpendRefScript = spendRef
      , updateAddressMintRefScript = mintRef
      , updateAddressRefAddress = refAddr
      } 

newAddressUsesPaymentScript :: DappScripts -> EmulatorTrace ()
newAddressUsesPaymentScript ts@DappScripts{..} = do
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

  
  let newAddr = lenderAddr{addressStakingCredential = Just $ StakingHash lenderCred}
      newActiveDatum = activeDatum{ lenderAddress = newAddr }

  callEndpoint @"update-address" h2 $
    UpdateAddressParams
      { updateAddressLoanAddress = loanAddr
      , updateAddressRedeemer = 
          UpdateLenderAddress $ Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
      , updateAddressInputs = [active1]
      , updateAddressKeyAddresses = [lenderAddr]
      , updateAddressLoanIds = [loanId1]
      , updateAddressIncludeKeyNFT = True
      , updateAddressOutputAddress = loanAddr
      , updateAddressOutputs = 
          [ ( Just newActiveDatum
            , lovelaceValueOf 3_000_000
           <> (uncurry singleton testToken1) 100
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol borrowerToken 1
           <> singleton beaconCurrencySymbol loanId1 1
            )
          ]
      , updateAddressAsInline = True
      , updateAddressScripts = ts
      , updateAddressWithRefScripts = True
      , updateAddressSpendRefScript = spendRef
      , updateAddressMintRefScript = mintRef
      , updateAddressRefAddress = refAddr
      } 

missingLoanOutput :: DappScripts -> EmulatorTrace ()
missingLoanOutput ts@DappScripts{..} = do
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

  
  let newAddr = lenderAddr{addressStakingCredential = Just $ StakingHash lenderCred}

  callEndpoint @"update-address" h2 $
    UpdateAddressParams
      { updateAddressLoanAddress = loanAddr
      , updateAddressRedeemer = UpdateLenderAddress newAddr
      , updateAddressInputs = [active1]
      , updateAddressKeyAddresses = [lenderAddr]
      , updateAddressLoanIds = [loanId1]
      , updateAddressIncludeKeyNFT = True
      , updateAddressOutputAddress = lenderAddr
      , updateAddressOutputs = 
          [ ( Nothing
            , lovelaceValueOf 3_000_000
           <> (uncurry singleton testToken1) 100
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol borrowerToken 1
           <> singleton beaconCurrencySymbol loanId1 1
            )
          ]
      , updateAddressAsInline = True
      , updateAddressScripts = ts
      , updateAddressWithRefScripts = True
      , updateAddressSpendRefScript = spendRef
      , updateAddressMintRefScript = mintRef
      , updateAddressRefAddress = refAddr
      } 

otherDatumFieldsChanged :: DappScripts -> EmulatorTrace ()
otherDatumFieldsChanged ts@DappScripts{..} = do
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

  
  let newAddr = lenderAddr{addressStakingCredential = Just $ StakingHash lenderCred}
      newActiveDatum = activeDatum{ lenderAddress = newAddr, loanPrinciple = 0 }

  callEndpoint @"update-address" h2 $
    UpdateAddressParams
      { updateAddressLoanAddress = loanAddr
      , updateAddressRedeemer = UpdateLenderAddress newAddr
      , updateAddressInputs = [active1]
      , updateAddressKeyAddresses = [lenderAddr]
      , updateAddressLoanIds = [loanId1]
      , updateAddressIncludeKeyNFT = True
      , updateAddressOutputAddress = loanAddr
      , updateAddressOutputs = 
          [ ( Just newActiveDatum
            , lovelaceValueOf 3_000_000
           <> (uncurry singleton testToken1) 100
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol borrowerToken 1
           <> singleton beaconCurrencySymbol loanId1 1
            )
          ]
      , updateAddressAsInline = True
      , updateAddressScripts = ts
      , updateAddressWithRefScripts = True
      , updateAddressSpendRefScript = spendRef
      , updateAddressMintRefScript = mintRef
      , updateAddressRefAddress = refAddr
      } 

valueChanged :: DappScripts -> EmulatorTrace ()
valueChanged ts@DappScripts{..} = do
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

  
  let newAddr = lenderAddr{addressStakingCredential = Just $ StakingHash lenderCred}
      newActiveDatum = activeDatum{ lenderAddress = newAddr }

  callEndpoint @"update-address" h2 $
    UpdateAddressParams
      { updateAddressLoanAddress = loanAddr
      , updateAddressRedeemer = UpdateLenderAddress newAddr
      , updateAddressInputs = [active1]
      , updateAddressKeyAddresses = [lenderAddr]
      , updateAddressLoanIds = [loanId1]
      , updateAddressIncludeKeyNFT = True
      , updateAddressOutputAddress = loanAddr
      , updateAddressOutputs = 
          [ ( Just newActiveDatum
            , lovelaceValueOf 3_000_000
           <> (uncurry singleton testToken1) 10
           <> singleton beaconCurrencySymbol "Active" 1
           <> singleton beaconCurrencySymbol borrowerToken 1
           <> singleton beaconCurrencySymbol loanId1 1
            )
          ]
      , updateAddressAsInline = True
      , updateAddressScripts = ts
      , updateAddressWithRefScripts = True
      , updateAddressSpendRefScript = spendRef
      , updateAddressMintRefScript = mintRef
      , updateAddressRefAddress = refAddr
      } 

benchUpdate :: DappScripts -> EmulatorTrace ()
benchUpdate ts@DappScripts{..} = do
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
      askDatum10 = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 5_000_000
        , loanTerm = 10000
        , collateral = [testToken1]
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  callEndpoint @"create-ask" h1 $
    CreateAskParams
      { createAskBeaconsMinted = [("Ask",10)]
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
          , ( Just askDatum10
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
        , loanCheckpoints = [10]
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
        , loanCheckpoints = [10]
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
        , loanCheckpoints = [10]
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
        , loanCheckpoints = [10]
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
        , loanCheckpoints = [10]
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
        , loanCheckpoints = [10]
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
        , loanCheckpoints = [10]
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
        , loanCheckpoints = [10]
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimPeriod = 10000
        }
      offerDatum10 = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 5_000_000
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

  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum10
            , lovelaceValueOf 10_000_000 
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

  void $ waitUntilSlot 26

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
  ask10 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum10
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
  offer10 <- txOutRefWithValueAndDatum 
            ( lovelaceValueOf 10_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
            offerDatum10

  let exp = slotToBeginPOSIXTime def 26
      loanId1 = txOutRefToLoanId offer1
      loanId2 = txOutRefToLoanId offer2
      loanId3 = txOutRefToLoanId offer3
      loanId4 = txOutRefToLoanId offer4
      loanId5 = txOutRefToLoanId offer5
      loanId6 = txOutRefToLoanId offer6
      loanId7 = txOutRefToLoanId offer7
      loanId8 = txOutRefToLoanId offer8
      loanId9 = txOutRefToLoanId offer9
      loanId10 = txOutRefToLoanId offer10
      activeDatum1 = ActiveDatum
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
      activeDatum2 = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 50_000_000
        , nextCheckpoints = [exp + 10]
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
        , nextCheckpoints = [exp + 10]
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
        , nextCheckpoints = [exp + 10]
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
        , nextCheckpoints = [exp + 10]
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
        , nextCheckpoints = [exp + 10]
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
        , nextCheckpoints = [exp + 10 + 2000]
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
        , nextCheckpoints = [exp + 10 + 2000]
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
        , nextCheckpoints = [exp + 10 + 2000]
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000 + 2000
        , loanExpiration = exp + 10000 + 2000
        , loanOutstanding = fromInteger 10_000_000
        , loanId = loanId9
        }
      activeDatum10 = ActiveDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = borrowerToken
        , lenderAddress = lenderAddr
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 5_000_000
        , nextCheckpoints = [exp + 10 + 2000]
        , pastCheckpoints = []
        , loanTerm = 10000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 1 1_000_000)]
        , claimExpiration = exp + 10000 + 10000 + 2000
        , loanExpiration = exp + 10000 + 2000
        , loanOutstanding = fromInteger 5_000_000
        , loanId = loanId10
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

  void $ waitUntilSlot 28

  callEndpoint @"accept-offer" h1 $ 
    AcceptOfferParams
      { acceptOfferBeaconsMinted = 
          [ [ ("Ask",-4)
            , ("Offer",-4)
            , (lenderToken,-4)
            , (loanId7,2)
            , (loanId8,2)
            , (loanId9,2)
            , (loanId10,2)
            , ("Active",4)
            , (borrowerToken,4)
            ]
          ]
      , acceptOfferBeaconRedeemers = 
          [ MintActiveBeacon borrowerCred 
              [ (ask7,offer7)
              , (ask8,offer8)
              , (ask9,offer9)
              , (ask10,offer10)
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
            , ask10
            , offer10
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
            , ( Just activeDatum10
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 5
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId10 1
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
            , ( Nothing
              , lovelaceValueOf 5_000_000
             <> singleton beaconCurrencySymbol loanId10 1
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
  active10 <- txOutRefWithValue $ ( lovelaceValueOf 3_000_000
                                <> (uncurry singleton testToken1) 5
                                <> singleton beaconCurrencySymbol "Active" 1
                                <> singleton beaconCurrencySymbol borrowerToken 1
                                <> singleton beaconCurrencySymbol loanId10 1
                                 )

  let newAddr = lenderAddr{addressStakingCredential = Just $ StakingHash lenderCred}
      newActiveDatum1 = activeDatum1{ lenderAddress = newAddr }
      newActiveDatum2 = activeDatum2{ lenderAddress = newAddr }
      newActiveDatum3 = activeDatum3{ lenderAddress = newAddr }
      newActiveDatum4 = activeDatum4{ lenderAddress = newAddr }
      newActiveDatum5 = activeDatum5{ lenderAddress = newAddr }
      newActiveDatum6 = activeDatum6{ lenderAddress = newAddr }
      newActiveDatum7 = activeDatum7{ lenderAddress = newAddr }
      newActiveDatum8 = activeDatum8{ lenderAddress = newAddr }
      newActiveDatum9 = activeDatum9{ lenderAddress = newAddr }
      newActiveDatum10 = activeDatum10{ lenderAddress = newAddr }

  callEndpoint @"update-address" h2 $
    UpdateAddressParams
      { updateAddressLoanAddress = loanAddr
      , updateAddressRedeemer = UpdateLenderAddress newAddr
      , updateAddressInputs = 
          [ active1
          -- , active2
          -- , active3
          -- , active4
          -- , active5
          -- , active6
          -- , active7
          -- , active8
          -- , active9
          -- , active10
          ]
      , updateAddressKeyAddresses = 
          [ lenderAddr
          -- , lenderAddr
          -- , lenderAddr
          -- , lenderAddr
          -- , lenderAddr
          -- , lenderAddr
          -- , lenderAddr
          -- , lenderAddr
          -- , lenderAddr
          -- , lenderAddr
          ]
      , updateAddressLoanIds = 
          [ loanId1
          -- , loanId2
          -- , loanId3
          -- , loanId4
          -- , loanId5
          -- , loanId6
          -- , loanId7
          -- , loanId8
          -- , loanId9
          -- , loanId10
          ]
      , updateAddressIncludeKeyNFT = True
      , updateAddressOutputAddress = loanAddr
      , updateAddressOutputs = 
          [ ( Just newActiveDatum1
              , lovelaceValueOf 3_000_000
             <> (uncurry singleton testToken1) 100
             <> singleton beaconCurrencySymbol "Active" 1
             <> singleton beaconCurrencySymbol borrowerToken 1
             <> singleton beaconCurrencySymbol loanId1 1
              )
            -- , ( Just newActiveDatum2
            --   , lovelaceValueOf 3_000_000
            --  <> (uncurry singleton testToken1) 50
            --  <> singleton beaconCurrencySymbol "Active" 1
            --  <> singleton beaconCurrencySymbol borrowerToken 1
            --  <> singleton beaconCurrencySymbol loanId2 1
            --   )
            -- , ( Just newActiveDatum3
            --   , lovelaceValueOf 3_000_000
            --  <> (uncurry singleton testToken1) 110
            --  <> singleton beaconCurrencySymbol "Active" 1
            --  <> singleton beaconCurrencySymbol borrowerToken 1
            --  <> singleton beaconCurrencySymbol loanId3 1
            --   )
            -- , ( Just newActiveDatum4
            --   , lovelaceValueOf 3_000_000
            --  <> (uncurry singleton testToken1) 55
            --  <> singleton beaconCurrencySymbol "Active" 1
            --  <> singleton beaconCurrencySymbol borrowerToken 1
            --  <> singleton beaconCurrencySymbol loanId4 1
            --   )
            -- , ( Just newActiveDatum5
            --   , lovelaceValueOf 3_000_000
            --  <> (uncurry singleton testToken1) 20
            --  <> singleton beaconCurrencySymbol "Active" 1
            --  <> singleton beaconCurrencySymbol borrowerToken 1
            --  <> singleton beaconCurrencySymbol loanId5 1
            --   )
            -- , ( Just newActiveDatum6
            --   , lovelaceValueOf 3_000_000
            --  <> (uncurry singleton testToken1) 30
            --  <> singleton beaconCurrencySymbol "Active" 1
            --  <> singleton beaconCurrencySymbol borrowerToken 1
            --  <> singleton beaconCurrencySymbol loanId6 1
            --   )
            -- , ( Just newActiveDatum7
            --   , lovelaceValueOf 3_000_000
            --  <> (uncurry singleton testToken1) 40
            --  <> singleton beaconCurrencySymbol "Active" 1
            --  <> singleton beaconCurrencySymbol borrowerToken 1
            --  <> singleton beaconCurrencySymbol loanId7 1
            --   )
            -- , ( Just newActiveDatum8
            --   , lovelaceValueOf 3_000_000
            --  <> (uncurry singleton testToken1) 45
            --  <> singleton beaconCurrencySymbol "Active" 1
            --  <> singleton beaconCurrencySymbol borrowerToken 1
            --  <> singleton beaconCurrencySymbol loanId8 1
            --   )
            -- , ( Just newActiveDatum9
            --   , lovelaceValueOf 3_000_000
            --  <> (uncurry singleton testToken1) 10
            --  <> singleton beaconCurrencySymbol "Active" 1
            --  <> singleton beaconCurrencySymbol borrowerToken 1
            --  <> singleton beaconCurrencySymbol loanId9 1
            --   )
            -- , ( Just newActiveDatum10
            --   , lovelaceValueOf 3_000_000
            --  <> (uncurry singleton testToken1) 5
            --  <> singleton beaconCurrencySymbol "Active" 1
            --  <> singleton beaconCurrencySymbol borrowerToken 1
            --  <> singleton beaconCurrencySymbol loanId10 1
            --   )
          ]
      , updateAddressAsInline = True
      , updateAddressScripts = ts
      , updateAddressWithRefScripts = True
      , updateAddressSpendRefScript = spendRef
      , updateAddressMintRefScript = mintRef
      , updateAddressRefAddress = refAddr
      } 

-------------------------------------------------
-- Test Function
-------------------------------------------------
tests :: DappScripts -> TestTree
tests ts = do
  let opts = defaultCheckOptions & emulatorConfig .~ emConfig
  testGroup "Update Lender Address"
    [ checkPredicateOptions opts "Successfully update lender address"
        assertNoFailedTransactions (successfullyUpdateAddress ts)
    , checkPredicateOptions opts "Fail if input is missing LoanID"
        (Test.not assertNoFailedTransactions) (inputMissingLoanId ts)
    , checkPredicateOptions opts "Fail if missing Key NFT"
        (Test.not assertNoFailedTransactions) (missingKeyNft ts)
    , checkPredicateOptions opts "Fail if new address uses a payment script"
        (Test.not assertNoFailedTransactions) (newAddressUsesPaymentScript ts)
    , checkPredicateOptions opts "Fail if no output to loan address"
        (Test.not assertNoFailedTransactions) (missingLoanOutput ts)
    , checkPredicateOptions opts "Fail if other fields changed in datum"
        (Test.not assertNoFailedTransactions) (otherDatumFieldsChanged ts)
    , checkPredicateOptions opts "Fail if UTxO value changed"
        (Test.not assertNoFailedTransactions) (valueChanged ts)
    ]

testTrace :: DappScripts -> IO ()
testTrace = runEmulatorTraceIO' def emConfig . benchUpdate