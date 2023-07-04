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

module Test.CreateOffer
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

import Test.Common
import CardanoLoans

-------------------------------------------------
-- Create Offer Scenarios
-------------------------------------------------
successfullyCreateOffer :: DappScripts -> EmulatorTrace ()
successfullyCreateOffer ts@DappScripts{..} = do
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

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = Address lenderCred Nothing
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = [1,2,3]
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 0 1)]
        , claimPeriod = 10000
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  
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

mintMultipleOfferBeacons :: DappScripts -> EmulatorTrace ()
mintMultipleOfferBeacons ts@DappScripts{..} = do
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

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = Address lenderCred Nothing
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = [1,2,3]
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 0 1)]
        , claimPeriod = 10000
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  
  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",2),(lenderToken,1)]
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

mintMultipleLenderIds :: DappScripts -> EmulatorTrace ()
mintMultipleLenderIds ts@DappScripts{..} = do
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

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = Address lenderCred Nothing
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = [1,2,3]
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 0 1)]
        , claimPeriod = 10000
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  
  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,2)]
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

offerBeaconNotMintedToDappAddress :: DappScripts -> EmulatorTrace ()
offerBeaconNotMintedToDappAddress ts@DappScripts{..} = do
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

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = Address lenderCred Nothing
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = [1,2,3]
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 0 1)]
        , claimPeriod = 10000
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  
  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum
            , lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

lenderIdNotMintedToDappAddress :: DappScripts -> EmulatorTrace ()
lenderIdNotMintedToDappAddress ts@DappScripts{..} = do
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

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = Address lenderCred Nothing
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = [1,2,3]
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 0 1)]
        , claimPeriod = 10000
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  
  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum
            , lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

bothBeaconsNotMintedToDappAddress :: DappScripts -> EmulatorTrace ()
bothBeaconsNotMintedToDappAddress ts@DappScripts{..} = do
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

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = Address lenderCred Nothing
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = [1,2,3]
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 0 1)]
        , claimPeriod = 10000
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  
  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum
            , lovelaceValueOf 105_000_000 
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

mintBeaconsToDappAddressWithoutStaking :: DappScripts -> EmulatorTrace ()
mintBeaconsToDappAddressWithoutStaking ts@DappScripts{..} = do
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

  let lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = Address lenderCred Nothing
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = [1,2,3]
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 0 1)]
        , claimPeriod = 10000
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         Nothing

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  
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

beaconsStoredSeparately :: DappScripts -> EmulatorTrace ()
beaconsStoredSeparately ts@DappScripts{..} = do
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

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = Address lenderCred Nothing
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = [1,2,3]
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 0 1)]
        , claimPeriod = 10000
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  
  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum
            , lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offer" 1
            )
          , ( Just offerDatum
            , lovelaceValueOf 5_000_000
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

wrongBeaconSymbolInDatum :: DappScripts -> EmulatorTrace ()
wrongBeaconSymbolInDatum ts@DappScripts{..} = do
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

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      offerDatum = OfferDatum
        { beaconSym = adaSymbol
        , lenderId = lenderToken
        , lenderAddress = Address lenderCred Nothing
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = [1,2,3]
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 0 1)]
        , claimPeriod = 10000
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  
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

wrongLenderIdInDatum :: DappScripts -> EmulatorTrace ()
wrongLenderIdInDatum ts@DappScripts{..} = do
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

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = credentialAsToken borrowerCred
        , lenderAddress = Address lenderCred Nothing
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = [1,2,3]
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 0 1)]
        , claimPeriod = 10000
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  
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

lenderAddressUsesPaymentScript :: DappScripts -> EmulatorTrace ()
lenderAddressUsesPaymentScript ts@DappScripts{..} = do
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

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = [1,2,3]
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 0 1)]
        , claimPeriod = 10000
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  
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

datumHasNegativePrinciple :: DappScripts -> EmulatorTrace ()
datumHasNegativePrinciple ts@DappScripts{..} = do
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

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = Address lenderCred Nothing
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = -100_000_000
        , loanCheckpoints = [1,2,3]
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 0 1)]
        , claimPeriod = 10000
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  
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

datumHasZeroPrinciple :: DappScripts -> EmulatorTrace ()
datumHasZeroPrinciple ts@DappScripts{..} = do
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

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = Address lenderCred Nothing
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 0
        , loanCheckpoints = [1,2,3]
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 0 1)]
        , claimPeriod = 10000
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  
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

negativeCheckpoint :: DappScripts -> EmulatorTrace ()
negativeCheckpoint ts@DappScripts{..} = do
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

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = Address lenderCred Nothing
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = [1,-2,3]
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 0 1)]
        , claimPeriod = 10000
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  
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

unsortedCheckpoints :: DappScripts -> EmulatorTrace ()
unsortedCheckpoints ts@DappScripts{..} = do
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

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = Address lenderCred Nothing
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = [1,2,4,3]
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 0 1)]
        , claimPeriod = 10000
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  
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

datumHasNegativeTerm :: DappScripts -> EmulatorTrace ()
datumHasNegativeTerm ts@DappScripts{..} = do
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

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = Address lenderCred Nothing
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = [1,2,3]
        , loanTerm = -12000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 0 1)]
        , claimPeriod = 10000
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  
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

datumHasZeroTerm :: DappScripts -> EmulatorTrace ()
datumHasZeroTerm ts@DappScripts{..} = do
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

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = Address lenderCred Nothing
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = [1,2,3]
        , loanTerm = 0
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 0 1)]
        , claimPeriod = 10000
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  
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

successfullyAllowZeroInterest :: DappScripts -> EmulatorTrace ()
successfullyAllowZeroInterest ts@DappScripts{..} = do
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

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = Address lenderCred Nothing
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = [1,2,3]
        , loanTerm = 12000
        , loanInterest = unsafeRatio 0 10
        , collateralization = [(testToken1,unsafeRatio 0 1)]
        , claimPeriod = 10000
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  
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

datumHasNegativeInterest :: DappScripts -> EmulatorTrace ()
datumHasNegativeInterest ts@DappScripts{..} = do
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

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = Address lenderCred Nothing
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = [1,2,3]
        , loanTerm = 12000
        , loanInterest = unsafeRatio (-1) 10
        , collateralization = [(testToken1,unsafeRatio 0 1)]
        , claimPeriod = 10000
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  
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

emptyCollateral :: DappScripts -> EmulatorTrace ()
emptyCollateral ts@DappScripts{..} = do
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

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = Address lenderCred Nothing
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = [1,2,3]
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , collateralization = []
        , claimPeriod = 10000
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  
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

atLeastOneCollateralizationIsNegative :: DappScripts -> EmulatorTrace ()
atLeastOneCollateralizationIsNegative ts@DappScripts{..} = do
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

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = Address lenderCred Nothing
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = [1,2,3]
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio (-1) 1)]
        , claimPeriod = 10000
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  
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

datumHasZeroClaimPeriod :: DappScripts -> EmulatorTrace ()
datumHasZeroClaimPeriod ts@DappScripts{..} = do
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

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = Address lenderCred Nothing
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = [1,2,3]
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 0 1)]
        , claimPeriod = 0
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  
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

datumHasNegativeClaimPeriod :: DappScripts -> EmulatorTrace ()
datumHasNegativeClaimPeriod ts@DappScripts{..} = do
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

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = Address lenderCred Nothing
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = [1,2,3]
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 0 1)]
        , claimPeriod = -10000
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  
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

nonInlineDatum :: DappScripts -> EmulatorTrace ()
nonInlineDatum ts@DappScripts{..} = do
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

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = Address lenderCred Nothing
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = [1,2,3]
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 0 1)]
        , claimPeriod = 10000
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  
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
      , createOfferAsInline = False
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

beaconsNotStoredWithOfferDatum :: DappScripts -> EmulatorTrace ()
beaconsNotStoredWithOfferDatum ts@DappScripts{..} = do
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

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      offerDatum = AskDatum
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

beaconsNotStoredWithMininumADA :: DappScripts -> EmulatorTrace ()
beaconsNotStoredWithMininumADA ts@DappScripts{..} = do
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

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = Address lenderCred Nothing
        , loanAsset = testToken2
        , loanPrinciple = 10
        , loanCheckpoints = [1,2,3]
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 0 1)]
        , claimPeriod = 10000
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  
  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum
            , lovelaceValueOf 2_500_000 
           <> singleton beaconCurrencySymbol "Offer" 1
           <> singleton beaconCurrencySymbol lenderToken 1
           <> (uncurry singleton testToken2) 10
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

beaconsNotStoredWithLoanAmount :: DappScripts -> EmulatorTrace ()
beaconsNotStoredWithLoanAmount ts@DappScripts{..} = do
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

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = Address lenderCred Nothing
        , loanAsset = testToken2
        , loanPrinciple = 10
        , loanCheckpoints = [1,2,3]
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 0 1)]
        , claimPeriod = 10000
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  
  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum
            , lovelaceValueOf 5_000_000 
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

lenderDidNotApprove :: DappScripts -> EmulatorTrace ()
lenderDidNotApprove ts@DappScripts{..} = do
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
      lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = Address lenderCred Nothing
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = [1,2,3]
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 0 1)]
        , claimPeriod = 10000
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  
  callEndpoint @"create-offer" h1 $
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

offerBeaconHasWrongName :: DappScripts -> EmulatorTrace ()
offerBeaconHasWrongName ts@DappScripts{..} = do
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

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = Address lenderCred Nothing
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = [1,2,3]
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 0 1)]
        , claimPeriod = 10000
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  
  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offr",1),(lenderToken,1)]
      , createOfferBeaconRedeemer = MintOfferBeacon lenderCred
      , createOfferLoanAddress = loanAddr
      , createOfferUTxOs = 
          [ ( Just offerDatum
            , lovelaceValueOf 105_000_000 
           <> singleton beaconCurrencySymbol "Offr" 1
           <> singleton beaconCurrencySymbol lenderToken 1
            )
          ]
      , createOfferAsInline = True
      , createOfferScripts = ts
      , createOfferWithRefScript = True
      , createOfferRefScript = mintRef
      , createOfferRefAddress = refAddr
      }

mintAdditionalToken :: DappScripts -> EmulatorTrace ()
mintAdditionalToken ts@DappScripts{..} = do
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

  let borrowerCred = PubKeyCredential
                   $ unPaymentPubKeyHash 
                   $ mockWalletPaymentPubKeyHash 
                   $ knownWallet 1
      lenderCred = PubKeyCredential
                 $ unPaymentPubKeyHash 
                 $ mockWalletPaymentPubKeyHash 
                 $ knownWallet 2
      lenderToken = credentialAsToken lenderCred
      offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = lenderToken
        , lenderAddress = Address lenderCred Nothing
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = [1,2,3]
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 10
        , collateralization = [(testToken1,unsafeRatio 0 1)]
        , claimPeriod = 10000
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  
  callEndpoint @"create-offer" h2 $
    CreateOfferParams
      { createOfferBeaconsMinted = [("Offer",1),(lenderToken,1),("Other",1)]
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

-------------------------------------------------
-- Test Function
-------------------------------------------------
tests :: DappScripts -> TestTree
tests ts = do
  let opts = defaultCheckOptions & emulatorConfig .~ emConfig
  testGroup "Create an Offer"
    [ checkPredicateOptions opts "Successfully create Offer"
        assertNoFailedTransactions (successfullyCreateOffer ts)
    , checkPredicateOptions opts "Fail if multiple Offer beacons minted"
        (Test.not assertNoFailedTransactions) (mintMultipleOfferBeacons ts)
    , checkPredicateOptions opts "Fail if multiple LenderIDs minted"
        (Test.not assertNoFailedTransactions) (mintMultipleLenderIds ts)
    , checkPredicateOptions opts "Fail if Offer beacon not minted to dApp address while LenderID is"
        (Test.not assertNoFailedTransactions) (offerBeaconNotMintedToDappAddress ts)
    , checkPredicateOptions opts "Fail if LenderID not minted to dApp address while Offer beaocn is"
        (Test.not assertNoFailedTransactions) (lenderIdNotMintedToDappAddress ts)
    , checkPredicateOptions opts "Fail if both beacons not minted to a dApp address"
        (Test.not assertNoFailedTransactions) (bothBeaconsNotMintedToDappAddress ts)
    , checkPredicateOptions opts "Fail if beacons minted to dApp address without staking"
        (Test.not assertNoFailedTransactions) (mintBeaconsToDappAddressWithoutStaking ts)
    , checkPredicateOptions opts "Fail if beacons stored separately at dApp address"
        (Test.not assertNoFailedTransactions) (beaconsStoredSeparately ts)
    , checkPredicateOptions opts "Fail if OfferDatum has wrong beacon symbol"
        (Test.not assertNoFailedTransactions) (wrongBeaconSymbolInDatum ts)
    , checkPredicateOptions opts "Fail if OfferDatum has wrong LenderID"
        (Test.not assertNoFailedTransactions) (wrongLenderIdInDatum ts)
    , checkPredicateOptions opts "Fail if OfferDatum lender address uses a payment script"
        (Test.not assertNoFailedTransactions) (lenderAddressUsesPaymentScript ts)
    , checkPredicateOptions opts "Fail if OfferDatum has negative loan principle"
        (Test.not assertNoFailedTransactions) (datumHasNegativePrinciple ts)
    , checkPredicateOptions opts "Fail if OfferDatum loan principle is zero"
        (Test.not assertNoFailedTransactions) (datumHasZeroPrinciple ts)
    , checkPredicateOptions opts "Fail if at least one OfferDatum loan checkpoint is negative"
        (Test.not assertNoFailedTransactions) (negativeCheckpoint ts)
    , checkPredicateOptions opts "Fail if OfferDatum loan checkpoints are not in ascending order"
        (Test.not assertNoFailedTransactions) (unsortedCheckpoints ts)
    , checkPredicateOptions opts "Fail if OfferDatum loan term is negative"
        (Test.not assertNoFailedTransactions) (datumHasNegativeTerm ts)
    , checkPredicateOptions opts "Fail if OfferDatum loan term is zero"
        (Test.not assertNoFailedTransactions) (datumHasZeroTerm ts)
    , checkPredicateOptions opts "Successfully allow zero interest"
        assertNoFailedTransactions (successfullyAllowZeroInterest ts)
    , checkPredicateOptions opts "Fail if OfferDatum has negative interest"
        (Test.not assertNoFailedTransactions) (datumHasNegativeInterest ts)
    , checkPredicateOptions opts "Fail if OfferDatum collateralization list is empty"
        (Test.not assertNoFailedTransactions) (emptyCollateral ts)
    , checkPredicateOptions opts "Fail if OfferDatum collateralization has a negetive price"
        (Test.not assertNoFailedTransactions) (atLeastOneCollateralizationIsNegative ts)
    , checkPredicateOptions opts "Fail if OfferDatum claim period is zero"
        (Test.not assertNoFailedTransactions) (datumHasZeroClaimPeriod ts)
    , checkPredicateOptions opts "Fail if OfferDatum claim period is negative"
        (Test.not assertNoFailedTransactions) (datumHasNegativeClaimPeriod ts)
    , checkPredicateOptions opts "Fail if OfferDatum is not inline"
        (Test.not assertNoFailedTransactions) (nonInlineDatum ts)
    , checkPredicateOptions opts "Fail if beacons not stored with an OfferDatum"
        (Test.not assertNoFailedTransactions) (beaconsNotStoredWithOfferDatum ts)
    , checkPredicateOptions opts "Fail if beacons not stored with minimum ADA amount"
        (Test.not assertNoFailedTransactions) (beaconsNotStoredWithMininumADA ts)
    , checkPredicateOptions opts "Fail if beacons not stored with loan amount"
        (Test.not assertNoFailedTransactions) (beaconsNotStoredWithLoanAmount ts)
    , checkPredicateOptions opts "Fail if the lender did not approve creation"
        (Test.not assertNoFailedTransactions) (lenderDidNotApprove ts)
    , checkPredicateOptions opts "Fail if Offer beacon has the wrong token name"
        (Test.not assertNoFailedTransactions) (offerBeaconHasWrongName ts)
    , checkPredicateOptions opts "Fail if additional tokens minting by policy"
        (Test.not assertNoFailedTransactions) (mintAdditionalToken ts)
    ]

testTrace :: DappScripts -> IO ()
testTrace = runEmulatorTraceIO' def emConfig . lenderDidNotApprove