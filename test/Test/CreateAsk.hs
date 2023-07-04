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

module Test.CreateAsk
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
-- Create Ask Scenarios
-------------------------------------------------
successfullyCreateSingleAsk :: DappScripts -> EmulatorTrace ()
successfullyCreateSingleAsk ts@DappScripts{..} = do
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

successfullyCreateMultipleAsks :: DappScripts -> EmulatorTrace ()
successfullyCreateMultipleAsks ts@DappScripts{..} = do
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
  
  callEndpoint @"create-ask" h1 $
    CreateAskParams
      { createAskBeaconsMinted = [("Ask",2)]
      , createAskBeaconRedeemer = MintAskBeacon borrowerCred
      , createAskLoanAddress = loanAddr
      , createAskUTxOs = 
          [ ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 200_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          ]
      , createAskAsInline = True
      , createAskScripts = ts
      , createAskWithRefScript = True
      , createAskRefScript = mintRef
      , createAskRefAddress = refAddr
      }
  
mintAskTokenWithDifferentName :: DappScripts -> EmulatorTrace ()
mintAskTokenWithDifferentName ts@DappScripts{..} = do
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
  
  callEndpoint @"create-ask" h1 $
    CreateAskParams
      { createAskBeaconsMinted = [("Ak",1)]
      , createAskBeaconRedeemer = MintAskBeacon borrowerCred
      , createAskLoanAddress = loanAddr
      , createAskUTxOs = 
          [ ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ak" 1
            )
          ]
      , createAskAsInline = True
      , createAskScripts = ts
      , createAskWithRefScript = True
      , createAskRefScript = mintRef
      , createAskRefAddress = refAddr
      }

mintAdditionalToken :: DappScripts -> EmulatorTrace ()
mintAdditionalToken ts@DappScripts{..} = do
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
  
  callEndpoint @"create-ask" h1 $
    CreateAskParams
      { createAskBeaconsMinted = [("Ask",1),("Other",1)]
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

mintAskBeaconToNonDappAddress :: DappScripts -> EmulatorTrace ()
mintAskBeaconToNonDappAddress ts@DappScripts{..} = do
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
  
  callEndpoint @"create-ask" h1 $
    CreateAskParams
      { createAskBeaconsMinted = [("Ask",1)]
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

mintAskToAddressWithoutStaking :: DappScripts -> EmulatorTrace ()
mintAskToAddressWithoutStaking ts@DappScripts{..} = do
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
                         Nothing

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  
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

mintAskToDappAddressWithDifferentStakingThanRedeemer :: DappScripts -> EmulatorTrace ()
mintAskToDappAddressWithDifferentStakingThanRedeemer ts@DappScripts{..} = do
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
      wrongCred = PubKeyCredential
                $ unPaymentPubKeyHash 
                $ mockWalletPaymentPubKeyHash 
                $ knownWallet 2
      askDatum = AskDatum
        { beaconSym = beaconCurrencySymbol
        , borrowerId = credentialAsToken borrowerCred
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanTerm = 12000
        , collateral = [testToken1]
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash wrongCred)

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  
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
  
atLeastOneAskBeaconWithdrawn :: DappScripts -> EmulatorTrace ()
atLeastOneAskBeaconWithdrawn ts@DappScripts{..} = do
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
  
  callEndpoint @"create-ask" h1 $
    CreateAskParams
      { createAskBeaconsMinted = [("Ask",2)]
      , createAskBeaconRedeemer = MintAskBeacon borrowerCred
      , createAskLoanAddress = loanAddr
      , createAskUTxOs = 
          [ ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 200_000_000}
            , lovelaceValueOf 3_000_000
            )
          ]
      , createAskAsInline = True
      , createAskScripts = ts
      , createAskWithRefScript = True
      , createAskRefScript = mintRef
      , createAskRefAddress = refAddr
      }

multipleAskTokensInSingleUTxO :: DappScripts -> EmulatorTrace ()
multipleAskTokensInSingleUTxO ts@DappScripts{..} = do
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
  
  callEndpoint @"create-ask" h1 $
    CreateAskParams
      { createAskBeaconsMinted = [("Ask",3)]
      , createAskBeaconRedeemer = MintAskBeacon borrowerCred
      , createAskLoanAddress = loanAddr
      , createAskUTxOs = 
          [ ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 2
            )
          , ( Just askDatum{loanPrinciple = 200_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          ]
      , createAskAsInline = True
      , createAskScripts = ts
      , createAskWithRefScript = True
      , createAskRefScript = mintRef
      , createAskRefAddress = refAddr
      }

multipleAskTokensInSingleUTxO2 :: DappScripts -> EmulatorTrace ()
multipleAskTokensInSingleUTxO2 ts@DappScripts{..} = do
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
  
  callEndpoint @"create-ask" h1 $
    CreateAskParams
      { createAskBeaconsMinted = [("Ask",2)]
      , createAskBeaconRedeemer = MintAskBeacon borrowerCred
      , createAskLoanAddress = loanAddr
      , createAskUTxOs = 
          [ ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 2
            )
          ]
      , createAskAsInline = True
      , createAskScripts = ts
      , createAskWithRefScript = True
      , createAskRefScript = mintRef
      , createAskRefAddress = refAddr
      }

datumHasWrongBeaconSymbol :: DappScripts -> EmulatorTrace ()
datumHasWrongBeaconSymbol ts@DappScripts{..} = do
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
        { beaconSym = adaSymbol
        , borrowerId = credentialAsToken borrowerCred
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanTerm = 12000
        , collateral = [testToken1]
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  
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

atLeastOneDatumIsWrong :: DappScripts -> EmulatorTrace ()
atLeastOneDatumIsWrong ts@DappScripts{..} = do
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
  
  callEndpoint @"create-ask" h1 $
    CreateAskParams
      { createAskBeaconsMinted = [("Ask",2)]
      , createAskBeaconRedeemer = MintAskBeacon borrowerCred
      , createAskLoanAddress = loanAddr
      , createAskUTxOs = 
          [ ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{beaconSym = adaSymbol}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          ]
      , createAskAsInline = True
      , createAskScripts = ts
      , createAskWithRefScript = True
      , createAskRefScript = mintRef
      , createAskRefAddress = refAddr
      }

datumHasWrongBorrwerId :: DappScripts -> EmulatorTrace ()
datumHasWrongBorrwerId ts@DappScripts{..} = do
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
        , borrowerId = ""
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanTerm = 12000
        , collateral = [testToken1]
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  
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

datumHasNegativePrinciple :: DappScripts -> EmulatorTrace ()
datumHasNegativePrinciple ts@DappScripts{..} = do
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
        , loanPrinciple = -100_000_000
        , loanTerm = 12000
        , collateral = [testToken1]
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  
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

datumHasZeroPrinciple :: DappScripts -> EmulatorTrace ()
datumHasZeroPrinciple ts@DappScripts{..} = do
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
        , loanPrinciple = 0
        , loanTerm = 12000
        , collateral = [testToken1]
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  
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

datumHasNegativeTerm :: DappScripts -> EmulatorTrace ()
datumHasNegativeTerm ts@DappScripts{..} = do
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
        , loanTerm = -12000
        , collateral = [testToken1]
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  
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

datumHasZeroTerm :: DappScripts -> EmulatorTrace ()
datumHasZeroTerm ts@DappScripts{..} = do
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
        , loanTerm = 0
        , collateral = [testToken1]
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  
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

emptyCollateral :: DappScripts -> EmulatorTrace ()
emptyCollateral ts@DappScripts{..} = do
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
        , collateral = []
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  
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

askBeaconNotStoredWithAskDatum :: DappScripts -> EmulatorTrace ()
askBeaconNotStoredWithAskDatum ts@DappScripts{..} = do
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
      askDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = credentialAsToken borrowerCred
        , lenderAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = []
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 2
        , collateralization = [(testToken1, unsafeRatio 1 1)]
        , claimPeriod = 0
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  
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

atLeastOneAskStoredWithWrongDatumType :: DappScripts -> EmulatorTrace ()
atLeastOneAskStoredWithWrongDatumType ts@DappScripts{..} = do
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
      offerDatum = OfferDatum
        { beaconSym = beaconCurrencySymbol
        , lenderId = credentialAsToken borrowerCred
        , lenderAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing
        , loanAsset = (adaSymbol,adaToken)
        , loanPrinciple = 100_000_000
        , loanCheckpoints = []
        , loanTerm = 12000
        , loanInterest = unsafeRatio 1 2
        , collateralization = [(testToken1, unsafeRatio 1 1)]
        , claimPeriod = 0
        }
      loanAddr = Address (ScriptCredential spendingValidatorHash)
                         (Just $ StakingHash borrowerCred)

  mintRef <- txOutRefWithValue $ lovelaceValueOf minUTxOMintRef
  
  callEndpoint @"create-ask" h1 $
    CreateAskParams
      { createAskBeaconsMinted = [("Ask",2)]
      , createAskBeaconRedeemer = MintAskBeacon borrowerCred
      , createAskLoanAddress = loanAddr
      , createAskUTxOs = 
          [ ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just offerDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          ]
      , createAskAsInline = True
      , createAskScripts = ts
      , createAskWithRefScript = True
      , createAskRefScript = mintRef
      , createAskRefAddress = refAddr
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
  
  callEndpoint @"create-ask" h2 $
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

mintWithBurnRedeemer :: DappScripts -> EmulatorTrace ()
mintWithBurnRedeemer ts@DappScripts{..} = do
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
  
  callEndpoint @"create-ask" h1 $
    CreateAskParams
      { createAskBeaconsMinted = [("Ask",1)]
      , createAskBeaconRedeemer = BurnBeacons
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

askDatumNotInline :: DappScripts -> EmulatorTrace ()
askDatumNotInline ts@DappScripts{..} = do
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
      , createAskAsInline = False
      , createAskScripts = ts
      , createAskWithRefScript = True
      , createAskRefScript = mintRef
      , createAskRefAddress = refAddr
      }

benchCreateAsk :: DappScripts -> EmulatorTrace ()
benchCreateAsk ts@DappScripts{..} = do
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
  
  callEndpoint @"create-ask" h1 $
    CreateAskParams
      { createAskBeaconsMinted = [("Ask",68)]
      , createAskBeaconRedeemer = MintAskBeacon borrowerCred
      , createAskLoanAddress = loanAddr
      , createAskUTxOs = 
          [ ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 200_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 300_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 200_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 300_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 200_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 300_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 200_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 300_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 200_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 300_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 200_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 300_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 200_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 300_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 200_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 300_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 200_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 300_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 200_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 300_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 200_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 300_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 200_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 300_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 200_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 300_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 200_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 300_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 200_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 300_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 200_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 300_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 200_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 300_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 200_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 300_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 200_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 300_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 200_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 300_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 200_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 300_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 200_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 300_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 200_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 300_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 200_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 300_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 200_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 300_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 200_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 300_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 300_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 200_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 300_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          ]
      , createAskAsInline = True
      , createAskScripts = ts
      , createAskWithRefScript = True
      , createAskRefScript = mintRef
      , createAskRefAddress = refAddr
      }

-------------------------------------------------
-- Test Function
-------------------------------------------------
tests :: DappScripts -> TestTree
tests ts = do
  let opts = defaultCheckOptions & emulatorConfig .~ emConfig
  testGroup "Create an Ask"
    [ checkPredicateOptions opts "Successfully create a single Ask"
        assertNoFailedTransactions (successfullyCreateSingleAsk ts)
    , checkPredicateOptions opts "Successfully create multiple Asks"
        assertNoFailedTransactions (successfullyCreateMultipleAsks ts)
    , checkPredicateOptions opts "Fail if Ask beacon does not have the token name 'Ask'"
        (Test.not assertNoFailedTransactions) (mintAskTokenWithDifferentName ts)
    , checkPredicateOptions opts "Fail if additional tokens minted by policy"
        (Test.not assertNoFailedTransactions) (mintAdditionalToken ts)
    , checkPredicateOptions opts "Fail if Ask beacon minted to a non-dApp address"
        (Test.not assertNoFailedTransactions) (mintAskBeaconToNonDappAddress ts)
    , checkPredicateOptions opts "Fail if Ask beacon minted to dApp address without staking"
        (Test.not assertNoFailedTransactions) (mintAskToAddressWithoutStaking ts)
    , checkPredicateOptions opts "Fail if Ask beacon minted to dApp address with different staking than redeemer"
        (Test.not assertNoFailedTransactions) (mintAskToDappAddressWithDifferentStakingThanRedeemer ts)
    , checkPredicateOptions opts "Fail if at least one Ask beacon does not go to a dApp address"
        (Test.not assertNoFailedTransactions) (atLeastOneAskBeaconWithdrawn ts)
    , checkPredicateOptions opts "Fail if a UTxO has more than one Ask beacon (test 1)"
        (Test.not assertNoFailedTransactions) (multipleAskTokensInSingleUTxO ts)
    , checkPredicateOptions opts "Fail if a UTxO has more than one Ask beacon (test 2)"
        (Test.not assertNoFailedTransactions) (multipleAskTokensInSingleUTxO2 ts)
    , checkPredicateOptions opts "Fail if AskDatum has wrong beacon symbol"
        (Test.not assertNoFailedTransactions) (datumHasWrongBeaconSymbol ts)
    , checkPredicateOptions opts "Fail if at least one AskDatum is wrong"
        (Test.not assertNoFailedTransactions) (atLeastOneDatumIsWrong ts)
    , checkPredicateOptions opts "Fail if AskDatum has wrong BorrowerID"
        (Test.not assertNoFailedTransactions) (datumHasWrongBorrwerId ts)
    , checkPredicateOptions opts "Fail if AskDatum loan_principle < 0"
        (Test.not assertNoFailedTransactions) (datumHasNegativePrinciple ts)
    , checkPredicateOptions opts "Fail if AskDatum loan_principle == 0"
        (Test.not assertNoFailedTransactions) (datumHasZeroPrinciple ts)
    , checkPredicateOptions opts "Fail if AskDatum loan_term < 0"
        (Test.not assertNoFailedTransactions) (datumHasNegativeTerm ts)
    , checkPredicateOptions opts "Fail if AskDatum loan_term == 0"
        (Test.not assertNoFailedTransactions) (datumHasZeroTerm ts)
    , checkPredicateOptions opts "Fail if AskDatum collateral is empty"
        (Test.not assertNoFailedTransactions) (emptyCollateral ts)
    , checkPredicateOptions opts "Fail if Ask beacon not stored with AskDatum"
        (Test.not assertNoFailedTransactions) (askBeaconNotStoredWithAskDatum ts)
    , checkPredicateOptions opts "Fail if at least one Ask beacon not stored with an AskDatum"
        (Test.not assertNoFailedTransactions) (atLeastOneAskStoredWithWrongDatumType ts)
    , checkPredicateOptions opts "Fail if borrower did not approve creation"
        (Test.not assertNoFailedTransactions) (borrowerDidNotApprove ts)
    , checkPredicateOptions opts "Fail if burn redeemer used to mint"
        (Test.not assertNoFailedTransactions) (mintWithBurnRedeemer ts)
    , checkPredicateOptions opts "Fail if AskDatum is not inline"
        (Test.not assertNoFailedTransactions) (askDatumNotInline ts)
    ]

testTrace :: DappScripts -> IO ()
testTrace = runEmulatorTraceIO' def benchConfig . benchCreateAsk