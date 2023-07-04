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

module Test.CloseAsk
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
-- Close Ask Scenarios
-------------------------------------------------
successfullyCloseSingleAsk :: DappScripts -> EmulatorTrace ()
successfullyCloseSingleAsk ts@DappScripts{..} = do
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

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum
  
  callEndpoint @"close-ask" h1 $
    CloseAskParams
      { closeAskBeaconsBurned = [("Ask",-1)]
      , closeAskBeaconRedeemer = BurnBeacons
      , closeAskLoanAddress = loanAddr
      , closeAskUTxOs = [ask1]
      , closeAskScripts = ts
      , closeAskWithRefScripts = True
      , closeAskSpendRefScript = spendRef
      , closeAskMintRefScript = mintRef
      , closeAskRefAddress = refAddr
      }

successfullyCloseMultipleAsks :: DappScripts -> EmulatorTrace ()
successfullyCloseMultipleAsks ts@DappScripts{..} = do
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

  void $ waitUntilSlot 6

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum
  ask2 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum{loanPrinciple = 200_000_000}
  
  callEndpoint @"close-ask" h1 $
    CloseAskParams
      { closeAskBeaconsBurned = [("Ask",-2)]
      , closeAskBeaconRedeemer = BurnBeacons
      , closeAskLoanAddress = loanAddr
      , closeAskUTxOs = [ask1,ask2]
      , closeAskScripts = ts
      , closeAskWithRefScripts = True
      , closeAskSpendRefScript = spendRef
      , closeAskMintRefScript = mintRef
      , closeAskRefAddress = refAddr
      }

utxoDoesNotHaveAskDatum :: DappScripts -> EmulatorTrace ()
utxoDoesNotHaveAskDatum ts@DappScripts{..} = do
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
  
  callEndpoint @"close-ask" h1 $
    CloseAskParams
      { closeAskBeaconsBurned = []
      , closeAskBeaconRedeemer = BurnBeacons
      , closeAskLoanAddress = loanAddr
      , closeAskUTxOs = [ask1]
      , closeAskScripts = ts
      , closeAskWithRefScripts = True
      , closeAskSpendRefScript = spendRef
      , closeAskMintRefScript = mintRef
      , closeAskRefAddress = refAddr
      }

successfullyCloseInvalidAsk :: DappScripts -> EmulatorTrace ()
successfullyCloseInvalidAsk ts@DappScripts{..} = do
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
  
  callEndpoint @"close-ask" h1 $
    CloseAskParams
      { closeAskBeaconsBurned = []
      , closeAskBeaconRedeemer = BurnBeacons
      , closeAskLoanAddress = loanAddr
      , closeAskUTxOs = [ask1]
      , closeAskScripts = ts
      , closeAskWithRefScripts = True
      , closeAskSpendRefScript = spendRef
      , closeAskMintRefScript = mintRef
      , closeAskRefAddress = refAddr
      }

askBeaconNotBurned :: DappScripts -> EmulatorTrace ()
askBeaconNotBurned ts@DappScripts{..} = do
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

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum
  
  callEndpoint @"close-ask" h1 $
    CloseAskParams
      { closeAskBeaconsBurned = []
      , closeAskBeaconRedeemer = BurnBeacons
      , closeAskLoanAddress = loanAddr
      , closeAskUTxOs = [ask1]
      , closeAskScripts = ts
      , closeAskWithRefScripts = True
      , closeAskSpendRefScript = spendRef
      , closeAskMintRefScript = mintRef
      , closeAskRefAddress = refAddr
      }

atLeastOneAskBeaconNotBurned :: DappScripts -> EmulatorTrace ()
atLeastOneAskBeaconNotBurned ts@DappScripts{..} = do
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

  void $ waitUntilSlot 6

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum
  ask2 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum{loanPrinciple = 200_000_000}
  
  callEndpoint @"close-ask" h1 $
    CloseAskParams
      { closeAskBeaconsBurned = [("Ask",-1)]
      , closeAskBeaconRedeemer = BurnBeacons
      , closeAskLoanAddress = loanAddr
      , closeAskUTxOs = [ask1,ask2]
      , closeAskScripts = ts
      , closeAskWithRefScripts = True
      , closeAskSpendRefScript = spendRef
      , closeAskMintRefScript = mintRef
      , closeAskRefAddress = refAddr
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
  spendRef <- txOutRefWithValue $ lovelaceValueOf minUTxOSpendRef
  
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

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum
  
  callEndpoint @"close-ask" h2 $
    CloseAskParams
      { closeAskBeaconsBurned = [("Ask",-1)]
      , closeAskBeaconRedeemer = BurnBeacons
      , closeAskLoanAddress = loanAddr
      , closeAskUTxOs = [ask1]
      , closeAskScripts = ts
      , closeAskWithRefScripts = True
      , closeAskSpendRefScript = spendRef
      , closeAskMintRefScript = mintRef
      , closeAskRefAddress = refAddr
      }

benchCloseAsk :: DappScripts -> EmulatorTrace ()
benchCloseAsk ts@DappScripts{..} = do
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
      { createAskBeaconsMinted = [("Ask",17)]
      , createAskBeaconRedeemer = MintAskBeacon borrowerCred
      , createAskLoanAddress = loanAddr
      , createAskUTxOs = 
          [ ( Just askDatum
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 200_000_000}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 200_000_001}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 200_000_002}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 200_000_003}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 200_000_004}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 200_000_005}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 200_000_006}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 200_000_007}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 200_000_008}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 200_000_009}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 200_000_010}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 200_000_011}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 200_000_012}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 200_000_013}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 200_000_014}
            , lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1
            )
          , ( Just askDatum{loanPrinciple = 200_000_015}
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

  ask1 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum
  ask2 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum{loanPrinciple = 200_000_000}
  ask3 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum{loanPrinciple = 200_000_001}
  ask4 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum{loanPrinciple = 200_000_002}
  ask5 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum{loanPrinciple = 200_000_003}
  ask6 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum{loanPrinciple = 200_000_004}
  ask7 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum{loanPrinciple = 200_000_005}
  ask8 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum{loanPrinciple = 200_000_006}
  ask9 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum{loanPrinciple = 200_000_007}
  ask10 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum{loanPrinciple = 200_000_008}
  ask11 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum{loanPrinciple = 200_000_009}
  ask12 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum{loanPrinciple = 200_000_010}
  ask13 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum{loanPrinciple = 200_000_011}
  ask14 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum{loanPrinciple = 200_000_012}
  ask15 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum{loanPrinciple = 200_000_013}
  ask16 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum{loanPrinciple = 200_000_014}
  ask17 <- txOutRefWithValueAndDatum 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol "Ask" 1)
            askDatum{loanPrinciple = 200_000_015}
  
  callEndpoint @"close-ask" h1 $
    CloseAskParams
      { closeAskBeaconsBurned = [("Ask",-11)]
      , closeAskBeaconRedeemer = BurnBeacons
      , closeAskLoanAddress = loanAddr
      , closeAskUTxOs = 
          [ ask1
          , ask2
          , ask3
          , ask4
          , ask5
          , ask6
          , ask7
          , ask8
          , ask9
          , ask10
          , ask11
          ]
      , closeAskScripts = ts
      , closeAskWithRefScripts = True
      , closeAskSpendRefScript = spendRef
      , closeAskMintRefScript = mintRef
      , closeAskRefAddress = refAddr
      }

-------------------------------------------------
-- Test Function
-------------------------------------------------
tests :: DappScripts -> TestTree
tests ts = do
  let opts = defaultCheckOptions & emulatorConfig .~ emConfig
  testGroup "Close Ask(s)"
    [ checkPredicateOptions opts "Successfully close single Ask"
        assertNoFailedTransactions (successfullyCloseSingleAsk ts)
    , checkPredicateOptions opts "Successfully close multiple Asks"
        assertNoFailedTransactions (successfullyCloseMultipleAsks ts)
    , checkPredicateOptions opts "Successfully close Ask UTxO that is missing an Ask beacon"
        assertNoFailedTransactions (successfullyCloseInvalidAsk ts)
    , checkPredicateOptions opts "Fail if UTxO datum is not an AskDatum"
        (Test.not assertNoFailedTransactions) (utxoDoesNotHaveAskDatum ts)
    , checkPredicateOptions opts "Fail if only Ask beacon not burned"
        (Test.not assertNoFailedTransactions) (askBeaconNotBurned ts)
    , checkPredicateOptions opts "Fail if at least one Ask beacon not burned"
        (Test.not assertNoFailedTransactions) (atLeastOneAskBeaconNotBurned ts)
    , checkPredicateOptions opts "Fail if borrower did not approve"
        (Test.not assertNoFailedTransactions) (borrowerDidNotApprove ts)
    ]

testTrace :: DappScripts -> IO ()
testTrace = runEmulatorTraceIO' def emConfig . benchCloseAsk