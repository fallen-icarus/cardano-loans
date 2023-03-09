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

module Test.Offer
(
  tests,
  testTrace
) where

import Prelude (IO)
import Control.Lens hiding (from)
import PlutusTx.Prelude
import Plutus.Trace
import Wallet.Emulator.Wallet
import Plutus.Contract.Test as Test
import Test.Tasty
import Ledger.Ada (lovelaceValueOf)
import Data.Default
import Plutus.V2.Ledger.Api
import Ledger.Address
import Plutus.Script.Utils.V2.Generators (alwaysSucceedValidatorHash)

import Test.Common
import CardanoLoans

-------------------------------------------------
-- Offer Scenarios
-------------------------------------------------
offerToStakePubKey :: EmulatorTrace ()
offerToStakePubKey = do
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
      offerDatum = OfferDatum'
        { offerBeacon' = (beaconPolicySymbol,"Offer")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 12000
        , loanBacking' = 100_000_000
        , loanInterest' = unsafeRatio 1 10
        , collateralRates' = [(testToken1,unsafeRatio 1 2)]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"offer" h2 $
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

mintMultipleOfferBeacons :: EmulatorTrace ()
mintMultipleOfferBeacons = do
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
      offerDatum = OfferDatum'
        { offerBeacon' = (beaconPolicySymbol,"Offer")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 12000
        , loanInterest' = unsafeRatio 1 10
        , loanBacking' = 100_000_000
        , collateralRates' = [(testToken1,unsafeRatio 1 2)]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"offer" h2 $
    OfferParams
      { offerBeaconsMinted = [("Offer",2),(pubKeyAsToken lenderPubKey,1)]
      , offerBeaconRedeemer = MintOfferToken' lenderPubKey
      , offerBeaconPolicy = beaconPolicy
      , offerAddress = addr
      , offerInfo = 
          [ ( Just offerDatum
            , lovelaceValueOf 103_000_000 <> singleton beaconPolicySymbol "Offer" 2
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
            )
          ]
      , offerAsInline = True
      }

mintOfferTokenWithWrongName :: EmulatorTrace ()
mintOfferTokenWithWrongName = do
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
      offerDatum = OfferDatum'
        { offerBeacon' = (beaconPolicySymbol,"Offer")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 12000
        , loanInterest' = unsafeRatio 1 10
        , loanBacking' = 100_000_000
        , collateralRates' = [(testToken1,unsafeRatio 1 2)]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"offer" h2 $
    OfferParams
      { offerBeaconsMinted = [("Offe",1),(pubKeyAsToken lenderPubKey,1)]
      , offerBeaconRedeemer = MintOfferToken' lenderPubKey
      , offerBeaconPolicy = beaconPolicy
      , offerAddress = addr
      , offerInfo = 
          [ ( Just offerDatum
            , lovelaceValueOf 103_000_000 <> singleton beaconPolicySymbol "Offe" 1
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
            )
          ]
      , offerAsInline = True
      }

mintMultipleLenderIds :: EmulatorTrace ()
mintMultipleLenderIds = do
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
      offerDatum = OfferDatum'
        { offerBeacon' = (beaconPolicySymbol,"Offer")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 12000
        , loanInterest' = unsafeRatio 1 10
        , loanBacking' = 100_000_000
        , collateralRates' = [(testToken1,unsafeRatio 1 2)]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"offer" h2 $
    OfferParams
      { offerBeaconsMinted = [("Offer",1),(pubKeyAsToken lenderPubKey,2)]
      , offerBeaconRedeemer = MintOfferToken' lenderPubKey
      , offerBeaconPolicy = beaconPolicy
      , offerAddress = addr
      , offerInfo = 
          [ ( Just offerDatum
            , lovelaceValueOf 103_000_000 <> singleton beaconPolicySymbol "Offer" 1
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 2
            )
          ]
      , offerAsInline = True
      }

mintWrongLenderId :: EmulatorTrace ()
mintWrongLenderId = do
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
      offerDatum = OfferDatum'
        { offerBeacon' = (beaconPolicySymbol,"Offer")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 12000
        , loanInterest' = unsafeRatio 1 10
        , loanBacking' = 100_000_000
        , collateralRates' = [(testToken1,unsafeRatio 1 2)]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"offer" h2 $
    OfferParams
      { offerBeaconsMinted = [("Offer",1),("pubKeyAsToken lenderPubKey",1)]
      , offerBeaconRedeemer = MintOfferToken' lenderPubKey
      , offerBeaconPolicy = beaconPolicy
      , offerAddress = addr
      , offerInfo = 
          [ ( Just offerDatum
            , lovelaceValueOf 103_000_000 <> singleton beaconPolicySymbol "Offer" 1
           <> singleton beaconPolicySymbol ("pubKeyAsToken lenderPubKey") 1
            )
          ]
      , offerAsInline = True
      }

mintAdditionalTokens :: EmulatorTrace ()
mintAdditionalTokens = do
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
      offerDatum = OfferDatum'
        { offerBeacon' = (beaconPolicySymbol,"Offer")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 12000
        , loanInterest' = unsafeRatio 1 10
        , loanBacking' = 100_000_000
        , collateralRates' = [(testToken1,unsafeRatio 1 2)]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"offer" h2 $
    OfferParams
      { offerBeaconsMinted = [("Offer",1),(pubKeyAsToken lenderPubKey,1),("other",1)]
      , offerBeaconRedeemer = MintOfferToken' lenderPubKey
      , offerBeaconPolicy = beaconPolicy
      , offerAddress = addr
      , offerInfo = 
          [ ( Just offerDatum
            , lovelaceValueOf 103_000_000 <> singleton beaconPolicySymbol "Offer" 1
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
           <> singleton beaconPolicySymbol "other" 1
            )
          ]
      , offerAsInline = True
      }

mintToPaymentPubKey :: EmulatorTrace ()
mintToPaymentPubKey = do
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
      offerDatum = OfferDatum'
        { offerBeacon' = (beaconPolicySymbol,"Offer")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 12000
        , loanInterest' = unsafeRatio 1 10
        , loanBacking' = 100_000_000
        , collateralRates' = [(testToken1,unsafeRatio 1 2)]
        }
      addr = Address (PubKeyCredential $ unPaymentPubKeyHash lenderPubKey)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"offer" h2 $
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

mintToOtherPaymentScript :: EmulatorTrace ()
mintToOtherPaymentScript = do
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
      offerDatum = OfferDatum'
        { offerBeacon' = (beaconPolicySymbol,"Offer")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 12000
        , loanInterest' = unsafeRatio 1 10
        , loanBacking' = 100_000_000
        , collateralRates' = [(testToken1,unsafeRatio 1 2)]
        }
      addr = Address (ScriptCredential alwaysSucceedValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"offer" h2 $
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

mintToAddressWithoutStaking :: EmulatorTrace ()
mintToAddressWithoutStaking = do
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
      offerDatum = OfferDatum'
        { offerBeacon' = (beaconPolicySymbol,"Offer")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 12000
        , loanInterest' = unsafeRatio 1 10
        , loanBacking' = 100_000_000
        , collateralRates' = [(testToken1,unsafeRatio 1 2)]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     Nothing
  
  callEndpoint @"offer" h2 $
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

mintToAddressUsingStakingScript :: EmulatorTrace ()
mintToAddressUsingStakingScript = do
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
      offerDatum = OfferDatum'
        { offerBeacon' = (beaconPolicySymbol,"Offer")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 12000
        , loanInterest' = unsafeRatio 1 10
        , loanBacking' = 100_000_000
        , collateralRates' = [(testToken1,unsafeRatio 1 2)]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ ScriptCredential alwaysSucceedValidatorHash)
  
  callEndpoint @"offer" h2 $
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

storeOfferBeaconAndLenderIdSeparately :: EmulatorTrace ()
storeOfferBeaconAndLenderIdSeparately = do
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
      offerDatum = OfferDatum'
        { offerBeacon' = (beaconPolicySymbol,"Offer")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 12000
        , loanInterest' = unsafeRatio 1 10
        , loanBacking' = 100_000_000
        , collateralRates' = [(testToken1,unsafeRatio 1 2)]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"offer" h2 $
    OfferParams
      { offerBeaconsMinted = [("Offer",1),(pubKeyAsToken lenderPubKey,1)]
      , offerBeaconRedeemer = MintOfferToken' lenderPubKey
      , offerBeaconPolicy = beaconPolicy
      , offerAddress = addr
      , offerInfo = 
          [ ( Just offerDatum
            , lovelaceValueOf 103_000_000 <> singleton beaconPolicySymbol "Offer" 1
            )
          , (Just offerDatum
            , lovelaceValueOf 2_000_000 <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
            )
          ]
      , offerAsInline = True
      }

wrongOfferBeacon :: EmulatorTrace ()
wrongOfferBeacon = do
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
      offerDatum = OfferDatum'
        { offerBeacon' = (beaconPolicySymbol,"Ofer")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 12000
        , loanInterest' = unsafeRatio 1 10
        , loanBacking' = 100_000_000
        , collateralRates' = [(testToken1,unsafeRatio 1 2)]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"offer" h2 $
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

wrongLenderId :: EmulatorTrace ()
wrongLenderId = do
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
      offerDatum = OfferDatum'
        { offerBeacon' = (beaconPolicySymbol,"Offer")
        , lenderId' = (beaconPolicySymbol,"pubKeyAsToken lenderPubKey")
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 12000
        , loanInterest' = unsafeRatio 1 10
        , loanBacking' = 100_000_000
        , collateralRates' = [(testToken1,unsafeRatio 1 2)]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"offer" h2 $
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

negativePrinciple :: EmulatorTrace ()
negativePrinciple = do
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
      offerDatum = OfferDatum'
        { offerBeacon' = (beaconPolicySymbol,"Offer")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = -100_000_000
        , loanTerm' = 12000
        , loanInterest' = unsafeRatio 1 10
        , loanBacking' = 100_000_000
        , collateralRates' = [(testToken1,unsafeRatio 1 2)]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"offer" h2 $
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

negativeTerm :: EmulatorTrace ()
negativeTerm = do
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
      offerDatum = OfferDatum'
        { offerBeacon' = (beaconPolicySymbol,"Offer")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = -12000
        , loanInterest' = unsafeRatio 1 10
        , loanBacking' = 100_000_000
        , collateralRates' = [(testToken1,unsafeRatio 1 2)]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"offer" h2 $
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

negativeInterest :: EmulatorTrace ()
negativeInterest = do
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
      offerDatum = OfferDatum'
        { offerBeacon' = (beaconPolicySymbol,"Offer")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 12000
        , loanInterest' = unsafeRatio (-1) 10
        , loanBacking' = 100_000_000
        , collateralRates' = [(testToken1,unsafeRatio 1 2)]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"offer" h2 $
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

emptyCollateral :: EmulatorTrace ()
emptyCollateral = do
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
      offerDatum = OfferDatum'
        { offerBeacon' = (beaconPolicySymbol,"Offer")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 12000
        , loanInterest' = unsafeRatio 1 10
        , loanBacking' = 100_000_000
        , collateralRates' = []
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"offer" h2 $
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

negativeCollateralRate :: EmulatorTrace ()
negativeCollateralRate = do
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
      offerDatum = OfferDatum'
        { offerBeacon' = (beaconPolicySymbol,"Offer")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 12000
        , loanInterest' = unsafeRatio 1 10
        , loanBacking' = 100_000_000
        , collateralRates' = [(testToken1,unsafeRatio 1 2),(testToken2,unsafeRatio (-2) 1)]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"offer" h2 $
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

notInline :: EmulatorTrace ()
notInline = do
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
      offerDatum = OfferDatum'
        { offerBeacon' = (beaconPolicySymbol,"Offer")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 12000
        , loanInterest' = unsafeRatio 1 10
        , loanBacking' = 100_000_000
        , collateralRates' = [(testToken1,unsafeRatio 1 2)]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"offer" h2 $
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
      , offerAsInline = False
      }

datumNotOfferDatum :: EmulatorTrace ()
datumNotOfferDatum = do
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
      offerDatum = AskDatum'
        { askBeacon' = (beaconPolicySymbol,"Ask")
        , borrowerId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 12000
        , collateral' = [testToken1]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"offer" h2 $
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

lenderDidNotSign :: EmulatorTrace ()
lenderDidNotSign = do
  h2 <- activateContractWallet (knownWallet 1) endpoints

  let lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
      offerDatum = OfferDatum'
        { offerBeacon' = (beaconPolicySymbol,"Offer")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 12000
        , loanInterest' = unsafeRatio 1 10
        , loanBacking' = 100_000_000
        , collateralRates' = [(testToken1,unsafeRatio 1 2)]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"offer" h2 $
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

offerUTxODoesntHaveLoanAmount :: EmulatorTrace ()
offerUTxODoesntHaveLoanAmount = do
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
      offerDatum = OfferDatum'
        { offerBeacon' = (beaconPolicySymbol,"Offer")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 12000
        , loanInterest' = unsafeRatio 1 10
        , loanBacking' = 50
        , collateralRates' = [(testToken1,unsafeRatio 1 2)]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"offer" h2 $
    OfferParams
      { offerBeaconsMinted = [("Offer",1),(pubKeyAsToken lenderPubKey,1)]
      , offerBeaconRedeemer = MintOfferToken' lenderPubKey
      , offerBeaconPolicy = beaconPolicy
      , offerAddress = addr
      , offerInfo = 
          [ ( Just offerDatum
            , lovelaceValueOf 100_000_000 <> singleton beaconPolicySymbol "Offer" 1
           <> singleton beaconPolicySymbol (pubKeyAsToken lenderPubKey) 1
            )
          ]
      , offerAsInline = True
      }

negativeDownPayment :: EmulatorTrace ()
negativeDownPayment = do
  h2 <- activateContractWallet (knownWallet 2) endpoints

  let lenderPubKey = mockWalletPaymentPubKeyHash $ knownWallet 2
      offerDatum = OfferDatum'
        { offerBeacon' = (beaconPolicySymbol,"Offer")
        , lenderId' = (beaconPolicySymbol,pubKeyAsToken lenderPubKey)
        , loanAsset' = (adaSymbol,adaToken)
        , loanPrinciple' = 100_000_000
        , loanTerm' = 12000
        , loanBacking' = -100_000_000
        , loanInterest' = unsafeRatio 1 10
        , collateralRates' = [(testToken1,unsafeRatio 1 2)]
        }
      addr = Address (ScriptCredential loanValidatorHash)
                     (Just $ StakingHash
                           $ PubKeyCredential
                           $ unPaymentPubKeyHash
                           $ mockWalletPaymentPubKeyHash
                           $ knownWallet 1)
  
  callEndpoint @"offer" h2 $
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

-------------------------------------------------
-- Test Function
-------------------------------------------------
tests :: TestTree
tests = do
  let opts = defaultCheckOptions & emulatorConfig .~ emConfig
  testGroup "Offer a loan"
    [ checkPredicateOptions opts "Fail if more than one offer beacon minted in tx"
        (Test.not assertNoFailedTransactions) mintMultipleOfferBeacons
    , checkPredicateOptions opts "Fail if the offer beacon has the wrong token name"
        (Test.not assertNoFailedTransactions) mintOfferTokenWithWrongName
    , checkPredicateOptions opts "Fail if more than one lender ID minted in tx"
        (Test.not assertNoFailedTransactions) mintMultipleLenderIds
    , checkPredicateOptions opts "Fail if the lender ID token name does not match the supplied pubkey hash"
        (Test.not assertNoFailedTransactions) mintWrongLenderId
    , checkPredicateOptions opts "Fail if addition beacons minted in tx"
        (Test.not assertNoFailedTransactions) mintAdditionalTokens
    , checkPredicateOptions opts "Fail if offer beacon minted to payment pubkey address"
        (Test.not assertNoFailedTransactions) mintToPaymentPubKey
    , checkPredicateOptions opts "Fail if minted to other payment script address"
        (Test.not assertNoFailedTransactions) mintToOtherPaymentScript
    , checkPredicateOptions opts "Fail if minted to address without a staking credential"
        (Test.not assertNoFailedTransactions) mintToAddressWithoutStaking
    , checkPredicateOptions opts "Fail if minted to address using a staking script"
        (Test.not assertNoFailedTransactions) mintToAddressUsingStakingScript
    , checkPredicateOptions opts "Fail if offer beacon and lender ID stored separately"
        (Test.not assertNoFailedTransactions) storeOfferBeaconAndLenderIdSeparately
    , checkPredicateOptions opts "Fail if output datum has wrong offerBeacon"
        (Test.not assertNoFailedTransactions) wrongOfferBeacon
    , checkPredicateOptions opts "Fail if output datum has wrong lenderId"
        (Test.not assertNoFailedTransactions) wrongLenderId
    , checkPredicateOptions opts "Fail if output datum has loanPrinciple <= 0"
        (Test.not assertNoFailedTransactions) negativePrinciple
    , checkPredicateOptions opts "Fail if output datum has loanTerm <= 0"
        (Test.not assertNoFailedTransactions) negativeTerm
    , checkPredicateOptions opts "Fail if output datum has loanInterest <= 0"
        (Test.not assertNoFailedTransactions) negativeInterest
    , checkPredicateOptions opts "Fail if output datum collateralRate list is empty"
        (Test.not assertNoFailedTransactions) emptyCollateral
    , checkPredicateOptions opts "Fail if output datum has any collateral rates <= 0"
        (Test.not assertNoFailedTransactions) negativeCollateralRate
    , checkPredicateOptions opts "Fail if output datum not inline"
        (Test.not assertNoFailedTransactions) notInline
    , checkPredicateOptions opts "Fail if output datum is not OfferDatum"
        (Test.not assertNoFailedTransactions) datumNotOfferDatum
    , checkPredicateOptions opts "Fail if lender did not sign tx"
        (Test.not assertNoFailedTransactions) lenderDidNotSign
    , checkPredicateOptions opts "Fail if offer utxo doesn't have loan amount."
        (Test.not assertNoFailedTransactions) offerUTxODoesntHaveLoanAmount
    , checkPredicateOptions opts "Fail if output datum loanBacking <= 0"
        (Test.not assertNoFailedTransactions) negativeDownPayment
    , checkPredicateOptions opts "Successfully create loan offer"
        assertNoFailedTransactions offerToStakePubKey
    ]

testTrace :: IO ()
testTrace = runEmulatorTraceIO' def emConfig negativeDownPayment