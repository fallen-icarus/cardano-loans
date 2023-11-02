{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

{- | 
 
All that is required to interface with the smart contracts is to add this module to your import
list:

@
import CardanoLoans
@

This module re-exports the "CardanoLoans.Utils" module. If you would like to work with the
blueprints directly, you can just use:

@
import CardanoLoans.Blueprints
@

The "CardanoLoans.Utils" module has types and functions that are helpful when interfacing with
the contracts.
-}

module CardanoLoans
  ( -- * On-Chan Data Types
    LoanDatum(..)
  , LoanRedeemer(..)
  , BeaconRedeemer(..)
  , PaymentDatum(..)
    
    -- * Contracts
  , proxyScript
  , proxyValidator
  , proxyValidatorHash
  , loanScript
  , loanValidator
  , loanValidatorHash
  , beaconScript
  , beaconMintingPolicy
  , beaconMintingPolicyHash
  , beaconCurrencySymbol

    -- * Re-Export
  , module CardanoLoans.Utils
  ) where

import Prelude hiding (fromInteger)
import Plutus.V2.Ledger.Api as Api
import qualified PlutusTx
import GHC.Generics (Generic)
import Ledger (Script(..))
import Plutus.Script.Utils.V2.Scripts
import qualified Data.Map as Map

import CardanoLoans.Blueprints
import CardanoLoans.Utils

-------------------------------------------------
-- On-Chain Data Types
-------------------------------------------------
data LoanDatum 
  = AskDatum
      { beaconSym :: CurrencySymbol
      , borrowerId :: TokenName
      , loanAsset :: (CurrencySymbol,TokenName)
      , loanPrinciple :: Integer
      , loanTerm :: POSIXTime
      , collateral :: [(CurrencySymbol,TokenName)]
      }
  | OfferDatum
      { beaconSym :: CurrencySymbol
      , lenderId :: TokenName
      , lenderAddress :: Address
      , loanAsset :: (CurrencySymbol,TokenName)
      , loanPrinciple :: Integer
      , rolloverFrequency :: Maybe POSIXTime
      , loanTerm :: POSIXTime
      , loanInterest :: PlutusRational
      , minPayment :: Integer
      , collateralization :: [((CurrencySymbol,TokenName),PlutusRational)]
      , collateralIsSwappable :: Bool
      , claimPeriod :: POSIXTime
      , offerDeposit :: Integer
      }
  | ActiveDatum
      { beaconSym :: CurrencySymbol
      , borrowerId :: TokenName
      , lenderAddress :: Address
      , loanAsset :: (CurrencySymbol,TokenName)
      , loanPrinciple :: Integer
      , rolloverFrequency :: Maybe POSIXTime
      , lastCheckpoint :: POSIXTime
      , loanTerm :: POSIXTime
      , loanInterest :: PlutusRational
      , minPayment :: Integer
      , collateralization :: [((CurrencySymbol,TokenName),PlutusRational)]
      , collateralIsSwappable :: Bool
      , claimExpiration :: POSIXTime
      , loanExpiration :: POSIXTime
      , loanOutstanding :: PlutusRational
      , loanId :: TokenName
      }
  deriving (Generic,Show)

-- | Custom instance is needed since Aiken uses a different representation.
instance ToData LoanDatum where
  toBuiltinData AskDatum{..} = dataToBuiltinData $
    Constr 0 [ toData beaconSym
             , toData borrowerId
             , List [toData $ fst loanAsset, toData $ snd loanAsset]
             , toData loanPrinciple
             , toData loanTerm
             , Map $ map (\(x,y) -> (toData x, toData y)) collateral
             ]
  toBuiltinData OfferDatum{..} = dataToBuiltinData $
    Constr 1 [ toData beaconSym
             , toData lenderId
             , toData lenderAddress
             , List [toData $ fst loanAsset, toData $ snd loanAsset]
             , toData loanPrinciple
             , toData rolloverFrequency
             , toData loanTerm
             , toData loanInterest
             , toData minPayment
             , Map $ map (\((x,y),r) -> (List [toData x, toData y],toData r)) collateralization
             , toData collateralIsSwappable
             , toData claimPeriod
             , toData offerDeposit
             ]
  toBuiltinData ActiveDatum{..} = dataToBuiltinData $ 
    Constr 2 [ toData beaconSym
             , toData borrowerId
             , toData lenderAddress
             , List [toData $ fst loanAsset, toData $ snd loanAsset]
             , toData loanPrinciple
             , toData rolloverFrequency
             , toData lastCheckpoint
             , toData loanTerm
             , toData loanInterest
             , toData minPayment
             , Map $ map (\((x,y),r) -> (List [toData x, toData y],toData r)) collateralization
             , toData collateralIsSwappable
             , toData claimExpiration
             , toData loanExpiration
             , toData loanOutstanding
             , toData loanId
             ]

instance FromData LoanDatum where
  fromBuiltinData 
    (BuiltinData 
      (Constr 0 
        [ sym
        , bId
        , List [lsym,lname]
        , lPrinciple
        , lterm
        , Map collats
        ]
      )
    ) = Just $ AskDatum
      { beaconSym = unsafeFromData sym
      , borrowerId = unsafeFromData bId
      , loanAsset = (unsafeFromData lsym, unsafeFromData lname)
      , loanPrinciple = unsafeFromData lPrinciple
      , loanTerm = unsafeFromData lterm
      , collateral = 
          map (\(collatsym,collatname) -> (unsafeFromData collatsym, unsafeFromData collatname)) 
              collats
      }
  fromBuiltinData 
    (BuiltinData 
      (Constr 1
        [ sym
        , lId
        , lAddress
        , List [lsym,lname]
        , lPrinciple
        , lRolloverFrequency
        , lterm
        , lInterest
        , lMinPayment
        , Map collats
        , lSwappable
        , lClaimPeriod
        , lOfferDeposit
        ]
      )
    ) = Just $ OfferDatum
      { beaconSym = unsafeFromData sym
      , lenderId = unsafeFromData lId
      , lenderAddress = unsafeFromData lAddress
      , loanAsset = (unsafeFromData lsym, unsafeFromData lname)
      , loanPrinciple = unsafeFromData lPrinciple
      , rolloverFrequency = unsafeFromData lRolloverFrequency
      , loanTerm = unsafeFromData lterm
      , loanInterest = unsafeFromData lInterest
      , minPayment = unsafeFromData lMinPayment
      , collateralization = 
          map (\(List [collatsym,collatname],price) -> 
                ((unsafeFromData collatsym, unsafeFromData collatname),unsafeFromData price)
              ) 
              collats
      , collateralIsSwappable = unsafeFromData lSwappable
      , claimPeriod = unsafeFromData lClaimPeriod
      , offerDeposit = unsafeFromData lOfferDeposit
      }
  fromBuiltinData 
    (BuiltinData 
      (Constr 2
        [ sym
        , bId
        , lAddress
        , List [lsym,lname]
        , lPrinciple
        , lRolloverFrequency
        , lLastCheckpoint
        , lterm
        , lInterest
        , lMinPayment
        , Map collats
        , lSwappable
        , lClaimExpired
        , lExpired
        , lOutstanding
        , lloanId
        ]
      )
    ) = Just $ ActiveDatum
      { beaconSym = unsafeFromData sym
      , borrowerId = unsafeFromData bId
      , lenderAddress = unsafeFromData lAddress
      , loanAsset = (unsafeFromData lsym, unsafeFromData lname)
      , loanPrinciple = unsafeFromData lPrinciple
      , rolloverFrequency = unsafeFromData lRolloverFrequency
      , lastCheckpoint = unsafeFromData lLastCheckpoint
      , loanTerm = unsafeFromData lterm
      , loanInterest = unsafeFromData lInterest
      , minPayment = unsafeFromData lMinPayment
      , collateralization = 
          map (\(List [collatsym,collatname],price) -> 
                ((unsafeFromData collatsym, unsafeFromData collatname),unsafeFromData price)
              ) 
              collats
      , collateralIsSwappable = unsafeFromData lSwappable
      , claimExpiration = unsafeFromData lClaimExpired
      , loanExpiration = unsafeFromData lExpired
      , loanOutstanding = unsafeFromData lOutstanding
      , loanId = unsafeFromData lloanId
      }
  fromBuiltinData _ = Nothing

data LoanRedeemer
  = CloseAsk
  | CloseOffer
  | AcceptOffer
  | MakePayment
  | Rollover Integer
  | ClaimExpired
  | UpdateLenderAddress Address Integer
  | Unlock
  deriving (Generic,Show)

data BeaconRedeemer
  = CreateAsk 
      Credential -- ^ Borrower's staking credential.
      [(CurrencySymbol,TokenName)] -- ^ Loan assets asked for.
  | CreateOffer
      Credential -- ^ Lender's staking credential.
      [(CurrencySymbol,TokenName)] -- ^ Loan assets offered.
  | CreateActive 
      Credential -- ^ Borrower's staking credential.
      [(TxOutRef,TxOutRef)] -- ^ Ask UTxO and Offer UTxO pairing. (Ask,Offer)
  | BurnBeacons
  deriving (Generic,Show)

instance ToData BeaconRedeemer where
  toBuiltinData (CreateAsk cred assets) = 
    dataToBuiltinData $ 
      Constr 0 [ toData cred
               , Map $ map (\(x,y) -> (toData x, toData y)) assets
               ]
  toBuiltinData (CreateOffer cred assets) = 
    dataToBuiltinData $ 
      Constr 1 [ toData cred
               , Map $ map (\(x,y) -> (toData x, toData y)) assets
               ]
  toBuiltinData (CreateActive cred pairing) = dataToBuiltinData $
    Constr 2 [ toData cred
             , Map $ map (\(x,y) -> (toData x, toData y)) pairing
             ]
  toBuiltinData BurnBeacons = dataToBuiltinData $ Constr 3 []

newtype PaymentDatum = PaymentDatum (CurrencySymbol,TokenName)
  deriving (Generic)

instance ToData PaymentDatum where
  toBuiltinData (PaymentDatum (sym,name)) = dataToBuiltinData $ List [toData sym, toData name]

PlutusTx.unstableMakeIsData ''LoanRedeemer

-------------------------------------------------
-- Contracts
-------------------------------------------------
proxyScript :: Ledger.Script
proxyScript = parseScriptFromCBOR $ blueprints Map.! "cardano_loans.proxy"

proxyValidator :: Validator
proxyValidator = Validator proxyScript

proxyValidatorHash :: ValidatorHash
proxyValidatorHash = validatorHash proxyValidator

loanScript :: Ledger.Script
loanScript = 
  applyArguments 
    (parseScriptFromCBOR $ blueprints Map.! "cardano_loans.spend")
    [toData proxyValidatorHash]

loanValidator :: Validator
loanValidator = Validator loanScript

loanValidatorHash :: ValidatorHash
loanValidatorHash = validatorHash loanValidator

beaconScript :: Ledger.Script
beaconScript =
  applyArguments
    (parseScriptFromCBOR $ blueprints Map.! "cardano_loans.mint")
    [toData proxyValidatorHash, toData loanValidatorHash]

beaconMintingPolicy :: MintingPolicy
beaconMintingPolicy = MintingPolicy beaconScript

beaconMintingPolicyHash :: MintingPolicyHash
beaconMintingPolicyHash = mintingPolicyHash beaconMintingPolicy

beaconCurrencySymbol :: CurrencySymbol
beaconCurrencySymbol = scriptCurrencySymbol beaconMintingPolicy
