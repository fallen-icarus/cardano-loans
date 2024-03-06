{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module CardanoLoans
  ( -- * On-chain Datums
    AskDatum(..)
  , OfferDatum(..)
  , ActiveDatum(..)
  , PaymentDatum(..)

    -- * On-chain Redeemers
  , LoanRedeemer(..) 
  , NegotiationBeaconsRedeemer(..) 
  , ActiveBeaconsRedeemer(..) 

    -- * Contracts
  , proxyScript
  , proxyValidatorHash
  , loanScript
  , loanValidatorHash
  , activeBeaconScript
  , activeBeaconCurrencySymbol
  , negotiationBeaconScript
  , negotiationBeaconCurrencySymbol

    -- * Beacon Names
  , genLoanAssetBeaconName
  , genLoanId
  , genLenderID
  , genBorrowerID
    
    -- * Calculations
  , applyInterest

    -- * Creating Datums
  , NewAskInfo(..)
  , unsafeCreateAskDatum
  , NewOfferInfo(..)
  , unsafeCreateOfferDatum
  , createAcceptanceDatum
  , createPostPaymentActiveDatum

    -- * Re-exports
  , module CardanoLoans.Types
  , module CardanoLoans.Utils
  ) where

import qualified PlutusTx
import qualified PlutusTx.Ratio as PlutusTx
import qualified PlutusTx.Prelude as PlutusTx
import GHC.Generics (Generic)
import qualified Data.Map as Map
import Data.Aeson
import qualified Plutus.Script.Utils.Scripts as PV2
import qualified PlutusLedgerApi.V2 as PV2
import Relude

import CardanoLoans.Blueprints
import CardanoLoans.Types
import CardanoLoans.Utils

-------------------------------------------------
-- On-Chain Data Types
-------------------------------------------------
data AskDatum = AskDatum
  -- | The policy id for the negotiation beacon script.
  { negotiationBeaconId :: NegotiationBeaconID
  -- | The policy id for the active beacon script.
  , activeBeaconId :: ActiveBeaconID
  -- | The borrower's staking credential as a token name.
  , borrowerId :: BorrowerID
  -- | The asset to be loaned out.
  , loanAsset :: Asset
  -- | The token name for the loan asset beacon.
  , assetBeacon :: AssetBeacon
  -- | The size of the loan.
  , loanPrinciple :: Integer
  -- | How long the loan is active once accepted.
  , loanTerm :: POSIXTime
  -- | The assets that will be used as collateral
  , collateral :: Collateral
  } deriving (Generic,Show)

data OfferDatum = OfferDatum
  -- | The policy id for the negotiation beacon script.
  { negotiationBeaconId :: NegotiationBeaconID
  -- | The policy id for the active beacon script.
  , activeBeaconId :: ActiveBeaconID
  -- | The prefixed lender's staking credential as a token name.
  , lenderId :: LenderID
  -- | The lender's address.
  , lenderAddress :: Address
  -- | The asset to be loaned out.
  , loanAsset :: Asset
  -- | The token name for the loan asset beacon.
  , assetBeacon :: AssetBeacon
  -- | The size of the loan.
  , loanPrinciple :: Integer
  -- | The frequency at which interest must be applied.
  , rolloverFrequency :: Maybe POSIXTime
  -- | How long the loan is active once accepted.
  , loanTerm :: POSIXTime
  -- | The interest that is applied with each rollover.
  , loanInterest :: Fraction
  -- | The minimum loan partial payment that can be made.
  , minPayment :: Integer
  -- | The relative values of the collateral assets.
  , collateralization :: Collateralization
  -- | Whether collateral can be swapped out during a loan payment.
  , collateralIsSwappable :: Bool
  -- | How long the lender will have to claim the defaulted UTxO.
  , claimPeriod :: POSIXTime
  -- | How much ADA was used for the UTxO's minUTxOValue.
  , offerDeposit :: Integer
  -- | An optional offer expiration time.
  , offerExpiration :: Maybe POSIXTime
  } deriving (Generic,Show)

data ActiveDatum = ActiveDatum
  -- | The policy id for the active beacon script.
  { activeBeaconId :: ActiveBeaconID
  -- | The borrower's staking credential as a token name.
  , borrowerId :: BorrowerID
  -- | The lender's address.
  , lenderAddress :: Address
  -- | The asset to be loaned out.
  , loanAsset :: Asset
  -- | The token name for the loan asset beacon.
  , assetBeacon :: AssetBeacon
  -- | The size of the loan.
  , loanPrinciple :: Integer
  -- | The frequency at which interest must be applied.
  , rolloverFrequency :: Maybe POSIXTime
  -- | The last time interest was applied.
  , lastCheckpoint :: POSIXTime
  -- | How long the loan is active once accepted.
  , loanTerm :: POSIXTime
  -- | The interest that is applied with each rollover.
  , loanInterest :: Fraction
  -- | The minimum loan partial payment that can be made.
  , minPayment :: Integer
  -- | The relative values of the collateral assets.
  , collateralization :: Collateralization
  -- | Whether collateral can be swapped out during a loan payment.
  , collateralIsSwappable :: Bool
  -- | The time at which the lender's claim period will expire.
  , claimExpiration :: POSIXTime
  -- | The time at which the loan will expire.
  , loanExpiration :: POSIXTime
  -- | The loan's remaining unpaid balance.
  , loanOutstanding :: Fraction
  -- | The loan's unique indentifier.
  , loanId :: LoanID
  } deriving (Generic,Show)

newtype PaymentDatum = PaymentDatum (CurrencySymbol,TokenName)
  deriving (Generic)

instance PV2.ToData PaymentDatum where
  toBuiltinData (PaymentDatum (sym,name)) = 
    PV2.BuiltinData $ PV2.List [PV2.toData sym, PV2.toData name]

data LoanRedeemer
  -- | Close or update an Ask UTxO.
  = CloseOrUpdateAsk
  -- | Close or update an Offer UTxO.
  | CloseOrUpdateOffer
  -- | Convert an Ask UTxO and an Offer UTxO into an Active UTxO.
  | AcceptOffer
  -- | Make a payment on a loan. The amount is the size of the payment.
  | MakePayment Integer
  -- | Apply interest to a loan and deposit the specified amount of ada if needed.
  | Rollover Integer
  -- | Claim collateral for an expired loan using the Key NFT.
  | SpendWithKeyNFT
  -- | Update the address where loan payments must go. The `Integer` is the amount of ada being
  -- deposited if needed.
  | UpdateLenderAddress Address Integer
  -- | Clean up remaining beacons or claim "Lost" collateral.
  | Unlock
  deriving (Generic,Show)

data NegotiationBeaconsRedeemer
  -- | Create, close, or update some Ask UTxOs (1 or more). The credential is the borrower's
  -- staking credential.
  = CreateCloseOrUpdateAsk Credential
  -- | Create, close, or update some Offer UTxOs (1 or more). The credential is the lender's
  -- staking credential.
  | CreateCloseOrUpdateOffer Credential
  -- | Burn any beacons.
  | BurnNegotiationBeacons
  -- | Register the script.
  | RegisterNegotiationScript
  deriving (Generic,Show)

data ActiveBeaconsRedeemer
  -- | Create some Active UTxOs (1 or more) for the borrower with the supplied credential.
  -- The CurrencySymbol is the policy id for the negotiation beacons.
  = CreateActive Credential CurrencySymbol
  -- | Burn the lock and key NFT to claim expired collateral.
  | BurnKeyAndClaimExpired
  -- Burn all remaining beacons and claim "Lost" collateral.
  | BurnRemainderOrUnlockLost
  -- | Burn any beacons.
  | BurnActiveBeacons
  deriving (Generic,Show)

PlutusTx.makeIsDataIndexed ''AskDatum [('AskDatum,0)]
PlutusTx.makeIsDataIndexed ''OfferDatum [('OfferDatum,1)]
PlutusTx.makeIsDataIndexed ''ActiveDatum [('ActiveDatum,2)]
PlutusTx.unstableMakeIsData ''LoanRedeemer
PlutusTx.unstableMakeIsData ''NegotiationBeaconsRedeemer
PlutusTx.unstableMakeIsData ''ActiveBeaconsRedeemer

-------------------------------------------------
-- Contracts
-------------------------------------------------
proxyScript :: SerialisedScript
proxyScript = parseScriptFromCBOR $ blueprints Map.! "cardano_loans.proxy_script"

proxyValidatorHash :: PV2.ValidatorHash
proxyValidatorHash = PV2.ValidatorHash $ PV2.getScriptHash $ scriptHash proxyScript

loanScript :: SerialisedScript
loanScript =
  applyArguments
    (parseScriptFromCBOR $ blueprints Map.! "cardano_loans.loan_script")
    [PV2.toData proxyValidatorHash]

loanValidatorHash :: PV2.ValidatorHash
loanValidatorHash = PV2.ValidatorHash $ PV2.getScriptHash $ scriptHash loanScript

activeBeaconScript :: SerialisedScript
activeBeaconScript =
  applyArguments
    (parseScriptFromCBOR $ blueprints Map.! "cardano_loans.active_beacon_script")
    [PV2.toData loanValidatorHash]

activeBeaconCurrencySymbol :: PV2.CurrencySymbol
activeBeaconCurrencySymbol = PV2.CurrencySymbol $ PV2.getScriptHash $ scriptHash activeBeaconScript

negotiationBeaconScript :: SerialisedScript
negotiationBeaconScript =
  applyArguments
    (parseScriptFromCBOR $ blueprints Map.! "cardano_loans.negotiation_beacon_script")
    [ PV2.toData proxyValidatorHash
    , PV2.toData loanValidatorHash
    , PV2.toData activeBeaconCurrencySymbol
    ]

negotiationBeaconCurrencySymbol :: PV2.CurrencySymbol
negotiationBeaconCurrencySymbol = 
  PV2.CurrencySymbol $ PV2.getScriptHash $ scriptHash negotiationBeaconScript

-------------------------------------------------
-- Beacon Names
-------------------------------------------------
-- | Create the Asset beacon name for the loan asset.
genLoanAssetBeaconName :: Asset -> AssetBeacon
genLoanAssetBeaconName (Asset ((CurrencySymbol sym),(TokenName name))) =
  AssetBeacon $ TokenName $ PlutusTx.sha2_256 $ "Asset" <> sym <> name

-- | Create the loan id from the offer's output reference.
genLoanId :: TxOutRef -> LoanID
genLoanId (TxOutRef (TxId txHash) index) = 
  let TokenName index' = show index
  in LoanID $ TokenName $ PlutusTx.sha2_256 $ txHash <> index'

-- | Create the prefixed LenderID from the lender's staking credential.
genLenderID :: Credential -> LenderID
genLenderID cred = LenderID $ TokenName $ case cred of
  PV2.PubKeyCredential (PV2.PubKeyHash pkh) -> unsafeToBuiltinByteString "00" <> pkh
  PV2.ScriptCredential (PV2.ScriptHash sh) -> unsafeToBuiltinByteString "01" <> sh

-- | Create the BorrowerID from the borrower's staking credential.
genBorrowerID :: Credential -> BorrowerID
genBorrowerID cred = BorrowerID $ TokenName $ case cred of
  PV2.PubKeyCredential (PV2.PubKeyHash pkh) -> pkh
  PV2.ScriptCredential (PV2.ScriptHash sh) -> sh

-------------------------------------------------
-- Calculations
-------------------------------------------------
type Balance = Fraction
type Interest = Fraction

-- | Apply interest to the loan's current outstanding balance. The calculation is:
-- balance * (1 + interest).
applyInterest :: Balance -> Interest -> Balance
applyInterest (Fraction (balNum,balDen)) (Fraction (interestNum,interestDen)) =
    Fraction (totalNum `div` newGcd, totalDen `div` newGcd)
  where
    totalNum = balNum * (interestDen + interestNum) -- Balance * (1 + interest)
    totalDen = balDen * interestDen
    newGcd = PlutusTx.gcd totalNum totalDen

-- | Subtract the payment amount from the loan's outstanding balance.
subtractPayment :: Integer -> Balance -> Balance
subtractPayment paymentAmount (Fraction (balNum,balDen)) =
    Fraction (totalNum, balDen)
  where
    totalNum = balNum - balDen * paymentAmount

-------------------------------------------------
-- Creating datums
-------------------------------------------------
-- | Required information for creating an AskDatum.
data NewAskInfo = NewAskInfo
  -- | The borrower's staking credential.
  { borrowerId :: Credential
  -- | The asset to be loaned out.
  , loanAsset :: Asset
  -- | The size of the loan.
  , loanPrinciple :: Integer
  -- | How long the loan is active once accepted.
  , loanTerm :: POSIXTime
  -- | The assets that will be used as collateral
  , collateral :: [Asset]
  } deriving (Show)

-- | Convert the ask info to the AskDatum without checking any invariants. This is
-- useful for testing the smart contracts.
unsafeCreateAskDatum :: NewAskInfo -> AskDatum
unsafeCreateAskDatum NewAskInfo{..} = AskDatum
  { negotiationBeaconId = NegotiationBeaconID negotiationBeaconCurrencySymbol
  , activeBeaconId = ActiveBeaconID activeBeaconCurrencySymbol
  , borrowerId = genBorrowerID borrowerId
  , loanAsset = loanAsset
  , assetBeacon = genLoanAssetBeaconName loanAsset
  , loanPrinciple = loanPrinciple
  , loanTerm = loanTerm
  , collateral = Collateral collateral
  }

data NewOfferInfo = NewOfferInfo
  -- | The lender's staking credential.
  { lenderId :: Credential
  -- | The lender's address.
  , lenderAddress :: Address
  -- | The asset to be loaned out.
  , loanAsset :: Asset
  -- | The size of the loan.
  , loanPrinciple :: Integer
  -- | The frequency at which interest must be applied.
  , rolloverFrequency :: Maybe POSIXTime
  -- | How long the loan is active once accepted.
  , loanTerm :: POSIXTime
  -- | The interest that is applied with each rollover.
  , loanInterest :: Fraction
  -- | The minimum loan partial payment that can be made.
  , minPayment :: Integer
  -- | The relative values of the collateral assets.
  , collateralization :: [(Asset,Fraction)]
  -- | Whether collateral can be swapped out during a loan payment.
  , collateralIsSwappable :: Bool
  -- | How long the lender will have to claim the defaulted UTxO.
  , claimPeriod :: POSIXTime
  -- | How much ADA was used for the UTxO's minUTxOValue.
  , offerDeposit :: Integer
  -- | An optional offer expiration time.
  , offerExpiration :: Maybe POSIXTime
  } deriving (Show)

-- | Convert the offer info to the OfferDatum without checking any invariants. This is
-- useful for testing the smart contracts.
unsafeCreateOfferDatum :: NewOfferInfo -> OfferDatum
unsafeCreateOfferDatum NewOfferInfo{..} = OfferDatum
  { negotiationBeaconId = NegotiationBeaconID negotiationBeaconCurrencySymbol
  , activeBeaconId = ActiveBeaconID activeBeaconCurrencySymbol
  , lenderId = genLenderID lenderId
  , lenderAddress = lenderAddress
  , loanAsset = loanAsset
  , assetBeacon = genLoanAssetBeaconName loanAsset
  , loanPrinciple = loanPrinciple
  , rolloverFrequency = rolloverFrequency
  , loanTerm = loanTerm
  , loanInterest = loanInterest
  , minPayment = minPayment
  , collateralization = Collateralization collateralization
  , collateralIsSwappable = collateralIsSwappable
  , claimPeriod = claimPeriod
  , offerDeposit = offerDeposit
  , offerExpiration = offerExpiration
  }

-- | Create an ActiveDatum from an OfferDatum, offer output reference, BorrowerID, and loan 
-- start time.
createAcceptanceDatum :: Credential -> TxOutRef -> POSIXTime -> OfferDatum -> ActiveDatum
createAcceptanceDatum borrowerCred offerId startTime OfferDatum{..} = ActiveDatum
  { activeBeaconId = ActiveBeaconID activeBeaconCurrencySymbol
  , borrowerId = genBorrowerID borrowerCred
  , lenderAddress = lenderAddress
  , loanAsset = loanAsset
  , assetBeacon = assetBeacon
  , loanPrinciple = loanPrinciple
  , rolloverFrequency = rolloverFrequency
  , loanTerm = loanTerm
  , loanInterest = loanInterest
  , minPayment = minPayment
  , collateralization = collateralization
  , collateralIsSwappable = collateralIsSwappable
  , lastCheckpoint = startTime
  , claimExpiration = startTime + loanTerm + claimPeriod
  , loanExpiration = startTime + loanTerm
  , loanOutstanding = applyInterest (Fraction (loanPrinciple,1)) loanInterest
  , loanId = genLoanId offerId
  }

createPostPaymentActiveDatum :: Integer -> ActiveDatum -> ActiveDatum
createPostPaymentActiveDatum paymentAmount activeDatum@ActiveDatum{loanOutstanding} =
  activeDatum{loanOutstanding = subtractPayment paymentAmount loanOutstanding}

-- -- | Required information from the user for creating a loan datum.
-- data UserLoanInfo
--   = NewAskInfo
--       -- | The borrower's staking credential.
--       { borrowerId :: Credential
--       -- | The asset to be loaned out.
--       , loanAsset :: Asset
--       -- | The size of the loan.
--       , loanPrinciple :: Integer
--       -- | How long the loan is active once accepted.
--       , loanTerm :: POSIXTime
--       -- | The assets that will be used as collateral
--       , collateral :: [Asset]
--       }
--   | NewOfferInfo
--       -- | The lender's staking credential.
--       { lenderId :: Credential
--       -- | The lender's address.
--       , lenderAddress :: Address
--       -- | The asset to be loaned out.
--       , loanAsset :: Asset
--       -- | The size of the loan.
--       , loanPrinciple :: Integer
--       -- | The frequency at which interest must be applied.
--       , rolloverFrequency :: Maybe POSIXTime
--       -- | How long the loan is active once accepted.
--       , loanTerm :: POSIXTime
--       -- | The interest that is applied with each rollover.
--       , loanInterest :: Fraction
--       -- | The minimum loan partial payment that can be made.
--       , minPayment :: Integer
--       -- | The relative values of the collateral assets.
--       , collateralization :: Collateralization
--       -- | Whether collateral can be swapped out during a loan payment.
--       , collateralIsSwappable :: Bool
--       -- | How long the lender will have to claim the defaulted UTxO.
--       , claimPeriod :: POSIXTime
--       -- | How much ADA was used for the UTxO's minUTxOValue.
--       , offerDeposit :: Integer
--       -- | An optional offer expiration time.
--       , offerExpiration :: Maybe POSIXTime
--       }
--   | NewActiveInfo
--       -- | The borrower's staking credential.
--       { borrowerId :: Credential
--       -- | The lender's address.
--       , lenderAddress :: Address
--       -- | The asset to be loaned out.
--       , loanAsset :: Asset
--       -- | The size of the loan.
--       , loanPrinciple :: Integer
--       -- | The frequency at which interest must be applied.
--       , rolloverFrequency :: Maybe POSIXTime
--       -- | The last time interest was applied.
--       , lastCheckpoint :: POSIXTime
--       -- | How long the loan is active once accepted.
--       , loanTerm :: POSIXTime
--       -- | The interest that is applied with each rollover.
--       , loanInterest :: Fraction
--       -- | The minimum loan partial payment that can be made.
--       , minPayment :: Integer
--       -- | The relative values of the collateral assets.
--       , collateralization :: Collateralization
--       -- | Whether collateral can be swapped out during a loan payment.
--       , collateralIsSwappable :: Bool
--       -- | How long the lender will have to claim the defaulted UTxO.
--       , claimPeriod :: POSIXTime
--       -- | The corresponding Offer's output reference.
--       , offerId :: TxOutRef
--       -- | The loan's start time.
--       , startTime :: POSIXTime
--       }
--   deriving (Generic,Show)
--
-- -- | Convert the user info to the respective datum without checking an invariants. This is
-- -- useful for testing the smart contracts.
-- unsafeCreateLoanDatum :: UserLoanInfo -> LoanDatum
-- unsafeCreateLoanDatum info = case info of
--   NewAskInfo{..} -> AskDatum
--     { negotiationBeaconId = NegotiationBeaconID negotiationBeaconCurrencySymbol
--     , activeBeaconId = ActiveBeaconID activeBeaconCurrencySymbol
--     , borrowerId = genBorrowerID borrowerId
--     , loanAsset = loanAsset
--     , assetBeacon = genLoanAssetBeaconName loanAsset
--     , loanPrinciple = loanPrinciple
--     , loanTerm = loanTerm
--     , collateral = Collateral collateral
--     }
--   NewOfferInfo{..} -> OfferDatum
--     { negotiationBeaconId = NegotiationBeaconID negotiationBeaconCurrencySymbol
--     , activeBeaconId = ActiveBeaconID activeBeaconCurrencySymbol
--     , lenderId = genLenderID lenderId
--     , lenderAddress = lenderAddress
--     , loanAsset = loanAsset
--     , assetBeacon = genLoanAssetBeaconName loanAsset
--     , loanPrinciple = loanPrinciple
--     , rolloverFrequency = rolloverFrequency
--     , loanTerm = loanTerm
--     , loanInterest = loanInterest
--     , minPayment = minPayment
--     , collateralization = collateralization
--     , collateralIsSwappable = collateralIsSwappable
--     , claimPeriod = claimPeriod
--     , offerDeposit = offerDeposit
--     , offerExpiration = offerExpiration
--     }
--   NewActiveInfo{..} -> ActiveDatum
--     { activeBeaconId = ActiveBeaconID activeBeaconCurrencySymbol
--     , borrowerId = genBorrowerID borrowerId
--     , lenderAddress = lenderAddress
--     , loanAsset = loanAsset
--     , assetBeacon = genLoanAssetBeaconName loanAsset
--     , loanPrinciple = loanPrinciple
--     , rolloverFrequency = rolloverFrequency
--     , loanTerm = loanTerm
--     , loanInterest = loanInterest
--     , minPayment = minPayment
--     , collateralization = collateralization
--     , collateralIsSwappable = collateralIsSwappable
--     , lastCheckpoint = startTime
--     , claimExpiration = startTime + loanTerm + claimPeriod
--     , loanExpiration = startTime + loanTerm
--     , loanOutstanding = applyInterest (Fraction (loanPrinciple,1)) loanInterest
--     , loanId = genLoanId offerId
--     }
