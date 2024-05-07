{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module CardanoLoans
  ( -- * On-chain Datums
    AskDatum(..)
  , OfferDatum(..)
  , ActiveDatum(..)
  , PaymentDatum(..)

    -- * On-chain Redeemers
  , LoanRedeemer(..) 
  , PaymentObserverRedeemer(..)
  , InterestObserverRedeemer(..)
  , AddressUpdateObserverRedeemer(..)
  , NegotiationBeaconsRedeemer(..) 
  , ActiveBeaconsRedeemer(..) 

    -- * Contracts
  , proxyScript
  , proxyValidatorHash
  , loanScript
  , loanValidatorHash
  , paymentObserverScript
  , paymentObserverCurrencySymbol
  , interestObserverScript
  , interestObserverCurrencySymbol
  , addressUpdateObserverScript
  , addressUpdateObserverCurrencySymbol
  , activeBeaconScript
  , activeBeaconCurrencySymbol
  , negotiationBeaconScript
  , negotiationBeaconCurrencySymbol

    -- * Beacon Names
  , genLoanAssetBeaconName
  , genLoanId
  , genLenderId
  , genBorrowerId
  , askBeaconName
  , offerBeaconName
  , activeBeaconName
    
    -- * Calculations
  , applyInterest
  , applyInterestNTimes

    -- * Creating Datums
  , NewAskInfo(..)
  , unsafeCreateAskDatum
  , NewOfferInfo(..)
  , unsafeCreateOfferDatum
  , NewActiveInfo(..)
  , unsafeCreateActiveDatum
  , createAcceptanceDatumFromOffer
  , NewPaymentInfo(..)
  , unsafeCreatePostPaymentActiveDatum
  , createPostPaymentActiveDatum
  , NewInterestInfo(..)
  , unsafeCreatePostInterestActiveDatum
  , createPostInterestActiveDatum
  , NewAddressInfo(..)
  , unsafeCreatePostAddressUpdateActiveDatum

    -- * Re-exports
  , module CardanoLoans.Types
  , module CardanoLoans.Utils
  ) where

import qualified PlutusTx
import qualified PlutusTx.Ratio as PlutusTx
import qualified PlutusTx.Prelude as PlutusTx
import qualified Data.Map as Map
import Data.Aeson
import qualified Plutus.Script.Utils.Scripts as PV2
import qualified PlutusLedgerApi.V2 as PV2
import Relude

import CardanoLoans.Blueprints
import CardanoLoans.Types
import CardanoLoans.Utils

-------------------------------------------------
-- On-Chain Datums and Redeemers
-------------------------------------------------
data AskDatum = AskDatum
  -- | The policy id for the negotiation beacon script.
  { _negotiationBeaconId :: NegotiationBeaconId
  -- | The policy id for the active beacon script.
  , _activeBeaconId :: ActiveBeaconId
  -- | The borrower's staking credential as a token name.
  , _borrowerId :: BorrowerId
  -- | The asset to be loaned out.
  , _loanAsset :: Asset
  -- | The token name for the loan asset beacon.
  , _assetBeacon :: AssetBeacon
  -- | The size of the loan.
  , _loanPrincipal :: Integer
  -- | How long the loan is active once accepted.
  , _loanTerm :: POSIXTime
  -- | The assets that will be used as collateral
  , _collateral :: Collateral
  } deriving (Generic,Show)

instance ToJSON AskDatum where
  toJSON AskDatum{..} =
    object [ "negotiation_beacon_id" .= _negotiationBeaconId
           , "active_beacon_id" .= _activeBeaconId
           , "borrower_id" .= _borrowerId
           , "loan_asset" .= _loanAsset
           , "asset_beacon" .= _assetBeacon
           , "principal" .= _loanPrincipal
           , "loan_term" .= _loanTerm
           , "collateral" .= _collateral
           ]

data OfferDatum = OfferDatum
  -- | The policy id for the negotiation beacon script.
  { _negotiationBeaconId :: NegotiationBeaconId
  -- | The policy id for the active beacon script.
  , _activeBeaconId :: ActiveBeaconId
  -- | The prefixed lender's staking credential as a token name.
  , _lenderId :: LenderId
  -- | The lender's address.
  , _lenderAddress :: Address
  -- | The asset to be loaned out.
  , _loanAsset :: Asset
  -- | The token name for the loan asset beacon.
  , _assetBeacon :: AssetBeacon
  -- | The size of the loan.
  , _loanPrincipal :: Integer
  -- | The frequency at which interest must be applied.
  , _compoundFrequency :: Maybe POSIXTime
  -- | How long the loan is active once accepted.
  , _loanTerm :: POSIXTime
  -- | The interest that must be periodically applied.
  , _loanInterest :: Fraction
  -- | The minimum loan payment that must be made each loan epoch.
  , _minPayment :: Integer
  -- | The penalty that gets applied if the minimum payment has not been met this loan epoch.
  , _penalty :: Penalty
  -- | The relative values of the collateral assets.
  , _collateralization :: Collateralization
  -- | Whether collateral can be swapped out during a loan payment.
  , _collateralIsSwappable :: Bool
  -- | How long the lender will have to claim the defaulted UTxO.
  , _claimPeriod :: POSIXTime
  -- | How much ADA was used for the UTxO's minUTxOValue.
  , _offerDeposit :: Integer
  -- | An optional offer expiration time.
  , _offerExpiration :: Maybe POSIXTime
  } deriving (Generic,Show)

instance ToJSON OfferDatum where
  toJSON OfferDatum{..} =
    object [ "negotiation_beacon_id" .= _negotiationBeaconId
           , "active_beacon_id" .= _activeBeaconId
           , "lender_id" .= _lenderId
           , "lender_address" .= LenderAddress _lenderAddress
           , "loan_asset" .= _loanAsset
           , "asset_beacon" .= _assetBeacon
           , "principal" .= _loanPrincipal
           , "compound_frequency" .= _compoundFrequency
           , "loan_term" .= _loanTerm
           , "loan_interest" .= _loanInterest
           , "minimum_payment" .= _minPayment
           , "penalty" .= _penalty
           , "collateralization" .= _collateralization
           , "collateral_is_swappable" .= _collateralIsSwappable
           , "claim_period" .= _claimPeriod
           , "offer_deposit" .= _offerDeposit
           , "offer_expiration" .= _offerExpiration
           ]

data ActiveDatum = ActiveDatum
  -- | The policy id for the active beacon script.
  { _activeBeaconId :: ActiveBeaconId
  -- | The hash for the payment observer script.
  , _paymentObserverHash :: PV2.ScriptHash
  -- | The hash for the interest observer script.
  , _interestObserverHash :: PV2.ScriptHash
  -- | The hash for the address update observer script.
  , _addressUpdateObserverHash :: PV2.ScriptHash
  -- | The borrower's staking credential as a token name.
  , _borrowerId :: BorrowerId
  -- | The lender's address.
  , _lenderAddress :: Address
  -- | The asset to be loaned out.
  , _loanAsset :: Asset
  -- | The token name for the loan asset beacon.
  , _assetBeacon :: AssetBeacon
  -- | The size of the loan.
  , _loanPrincipal :: Integer
  -- | The frequency at which interest must be applied.
  , _compoundFrequency :: Maybe POSIXTime
  -- | The last time interest was applied.
  , _lastCompounding :: POSIXTime
  -- | How long the loan is active once accepted.
  , _loanTerm :: POSIXTime
  -- | The interest that must be periodically applied.
  , _loanInterest :: Fraction
  -- | The minimum loan partial payment that can be made.
  , _minPayment :: Integer
  -- | The penalty that gets applied if the minimum payment has not been met this loan epoch.
  , _penalty :: Penalty
  -- | The relative values of the collateral assets.
  , _collateralization :: Collateralization
  -- | Whether collateral can be swapped out during a loan payment.
  , _collateralIsSwappable :: Bool
  -- | The time at which the lender's claim period will expire.
  , _claimExpiration :: POSIXTime
  -- | The time at which the loan will expire.
  , _loanExpiration :: POSIXTime
  -- | The loan's remaining unpaid balance.
  , _loanOutstanding :: Fraction
  -- | The total payments made this loan epoch.
  , _totalEpochPayments :: Integer
  -- | The loan's unique indentifier.
  , _loanId :: LoanId
  } deriving (Generic,Show)

instance ToJSON ActiveDatum where
  toJSON ActiveDatum{..} =
    object [ "active_beacon_id" .= _activeBeaconId
           , "payment_observer_script" .= _paymentObserverHash
           , "interest_observer_script" .= _interestObserverHash
           , "address_update_observer_script" .= _addressUpdateObserverHash
           , "borrower_id" .= _borrowerId
           , "lender_address" .= LenderAddress _lenderAddress
           , "loan_asset" .= _loanAsset
           , "asset_beacon" .= _assetBeacon
           , "principal" .= _loanPrincipal
           , "compound_frequency" .= _compoundFrequency
           , "last_compounding" .= _lastCompounding
           , "loan_term" .= _loanTerm
           , "loan_interest" .= _loanInterest
           , "minimum_payment" .= _minPayment
           , "penalty" .= _penalty
           , "collateralization" .= _collateralization
           , "collateral_is_swappable" .= _collateralIsSwappable
           , "claim_expiration" .= _claimExpiration
           , "loan_expiration" .= _loanExpiration
           , "loan_outstanding" .= _loanOutstanding
           , "total_payment_this_epoch" .= _totalEpochPayments
           , "loan_id" .= _loanId
           ]

newtype PaymentDatum = PaymentDatum (CurrencySymbol,TokenName)
  deriving (Generic)

instance PV2.ToData PaymentDatum where
  toBuiltinData (PaymentDatum (sym,name)) = 
    PV2.BuiltinData $ PV2.List [PV2.toData sym, PV2.toData name]

instance PV2.FromData PaymentDatum where
  fromBuiltinData (PV2.BuiltinData (PV2.List [sym,tok])) =
    fmap PaymentDatum . (,) 
      <$> PV2.fromData sym 
      <*> PV2.fromData tok
  fromBuiltinData _ = Nothing

data LoanRedeemer
  -- | Close or update an Ask UTxO.
  = CloseOrUpdateAsk
  -- | Close or update an Offer UTxO.
  | CloseOrUpdateOffer
  -- | Convert an Ask UTxO and an Offer UTxO into an Active UTxO.
  | AcceptOffer
  -- | Make a payment on a loan. The amount is the size of the payment.
  | MakePayment { _paymentAmount :: Integer }
  -- | Apply interest to a loan N times and deposit the specified amount of ada if needed.
  | ApplyInterest { _depositIncrease :: Integer, _numberOfTimes :: Integer }
  -- | Claim collateral for an expired loan using the Key NFT.
  | SpendWithKeyNFT
  -- | Update the address where loan payments must go. Optionally deposit additional ada if needed.
  | UpdateLenderAddress { _newAddress :: Address, _depositIncrease :: Integer }
  -- | Clean up remaining beacons or claim "Lost" collateral.
  | Unlock
  deriving (Generic,Show)

data PaymentObserverRedeemer
  -- | Observer a borrower's loan payment transaction.
  = ObservePayment
  -- | Register the script.
  | RegisterPaymentObserverScript
  deriving (Generic,Show)

data InterestObserverRedeemer
  -- | Observer a borrower's loan interest application transaction.
  = ObserveInterest
  -- | Register the script.
  | RegisterInterestObserverScript
  deriving (Generic,Show)

data AddressUpdateObserverRedeemer
  -- | Observer a lender's address update transaction.
  = ObserveAddressUpdate
  -- | Register the script.
  | RegisterAddressUpdateObserverScript
  deriving (Generic,Show)

data NegotiationBeaconsRedeemer
  -- | Create, close, or update some Ask UTxOs (1 or more). The credential is the borrower's
  -- staking credential.
  = CreateCloseOrUpdateAsk { _borrowerStakeCredential :: Credential }
  -- | Create, close, or update some Offer UTxOs (1 or more). The credential is the lender's
  -- staking credential.
  | CreateCloseOrUpdateOffer { _lenderStakeCredential :: Credential }
  -- | Burn any beacons. This can only be used in the same transaction where CreateActive is used
  -- for the active beacon script.
  | BurnNegotiationBeacons
  -- | Register the script.
  | RegisterNegotiationScript
  deriving (Generic,Show)

data ActiveBeaconsRedeemer
  -- | Create some Active UTxOs (1 or more) for the borrower. The CurrencySymbol is the 
  -- policy id for the negotiation beacons.
  = CreateActive { _negotiationPolicyId :: CurrencySymbol }
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
PlutusTx.unstableMakeIsData ''PaymentObserverRedeemer
PlutusTx.unstableMakeIsData ''InterestObserverRedeemer
PlutusTx.unstableMakeIsData ''AddressUpdateObserverRedeemer
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
loanScript = parseScriptFromCBOR $ blueprints Map.! "cardano_loans.loan_script"

loanValidatorHash :: PV2.ValidatorHash
loanValidatorHash = PV2.ValidatorHash $ PV2.getScriptHash $ scriptHash loanScript

paymentObserverScript :: SerialisedScript
paymentObserverScript =
  applyArguments
    (parseScriptFromCBOR $ blueprints Map.! "cardano_loans.payment_observer_script")
    [PV2.toData loanValidatorHash]

paymentObserverCurrencySymbol :: PV2.CurrencySymbol
paymentObserverCurrencySymbol = 
  PV2.CurrencySymbol $ PV2.getScriptHash $ scriptHash paymentObserverScript

interestObserverScript :: SerialisedScript
interestObserverScript =
  applyArguments
    (parseScriptFromCBOR $ blueprints Map.! "cardano_loans.interest_observer_script")
    [PV2.toData loanValidatorHash]

interestObserverCurrencySymbol :: PV2.CurrencySymbol
interestObserverCurrencySymbol = 
  PV2.CurrencySymbol $ PV2.getScriptHash $ scriptHash interestObserverScript

addressUpdateObserverScript :: SerialisedScript
addressUpdateObserverScript =
  applyArguments
    (parseScriptFromCBOR $ blueprints Map.! "cardano_loans.address_update_observer_script")
    [ PV2.toData proxyValidatorHash
    , PV2.toData loanValidatorHash
    ]

addressUpdateObserverCurrencySymbol :: PV2.CurrencySymbol
addressUpdateObserverCurrencySymbol = 
  PV2.CurrencySymbol $ PV2.getScriptHash $ scriptHash addressUpdateObserverScript

activeBeaconScript :: SerialisedScript
activeBeaconScript =
  applyArguments
    (parseScriptFromCBOR $ blueprints Map.! "cardano_loans.active_beacon_script")
    [ PV2.toData loanValidatorHash
    , PV2.toData paymentObserverCurrencySymbol
    , PV2.toData interestObserverCurrencySymbol
    , PV2.toData addressUpdateObserverCurrencySymbol
    ]

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
genLoanId :: TxOutRef -> LoanId
genLoanId (TxOutRef (TxId txHash) index) = 
  let TokenName index' = show index
  in LoanId $ TokenName $ PlutusTx.sha2_256 $ txHash <> index'

-- | Create the prefixed LenderId from the lender's staking credential.
genLenderId :: Credential -> LenderId
genLenderId cred = LenderId $ TokenName $ case cred of
  PV2.PubKeyCredential (PV2.PubKeyHash pkh) -> unsafeToBuiltinByteString "00" <> pkh
  PV2.ScriptCredential (PV2.ScriptHash sh) -> unsafeToBuiltinByteString "01" <> sh

-- | Create the BorrowerId from the borrower's staking credential.
genBorrowerId :: Credential -> BorrowerId
genBorrowerId cred = BorrowerId $ TokenName $ case cred of
  PV2.PubKeyCredential (PV2.PubKeyHash pkh) -> pkh
  PV2.ScriptCredential (PV2.ScriptHash sh) -> sh

askBeaconName :: TokenName
askBeaconName = "Ask"

offerBeaconName :: TokenName
offerBeaconName = "Offer"

activeBeaconName :: TokenName
activeBeaconName = "Active"

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

-- | Apply interest N times and apply the penalty when necessary.
applyInterestNTimes :: Bool -> Penalty -> Integer -> Interest -> Balance -> Balance
applyInterestNTimes _ _ 0 _ (Fraction (balNum,balDen)) =
  let newGcd = PlutusTx.gcd balNum balDen
  in Fraction (balNum `div` newGcd, balDen `div` newGcd)
applyInterestNTimes 
  penalize 
  penalty 
  count 
  interest@(Fraction (interestNum,interestDen)) 
  (Fraction (balNum,balDen)) =
    if penalize then case penalty of
      NoPenalty ->
        applyInterestNTimes True penalty (count - 1) interest $ 
          Fraction (balNum * (interestDen + interestNum), balDen * interestDen)
      FixedFee fee ->
        applyInterestNTimes True penalty (count - 1) interest $ 
          Fraction 
            ( (balNum + (fee * balDen)) * (interestDen + interestNum)
            , balDen * interestDen
            )
      PercentFee (Fraction (feeNum,feeDen)) ->
        let (newBalNum,newBalDen) = (balNum * (feeDen + feeNum), balDen * feeDen)
        in applyInterestNTimes True penalty (count - 1) interest $ 
            Fraction 
              ( newBalNum * (interestDen + interestNum)
              , newBalDen * interestDen
              )
    else
      applyInterestNTimes True penalty (count - 1) interest $ 
        Fraction (balNum * (interestDen + interestNum), balDen * interestDen)

-------------------------------------------------
-- Creating datums
-------------------------------------------------
-- | Required information for creating an AskDatum.
data NewAskInfo = NewAskInfo
  -- | The borrower's staking credential.
  { _borrowerId :: Credential
  -- | The asset to be loaned out.
  , _loanAsset :: Asset
  -- | The size of the loan.
  , _loanPrincipal :: Integer
  -- | How long the loan is active once accepted.
  , _loanTerm :: POSIXTime
  -- | The assets that will be used as collateral
  , _collateral :: [Asset]
  } deriving (Show)

-- | Convert the ask info to the AskDatum without checking any invariants. This is
-- useful for testing the smart contracts.
unsafeCreateAskDatum :: NewAskInfo -> AskDatum
unsafeCreateAskDatum NewAskInfo{..} = AskDatum
  { _negotiationBeaconId = NegotiationBeaconId negotiationBeaconCurrencySymbol
  , _activeBeaconId = ActiveBeaconId activeBeaconCurrencySymbol
  , _borrowerId = genBorrowerId _borrowerId
  , _loanAsset = _loanAsset
  , _assetBeacon = genLoanAssetBeaconName _loanAsset
  , _loanPrincipal = _loanPrincipal
  , _loanTerm = _loanTerm
  , _collateral = Collateral _collateral
  }

data NewOfferInfo = NewOfferInfo
  -- | The lender's staking credential.
  { _lenderId :: Credential
  -- | The lender's address.
  , _lenderAddress :: Address
  -- | The asset to be loaned out.
  , _loanAsset :: Asset
  -- | The size of the loan.
  , _loanPrincipal :: Integer
  -- | The frequency at which interest must be applied.
  , _compoundFrequency :: Maybe POSIXTime
  -- | How long the loan is active once accepted.
  , _loanTerm :: POSIXTime
  -- | The interest that must be periodically applied.
  , _loanInterest :: Fraction
  -- | The minimum loan partial payment that can be made.
  , _minPayment :: Integer
  -- | The penalty that gets applied if the minimum payment has not been met this loan epoch.
  , _penalty :: Penalty
  -- | The relative values of the collateral assets.
  , _collateralization :: [(Asset,Fraction)]
  -- | Whether collateral can be swapped out during a loan payment.
  , _collateralIsSwappable :: Bool
  -- | How long the lender will have to claim the defaulted UTxO.
  , _claimPeriod :: POSIXTime
  -- | How much ADA was used for the UTxO's minUTxOValue.
  , _offerDeposit :: Integer
  -- | An optional offer expiration time.
  , _offerExpiration :: Maybe POSIXTime
  } deriving (Show)

-- | Convert the offer info to the OfferDatum without checking any invariants. This is
-- useful for testing the smart contracts.
unsafeCreateOfferDatum :: NewOfferInfo -> OfferDatum
unsafeCreateOfferDatum NewOfferInfo{..} = OfferDatum
  { _negotiationBeaconId = NegotiationBeaconId negotiationBeaconCurrencySymbol
  , _activeBeaconId = ActiveBeaconId activeBeaconCurrencySymbol
  , _lenderId = genLenderId _lenderId
  , _lenderAddress = _lenderAddress
  , _loanAsset = _loanAsset
  , _assetBeacon = genLoanAssetBeaconName _loanAsset
  , _loanPrincipal = _loanPrincipal
  , _compoundFrequency = _compoundFrequency
  , _loanTerm = _loanTerm
  , _loanInterest = _loanInterest
  , _minPayment = _minPayment
  , _penalty = _penalty
  , _collateralization = Collateralization _collateralization
  , _collateralIsSwappable = _collateralIsSwappable
  , _claimPeriod = _claimPeriod
  , _offerDeposit = _offerDeposit
  , _offerExpiration = _offerExpiration
  }

data NewActiveInfo = NewActiveInfo
  -- | The lender's address.
  { _lenderAddress :: Address
  -- | The asset to be loaned out.
  , _loanAsset :: Asset
  -- | The size of the loan.
  , _loanPrincipal :: Integer
  -- | The frequency at which interest must be applied.
  , _compoundFrequency :: Maybe POSIXTime
  -- | How long the loan is active once accepted.
  , _loanTerm :: POSIXTime
  -- | The interest that must be periodically applied.
  , _loanInterest :: Fraction
  -- | The minimum loan partial payment that can be made.
  , _minPayment :: Integer
  -- | The penalty that gets applied if the minimum payment has not been met this loan epoch.
  , _penalty :: Penalty
  -- | The relative values of the collateral assets.
  , _collateralization :: [(Asset,Fraction)]
  -- | Whether collateral can be swapped out during a loan payment.
  , _collateralIsSwappable :: Bool
  -- | How long the lender will have to claim the defaulted UTxO.
  , _claimPeriod :: POSIXTime
  -- | Loan start time.
  , _startTime :: POSIXTime
  -- The borrower's staking credential.
  , _borrowerCred :: Credential
  -- The associated offer's identifier.
  , _offerId :: TxOutRef
  } deriving (Show)

-- | Convert the active info to the ActiveDatum without checking any invariants. 
unsafeCreateActiveDatum :: NewActiveInfo -> ActiveDatum
unsafeCreateActiveDatum NewActiveInfo{..} = ActiveDatum
  { _activeBeaconId = ActiveBeaconId activeBeaconCurrencySymbol
  , _paymentObserverHash = scriptHash paymentObserverScript
  , _interestObserverHash = scriptHash interestObserverScript
  , _addressUpdateObserverHash = scriptHash addressUpdateObserverScript
  , _borrowerId = genBorrowerId _borrowerCred
  , _lenderAddress = _lenderAddress
  , _loanAsset = _loanAsset
  , _assetBeacon = genLoanAssetBeaconName _loanAsset
  , _loanPrincipal = _loanPrincipal
  , _compoundFrequency = _compoundFrequency
  , _loanTerm = _loanTerm
  , _loanInterest = _loanInterest
  , _minPayment = _minPayment
  , _penalty = _penalty
  , _collateralization = Collateralization _collateralization
  , _collateralIsSwappable = _collateralIsSwappable
  , _lastCompounding = _startTime
  , _claimExpiration = _startTime + _loanTerm + _claimPeriod
  , _loanExpiration = _startTime + _loanTerm
  , _loanOutstanding = applyInterest (Fraction (_loanPrincipal,1)) _loanInterest
  , _totalEpochPayments = 0
  , _loanId = genLoanId _offerId
  }

-- | Create an ActiveDatum from an OfferDatum, offer output reference, BorrowerId, and loan 
-- start time.
createAcceptanceDatumFromOffer :: Credential -> TxOutRef -> POSIXTime -> OfferDatum -> ActiveDatum
createAcceptanceDatumFromOffer borrowerCred offerId startTime OfferDatum{..} = ActiveDatum
  { _activeBeaconId = ActiveBeaconId activeBeaconCurrencySymbol
  , _paymentObserverHash = scriptHash paymentObserverScript
  , _interestObserverHash = scriptHash interestObserverScript
  , _addressUpdateObserverHash = scriptHash addressUpdateObserverScript
  , _borrowerId = genBorrowerId borrowerCred
  , _lenderAddress = _lenderAddress
  , _loanAsset = _loanAsset
  , _assetBeacon = _assetBeacon
  , _loanPrincipal = _loanPrincipal
  , _compoundFrequency = _compoundFrequency
  , _loanTerm = _loanTerm
  , _loanInterest = _loanInterest
  , _minPayment = _minPayment
  , _penalty = _penalty
  , _collateralization = _collateralization
  , _collateralIsSwappable = _collateralIsSwappable
  , _lastCompounding = startTime
  , _claimExpiration = startTime + _loanTerm + _claimPeriod
  , _loanExpiration = startTime + _loanTerm
  , _loanOutstanding = applyInterest (Fraction (_loanPrincipal,1)) _loanInterest
  , _totalEpochPayments = 0
  , _loanId = genLoanId offerId
  }

data NewPaymentInfo = NewPaymentInfo
  -- | The borrower's staking credential.
  { _borrowerCred :: Credential
  -- | The lender's address.
  , _lenderAddress :: Address
  -- | The asset to be loaned out.
  , _loanAsset :: Asset
  -- | The size of the loan.
  , _loanPrincipal :: Integer
  -- | The frequency at which interest must be applied.
  , _compoundFrequency :: Maybe POSIXTime
  -- | The last time interest was applied.
  , _lastCompounding :: POSIXTime
  -- | How long the loan is active once accepted.
  , _loanTerm :: POSIXTime
  -- | The interest that must be periodically applied.
  , _loanInterest :: Fraction
  -- | The minimum loan partial payment that can be made.
  , _minPayment :: Integer
  -- | The penalty that gets applied if the minimum payment has not been met this loan epoch.
  , _penalty :: Penalty
  -- | The relative values of the collateral assets.
  , _collateralization :: [(Asset,Fraction)]
  -- | Whether collateral can be swapped out during a loan payment.
  , _collateralIsSwappable :: Bool
  -- | The time at which the lender's claim period will expire.
  , _claimExpiration :: POSIXTime
  -- | The time at which the loan will expire.
  , _loanExpiration :: POSIXTime
  -- | The loan's remaining unpaid balance.
  , _loanOutstanding :: Fraction
  -- | The total payments made this loan epoch.
  , _totalEpochPayments :: Integer
  -- | The loan's unique indentifier.
  , _loanId :: LoanId
  -- | Payment amount.
  , _paymentAmount :: Integer
  } deriving (Generic,Show)

-- | Convert the active info to the ActiveDatum without checking any invariants. 
unsafeCreatePostPaymentActiveDatum :: NewPaymentInfo -> ActiveDatum
unsafeCreatePostPaymentActiveDatum NewPaymentInfo{..} = ActiveDatum
  { _activeBeaconId = ActiveBeaconId activeBeaconCurrencySymbol
  , _paymentObserverHash = scriptHash paymentObserverScript
  , _interestObserverHash = scriptHash interestObserverScript
  , _addressUpdateObserverHash = scriptHash addressUpdateObserverScript
  , _borrowerId = genBorrowerId _borrowerCred
  , _lenderAddress = _lenderAddress
  , _loanAsset = _loanAsset
  , _assetBeacon = genLoanAssetBeaconName _loanAsset
  , _loanPrincipal = _loanPrincipal
  , _compoundFrequency = _compoundFrequency
  , _loanTerm = _loanTerm
  , _loanInterest = _loanInterest
  , _minPayment = _minPayment
  , _penalty = _penalty
  , _collateralization = Collateralization _collateralization
  , _collateralIsSwappable = _collateralIsSwappable
  , _lastCompounding = _lastCompounding
  , _claimExpiration = _claimExpiration
  , _loanExpiration = _loanExpiration
  , _loanOutstanding = subtractPayment _paymentAmount _loanOutstanding
  , _totalEpochPayments = _totalEpochPayments + _paymentAmount
  , _loanId = _loanId
  }

createPostPaymentActiveDatum :: Integer -> ActiveDatum -> ActiveDatum
createPostPaymentActiveDatum paymentAmount activeDatum@ActiveDatum{..} =
  activeDatum
    { _loanOutstanding = subtractPayment paymentAmount _loanOutstanding
    , _totalEpochPayments = _totalEpochPayments + paymentAmount
    }

data NewInterestInfo = NewInterestInfo
  -- | The borrower's staking credential.
  { _borrowerCred :: Credential
  -- | The lender's address.
  , _lenderAddress :: Address
  -- | The asset to be loaned out.
  , _loanAsset :: Asset
  -- | The size of the loan.
  , _loanPrincipal :: Integer
  -- | The frequency at which interest must be applied.
  , _compoundFrequency :: Maybe POSIXTime
  -- | The last time interest was applied.
  , _lastCompounding :: POSIXTime
  -- | How long the loan is active once accepted.
  , _loanTerm :: POSIXTime
  -- | The interest that must be periodically applied.
  , _loanInterest :: Fraction
  -- | The minimum loan partial payment that can be made.
  , _minPayment :: Integer
  -- | The penalty that gets applied if the minimum payment has not been met this loan epoch.
  , _penalty :: Penalty
  -- | The relative values of the collateral assets.
  , _collateralization :: [(Asset,Fraction)]
  -- | Whether collateral can be swapped out during a loan payment.
  , _collateralIsSwappable :: Bool
  -- | The time at which the lender's claim period will expire.
  , _claimExpiration :: POSIXTime
  -- | The time at which the loan will expire.
  , _loanExpiration :: POSIXTime
  -- | The loan's remaining unpaid balance.
  , _loanOutstanding :: Fraction
  -- | The total payments made this loan epoch.
  , _totalEpochPayments :: Integer
  -- | The loan's unique indentifier.
  , _loanId :: LoanId
  -- | Number of times to apply the interest.
  , _numberOfTimes :: Integer
  } deriving (Generic,Show)

-- | Convert the active info to the ActiveDatum without checking any invariants. 
unsafeCreatePostInterestActiveDatum :: NewInterestInfo -> ActiveDatum
unsafeCreatePostInterestActiveDatum NewInterestInfo{..} = ActiveDatum
  { _activeBeaconId = ActiveBeaconId activeBeaconCurrencySymbol
  , _paymentObserverHash = scriptHash paymentObserverScript
  , _interestObserverHash = scriptHash interestObserverScript
  , _addressUpdateObserverHash = scriptHash addressUpdateObserverScript
  , _borrowerId = genBorrowerId _borrowerCred
  , _lenderAddress = _lenderAddress
  , _loanAsset = _loanAsset
  , _assetBeacon = genLoanAssetBeaconName _loanAsset
  , _loanPrincipal = _loanPrincipal
  , _compoundFrequency = _compoundFrequency
  , _loanTerm = _loanTerm
  , _loanInterest = _loanInterest
  , _minPayment = _minPayment
  , _penalty = _penalty
  , _collateralization = Collateralization _collateralization
  , _collateralIsSwappable = _collateralIsSwappable
  , _lastCompounding = _lastCompounding + fromMaybe 0 _compoundFrequency
  , _claimExpiration = _claimExpiration
  , _loanExpiration = _loanExpiration
  , _loanOutstanding = 
        applyInterestNTimes 
          (_minPayment > _totalEpochPayments) 
          _penalty 
          _numberOfTimes 
          _loanInterest 
          _loanOutstanding
  , _totalEpochPayments = 0
  , _loanId = _loanId
  }

createPostInterestActiveDatum :: Integer -> ActiveDatum -> ActiveDatum
createPostInterestActiveDatum numberOfTimes activeDatum@ActiveDatum{..} =
  activeDatum
    { _lastCompounding = _lastCompounding + fromMaybe 0 _compoundFrequency
    , _loanOutstanding = 
        applyInterestNTimes 
          (_minPayment > _totalEpochPayments) 
          _penalty 
          numberOfTimes 
          _loanInterest 
          _loanOutstanding
    , _totalEpochPayments = 0
    }

data NewAddressInfo = NewAddressInfo
  -- | The borrower's staking credential.
  { _borrowerCred :: Credential
  -- | The lender's address.
  , _lenderAddress :: Address
  -- | The asset to be loaned out.
  , _loanAsset :: Asset
  -- | The size of the loan.
  , _loanPrincipal :: Integer
  -- | The frequency at which interest must be applied.
  , _compoundFrequency :: Maybe POSIXTime
  -- | The last time interest was applied.
  , _lastCompounding :: POSIXTime
  -- | How long the loan is active once accepted.
  , _loanTerm :: POSIXTime
  -- | The interest that must be periodically applied.
  , _loanInterest :: Fraction
  -- | The minimum loan partial payment that can be made.
  , _minPayment :: Integer
  -- | The penalty that gets applied if the minimum payment has not been met this loan epoch.
  , _penalty :: Penalty
  -- | The relative values of the collateral assets.
  , _collateralization :: [(Asset,Fraction)]
  -- | Whether collateral can be swapped out during a loan payment.
  , _collateralIsSwappable :: Bool
  -- | The time at which the lender's claim period will expire.
  , _claimExpiration :: POSIXTime
  -- | The time at which the loan will expire.
  , _loanExpiration :: POSIXTime
  -- | The loan's remaining unpaid balance.
  , _loanOutstanding :: Fraction
  -- | The total payments made this loan epoch.
  , _totalEpochPayments :: Integer
  -- | The loan's unique indentifier.
  , _loanId :: LoanId
  } deriving (Generic,Show)

-- | Convert the active info to the ActiveDatum without checking any invariants. 
unsafeCreatePostAddressUpdateActiveDatum :: NewAddressInfo -> ActiveDatum
unsafeCreatePostAddressUpdateActiveDatum NewAddressInfo{..} = ActiveDatum
  { _activeBeaconId = ActiveBeaconId activeBeaconCurrencySymbol
  , _paymentObserverHash = scriptHash paymentObserverScript
  , _interestObserverHash = scriptHash interestObserverScript
  , _addressUpdateObserverHash = scriptHash addressUpdateObserverScript
  , _borrowerId = genBorrowerId _borrowerCred
  , _lenderAddress = _lenderAddress
  , _loanAsset = _loanAsset
  , _assetBeacon = genLoanAssetBeaconName _loanAsset
  , _loanPrincipal = _loanPrincipal
  , _compoundFrequency = _compoundFrequency
  , _loanTerm = _loanTerm
  , _loanInterest = _loanInterest
  , _minPayment = _minPayment
  , _penalty = _penalty
  , _collateralization = Collateralization _collateralization
  , _collateralIsSwappable = _collateralIsSwappable
  , _lastCompounding = _lastCompounding
  , _claimExpiration = _claimExpiration
  , _loanExpiration = _loanExpiration
  , _loanOutstanding = _loanOutstanding
  , _totalEpochPayments = _totalEpochPayments
  , _loanId = _loanId
  }
