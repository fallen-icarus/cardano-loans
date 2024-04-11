{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module CardanoLoans.Types
  ( NegotiationBeaconId(..)
  , ActiveBeaconId(..)
  , Fraction(..)
  , Penalty(..)
  , Asset(..)
  , LenderId(..)
  , BorrowerId(..)
  , LoanId(..)
  , AssetBeacon(..)
  , Collateralization(..)
  , Collateral(..)
  , LenderAddress(..)
  ) where

import Data.Bifunctor (bimap)
import Data.Aeson
import Data.Text qualified as T
import Prettyprinter

import qualified PlutusLedgerApi.V2 as PV2
import qualified PlutusTx

-------------------------------------------------
-- On-Chain Data Types
-------------------------------------------------
-- | A wrapper around the policy id for the negotation beacon script.
newtype NegotiationBeaconId = NegotiationBeaconId { _unNegotiationBeaconId :: PV2.CurrencySymbol }
  deriving (Show,Eq)
  deriving newtype (PV2.ToData,PV2.FromData,PV2.UnsafeFromData)

instance ToJSON NegotiationBeaconId where
  toJSON (NegotiationBeaconId currSym) = toJSON $ T.pack $ show currSym

-- | A wrapper around the policy id for the active beacon script.
newtype ActiveBeaconId = ActiveBeaconId { _unActiveBeaconId :: PV2.CurrencySymbol }
  deriving (Show,Eq)
  deriving newtype (PV2.ToData,PV2.FromData,PV2.UnsafeFromData)

instance ToJSON ActiveBeaconId where
  toJSON (ActiveBeaconId currSym) = toJSON $ T.pack $ show currSym

-- | A wrapper around two integers that make up a fraction. This is used
-- in the absence of a decimal type on change.
newtype Fraction = Fraction { _unFraction :: (Integer,Integer) }
  deriving (Show,Eq)

instance PV2.ToData Fraction where
  toBuiltinData (Fraction (num,den)) = 
    PV2.BuiltinData $ PV2.List [PV2.toData num, PV2.toData den]

instance PV2.FromData Fraction where
  fromBuiltinData (PV2.BuiltinData (PV2.List [num,den])) =
    fmap Fraction . (,) 
      <$> PV2.fromData num 
      <*> PV2.fromData den
  fromBuiltinData _ = Nothing

instance PV2.UnsafeFromData Fraction where
  unsafeFromBuiltinData (PV2.BuiltinData (PV2.List [num,den])) = 
    Fraction (unsafeFromData num, unsafeFromData den)
  unsafeFromBuiltinData _ = error "Could not convert Data to Fraction"

instance ToJSON Fraction where
  toJSON (Fraction (num,den)) =
    object [ "numerator" .= T.pack (show num)
           , "denominator" .= T.pack (show den)
           ]

instance Pretty Fraction where
  pretty (Fraction (num,den)) = 
    pretty num <+> "/" <+> pretty den <+> 
      tupled [pretty @Double (fromIntegral num / fromIntegral den) <> "%"]

-- | The penalty to apply whenever the minimum payment is not met.
data Penalty
  = NoPenalty
  | FixedFee Integer
  | PercentFee Fraction
  deriving (Show,Eq)

instance ToJSON Penalty where
  toJSON NoPenalty = "none"
  toJSON (FixedFee fee) = object [ "fixed_fee" .= fee ]
  toJSON (PercentFee fee) = object [ "percent_fee" .= toJSON fee ]

-- | A wrapper around an asset's full name (policy id, token name). It uses
-- a custom data encoding since Aiken uses a different encoding for it.
newtype Asset = Asset { _unAsset :: (PV2.CurrencySymbol,PV2.TokenName) }
  deriving (Show,Eq)

instance PV2.ToData Asset where
  toBuiltinData (Asset (sym,name)) = 
    PV2.BuiltinData $ PV2.List [PV2.toData sym, PV2.toData name]

instance PV2.FromData Asset where
  fromBuiltinData (PV2.BuiltinData (PV2.List [sym,name])) =
    fmap Asset . (,) 
      <$> PV2.fromData sym 
      <*> PV2.fromData name
  fromBuiltinData _ = Nothing

instance PV2.UnsafeFromData Asset where
  unsafeFromBuiltinData (PV2.BuiltinData (PV2.List [sym,name])) = 
    Asset (unsafeFromData sym, unsafeFromData name)
  unsafeFromBuiltinData _ = error "Could not convert Data to Asset"

instance ToJSON Asset where
  toJSON (Asset (currSym,PV2.TokenName tokName)) =
    object [ "policy_id" .= T.pack (show currSym)
           , "asset_name" .= T.pack (show $ PV2.PubKeyHash tokName)
           ]

instance Pretty Asset where
  pretty (Asset (currSym,PV2.TokenName tokName)) = 
    if currSym == "" 
    then "lovelace"
    else pretty $ T.pack (show currSym) <> "." <> T.pack (show $ PV2.PubKeyHash tokName)

-- | A wrapper around the token name for a lender id. It is prefixed with
-- either "00" or "01" depending on whether the lender's credential is a pub key credential
-- or a script credential, respectively.
newtype LenderId = LenderId { _unLenderId :: PV2.TokenName }
  deriving (Show,Eq)
  deriving newtype (PV2.ToData,PV2.FromData,PV2.UnsafeFromData)

instance ToJSON LenderId where
  toJSON (LenderId (PV2.TokenName tokName)) = toJSON $ T.pack $ show $ PV2.PubKeyHash tokName 

instance Pretty LenderId where
  pretty (LenderId (PV2.TokenName tokName)) = pretty $ T.pack $ show $ PV2.PubKeyHash tokName

-- | A wrapper around the token name for a borrower id.
newtype BorrowerId = BorrowerId { _unBorrowerId :: PV2.TokenName }
  deriving (Show,Eq)
  deriving newtype (PV2.ToData,PV2.FromData,PV2.UnsafeFromData)

instance ToJSON BorrowerId where
  toJSON (BorrowerId (PV2.TokenName tokName)) = toJSON $ T.pack $ show $ PV2.PubKeyHash tokName 

instance Pretty BorrowerId where
  pretty (BorrowerId (PV2.TokenName tokName)) = pretty $ T.pack $ show $ PV2.PubKeyHash tokName

-- | A wrapper around the token name for a loan asset's beacon name. The name is:
-- sha2_256 ( "Asset" ++ policy id ++ token name ).
newtype AssetBeacon = AssetBeacon { _unAssetBeacon :: PV2.TokenName }
  deriving (Show,Eq)
  deriving newtype (PV2.ToData,PV2.FromData,PV2.UnsafeFromData)

instance ToJSON AssetBeacon where
  toJSON (AssetBeacon (PV2.TokenName tokName)) = toJSON $ T.pack $ show $ PV2.PubKeyHash tokName 

instance Pretty AssetBeacon where
  pretty (AssetBeacon (PV2.TokenName tokName)) = pretty $ T.pack $ show $ PV2.PubKeyHash tokName 

-- | A wrapper around the token name for a loan's unique identifier. The name is:
-- sha2_256 ( offer tx hash ++ offer output index ).
newtype LoanId = LoanId { _unLoanId :: PV2.TokenName }
  deriving (Show,Eq)
  deriving newtype (PV2.ToData,PV2.FromData,PV2.UnsafeFromData)

instance ToJSON LoanId where
  toJSON (LoanId (PV2.TokenName tokName)) = toJSON $ T.pack $ show $ PV2.PubKeyHash tokName 

instance Pretty LoanId where
  pretty (LoanId (PV2.TokenName tokName)) = pretty $ T.pack $ show $ PV2.PubKeyHash tokName 

-- | A wrapper around a list of collateral and their values relative to the loan asset. It uses
-- a custom data encoding since Aiken uses a different encoding for it.
newtype Collateralization = Collateralization { _unCollateralization :: [(Asset,Fraction)] }
  deriving (Show,Eq)

instance PV2.ToData Collateralization where
  toBuiltinData (Collateralization xs) = 
    PV2.BuiltinData $ PV2.Map $ map (bimap PV2.toData PV2.toData) xs

instance PV2.FromData Collateralization where
  fromBuiltinData (PV2.BuiltinData (PV2.Map collats)) = 
    fmap Collateralization $ sequence $ 
        flip map collats $ \(x,y) -> (,) <$> PV2.fromData x <*> PV2.fromData y
  fromBuiltinData _ = Nothing

instance PV2.UnsafeFromData Collateralization where
  unsafeFromBuiltinData (PV2.BuiltinData (PV2.Map collats)) = 
    Collateralization $ map (bimap unsafeFromData unsafeFromData) collats
  unsafeFromBuiltinData _ = error "Could not convert Data to Collateralization"

instance ToJSON Collateralization where
  toJSON (Collateralization xs) = object [ "collateralization" .= map toJSON xs ]

-- | A wrapper around a list of collateral. It uses a custom data encoding since Aiken uses a 
-- different encoding for it.
newtype Collateral = Collateral { _unCollateral :: [Asset] }
  deriving (Show,Eq)

instance PV2.ToData Collateral where
  toBuiltinData (Collateral xs) = 
    PV2.BuiltinData $ PV2.Map $ map (bimap PV2.toData PV2.toData . _unAsset) xs

instance PV2.FromData Collateral where
  fromBuiltinData (PV2.BuiltinData (PV2.Map collats)) = 
    fmap Collateral $ sequence $ 
        flip map collats $ \(x,y) -> fmap Asset . (,) <$> PV2.fromData x <*> PV2.fromData y
  fromBuiltinData _ = Nothing

instance PV2.UnsafeFromData Collateral where
  unsafeFromBuiltinData (PV2.BuiltinData (PV2.Map collats)) = 
    Collateral $ map (Asset . bimap unsafeFromData unsafeFromData) collats
  unsafeFromBuiltinData _ = error "Could not convert Data to Collateral"

instance ToJSON Collateral where
  toJSON (Collateral xs) = object [ "collateral" .= map toJSON xs ]

newtype LenderAddress = LenderAddress { _unLenderAddress :: PV2.Address }
  deriving (Show,Eq)

instance ToJSON LenderAddress where
  toJSON (LenderAddress (PV2.Address paymentCred mStakeCred)) = 
    object [ "payment_pub_key_hash" .= fmap show (toPaymentPubKeyHash paymentCred)
           , "payment_script_hash" .= fmap show (toPaymentScriptHash paymentCred)
           , "stake_pub_key_hash" .= fmap show (toStakePubKeyHash mStakeCred)
           , "stake_script_hash" .= fmap show (toStakeScriptHash mStakeCred)
           ]

-------------------------------------------------
-- Helpers
-------------------------------------------------
unsafeFromData :: (PV2.UnsafeFromData a) => PV2.Data -> a
unsafeFromData = PV2.unsafeFromBuiltinData . PV2.dataToBuiltinData

toPaymentPubKeyHash :: PV2.Credential -> Maybe PV2.PubKeyHash
toPaymentPubKeyHash (PV2.PubKeyCredential k) = Just k
toPaymentPubKeyHash _ = Nothing

toPaymentScriptHash :: PV2.Credential -> Maybe PV2.ScriptHash
toPaymentScriptHash (PV2.ScriptCredential k) = Just k
toPaymentScriptHash _ = Nothing

toStakePubKeyHash :: Maybe PV2.StakingCredential -> Maybe PV2.PubKeyHash
toStakePubKeyHash (Just (PV2.StakingHash (PV2.PubKeyCredential pkh))) = Just pkh
toStakePubKeyHash _ = Nothing

toStakeScriptHash :: Maybe PV2.StakingCredential -> Maybe PV2.ScriptHash
toStakeScriptHash (Just (PV2.StakingHash (PV2.ScriptCredential k))) = Just k
toStakeScriptHash _ = Nothing

-------------------------------------------------
-- TemplateHaskell
-------------------------------------------------
PlutusTx.unstableMakeIsData ''Penalty
