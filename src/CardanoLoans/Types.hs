{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

module CardanoLoans.Types
  ( NegotiationBeaconID(..)
  , ActiveBeaconID(..)
  , Fraction(..)
  , Asset(..)
  , LenderID(..)
  , BorrowerID(..)
  , LoanID(..)
  , AssetBeacon(..)
  , Collateralization(..)
  , Collateral(..)
  ) where

import Data.Bifunctor (bimap)

import qualified PlutusLedgerApi.V2 as PV2

-------------------------------------------------
-- On-Chain Data Types
-------------------------------------------------
-- | A wrapper around the policy id for the negotation beacon script.
newtype NegotiationBeaconID = NegotiationBeaconID { unNegotiationBeaconID :: PV2.CurrencySymbol }
  deriving (Show,Eq)
  deriving newtype (PV2.ToData,PV2.FromData,PV2.UnsafeFromData)

-- | A wrapper around the policy id for the active beacon script.
newtype ActiveBeaconID = ActiveBeaconID { unActiveBeaconID :: PV2.CurrencySymbol }
  deriving (Show,Eq)
  deriving newtype (PV2.ToData,PV2.FromData,PV2.UnsafeFromData)

-- | A wrapper around two integers that make up a fraction. This is used
-- in the absence of a decimal type on change.
newtype Fraction = Fraction (Integer,Integer)
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

-- | A wrapper around an asset's full name (policy id, token name). It uses
-- a custom data encoding since Aiken uses a different encoding for it.
newtype Asset = Asset { unAsset :: (PV2.CurrencySymbol,PV2.TokenName) }
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

-- | A wrapper around the token name for a lender id. It is prefixed with
-- either "00" or "01" depending on whether the lender's credential is a pub key credential
-- or a script credential, respectively.
newtype LenderID = LenderID { unLenderID :: PV2.TokenName }
  deriving (Show,Eq)
  deriving newtype (PV2.ToData,PV2.FromData,PV2.UnsafeFromData)

-- | A wrapper around the token name for a borrower id.
newtype BorrowerID = BorrowerID { unBorrowerID :: PV2.TokenName }
  deriving (Show,Eq)
  deriving newtype (PV2.ToData,PV2.FromData,PV2.UnsafeFromData)

-- | A wrapper around the token name for a loan asset's beacon name. The name is:
-- sha2_256 ( "Asset" ++ policy id ++ token name ).
newtype AssetBeacon = AssetBeacon { unAssetBeacon :: PV2.TokenName }
  deriving (Show,Eq)
  deriving newtype (PV2.ToData,PV2.FromData,PV2.UnsafeFromData)

-- | A wrapper around the token name for a loan's unique identifier. The name is:
-- sha2_256 ( offer tx hash ++ offer output index ).
newtype LoanID = LoanID { unLoanID :: PV2.TokenName }
  deriving (Show,Eq)
  deriving newtype (PV2.ToData,PV2.FromData,PV2.UnsafeFromData)

-- | A wrapper around a list of collateral and their values relative to the loan asset. It uses
-- a custom data encoding since Aiken uses a different encoding for it.
newtype Collateralization = Collateralization [(Asset,Fraction)]
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

-- | A wrapper around a list of collateral. It uses a custom data encoding since Aiken uses a 
-- different encoding for it.
newtype Collateral = Collateral { unCollateral :: [Asset] }
  deriving (Show,Eq)

instance PV2.ToData Collateral where
  toBuiltinData (Collateral xs) = 
    PV2.BuiltinData $ PV2.Map $ map (bimap PV2.toData PV2.toData . unAsset) xs

instance PV2.FromData Collateral where
  fromBuiltinData (PV2.BuiltinData (PV2.Map collats)) = 
    fmap Collateral $ sequence $ 
        flip map collats $ \(x,y) -> fmap Asset . (,) <$> PV2.fromData x <*> PV2.fromData y
  fromBuiltinData _ = Nothing

instance PV2.UnsafeFromData Collateral where
  unsafeFromBuiltinData (PV2.BuiltinData (PV2.Map collats)) = 
    Collateral $ map (Asset . bimap unsafeFromData unsafeFromData) collats
  unsafeFromBuiltinData _ = error "Could not convert Data to Collateral"

-------------------------------------------------
-- Helpers
-------------------------------------------------
unsafeFromData :: (PV2.UnsafeFromData a) => PV2.Data -> a
unsafeFromData = PV2.unsafeFromBuiltinData . PV2.dataToBuiltinData
