{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module CLI.Data.Asset where

import Relude
import Data.Aeson
import Prettyprinter (Pretty(..),(<+>))

-------------------------------------------------
-- Lovelace
-------------------------------------------------
-- | A type representing lovelace values.
newtype Lovelace = Lovelace Integer
  deriving (Show,Eq,Num,Ord)

instance Pretty Lovelace where
  pretty (Lovelace l) = pretty l

instance FromJSON Lovelace where
  -- It is usually returned from Koios as a string.
  parseJSON = withText "Lovelace" (maybe mzero (return . Lovelace) . readMaybe . toString)

unLovelace :: Lovelace -> Integer
unLovelace (Lovelace i) = i

-------------------------------------------------
-- Native Assets
-------------------------------------------------
-- | The type representing native assets. ADA is not considered a native
-- asset.
data NativeAsset = NativeAsset
  { _policyId :: Text
  , _tokenName :: Text
  , _fingerprint :: Text
  , _quantity :: Integer
  } deriving (Show,Eq)

instance Pretty NativeAsset where
  pretty NativeAsset{..} = "+" <+> pretty _quantity <+> pretty (_policyId <> "." <> _tokenName)

instance ToJSON NativeAsset where
  toJSON NativeAsset{..} = 
    object
      [ "policy_id" .= _policyId
      , "token_name" .= _tokenName
      , "fingerprint" .= _fingerprint
      , "quantity" .= _quantity
      ]

instance FromJSON NativeAsset where
  parseJSON = 
    withObject "NativeAsset" $ \o ->
      NativeAsset
        <$> o .: "policy_id"
        <*> o .: "asset_name"
        <*> o .: "fingerprint"
        <*> (o .: "quantity" >>= maybe mzero return . readMaybe)
