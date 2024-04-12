{-# LANGUAGE StrictData #-}

module CLI.Data.TxCBOR where

import Relude
import Data.Aeson

newtype TxCBOR = TxCBOR Text

instance FromJSON TxCBOR where
  parseJSON (Object o) = TxCBOR <$> o .: "cborHex"
  parseJSON _ = mzero
