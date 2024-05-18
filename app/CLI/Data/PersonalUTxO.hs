{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE StrictData #-}

module CLI.Data.PersonalUTxO where

import Relude
import Data.Aeson
import Prettyprinter
import Prettyprinter.Render.Terminal

import CLI.Data.Bech32Address
import CLI.Data.Asset

import CardanoLoans

data PersonalUTxO = PersonalUTxO
  { _paymentAddress :: PaymentAddress
  , _utxoRef :: TxOutRef
  , _lovelaces :: Lovelace
  , _datumHash :: Maybe Text
  , _referenceScriptHash :: Maybe Text
  , _nativeAssets :: [NativeAsset]
  } deriving (Show,Eq)

instance Ord PersonalUTxO where
  PersonalUTxO{_utxoRef=ref1} <= PersonalUTxO{_utxoRef=ref2} = ref1 <= ref2

instance FromJSON PersonalUTxO where
  parseJSON =
      withObject "PersonalUTxO" $ \o ->
        PersonalUTxO
          <$> o .: "address"
          <*> ( (concatRef <$> o .: "tx_hash" <*> o .: "tx_index") >>= 
                  maybe mzero return . rightToMaybe . readTxOutRef)
          <*> o .: "value"
          <*> o .: "datum_hash"
          <*> (o .: "reference_script" >>= 
                maybe (return Nothing) (withObject "referenceScript" $ \i -> i .: "hash"))
          <*> o .: "asset_list"
    where
      concatRef :: String -> Integer -> String
      concatRef hash idx = hash <> "#" <> show idx

instance ToJSON PersonalUTxO where
  toJSON PersonalUTxO{..} =
    object [ "address" .= _paymentAddress
           , "utxo_id" .= (\(TxOutRef hash idx) -> show @Text hash <> "#" <> show idx) _utxoRef
           , "reference_script_hash" .= _referenceScriptHash
           , "datum_hash" .= _datumHash
           , "native_assets" .= _nativeAssets
           , "lovelace" .= unLovelace _lovelaces
           ]

prettyPersonalUTxO :: PersonalUTxO -> Doc AnsiStyle
prettyPersonalUTxO PersonalUTxO{_utxoRef=(TxOutRef txHash idx),..} =
  hsep $ mconcat
    [ [ pretty txHash 
      , "   "
      , pretty idx
      , "      "
      , pretty (unLovelace _lovelaces) <+> "lovelace"
      ]
    , map pretty _nativeAssets
    , mDatum
    , mScriptHash
    ]
  where
    mDatum :: [Doc AnsiStyle]
    mDatum = flip (maybe []) _datumHash $ \hash ->
      ["+" <+> annotate (color Green) ("DatumHash" <+> pretty hash)]

    mScriptHash :: [Doc AnsiStyle]
    mScriptHash = flip (maybe []) _referenceScriptHash $ \hash ->
      ["+" <+> annotate (color Blue) ("ScriptHash" <+> pretty hash)]

personalHeader :: Doc ann
personalHeader = 
  let s = "                           TxHash                                 TxIx        Amount"
  in mconcat
      [ pretty @String s
      , hardline
      , pretty $ replicate (length s + 2) '-'
      , hardline
      ]
