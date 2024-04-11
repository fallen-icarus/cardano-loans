{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE StrictData #-}

module CLI.Data.Transaction where

import Relude
import Data.Aeson

import CLI.Data.Bech32Address
import CLI.Data.Asset

import CardanoLoans

-- | The type respesenting the UTxOs returned as part of the tx_info API endpoint.
data TransactionUTxO = TransactionUTxO
  { _paymentAddress :: PaymentAddress
  , _stakeAddress :: Maybe StakeAddress
  , _utxoRef :: TxOutRef
  , _lovelaces :: Lovelace
  , _inlineDatum :: Maybe Value
  , _nativeAssets :: [NativeAsset]
  } deriving (Show,Eq)

instance FromJSON TransactionUTxO where
  parseJSON =
      withObject "TransactionUTxO" $ \o ->
        TransactionUTxO
          <$> (o .: "payment_addr" >>= withObject "payment_addr" (\o' -> o' .: "bech32"))
          <*> o .: "stake_addr"
          <*> ( (concatRef <$> o .: "tx_hash" <*> o .: "tx_index") >>= 
                  maybe mzero return . rightToMaybe . readTxOutRef)
          <*> o .: "value"
          <*> (o .: "inline_datum" >>= 
                maybe (return Nothing) (withObject "inline_datum" $ \i -> i .: "value"))
          <*> o .: "asset_list"
    where
      concatRef :: String -> Integer -> String
      concatRef hash idx = hash <> "#" <> show idx

data AssetMint = AssetMint
  { _policyId :: Text
  , _tokenName :: Text
  , _quantity :: Integer
  } deriving (Show,Eq)

instance FromJSON AssetMint where
  parseJSON =
    withObject "AssetMint" $ \o ->
      AssetMint
        <$> o .: "policy_id"
        <*> o .: "asset_name"
        <*> (o .: "quantity" >>= maybe mzero return . readMaybe)

data PlutusContract = PlutusContract
  { _datum :: Maybe Value
  , _redeemer :: Maybe Value
  } deriving (Show,Eq)

instance FromJSON PlutusContract where
  parseJSON =
    withObject "PlutusContract" $ \o ->
      PlutusContract
        <$> (o .: "input" >>= (.: "datum") >>= maybe (return Nothing) (.:? "value"))
        <*> (o .: "input" >>= (.: "redeemer") >>= (.: "datum") >>= (.: "value"))

-- | The type respesenting the overall information returned with the tx_info query.
data Transaction = Transaction
  { _txHash :: Text
  , _blockTime :: POSIXTime
  , _blockHeight :: Integer
  , _inputs :: [TransactionUTxO]
  , _outputs :: [TransactionUTxO]
  , _assetsMinted :: [AssetMint]
  , _plutusContracts :: Maybe [PlutusContract]
  } deriving (Show,Eq)

instance FromJSON Transaction where
  parseJSON =
    withObject "Transaction" $ \o ->
      Transaction
        <$> o .: "tx_hash"
        <*> o .: "tx_timestamp"
        <*> o .: "block_height"
        <*> o .: "inputs"
        <*> o .: "outputs"
        <*> o .: "assets_minted"
        <*> o .:? "plutus_contracts"
