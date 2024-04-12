{-# OPTIONS_GHC -Wno-orphans -Wno-missing-signatures #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}

module CLI.Query.Koios
  ( 
    queryPersonalAddress
  , querySlotTip
  , queryAsks
  , queryOffers
  , queryActives
  , querySpecificLoanUTxO
  , queryBorrowerHistory
  , queryLoanHistory
  , submitTx
  , evaluateTx
  ) where

import Relude
import Servant.API
import Data.Aeson
import Servant.Client
import qualified Data.Text as T
import Data.Maybe (fromJust)

import CardanoLoans

import CLI.Data.Bech32Address
import CLI.Data.Asset
import CLI.Data.PersonalUTxO
import CLI.Data.LoanUTxO
import CLI.Data.Transaction
import CLI.Data.CreditHistory
import CLI.Data.LoanHistory
import CLI.Data.TxCBOR

-------------------------------------------------
-- Post Types
-------------------------------------------------
instance ToHttpApiData CurrencySymbol where
  toQueryParam = show

instance ToHttpApiData TokenName where
  toQueryParam = T.pack . showTokenName

-- | A newtype for submitting a list of payment addresses with the "_extended" flag.
newtype ExtendedPaymentAddresses = ExtendedPaymentAddresses [PaymentAddress] 
  deriving (Show)

instance ToJSON ExtendedPaymentAddresses where
  toJSON (ExtendedPaymentAddresses as) = 
    object [ "_addresses" .= map unPaymentAddress as 
           , "_extended" .= True
           ]

newtype TargetAsset = TargetAsset Asset

instance ToJSON TargetAsset where
  toJSON (TargetAsset (Asset (currSym,tokName))) = object 
    [ "_asset_list" .= [[T.pack $ show currSym, T.pack $ showTokenName tokName]]
    , "_extended" .= True
    ]

newtype ExtendedUTxOList = ExtendedUTxOList { unUTxOList :: [TxOutRef] } deriving (Show)

instance ToJSON ExtendedUTxOList where
  toJSON (ExtendedUTxOList as) = 
    object [ "_utxo_refs" .= map (\(TxOutRef hash ix) -> T.pack $ show hash <> "#" <> show ix) as
           , "_extended" .= True
           ]

newtype SubmitTxCBOR = SubmitTxCBOR TxCBOR

instance ToJSON SubmitTxCBOR where
  toJSON (SubmitTxCBOR (TxCBOR cbor)) = 
    object [ "jsonrpc" .= ("2.0" :: Text)
           , "method" .= ("submitTransaction" :: Text)
           , "params" .= object [ "transaction" .= object [ "cbor" .= cbor ] ]
           , "id" .= (Nothing :: Maybe ())
           ]

newtype EvaluateTxCBOR = EvaluateTxCBOR TxCBOR

instance ToJSON EvaluateTxCBOR where
  toJSON (EvaluateTxCBOR (TxCBOR cbor)) = 
    object [ "jsonrpc" .= ("2.0" :: Text)
           , "method" .= ("evaluateTransaction" :: Text)
           , "params" .= object [ "transaction" .= object [ "cbor" .= cbor ] ]
           , "id" .= (Nothing :: Maybe ())
           ]

-------------------------------------------------
-- Intermediate Response Types
-------------------------------------------------
newtype SlotTip = SlotTip { _unSlotTip :: Integer }
  deriving (Show)

instance FromJSON SlotTip where
  parseJSON (Object o) = SlotTip <$> o .: "abs_slot"
  parseJSON _ = mzero

data MintTx = MintTx 
  { _txHash :: Text
  , _quantity :: Integer
  } deriving (Show)

instance FromJSON MintTx where
  parseJSON (Object o) =
    MintTx
      <$> o .: "tx_hash"
      <*> (o .: "quantity" >>= maybe mzero return . readMaybe)
  parseJSON _ = mzero

newtype MintTxs = MintTxs [MintTx] deriving (Show)

instance ToJSON MintTxs where
  toJSON (MintTxs txs) = object [ "_tx_hashes" .= map _txHash txs ]

instance FromJSON MintTxs where
  parseJSON = withObject "MintTxs" $ \o -> 
    MintTxs <$> o .: "minting_txs"

newtype LoanTx = LoanTx Text
  deriving (Show)

instance FromJSON LoanTx where
  parseJSON = withObject "LoanTx" $ \o -> 
    LoanTx <$> o .: "tx_hash"

newtype LoanTxs = LoanTxs [LoanTx] deriving (Show)

instance ToJSON LoanTxs where
  toJSON (LoanTxs txs) = object [ "_tx_hashes" .= map (\(LoanTx tx) -> tx) txs ]

-------------------------------------------------
-- Low-Level API
-------------------------------------------------
type KoiosApi
  =     "address_utxos"
     :> QueryParam' '[Required] "select" Text
     :> QueryParam' '[Required] "is_spent" Text
     :> QueryParam "asset_list" Text
     :> ReqBody '[JSON] ExtendedPaymentAddresses
     :> Post '[JSON] [PersonalUTxO]

  :<|>  "address_utxos"
     :> QueryParam' '[Required] "select" Text
     :> QueryParam' '[Required] "is_spent" Text
     :> QueryParam "asset_list" Text
     :> ReqBody '[JSON] ExtendedPaymentAddresses
     :> Post '[JSON] [LoanUTxO]

  :<|>  "asset_utxos"
     :> QueryParam' '[Required] "select" Text
     :> QueryParam' '[Required] "is_spent" Text
     :> QueryParam "asset_list" Text
     :> ReqBody '[JSON] TargetAsset
     :> Post '[JSON] [LoanUTxO]

  :<|>  "utxo_info"
     :> QueryParam' '[Required] "select" Text
     :> QueryParam' '[Required] "is_spent" Text
     :> QueryParam "asset_list" Text
     :> ReqBody '[JSON] ExtendedUTxOList
     :> Post '[JSON] [LoanUTxO]

  :<|>  "tip"
     :> Get '[JSON] [SlotTip]

  :<|>  "asset_history"
     :> QueryParam' '[Required] "select" Text
     :> QueryParam' '[Required] "_asset_policy" CurrencySymbol
     :> QueryParam' '[Required] "_asset_name" TokenName
     :> Get '[JSON] [MintTxs]

  :<|> "tx_info"
     :> QueryParam' '[Required] "select" Text
     :> ReqBody '[JSON] MintTxs
     :> Post '[JSON] [Transaction]

  :<|>  "asset_txs"
     :> QueryParam' '[Required] "select" Text
     :> QueryParam' '[Required] "_asset_policy" CurrencySymbol
     :> QueryParam' '[Required] "_asset_name" TokenName
     :> QueryParam' '[Required] "_history" Bool
     :> Get '[JSON] [LoanTx]

  :<|> "tx_info"
     :> QueryParam' '[Required] "select" Text
     :> ReqBody '[JSON] LoanTxs
     :> Post '[JSON] [Transaction]
  
  :<|>  ReqBody '[JSON] SubmitTxCBOR
     :> Post '[JSON] Value

  :<|>  ReqBody '[JSON] EvaluateTxCBOR
     :> Post '[JSON] Value


personalAddressUTxOsApi 
  :<|> borrowerAddresssUTxOsApi
  :<|> assetUTxOsApi 
  :<|> specificLoanUTxOsApi
  :<|> slotTipApi
  :<|> assetHistoryApi 
  :<|> borrowerTxInfoApi 
  :<|> loanTxsApi 
  :<|> loanTxInfoApi 
  :<|> submitTxApi
  :<|> evaluateTxApi
  = client (Proxy :: Proxy KoiosApi)

-------------------------------------------------
-- High-Level API
-------------------------------------------------
queryPersonalAddress :: PaymentAddress -> Bool -> ClientM [PersonalUTxO]
queryPersonalAddress addr keysOnly =
    sortOn (\PersonalUTxO{_utxoRef} -> _utxoRef) <$>
      personalAddressUTxOsApi select "eq.false" keyFilter (ExtendedPaymentAddresses [addr])
  where
    keyFilter
      | keysOnly = Just keysQueryParam
      | otherwise = Nothing
    select =
      toText $ intercalate ","
        [ "is_spent"
        , "tx_hash"
        , "tx_index"
        , "address"
        , "value"
        , "datum_hash"
        , "asset_list"
        , "reference_script"
        ]

querySlotTip :: ClientM Integer
querySlotTip = slotTipApi >>= \case
  [(SlotTip t)] -> return t
  _ -> error "slotTipApi error"

queryAsks :: Maybe AssetBeacon -> Collateral -> Maybe PaymentAddress -> ClientM [LoanUTxO]
queryAsks mAssetBeacon (Collateral cs) mBorrowerAddr = do
  let askBeacon = Asset (negotiationBeaconCurrencySymbol,"Ask")
      assetBeacon = (Asset . (negotiationBeaconCurrencySymbol,) . _unAssetBeacon) <$> mAssetBeacon
      assets = filter (/=Asset("","")) $ catMaybes [assetBeacon] <> cs
      select =
        toText $ intercalate ","
          [ "is_spent"
          , "tx_hash"
          , "tx_index"
          , "address"
          , "stake_address"
          , "value"
          , "inline_datum"
          , "asset_list"
          ]
  case mBorrowerAddr of
    Nothing -> do
      let assetFilter = Just $ assetToQueryParam assets
      assetUTxOsApi select "eq.false" assetFilter (TargetAsset askBeacon)
    Just addr -> do
      let assetFilter = Just $ assetToQueryParam $ askBeacon : assets
      borrowerAddresssUTxOsApi select "eq.false" assetFilter (ExtendedPaymentAddresses [addr])  

queryOffers :: Maybe AssetBeacon -> Maybe PaymentAddress -> Maybe LenderId -> ClientM [LoanUTxO]
queryOffers mAssetBeacon mBorrowerAddr mLenderId = do
  let offerBeacon = Asset (negotiationBeaconCurrencySymbol,"Offer")
      assetBeacon = (Asset . (negotiationBeaconCurrencySymbol,) . _unAssetBeacon) <$> mAssetBeacon
      lenderIdBeacon = (Asset . (negotiationBeaconCurrencySymbol,) . _unLenderId) <$> mLenderId
      select =
        toText $ intercalate ","
          [ "is_spent"
          , "tx_hash"
          , "tx_index"
          , "address"
          , "stake_address"
          , "value"
          , "inline_datum"
          , "asset_list"
          ]
  case mBorrowerAddr of
    Nothing -> do
      let assetFilter = Just $ assetToQueryParam $ catMaybes [assetBeacon,lenderIdBeacon]
      assetUTxOsApi select "eq.false" assetFilter (TargetAsset offerBeacon)
    Just addr -> do
      let assetFilter = Just $ assetToQueryParam $ catMaybes [assetBeacon,lenderIdBeacon,Just offerBeacon]
      borrowerAddresssUTxOsApi select "eq.false" assetFilter (ExtendedPaymentAddresses [addr])  

queryActives :: Maybe AssetBeacon -> Maybe PaymentAddress -> Maybe LoanId -> ClientM [LoanUTxO]
queryActives mAssetBeacon mBorrowerAddr mLoanId = do
  let activeBeacon = Asset (activeBeaconCurrencySymbol,"Active")
      assetBeacon = (Asset . (activeBeaconCurrencySymbol,) . _unAssetBeacon) <$> mAssetBeacon
      loanIdBeacon = (Asset . (activeBeaconCurrencySymbol,) . _unLoanId) <$> mLoanId
      select =
        toText $ intercalate ","
          [ "is_spent"
          , "tx_hash"
          , "tx_index"
          , "address"
          , "stake_address"
          , "value"
          , "inline_datum"
          , "asset_list"
          ]
  case mBorrowerAddr of
    Nothing -> do
      let assetFilter = Just $ assetToQueryParam $ catMaybes [assetBeacon,loanIdBeacon]
      assetUTxOsApi select "eq.false" assetFilter (TargetAsset activeBeacon)
    Just addr -> do
      let assetFilter = Just $ assetToQueryParam $ catMaybes [assetBeacon,loanIdBeacon,Just activeBeacon]
      borrowerAddresssUTxOsApi select "eq.false" assetFilter (ExtendedPaymentAddresses [addr])  

querySpecificLoanUTxO :: TxOutRef -> ClientM [LoanUTxO]
querySpecificLoanUTxO outRef =
    specificLoanUTxOsApi select "eq.false" Nothing (ExtendedUTxOList [outRef])  
  where
    select =
      toText $ intercalate ","
        [ "is_spent"
        , "tx_hash"
        , "tx_index"
        , "address"
        , "stake_address"
        , "value"
        , "inline_datum"
        , "asset_list"
        ]

queryBorrowerIdBurnHistory :: BorrowerId -> ClientM MintTxs
queryBorrowerIdBurnHistory (BorrowerId borrowerId) = do
    MintTxs res <- fromMaybe (MintTxs []) . viaNonEmpty head <$> 
      assetHistoryApi select activeBeaconCurrencySymbol borrowerId
    return $ MintTxs $ filter (\MintTx{_quantity} -> _quantity < 0) res
  where
    select =
      toText $ intercalate ","
        [ "minting_txs"
        ]

queryBorrowerHistory :: BorrowerId -> ClientM [CreditHistory]
queryBorrowerHistory borrowerId = do
    info <- queryBorrowerIdBurnHistory borrowerId >>= borrowerTxInfoApi select
    return $ concatMap (processCreditHistory borrowerId) info
  where
    select =
      toText $ intercalate ","
        [ "tx_hash"
        , "tx_timestamp"
        , "block_height"
        , "inputs"
        , "outputs"
        , "assets_minted"
        ]

queryLoanTxs :: LoanId -> ClientM [LoanTx]
queryLoanTxs (LoanId loanId) = do
    loanTxsApi select activeBeaconCurrencySymbol loanId True
  where
    select =
      toText $ intercalate ","
        [ "tx_hash"
        ]

queryLoanHistory :: LoanId -> ClientM [LoanHistory]
queryLoanHistory loanId = do
    info <- queryLoanTxs loanId >>= loanTxInfoApi select . LoanTxs
    return $ catMaybes $ map (processLoanHistory loanId) info
  where
    select =
      toText $ intercalate ","
        [ "tx_hash"
        , "tx_timestamp"
        , "block_height"
        , "inputs"
        , "outputs"
        , "assets_minted"
        , "plutus_contracts"
        ]

submitTx :: TxCBOR -> ClientM Value
submitTx = submitTxApi . SubmitTxCBOR

evaluateTx :: TxCBOR -> ClientM Value
evaluateTx = evaluateTxApi . EvaluateTxCBOR

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
assetToQueryParam :: [Asset] -> Text
assetToQueryParam assets = "cs.[" <> T.intercalate "," (go assets) <> "]"
  where
    go [] = []
    go (Asset (currSym,tokName):xs) = 
      let policyId = T.pack $ show currSym
          assetName = T.pack $ showTokenName tokName
       in ("{\"policy_id\":\"" <> policyId <> "\",\"asset_name\":\"" <> assetName <> "\"}") : go xs

keysQueryParam :: Text
keysQueryParam = "cs.[{\"policy_id\":\"" <> show activeBeaconCurrencySymbol <> "\"}]"

processCreditHistory :: BorrowerId -> Transaction -> [CreditHistory]
processCreditHistory (BorrowerId borrowerId) Transaction{..}
    | uniqueBurns == 1 = catMaybes $ map processSuccess _outputs
    | otherwise = catMaybes $ map processDefault _inputs
  where
    uniqueBurns :: Int
    uniqueBurns = length 
                . filter (\AssetMint{_policyId} -> _policyId == show activeBeaconCurrencySymbol)
                $ _assetsMinted

    processSuccess :: TransactionUTxO -> Maybe CreditHistory
    processSuccess TransactionUTxO{..} = do
      -- It must not have the borrower id.
      guard $ isJust $ flip find _nativeAssets $ \NativeAsset{_policyId,_tokenName} -> 
        _policyId == show activeBeaconCurrencySymbol && 
        _tokenName /= toText (showTokenName borrowerId)

      -- But is must still have the Active beacon.
      guard $ isJust $ flip find _nativeAssets $ \NativeAsset{_policyId,_tokenName} -> 
        _policyId == show activeBeaconCurrencySymbol && 
        _tokenName == toText (showTokenName activeBeaconName)

      return $ CreditHistory
        { _default = False
        , _remainingLovelace = _lovelaces
        , _remainingNativeAssets = _nativeAssets
        , _loanTerms = fromJust $ decodeDatum @ActiveDatum $ fromJust _inlineDatum
        }

    processDefault :: TransactionUTxO -> Maybe CreditHistory
    processDefault TransactionUTxO{..} = do
      -- It must have the borrower id.
      guard $ isJust $ flip find _nativeAssets $ \NativeAsset{_policyId,_tokenName} -> 
        _policyId == show activeBeaconCurrencySymbol && _tokenName == toText (showTokenName borrowerId)

      return $ CreditHistory
        { _default = True
        , _remainingLovelace = _lovelaces
        , _remainingNativeAssets = _nativeAssets
        , _loanTerms = fromJust $ decodeDatum @ActiveDatum $ fromJust _inlineDatum
        }

processLoanHistory :: LoanId -> Transaction -> Maybe LoanHistory
processLoanHistory loanId Transaction{..} = do
    contracts <- _plutusContracts
    PlutusContract{..} <- find isTargetLoan contracts
    let parsedDatum@ActiveDatum{_claimExpiration,_loanOutstanding} = 
          fromJust $ decodeDatum @ActiveDatum $ fromJust _datum
    return $ LoanHistory
      { _loanStatus = parsedDatum
      , _action = toAction _redeemer _loanOutstanding _claimExpiration
      , _timeStamp = _blockTime
      }
  where
    isTargetLoan :: PlutusContract -> Bool
    isTargetLoan PlutusContract{_datum} = 
      flip (maybe False) _datum $ 
        maybe False (\ActiveDatum{_loanId} -> _loanId == loanId) . decodeDatum

    toAction :: Maybe Value -> Fraction -> POSIXTime -> Text
    toAction red (Fraction (balanceNum,_)) claimExpiration =
      case fromJust $ decodeDatum @LoanRedeemer $ fromJust red of
        MakePayment amount -> mconcat
          [ "Made payment of "
          , show amount
          , "."
          ]
        ApplyInterest deposit times -> mconcat
          [ "Applied interest "
          , show times
          , " time(s) with deposit increase of "
          , show deposit
          , " lovelace."
          ]
        SpendWithKeyNFT -> "Defaulted collateral claimed by lender."
        UpdateLenderAddress _ deposit -> mconcat
          [ "Lender changed the required payment address "
          , " with deposit increase of "
          , show deposit
          , " lovelace."
          ]
        Unlock -> 
          if claimExpiration < _blockTime then "Lost collateral claimed by borrower."
          else if balanceNum <= 0 then "Finished loan closed by borrower."
          else "Invalid Active UTxO closed by borrower."
        _ -> error "Other loan redeemer used."
