{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StrictData #-}

{-# OPTIONS_GHC -Wno-orphans -Wno-missing-signatures #-}

module CLI.BlockfrostApi
(
  BlockfrostApiKey(..),

  queryAllAsks,
  queryOwnAsks,
  queryAllOffers,
  queryOwnOffers,
  queryAllBorrowerLoans,
  queryAllLenderLoans,
  queryBorrowerHistory
) where

import Servant.API
import Data.Aeson
import Data.Proxy
import Servant.Client
import Control.Monad
import qualified Data.Text as T
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust,fromJust)

import CLI.Types
import CardanoLoans

-------------------------------------------------
-- Core Types
-------------------------------------------------
-- | Newtype wrapper around api key for using blockfrost
newtype BlockfrostApiKey = BlockfrostApiKey String

instance ToHttpApiData BlockfrostApiKey where
  toQueryParam (BlockfrostApiKey apiKey) = T.pack apiKey

-- | Newtype wrapper around the beacon asset being queried.
data BeaconId = BeaconId (String,String)

instance ToHttpApiData BeaconId where
  toQueryParam (BeaconId (currSym,tokName)) = T.pack $ currSym <> tokName

-- | An address that contains a beacon.
-- The response type of the beaconAddressList api.
newtype BeaconAddress = BeaconAddress { unBeaconAddress :: String } deriving (Show)

instance FromJSON BeaconAddress where
  parseJSON (Object o) = BeaconAddress <$> o .: "address"
  parseJSON _ = mzero

instance ToHttpApiData BeaconAddress where
  toQueryParam = T.pack . unBeaconAddress

-- | The response type of the beaconInfoApi. This has all the information that may be needed
-- depending on the beacon being queried.
data RawBeaconInfo = RawBeaconInfo
  { rawBeaconAddress :: String
  , rawTxHash :: String
  , rawOutputIndex :: Integer
  , rawAmount :: [RawAssetInfo]
  , rawBeaconDataHash :: Maybe String
  } deriving (Show)

instance FromJSON RawBeaconInfo where
  parseJSON (Object o) =
    RawBeaconInfo
      <$> o .: "address"
      <*> o .: "tx_hash"
      <*> o .: "tx_index"
      <*> o .: "amount"
      <*> o .: "data_hash"
  parseJSON _ = mzero

-- | Blockfrost does not separate symbol and name with '.'
data RawAssetInfo = RawAssetInfo
  { rawUnit :: String  -- ^ CurrencySymbol <> TokenName
  , rawQuantity :: Integer
  } deriving (Show)

instance FromJSON RawAssetInfo where
  parseJSON (Object o) =
    RawAssetInfo
      <$> o .: "unit"
      <*> fmap read (o .: "quantity")
  parseJSON _ = mzero

instance FromJSON LoanDatum where
  parseJSON (Object o) = do
    r <- o .: "json_value" >>= return . decodeDatum
    case r of
      Just x -> return x
      Nothing -> mzero
  parseJSON _ = mzero

-- | The return type for the assetHistoryApi.
data RawAssetHistory = RawAssetHistory
  { rawAssetHistoryTxHash :: String
  , rawAssetHistoryAction :: String
  } deriving (Show)

instance FromJSON RawAssetHistory where
  parseJSON (Object o) =
    RawAssetHistory
      <$> o .: "tx_hash"
      <*> o .: "action"
  parseJSON _ = mzero

instance ToHttpApiData RawAssetHistory where
  toQueryParam = T.pack . rawAssetHistoryTxHash

-- | The return type for the defaultStatusApi.
-- If the status returned is 1, then the borrower successfully paid the loan.
-- If the status returned is 3, then the borrower defaulted on the loan.
newtype RawDefaultStatus = RawDefaultStatus { unRawDefaultStatus :: Integer } deriving (Show)

instance FromJSON RawDefaultStatus where
  parseJSON (Object o) = RawDefaultStatus <$> o .: "asset_mint_or_burn_count"
  parseJSON _ = mzero

-- | This has the information needed for the borrower's history.
data RawLoanInfo = RawLoanInfo 
  { rawLoanAmount :: [RawAssetInfo]
  , rawLoanDataHash :: Maybe String
  } deriving (Show)

instance FromJSON RawLoanInfo where
  parseJSON (Object o) = RawLoanInfo <$> o .: "amount" <*> o .: "data_hash"
  parseJSON _ = mzero

newtype RawLoan = RawLoan { unRawLoan :: [RawLoanInfo] } deriving (Show)

instance FromJSON RawLoan where
  parseJSON (Object o) = RawLoan <$> (o .: "inputs" >>= parseJSON)
  parseJSON _ = mzero

-------------------------------------------------
-- Blockfrost Api
-------------------------------------------------
type BlockfrostApi
  =    "assets"
    :> Header' '[Required] "project_id" BlockfrostApiKey
    :> Capture "asset" BeaconId
    :> "addresses"
    :> Get '[JSON] [BeaconAddress]

  :<|> "addresses"
    :> Header' '[Required] "project_id" BlockfrostApiKey
    :> Capture "address" BeaconAddress
    :> "utxos"
    :> Capture "asset" BeaconId
    :> Get '[JSON] [RawBeaconInfo]

  :<|> "scripts"
    :> Header' '[Required] "project_id" BlockfrostApiKey
    :> "datum"
    :> Capture "datum_hash" String
    :> Get '[JSON] Value

  :<|> "assets"
    :> Header' '[Required] "project_id" BlockfrostApiKey
    :> Capture "asset" BeaconId
    :> "history"
    :> Get '[JSON] [RawAssetHistory]

  :<|> "txs"
    :> Header' '[Required] "project_id" BlockfrostApiKey
    :> Capture "hash" RawAssetHistory
    :> Get '[JSON] RawDefaultStatus

  :<|> "txs"
    :> Header' '[Required] "project_id" BlockfrostApiKey
    :> Capture "hash" RawAssetHistory
    :> "utxos"
    :> Get '[JSON] RawLoan

beaconAddressListApi :<|> beaconInfoApi :<|> datumApi :<|> assetHistoryApi 
  :<|> defaultStatusApi :<|> loanInfoApi = client api
  where
    api :: Proxy BlockfrostApi
    api = Proxy

-------------------------------------------------
-- Blockfrost Query Functions
-------------------------------------------------
queryAllAsks :: BlockfrostApiKey -> String -> ClientM [LoanInfo]
queryAllAsks apiKey policyId = do
  let beaconId = BeaconId (policyId,"41736b")
  -- | Get all the addresses that currently hold the beacon.
  addrs <- beaconAddressListApi apiKey beaconId
  -- | Get all the beacon UTxOs for those addresses.
  beaconUTxOs <- concat <$> mapM (\z -> beaconInfoApi apiKey z beaconId) addrs
  -- | Get all the AskDatums attached to the beacon UTxOs.
  askInfos <- fetchDatumsLenient apiKey $ map rawBeaconDataHash beaconUTxOs
  return $ convertToLoanInfo Ask beaconUTxOs askInfos

queryOwnAsks :: BlockfrostApiKey -> String -> String -> ClientM [LoanInfo]
queryOwnAsks apiKey policyId addr = do
  let beaconAddr = BeaconAddress addr
      beaconId = BeaconId (policyId,"41736b")
  -- | Get all the beacon UTxOs for the address.
  beaconUTxOs <- beaconInfoApi apiKey beaconAddr beaconId
  -- | Get all the AskDatums attached to the beacon UTxOs.
  askInfos <- fetchDatumsLenient apiKey $ map rawBeaconDataHash beaconUTxOs
  return $ convertToLoanInfo Ask beaconUTxOs askInfos

queryAllOffers :: BlockfrostApiKey -> String -> String -> ClientM [LoanInfo]
queryAllOffers apiKey policyId addr = do
  let beaconAddr = BeaconAddress addr
      beaconId = BeaconId (policyId,"4f66666572")
  -- | Get all the beacon UTxOs for the address.
  beaconUTxOs <- beaconInfoApi apiKey beaconAddr beaconId
  -- | Get all the OfferDatums attached to the beacon UTxOs.
  offerInfos <- fetchDatumsLenient apiKey $ map rawBeaconDataHash beaconUTxOs
  return $ convertToLoanInfo Offer beaconUTxOs offerInfos

queryOwnOffers :: BlockfrostApiKey -> String -> String -> ClientM [LoanInfo]
queryOwnOffers apiKey policyId lenderPubKeyHash = do
  let offerBeacon = policyId <> "4f66666572"
      lenderBeacon = BeaconId (policyId,lenderPubKeyHash)
  -- | Get all the addresses that currently hold the lenderBeacon.
  addrs <- beaconAddressListApi apiKey lenderBeacon
  -- | Get all the lenderBeacon UTxOs for those addresses and filter for only the UTxOs
  -- with an offerBeacon.
  beaconUTxOs <- filterForAsset offerBeacon . concat 
             <$> mapM (\z -> beaconInfoApi apiKey z lenderBeacon) addrs
  -- | Get all the OfferDatums attached to the beacon UTxOs.
  offerInfos <- fetchDatumsLenient apiKey $ map rawBeaconDataHash beaconUTxOs
  return $ convertToLoanInfo Offer beaconUTxOs offerInfos

queryAllBorrowerLoans :: BlockfrostApiKey -> String -> String -> String -> ClientM [LoanInfo]
queryAllBorrowerLoans apiKey policyId borrowerStakeKeyHash addr = do
  let beaconAddr = BeaconAddress addr
      beaconId = BeaconId (policyId,borrowerStakeKeyHash)
  -- | Get all the beacon UTxOs for the address.
  beaconUTxOs <- beaconInfoApi apiKey beaconAddr beaconId
  -- | Get all the ActiveDatums attached to the beacon UTxOs.
  activeInfos <- fetchDatumsLenient apiKey $ map rawBeaconDataHash beaconUTxOs
  return $ convertToLoanInfo Loan beaconUTxOs activeInfos

queryAllLenderLoans :: BlockfrostApiKey -> String -> String -> ClientM [LoanInfo]
queryAllLenderLoans apiKey policyId lenderPubKeyHash = do
  let activeBeacon = policyId <> "416374697665"
      lenderBeacon = BeaconId (policyId,lenderPubKeyHash)
  -- | Get all the addresses that currently hold the lenderBeacon.
  addrs <- beaconAddressListApi apiKey lenderBeacon
  -- | Get all the lenderBeacon UTxOs for those addresses and filter for only the UTxOs
  -- with an activeBeacon.
  beaconUTxOs <- filterForAsset activeBeacon . concat 
             <$> mapM (\z -> beaconInfoApi apiKey z lenderBeacon) addrs
  -- | Get all the ActiveDatums attached to the beacon UTxOs.
  activeInfos <- fetchDatumsLenient apiKey $ map rawBeaconDataHash beaconUTxOs
  return $ convertToLoanInfo Loan beaconUTxOs activeInfos

queryBorrowerHistory :: BlockfrostApiKey -> String -> String -> ClientM [LoanHistory]
queryBorrowerHistory apiKey policyId borrowerPubKeyHash = do
  let borrowerBeacon = BeaconId (policyId,borrowerPubKeyHash)
      activeBeacon = policyId <> "416374697665"
  -- | Get all the burn transactions for the borrower ID.
  burnTxs <- filter ((== "burned") . rawAssetHistoryAction) <$> assetHistoryApi apiKey borrowerBeacon
  -- | Get the default status for each tx.
  defaultStatuses <- mapM (\z -> defaultStatusApi apiKey z) burnTxs
  -- | Get the loan info for each tx.
  loanInfos <- filterForAsset' activeBeacon . concatMap unRawLoan
           <$> mapM (\z -> loanInfoApi apiKey z) burnTxs
  -- | Get the active datums.
  activeDatums <- fetchDatumsLenient apiKey $ map rawLoanDataHash loanInfos
  return $ zipWith (convertToLoanHistory activeDatums) defaultStatuses loanInfos

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
-- | Skips ones that fail to decode.
fetchDatumsLenient :: FromJSON a => BlockfrostApiKey -> [Maybe String] -> ClientM (Map String a)
fetchDatumsLenient apiKey dhs =
  let go _ datumMap [] = return datumMap
      go key datumMap ((Just d):ds) = do
        i' <- fromJSON <$> datumApi key d
        case i' of
          Success i -> go key (Map.insert d i datumMap) ds
          Error _ -> go key datumMap ds
      go key datumMap (Nothing:ds) = go key datumMap ds
  in go apiKey Map.empty dhs

filterForAsset :: String -> [RawBeaconInfo] -> [RawBeaconInfo]
filterForAsset asset = filter (isJust . find ((==asset) . rawUnit) . rawAmount)

filterForAsset' :: String -> [RawLoanInfo] -> [RawLoanInfo]
filterForAsset' asset = filter (isJust . find ((==asset) . rawUnit) . rawLoanAmount)

convertToAsset :: RawAssetInfo -> Asset
convertToAsset RawAssetInfo{rawUnit=u,rawQuantity=q} =
  if u == "lovelace"
  then Asset
        { assetPolicyId = u
        , assetTokenName = ""
        , assetQuantity = q
        }
  else Asset
        { assetPolicyId = take 56 u  -- ^ The policy id is always 56 characters
        , assetTokenName = drop 56 u
        , assetQuantity = q
        }

convertToLoanInfo :: LoanType -> [RawBeaconInfo] -> Map String LoanDatum -> [LoanInfo]
convertToLoanInfo _ [] _ = []
convertToLoanInfo t ((RawBeaconInfo addr tx ix amount dHash):rs) datumMap =
    info : convertToLoanInfo t rs datumMap
  where info = LoanInfo
                { loanType = t
                , address = addr
                , txHash = tx
                , outputIndex = show ix
                , uTxOValue = map convertToAsset amount
                , loanInfo = fromJust $ join $ fmap (\z -> Map.lookup z datumMap) dHash
                }

convertToLoanHistory :: Map String LoanDatum -> RawDefaultStatus -> RawLoanInfo -> LoanHistory
convertToLoanHistory datumMap (RawDefaultStatus s) (RawLoanInfo amount dHash) =
  LoanHistory
    { defaultStatus = if s == 1 then False else True
    , remainingUTxOValue = map convertToAsset amount
    , loan = fromJust $ join $ fmap (\z -> Map.lookup z datumMap) dHash
    }