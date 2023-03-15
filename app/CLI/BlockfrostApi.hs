{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StrictData #-}

{-# OPTIONS_GHC -Wno-missing-signatures -Wno-incomplete-patterns #-}

module CLI.BlockfrostApi
(
  BlockfrostApiKey(..),
  queryAllAsks,
  queryBorrowerAsks,
  queryAllOffersFromLender,
  queryAllOffersToBorrower,
  queryAllLendersActiveLoans,
  queryAllBorrowersActiveLoans,
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
import qualified Data.Vector as Vector
import Data.Maybe (isJust,fromJust)

import CLI.Types

-------------------------------------------------
-- Core Types
-------------------------------------------------
-- | Newtype wrapper around api key for using blockfrost
newtype BlockfrostApiKey = BlockfrostApiKey String

instance ToHttpApiData BlockfrostApiKey where
  toQueryParam (BlockfrostApiKey apiKey) = T.pack apiKey

-- | Newtype wrapper around the beacon. Strings are used here to make it easier for dealing
-- with token name encodings.
newtype BeaconId = BeaconId (String,String)

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

-- | The inline ask datum type used in askDatumApi.
data RawAskDatum = RawAskDatum
  { rawAskBeacon :: RawAssetClass
  , rawAskBorrowerId :: RawAssetClass
  , rawAskLoanAsset :: RawAssetClass
  , rawAskLoanPrinciple :: Integer
  , rawAskLoanTerm :: Integer
  , rawAskCollateral :: [RawAssetClass]
  } deriving (Show)

instance FromJSON RawAskDatum where
  parseJSON (Object o) =
    RawAskDatum
      <$> ( o .: "json_value" >>= fmap (Vector.! 0) . (.: "fields") >>= parseJSON )
      <*> ( o .: "json_value" >>= fmap (Vector.! 1) . (.: "fields") >>= parseJSON )
      <*> ( o .: "json_value" >>= fmap (Vector.! 2) . (.: "fields") >>= parseJSON )
      <*> ( o .: "json_value" >>= fmap (Vector.! 3) . (.: "fields") >>= (.: "int") )
      <*> ( o .: "json_value" >>= fmap (Vector.! 4) . (.: "fields") >>= (.: "int") )
      <*> ( o .: "json_value" >>= fmap (Vector.! 5) . (.: "fields") >>= (.: "list") >>= parseJSON )
  parseJSON _ = mzero

-- | Used to decode the datums.
newtype RawAssetClass = RawAssetClass { unRawAssetClass :: (String,String) } deriving (Show)

instance FromJSON RawAssetClass where
  parseJSON (Object o) =
    fmap RawAssetClass . (,)
      <$> ((o .: "fields") >>= (.: "bytes") . (Vector.! 0))
      <*> ((o .: "fields") >>= (.: "bytes") . (Vector.! 1)) 
  parseJSON _ = mzero

-- | Custom Rational type for decoding.
data RawRational = RawRational { rawNumerator :: Integer, rawDenominator :: Integer } deriving (Show)

instance FromJSON RawRational where
  parseJSON (Object o) =
    RawRational
      <$> ((o .: "fields") >>= (.: "int") . (Vector.! 0))
      <*> ((o .: "fields") >>= (.: "int") . (Vector.! 1)) 
  parseJSON _ = mzero

newtype RawCollateralRate = RawCollateralRate { unRawCollateralRate :: (RawAssetClass,RawRational) }
  deriving (Show)

instance FromJSON RawCollateralRate where
  parseJSON (Object o) = RawCollateralRate <$> ((o .: "fields") >>= parseJSON)
  parseJSON _ = mzero

-- | The inline offer datum type used in the offerDatumApi.
data RawOfferDatum = RawOfferDatum
  { rawOfferBeacon :: RawAssetClass
  , rawOfferLenderId :: RawAssetClass
  , rawOfferLoanAsset :: RawAssetClass
  , rawOfferLoanPrinciple :: Integer
  , rawOfferLoanTerm :: Integer
  , rawOfferLoanInterest :: RawRational
  , rawOfferLoanBacking :: Integer
  , rawOfferCollateralRates :: [RawCollateralRate]
  } deriving (Show)

instance FromJSON RawOfferDatum where
  parseJSON (Object o) =
    RawOfferDatum
      <$> ( o .: "json_value" >>= fmap (Vector.! 0) . (.: "fields") >>= parseJSON )
      <*> ( o .: "json_value" >>= fmap (Vector.! 1) . (.: "fields") >>= parseJSON )
      <*> ( o .: "json_value" >>= fmap (Vector.! 2) . (.: "fields") >>= parseJSON )
      <*> ( o .: "json_value" >>= fmap (Vector.! 3) . (.: "fields") >>= (.: "int") )
      <*> ( o .: "json_value" >>= fmap (Vector.! 4) . (.: "fields") >>= (.: "int") )
      <*> ( o .: "json_value" >>= fmap (Vector.! 5) . (.: "fields") >>= parseJSON )
      <*> ( o .: "json_value" >>= fmap (Vector.! 6) . (.: "fields") >>= (.: "int") )
      <*> ( o .: "json_value" >>= fmap (Vector.! 7) . (.: "fields") >>= (.: "list") >>= parseJSON )
  parseJSON _ = mzero

-- | The inline active datum type used in the activeDatumApi.
data RawActiveDatum = RawActiveDatum
  { rawActiveBeacon :: RawAssetClass
  , rawActiveLenderId :: RawAssetClass
  , rawActiveBorrowerId :: RawAssetClass
  , rawActiveLoanAsset :: RawAssetClass
  , rawActiveLoanPrinciple :: Integer
  , rawActiveLoanTerm :: Integer
  , rawActiveLoanInterest :: RawRational
  , rawActiveLoanBacking :: Integer
  , rawActiveCollateralRates :: [RawCollateralRate]
  , rawActiveLoanExpiration :: Integer
  , rawActiveLoanOutstanding :: RawRational
  } deriving (Show)

instance FromJSON RawActiveDatum where
  parseJSON (Object o) =
    RawActiveDatum
      <$> ( o .: "json_value" >>= fmap (Vector.! 0) . (.: "fields") >>= parseJSON )
      <*> ( o .: "json_value" >>= fmap (Vector.! 1) . (.: "fields") >>= parseJSON )
      <*> ( o .: "json_value" >>= fmap (Vector.! 2) . (.: "fields") >>= parseJSON )
      <*> ( o .: "json_value" >>= fmap (Vector.! 3) . (.: "fields") >>= parseJSON )
      <*> ( o .: "json_value" >>= fmap (Vector.! 4) . (.: "fields") >>= (.: "int") )
      <*> ( o .: "json_value" >>= fmap (Vector.! 5) . (.: "fields") >>= (.: "int") )
      <*> ( o .: "json_value" >>= fmap (Vector.! 6) . (.: "fields") >>= parseJSON )
      <*> ( o .: "json_value" >>= fmap (Vector.! 7) . (.: "fields") >>= (.: "int") )
      <*> ( o .: "json_value" >>= fmap (Vector.! 8) . (.: "fields") >>= (.: "list") >>= parseJSON )
      <*> ( o .: "json_value" >>= fmap (Vector.! 9) . (.: "fields") >>= (.: "int") )
      <*> ( o .: "json_value" >>= fmap (Vector.! 10) . (.: "fields") >>= parseJSON )
  parseJSON _ = mzero

class RawDatum a where
  fetchDatums :: BlockfrostApiKey -> [Maybe String] -> ClientM (Map String a)

instance RawDatum RawAskDatum where
  fetchDatums apiKey dhs = 
    let go _ datumMap [] = return datumMap
        go key datumMap ((Just d):ds) = do
          i <- askDatumApi key d
          go key (Map.insert d i datumMap) ds
    in go apiKey Map.empty dhs

instance RawDatum RawOfferDatum where
  fetchDatums apiKey dhs = 
    let go _ datumMap [] = return datumMap
        go key datumMap ((Just d):ds) = do
          i <- offerDatumApi key d
          go key (Map.insert d i datumMap) ds
    in go apiKey Map.empty dhs

instance RawDatum RawActiveDatum where
  fetchDatums apiKey dhs = 
    let go _ datumMap [] = return datumMap
        go key datumMap ((Just d):ds) = do
          i <- activeDatumApi key d
          go key (Map.insert d i datumMap) ds
    in go apiKey Map.empty dhs

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
-- IF the status returned is 3, then the borrower defaulted on the loan.
newtype RawDefaultStatus = RawDefaultStatus { unRawDefaultStatus :: Integer } deriving (Show)

instance FromJSON RawDefaultStatus where
  parseJSON (Object o) = RawDefaultStatus <$> o .: "asset_mint_or_burn_count"
  parseJSON _ = mzero

-- | This has the information needed for the borrower's history.
data RawLoanInfo = RawLoanInfo
  { rawLoanAddress :: String
  , rawLoanTxHash :: String
  , rawLoanOutputIndex :: Integer
  , rawLoanAmount :: [RawAssetInfo]
  , rawLoanDataHash :: Maybe String
  } deriving (Show)

instance FromJSON RawLoanInfo where
  parseJSON (Object o) =
    RawLoanInfo
      <$> o .: "address"
      <*> o .: "tx_hash"
      <*> o .: "output_index"
      <*> o .: "amount"
      <*> o .: "data_hash"
  parseJSON _ = mzero

-- | The return type for the loanInfoApi.
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
    :> Get '[JSON] RawAskDatum

  :<|> "scripts"
    :> Header' '[Required] "project_id" BlockfrostApiKey
    :> "datum"
    :> Capture "datum_hash" String
    :> Get '[JSON] RawOfferDatum
  
  :<|> "scripts"
    :> Header' '[Required] "project_id" BlockfrostApiKey
    :> "datum"
    :> Capture "datum_hash" String
    :> Get '[JSON] RawActiveDatum
  
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

beaconAddressListApi :<|> beaconInfoApi :<|> askDatumApi :<|> offerDatumApi :<|> activeDatumApi 
  :<|> assetHistoryApi :<|> defaultStatusApi :<|> loanInfoApi = client api
  where
    api :: Proxy BlockfrostApi
    api = Proxy

-------------------------------------------------
-- Blockfrost Query Functions
-------------------------------------------------
queryAllAsks :: BlockfrostApiKey -> ClientM [AvailableAsk]
queryAllAsks apiKey = do
  let askBeaconId = BeaconId (show beaconSym,"41736b")
  -- | Get all the addresses that currently hold the ask beacon.
  addrs <- beaconAddressListApi apiKey askBeaconId
  -- | Get all the ask utxos for those addresses.
  addrUtxos <- concat <$> mapM (\z -> beaconInfoApi apiKey z askBeaconId) addrs
  -- | Get all the ask datums.
  askDatums <- fetchDatums apiKey $ map rawBeaconDataHash addrUtxos
  return $ convertAsk addrUtxos askDatums

queryBorrowerAsks :: BlockfrostApiKey -> String -> ClientM [AvailableAsk]
queryBorrowerAsks apiKey addr = do
  let askBeaconId = BeaconId (show beaconSym,"41736b")
      borrowerAddr = BeaconAddress addr
  -- | Get all the ask utxos for the address.
  addrUtxos <- beaconInfoApi apiKey borrowerAddr askBeaconId
  -- | Get all the ask datums.
  askDatums <- fetchDatums apiKey $ map rawBeaconDataHash addrUtxos
  return $ convertAsk addrUtxos askDatums

queryAllOffersFromLender :: BlockfrostApiKey -> String -> ClientM [AvailableOffer]
queryAllOffersFromLender apiKey lenderPubKeyHash = do
  let lender = BeaconId (show beaconSym,lenderPubKeyHash)
      offerBeacon' = show beaconSym <> "4f66666572"
  -- | Get all the addresses that currently hold the lender ID.
  addrs <- beaconAddressListApi apiKey lender
  -- | Get all the lender ID utxos for those addresses.
  addrUtxos <- concat <$> mapM (\z -> beaconInfoApi apiKey z lender) addrs
  -- | Filter for utxos that have an offer beacon. Then get the datums for those utxos.
  let offerUtxos = filterForAsset offerBeacon' addrUtxos
  offerDatums <- fetchDatums apiKey $ map rawBeaconDataHash offerUtxos
  return $ convertOffer offerUtxos offerDatums

queryAllOffersToBorrower :: BlockfrostApiKey -> String -> ClientM [AvailableOffer]
queryAllOffersToBorrower apiKey addr = do
  let borrowerAddr = BeaconAddress addr
      offerBeaconId = BeaconId (show beaconSym,"4f66666572")
  -- | Get all the offer utxos for those addresses.
  offerUtxos <- beaconInfoApi apiKey borrowerAddr offerBeaconId
  -- | Get the datums for those utxos.
  offerDatums <- fetchDatums apiKey $ map rawBeaconDataHash offerUtxos
  return $ convertOffer offerUtxos offerDatums

queryAllLendersActiveLoans :: BlockfrostApiKey -> String -> ClientM [AvailableActive]
queryAllLendersActiveLoans apiKey lenderPubKeyHash = do
  let lender = BeaconId (show beaconSym,lenderPubKeyHash)
      activeId = show beaconSym <> "416374697665"
  -- | Get all the addresses that currently hold the lender ID.
  addrs <- beaconAddressListApi apiKey lender
  -- | Get all the lender ID utxos for those addresses.
  addrUtxos <- concat <$> mapM (\z -> beaconInfoApi apiKey z lender) addrs
  -- | Filter for utxos that have an active beacon. Then get the datums for those utxos.
  let activeUtxos = filterForAsset activeId addrUtxos
  activeDatums <- fetchDatums apiKey $ map rawBeaconDataHash activeUtxos
  return $ convertActive activeUtxos activeDatums

queryAllBorrowersActiveLoans :: BlockfrostApiKey -> String -> ClientM [AvailableActive]
queryAllBorrowersActiveLoans apiKey borrowerPubKeyHash = do
  let borrower = BeaconId (show beaconSym,borrowerPubKeyHash)
      activeId = show beaconSym <> "416374697665"
  -- | Get all the addresses that currently hold the borrower ID (there should only be one).
  addrs <- beaconAddressListApi apiKey borrower
  -- | Get all the borrower ID utxos for those addresses. Only the active loans will still have
  -- the borrower's ID.
  addrUtxos <- concat <$> mapM (\z -> beaconInfoApi apiKey z borrower) addrs
  -- | Filter for utxos that have an active beacon. Then get the datums for those utxos.
  let activeUtxos = filterForAsset activeId addrUtxos
  activeDatums <- fetchDatums apiKey $ map rawBeaconDataHash activeUtxos
  return $ convertActive activeUtxos activeDatums

queryBorrowerHistory :: BlockfrostApiKey -> String -> ClientM [LoanHistory]
queryBorrowerHistory apiKey borrowerPubKeyHash = do
  let borrower = BeaconId (show beaconSym,borrowerPubKeyHash)
      activeId = show beaconSym <> "416374697665"
  -- | Get all the burn transaction for the borrower ID.
  burnTxs <- filter ((== "burned") . rawAssetHistoryAction) <$> assetHistoryApi apiKey borrower
  -- | Get the default status for each tx.
  defaultStatuses <- mapM (\z -> defaultStatusApi apiKey z) burnTxs
  -- | Get the loan info for each tx.
  loanInfos <- filterForAsset' activeId . concatMap unRawLoan 
           <$> mapM (\z -> loanInfoApi apiKey z) burnTxs
  -- | Get the active datums.
  activeDatums <- fetchDatums apiKey $ map rawLoanDataHash loanInfos
  return $ zipWith (convertLoanHistory activeDatums) defaultStatuses loanInfos

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
filterForAsset :: String -> [RawBeaconInfo] -> [RawBeaconInfo]
filterForAsset asset = filter (isJust . find ((==asset) . rawUnit) . rawAmount)

filterForAsset' :: String -> [RawLoanInfo] -> [RawLoanInfo]
filterForAsset' asset = filter (isJust . find ((==asset) . rawUnit) . rawLoanAmount)

toAvailableAsset :: RawAssetInfo -> AvailableAsset
toAvailableAsset RawAssetInfo{rawUnit=u,rawQuantity=q} =
  if u == "lovelace"
  then AvailableAsset
          { assetPolicyId = u
          , assetTokenName = ""
          , assetQuantity = q
          }
  else AvailableAsset
          { assetPolicyId = take 56 u  -- ^ The policy id is always 56 characters
          , assetTokenName = drop 56 u
          , assetQuantity = q
          }

toAssetId :: RawAssetClass -> String
toAssetId (RawAssetClass (currSym,tokName)) = 
  if currSym == ""
  then "lovelace"
  else currSym <> "." <> tokName

convertAsk :: [RawBeaconInfo] -> Map String RawAskDatum -> [AvailableAsk]
convertAsk [] _ = []
convertAsk ((RawBeaconInfo addr tx ix amount dHash):rs) datumMap = AvailableAsk
    { availableAskAddress = addr
    , availableAskTxIx = tx <> "#" <> show ix
    , availableAskAssets = map toAvailableAsset amount
    , availableAskBorrowerID = snd $ unRawAssetClass $ rawAskBorrowerId askDatum
    , availableAskLoanAsset = toAssetId $ rawAskLoanAsset askDatum
    , availableAskLoanPrinciple = rawAskLoanPrinciple askDatum
    , availableAskLoanTerm = rawAskLoanTerm askDatum `div` 1000  -- ^ Converted to slots
    , availableAskCollateral = map toAssetId $ rawAskCollateral askDatum
    } : convertAsk rs datumMap
  where
    askDatum :: RawAskDatum
    askDatum = fromJust $ Map.lookup (fromJust dHash) datumMap

convertRate :: (RawAssetClass,RawRational) -> AvailableCollateralRate
convertRate (asset,rawRat) = 
  AvailableCollateralRate (toAssetId asset,(rawNumerator rawRat,rawDenominator rawRat))

convertOffer :: [RawBeaconInfo] -> Map String RawOfferDatum -> [AvailableOffer]
convertOffer [] _ = []
convertOffer ((RawBeaconInfo addr tx ix amount dHash):rs) datumMap = AvailableOffer
    { availableOfferAddress = addr
    , availableOfferTxIx = tx <> "#" <> show ix
    , availableOfferAssets = map toAvailableAsset amount
    , availableOfferLenderID = snd $ unRawAssetClass $ rawOfferLenderId offerDatum
    , availableOfferLoanAsset = toAssetId $ rawOfferLoanAsset offerDatum
    , availableOfferLoanPrinciple = rawOfferLoanPrinciple offerDatum
    , availableOfferLoanTerm = rawOfferLoanTerm offerDatum `div` 1000  -- ^ Converted to slots
    , availableOfferLoanInterestNumerator = rawNumerator $ rawOfferLoanInterest offerDatum
    , availableOfferLoanInterestDenominator = rawDenominator $ rawOfferLoanInterest offerDatum
    , availableOfferLoanBacking = rawOfferLoanBacking offerDatum
    , availableOfferCollateralRates = map (convertRate . unRawCollateralRate) 
                                    $ rawOfferCollateralRates offerDatum
    } : convertOffer rs datumMap
  where
    offerDatum :: RawOfferDatum
    offerDatum = fromJust $ Map.lookup (fromJust dHash) datumMap

convertActive :: [RawBeaconInfo] -> Map String RawActiveDatum -> [AvailableActive]
convertActive [] _ = []
convertActive ((RawBeaconInfo addr tx ix amount dHash):rs) datumMap = AvailableActive
    { availableActiveAddress = addr
    , availableActiveTxIx = tx <> "#" <> show ix
    , availableActiveAssets = map toAvailableAsset amount
    , availableActiveLenderID = snd $ unRawAssetClass $ rawActiveLenderId activeDatum
    , availableActiveBorrowerID = snd $ unRawAssetClass $ rawActiveBorrowerId activeDatum
    , availableActiveLoanAsset = toAssetId $ rawActiveLoanAsset activeDatum
    , availableActiveLoanPrinciple = rawActiveLoanPrinciple activeDatum
    , availableActiveLoanTerm = rawActiveLoanTerm activeDatum `div` 1000  -- ^ Converted to slots
    , availableActiveLoanInterestNumerator = rawNumerator $ rawActiveLoanInterest activeDatum
    , availableActiveLoanInterestDenominator = rawDenominator $ rawActiveLoanInterest activeDatum
    , availableActiveLoanBacking = rawActiveLoanBacking activeDatum
    , availableActiveCollateralRates = map (convertRate . unRawCollateralRate) 
                                     $ rawActiveCollateralRates activeDatum
    , availableActiveLoanExpiration = rawActiveLoanExpiration activeDatum
    , availableActiveLoanOutstandingNumerator = rawNumerator $ rawActiveLoanOutstanding activeDatum
    , availableActiveLoanOutstandingDenominator = rawDenominator $ rawActiveLoanOutstanding activeDatum
    } : convertActive rs datumMap
  where
    activeDatum :: RawActiveDatum
    activeDatum = fromJust $ Map.lookup (fromJust dHash) datumMap
    
convertLoanHistory :: Map String RawActiveDatum -> RawDefaultStatus -> RawLoanInfo -> LoanHistory
convertLoanHistory datumMap (RawDefaultStatus s) (RawLoanInfo addr tx ix amount dHash) = LoanHistory
  { defaultStatus = if s == 1 then False else True
  , loanInfo = AvailableActive
      { availableActiveAddress = addr
      , availableActiveTxIx = tx <> "#" <> show ix
      , availableActiveAssets = map toAvailableAsset amount
      , availableActiveLenderID = snd $ unRawAssetClass $ rawActiveLenderId activeDatum
      , availableActiveBorrowerID = snd $ unRawAssetClass $ rawActiveBorrowerId activeDatum
      , availableActiveLoanAsset = toAssetId $ rawActiveLoanAsset activeDatum
      , availableActiveLoanPrinciple = rawActiveLoanPrinciple activeDatum
      , availableActiveLoanTerm = rawActiveLoanTerm activeDatum `div` 1000  -- ^ Converted to slots
      , availableActiveLoanInterestNumerator = rawNumerator $ rawActiveLoanInterest activeDatum
      , availableActiveLoanInterestDenominator = rawDenominator $ rawActiveLoanInterest activeDatum
      , availableActiveLoanBacking = rawActiveLoanBacking activeDatum
      , availableActiveCollateralRates = map (convertRate . unRawCollateralRate) 
                                      $ rawActiveCollateralRates activeDatum
      , availableActiveLoanExpiration = rawActiveLoanExpiration activeDatum
      , availableActiveLoanOutstandingNumerator = rawNumerator $ rawActiveLoanOutstanding activeDatum
      , availableActiveLoanOutstandingDenominator = rawDenominator $ rawActiveLoanOutstanding activeDatum
      }
  }
  where
    activeDatum :: RawActiveDatum
    activeDatum = fromJust $ Map.lookup (fromJust dHash) datumMap

