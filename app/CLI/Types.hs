{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module CLI.Types where

import Data.Aeson
import Data.Text (Text)
import GHC.Word (Word32)

import CardanoLoans hiding ((-),(+),(*))
import qualified CardanoLoans as CL

data Command
  = ExportScript Script FilePath
  | CreateLoanDatum LoanDatum' FilePath
  | CreateLoanRedeemer LoanRedeemer FilePath
  | CreateBeaconRedeemer BeaconRedeemer FilePath
  | Query Query
  | ConvertTime ConvertTime
  | ConvertAddress ConvertAddress Output

data LoanDatum' = CollateralDatum LoanDatum | LenderDatum TokenName

data ConvertTime
  = POSIXTimeToSlot POSIXTime
  | SlotToPOSIXTime Slot

data ConvertAddress
  = PlutusToBech32 Address
  | Bech32ToPlutus Text

data Query
  = QueryAllAsks Network ApiEndpoint Output
  | QueryOwnAsks Network ApiEndpoint LoanAddress Output
  | QueryAllOffers Network ApiEndpoint LoanAddress Output
  | QueryOwnOffers Network ApiEndpoint LenderID Output
  | QueryAllBorrowersActiveLoans Network ApiEndpoint BorrowerID LoanAddress Output
  | QueryAllBorrowersFinishedLoans Network ApiEndpoint BorrowerID LoanAddress Output
  | QuerySpecificLoan Network ApiEndpoint LoanID Output
  | QueryOwnKeys Network ApiEndpoint LoanAddress Output
  | QueryBorrowersHistory Network ApiEndpoint BorrowerID Output

data Script = BeaconPolicy | LoanScript

-- | For when saving to file is optional
data Output = Stdout | File FilePath

type LoanAddress = String
type LenderID = String
type BorrowerID = String
type LoanID = String

data ApiEndpoint
  = Blockfrost String -- ^ ApiKey
  | Koios

data Network 
  = PreProdTestnet

-- | Used as an intermediate type.
data ActiveDatum' 
  = AcceptDatum'
      { beaconSym' :: CurrencySymbol
      , borrowerId' :: TokenName
      , lenderAddress' :: Address
      , loanAsset' :: (CurrencySymbol,TokenName)
      , loanPrinciple' :: Integer
      , loanCheckpoints' :: [POSIXTime]
      , loanTerm' :: POSIXTime
      , loanInterest' :: PlutusRational
      , collateralization' :: [((CurrencySymbol,TokenName),PlutusRational)]
      , claimPeriod' :: POSIXTime
      , loanId' :: TokenName
      , loanStart' :: POSIXTime
      }
  | CollateralPaymentDatum'
      { beaconSym' :: CurrencySymbol
      , borrowerId' :: TokenName
      , lenderAddress' :: Address
      , loanAsset' :: (CurrencySymbol,TokenName)
      , loanPrinciple' :: Integer
      , nextCheckpoints' :: [POSIXTime]
      , pastCheckpoints' :: [POSIXTime]
      , loanTerm' :: POSIXTime
      , loanInterest' :: PlutusRational
      , collateralization' :: [((CurrencySymbol,TokenName),PlutusRational)]
      , claimExpiration' :: POSIXTime
      , loanExpiration' :: POSIXTime
      , currentBalance' :: PlutusRational
      , paymentAmount' :: Integer
      , loanId' :: TokenName
      }
  | RolloverDatum'
      { beaconSym' :: CurrencySymbol
      , borrowerId' :: TokenName
      , lenderAddress' :: Address
      , loanAsset' :: (CurrencySymbol,TokenName)
      , loanPrinciple' :: Integer
      , nextCheckpoints' :: [POSIXTime]
      , pastCheckpoints' :: [POSIXTime]
      , loanTerm' :: POSIXTime
      , loanInterest' :: PlutusRational
      , collateralization' :: [((CurrencySymbol,TokenName),PlutusRational)]
      , claimExpiration' :: POSIXTime
      , loanExpiration' :: POSIXTime
      , currentBalance' :: PlutusRational
      , loanId' :: TokenName
      }

convertToLoanDatum :: ActiveDatum' -> LoanDatum
convertToLoanDatum AcceptDatum'{..} = 
  ActiveDatum 
    { beaconSym = beaconSym'
    , borrowerId = borrowerId'
    , lenderAddress = lenderAddress'
    , loanAsset = loanAsset'
    , loanPrinciple = loanPrinciple'
    , nextCheckpoints = map (+loanStart') loanCheckpoints'
    , pastCheckpoints = []
    , loanTerm = loanTerm'
    , loanInterest = loanInterest'
    , collateralization = collateralization'
    , claimExpiration = loanStart' + loanTerm' + claimPeriod'
    , loanExpiration = loanStart' + loanTerm'
    , loanOutstanding = CL.fromInteger loanPrinciple'
    , loanId = loanId'
    }
convertToLoanDatum CollateralPaymentDatum'{..} =
  ActiveDatum
    { beaconSym = beaconSym'
    , borrowerId = borrowerId'
    , lenderAddress = lenderAddress'
    , loanAsset = loanAsset'
    , loanPrinciple = loanPrinciple'
    , nextCheckpoints = nextCheckpoints'
    , pastCheckpoints = pastCheckpoints'
    , loanTerm = loanTerm'
    , loanInterest = loanInterest'
    , collateralization = collateralization'
    , claimExpiration = claimExpiration'
    , loanExpiration = loanExpiration'
    , loanOutstanding = currentBalance' CL.- CL.fromInteger paymentAmount'
    , loanId = loanId'
    }
convertToLoanDatum RolloverDatum'{..} =
  ActiveDatum
    { beaconSym = beaconSym'
    , borrowerId = borrowerId'
    , lenderAddress = lenderAddress'
    , loanAsset = loanAsset'
    , loanPrinciple = loanPrinciple'
    , nextCheckpoints = tail nextCheckpoints'
    , pastCheckpoints = head nextCheckpoints' : pastCheckpoints'
    , loanTerm = loanTerm'
    , loanInterest = loanInterest'
    , collateralization = collateralization'
    , claimExpiration = claimExpiration'
    , loanExpiration = loanExpiration'
    , loanOutstanding = currentBalance' CL.* (CL.fromInteger 1 CL.+ loanInterest')
    , loanId = loanId'
    }

data AddressInfo' = AddressInfo'
  { addressSpendingKeyHash :: Maybe Text
  , addressSpendingScriptHash :: Maybe Text
  , addressStakeKeyHash :: Maybe Text
  , addressStakeScriptHash :: Maybe Text
  , addressNetworkTag :: Word32
  }

instance ToJSON AddressInfo' where
  toJSON AddressInfo'{..} =
    object [ "payment_pubkey_hash" .= addressSpendingKeyHash 
           , "payment_script_hash" .= addressSpendingScriptHash
           , "staking_pubkey_hash" .= addressStakeKeyHash
           , "staking_script_hash" .= addressStakeScriptHash
           , "network_tag" .= addressNetworkTag
           ]

data Asset = Asset
  { assetPolicyId :: String
  , assetTokenName :: String
  , assetQuantity :: Integer
  } deriving (Show)

instance ToJSON Asset where
  toJSON Asset{..} =
    object [ "asset" .= if assetPolicyId == "lovelace" 
                        then "lovelace" 
                        else assetPolicyId <> "." <> assetTokenName
           , "quantity" .= assetQuantity
           ]

data LoanPhase = Ask | Offer | Loan deriving (Show)

data LoanUTxO = LoanUTxO
  { loanPhase :: LoanPhase
  , address :: String
  , txHash :: String
  , outputIndex :: String
  , uTxOValue :: [Asset]
  , loanInfo :: LoanDatum
  }

instance ToJSON LoanUTxO where
  toJSON LoanUTxO{..} =
    object [ "phase" .= show loanPhase
           , "address" .= address
           , "tx_hash" .= txHash
           , "output_index" .= outputIndex
           , "utxo_assets" .= uTxOValue
           , "loan_info" .= loanInfo
           ]

instance ToJSON LoanDatum where
  toJSON AskDatum{..} =
    object [ "loan_beacon" .= show beaconSym
           , "borrower_id" .= idToString borrowerId
           , "loan_asset" .= toAsset loanAsset
           , "principle" .= loanPrinciple
           , "term" .= getPOSIXTime loanTerm `divide` 1000
           , "collateral" .= map toAsset collateral
           ]
  toJSON OfferDatum{..} =
    object [ "loan_beacon" .= show beaconSym
           , "lender_id" .= idToString lenderId
           , "lender_address_payment_pubkey_hash" .= (show <$> toPubKeyHash lenderAddress)
           , "lender_address_payment_script_hash" .= (show <$> toValidatorHash lenderAddress)
           , "lender_address_staking_pubkey_hash" .= (show <$> toStakePubKeyHash lenderAddress)
           , "lender_address_staking_script_hash" .= (show <$> toStakeValidatorHash lenderAddress)
           , "loan_asset" .= toAsset loanAsset
           , "principle" .= loanPrinciple
           , "checkpoints" .= map getPOSIXTime loanCheckpoints
           , "term" .= getPOSIXTime loanTerm `divide` 1000
           , "interest" .= loanInterest
           , "collateralization" .= map (\(x,y) -> (toAsset x, y)) collateralization
           , "claim_period" .= getPOSIXTime claimPeriod
           ]
  toJSON ActiveDatum{..} =
    object [ "loan_beacon" .= show beaconSym
           , "borrower_id" .= idToString borrowerId
           , "lender_address_payment_pubkey_hash" .= (show <$> toPubKeyHash lenderAddress)
           , "lender_address_payment_script_hash" .= (show <$> toValidatorHash lenderAddress)
           , "lender_address_staking_pubkey_hash" .= (show <$> toStakePubKeyHash lenderAddress)
           , "lender_address_staking_script_hash" .= (show <$> toStakeValidatorHash lenderAddress)
           , "loan_asset" .= toAsset loanAsset
           , "principle" .= loanPrinciple
           , "next_checkpoints" .= map (getSlot . posixTimeToSlot) nextCheckpoints
           , "past_checkpoints" .= map (getSlot . posixTimeToSlot) pastCheckpoints
           , "term" .= getPOSIXTime loanTerm `divide` 1000
           , "interest" .= loanInterest
           , "collateralization" .= map (\(x,y) -> (toAsset x, y)) collateralization
           , "expiration_slot" .= getSlot (posixTimeToSlot loanExpiration)
           , "claim_period_expiration_slot" .= getSlot (posixTimeToSlot claimExpiration)
           , "balance_owed" .= loanOutstanding
           , "loan_id" .= idToString loanId
           ]

data KeyUTxO = KeyUTxO
  { keyTxHash :: String
  , keyOutputIndex :: String
  , keyUTxOValue :: [Asset]
  }

instance ToJSON KeyUTxO where
  toJSON KeyUTxO{..} =
    object [ "tx_hash" .= keyTxHash
           , "output_index" .= keyOutputIndex
           , "utxo_assets" .= keyUTxOValue
           ]

data LoanHistory = LoanHistory
  { defaultStatus :: Bool
  , remainingUTxOValue :: [Asset]
  , loan :: LoanDatum
  } deriving (Show)

instance ToJSON LoanHistory where
  toJSON LoanHistory{..} =
    object [ "default" .= defaultStatus
           , "utxo_assets" .= remainingUTxOValue
           , "loan_info" .= toJSON loan
           ]