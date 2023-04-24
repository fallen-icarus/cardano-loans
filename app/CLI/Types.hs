{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CLI.Types where

import Data.Aeson

import CardanoLoans as CL

data Command
  = ExportScript Script FilePath
  | CreateLoanDatum LoanDatum FilePath
  | CreateLoanRedeemer LoanRedeemer FilePath
  | CreateBeaconRedeemer BeaconRedeemer FilePath
  | QueryBeacons Query

data Query
  = QueryAllAsks Network CurrencySymbol Output
  | QueryOwnAsks Network CurrencySymbol LoanAddress Output
  | QueryOwnOffers Network CurrencySymbol PaymentPubKeyHash Output
  | QueryAllOffers Network CurrencySymbol LoanAddress Output
  | QueryAllBorrowerLoans Network CurrencySymbol PaymentPubKeyHash LoanAddress Output
  | QueryAllLenderLoans Network CurrencySymbol PaymentPubKeyHash Output

data Script = BeaconPolicy | LoanScript

-- | For when saving to file is optional
data Output = Stdout | File FilePath

newtype LoanAddress = LoanAddress String

instance Show LoanAddress where
  show (LoanAddress s) = s

data Network 
  = PreProdTestnet String  -- ^ Api key

-- | Used as an intermediate type while loanOutstanding is calculated.
data ActiveDatum' 
  = AcceptDatum'
      { loanBeaconSym' :: CurrencySymbol
      , lenderId' :: TokenName
      , borrowerId' :: TokenName
      , loanAsset' :: (CurrencySymbol,TokenName)
      , loanPrinciple' :: Integer
      , loanTerm' :: POSIXTime
      , loanInterest' :: PlutusRational
      , collateralization' :: [((CurrencySymbol,TokenName),PlutusRational)]
      , loanExpiration' :: POSIXTime
      }
  | RepaymentDatum'
      { loanBeaconSym' :: CurrencySymbol
      , lenderId' :: TokenName
      , borrowerId' :: TokenName
      , loanAsset' :: (CurrencySymbol,TokenName)
      , loanPrinciple' :: Integer
      , loanTerm' :: POSIXTime
      , loanInterest' :: PlutusRational
      , collateralization' :: [((CurrencySymbol,TokenName),PlutusRational)]
      , loanExpiration' :: POSIXTime
      , currentOutstanding' :: PlutusRational
      , repayAmount' :: Integer
      }

convertToLoanDatum :: ActiveDatum' -> LoanDatum
convertToLoanDatum AcceptDatum'{..} = 
  ActiveDatum 
    { loanBeaconSym = loanBeaconSym'
    , lenderId = lenderId'
    , borrowerId = borrowerId'
    , loanAsset = loanAsset'
    , loanPrinciple = loanPrinciple'
    , loanTerm = loanTerm'
    , loanInterest = loanInterest'
    , collateralization = collateralization'
    , loanExpiration = loanExpiration'
    , loanOutstanding = CL.fromInteger loanPrinciple' CL.* (unsafeRatio 1 1 CL.+ loanInterest')
    }
convertToLoanDatum RepaymentDatum'{..} =
  ActiveDatum 
    { loanBeaconSym = loanBeaconSym'
    , lenderId = lenderId'
    , borrowerId = borrowerId'
    , loanAsset = loanAsset'
    , loanPrinciple = loanPrinciple'
    , loanTerm = loanTerm'
    , loanInterest = loanInterest'
    , collateralization = collateralization'
    , loanExpiration = loanExpiration'
    , loanOutstanding = currentOutstanding' CL.- CL.fromInteger repayAmount'
    }

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

data LoanType = Ask | Offer | Loan deriving (Show)

data LoanInfo = LoanInfo
  { loanType :: LoanType
  , address :: String
  , txHash :: String
  , outputIndex :: String
  , uTxOValue :: [Asset]
  , loanInfo :: LoanDatum
  }

instance ToJSON LoanInfo where
  toJSON LoanInfo{..} =
    object [ "type" .= show loanType
           , "address" .= address
           , "tx_hash" .= txHash
           , "output_index" .= outputIndex
           , "utxo_assets" .= uTxOValue
           , "loan_info" .= loanInfo
           ]