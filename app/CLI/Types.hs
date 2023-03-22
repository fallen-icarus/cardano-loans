{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module CLI.Types where

import Data.Aeson

import CardanoLoans

-- | This variable shortcuts the need to actually compile the beacon policy for off-chain use.
-- This should be changed whenever the on-chain code is changed.
beaconSym :: CurrencySymbol
beaconSym = (\(Right sym) -> sym) 
          $ readCurrencySymbol "53267afd1e0b2db650c3c08f034ef95e686b8ff74f039caf89274c64"

-- | For when saving to file is optional
data Output = Stdout | File FilePath

data Command
  = ExportScript Script FilePath
  | BorrowerCmd BorrowerCmd
  | LenderCmd LenderCmd

data Script = Policy | Spending

-- | The active datum for accepting the loans does not need all the information from the user.
-- It can calculate some of the info on its own. This type is used with the relevant parser to
-- make this easy.
data ActiveDatum' = ActiveDatum'
  { activeBeacon' :: (CurrencySymbol,TokenName)
  , lenderId' :: (CurrencySymbol,TokenName)
  , borrowerId' :: (CurrencySymbol,TokenName)
  , loanAsset' :: (CurrencySymbol,TokenName)
  , loanPrinciple' :: Integer
  , loanTerm' :: POSIXTime
  , loanInterest' :: PlutusRational
  , loanBacking' :: Integer
  , collateralRates' :: [((CurrencySymbol,TokenName),PlutusRational)]
  }

data BorrowerCmd
  = BorrowerAskDatum LoanDatum FilePath
  | BorrowerPaymentActiveDatum LoanDatum FilePath
  | BorrowerAcceptActiveDatum LoanDatum FilePath
  | CreateAskBeaconRedeemer PaymentPubKeyHash FilePath
  | CreateActiveBeaconRedeemer PaymentPubKeyHash PaymentPubKeyHash FilePath
  | CreateCloseAskRedeemer FilePath
  | CreateBorrowerBurnBeaconRedeemer FilePath
  | CreateAcceptOfferRedeemer FilePath
  | CreateRepayRedeemer FilePath
  | QueryBorrowerCurrentAsks String Network Output
  | QueryBorrowerCurrentOffers String Network Output
  | QueryBorrowerCurrentLoans PaymentPubKeyHash Network Output
  | ConvertPOSIXToSlot POSIXTime

data LenderCmd
  = CreateOfferDatum LoanDatum FilePath
  | CreateClaimRedeemer FilePath
  | CreateOfferBeaconRedeemer PaymentPubKeyHash FilePath
  | CreateBurnBeaconRedeemer FilePath
  | CreateCloseOfferRedeemer FilePath
  | QueryAllAsks Network Output
  | QueryBorrowerHistory PaymentPubKeyHash Network Output
  | QueryLenderCurrentOffers PaymentPubKeyHash Network Output
  | QueryLenderCurrentLoans PaymentPubKeyHash Network Output

data Network 
  = PreProdTestnet String  -- ^ Api key

-- | The assets at the available utxo.
data AvailableAsset = AvailableAsset
  { assetPolicyId :: String
  , assetTokenName :: String
  , assetQuantity :: Integer
  } deriving (Show)

instance ToJSON AvailableAsset where
  toJSON AvailableAsset{..} =
    object [ "asset" .= if assetPolicyId == "lovelace" 
                        then "lovelace" 
                        else assetPolicyId <> "." <> assetTokenName
           , "quantity" .= assetQuantity
           ]

data AvailableAsk = AvailableAsk
  { availableAskAddress :: String
  , availableAskTxIx :: String
  , availableAskAssets :: [AvailableAsset]
  , availableAskBorrowerID :: String
  , availableAskLoanAsset :: String
  , availableAskLoanPrinciple :: Integer
  , availableAskLoanTerm :: Integer  -- ^ Converted to number of slots (1 slot == 1000 POSIXTime)
  , availableAskCollateral :: [String]
  } deriving (Show)

instance ToJSON AvailableAsk where
  toJSON AvailableAsk{..} =
    object [ "ask_address" .= availableAskAddress
           , "ask_tx_ix" .= availableAskTxIx
           , "utxo_assets" .= availableAskAssets
           , "borrower_id" .= availableAskBorrowerID
           , "loan_asset" .= availableAskLoanAsset
           , "loan_principle" .= availableAskLoanPrinciple
           , "length_of_loan_in_slots" .= availableAskLoanTerm
           , "assets_available_for_collateral" .= availableAskCollateral
           ]

newtype AvailableCollateralRate = AvailableCollateralRate (String,(Integer,Integer))
  deriving (Show)

instance ToJSON AvailableCollateralRate where
  toJSON (AvailableCollateralRate (asset,(num,den))) =
    object [ "collateral_asset_name" .= asset
           , "collateral_rate_numerator" .= num
           , "collateral_rate_denominator" .= den
           ]

data AvailableOffer = AvailableOffer
  { availableOfferAddress :: String
  , availableOfferTxIx :: String
  , availableOfferAssets :: [AvailableAsset]
  , availableOfferLenderID :: String
  , availableOfferLoanAsset :: String
  , availableOfferLoanPrinciple :: Integer
  , availableOfferLoanTerm :: Integer -- ^ Converted to number of slots (1 slot == 1000 POSIXTime)
  , availableOfferLoanInterestNumerator :: Integer
  , availableOfferLoanInterestDenominator :: Integer
  , availableOfferLoanBacking :: Integer
  , availableOfferCollateralRates :: [AvailableCollateralRate]
  } deriving (Show)

instance ToJSON AvailableOffer where
  toJSON AvailableOffer{..} =
    object [ "offer_address" .= availableOfferAddress
           , "offer_tx_ix" .= availableOfferTxIx
           , "utxo_assets" .= availableOfferAssets
           , "lender_id" .= availableOfferLenderID
           , "loan_asset" .= availableOfferLoanAsset
           , "loan_principle" .= availableOfferLoanPrinciple
           , "length_of_loan_in_slots" .= availableOfferLoanTerm
           , "interest_numerator" .= availableOfferLoanInterestNumerator
           , "interest_denominator" .= availableOfferLoanInterestDenominator
           , "required_backing" .= availableOfferLoanBacking
           , "collateral_rates" .= availableOfferCollateralRates
           ]

data AvailableActive = AvailableActive
  { availableActiveAddress :: String
  , availableActiveTxIx :: String
  , availableActiveAssets :: [AvailableAsset]
  , availableActiveLenderID :: String
  , availableActiveBorrowerID :: String
  , availableActiveLoanAsset :: String
  , availableActiveLoanPrinciple :: Integer
  , availableActiveLoanTerm :: Integer -- ^ Converted to number of slots (1 slot == 1000 POSIXTime)
  , availableActiveLoanInterestNumerator :: Integer
  , availableActiveLoanInterestDenominator :: Integer
  , availableActiveLoanBacking :: Integer
  , availableActiveCollateralRates :: [AvailableCollateralRate]
  , availableActiveLoanExpiration :: Integer
  , availableActiveLoanOutstandingNumerator :: Integer
  , availableActiveLoanOutstandingDenominator :: Integer
  } deriving (Show)

instance ToJSON AvailableActive where
  toJSON AvailableActive{..} =
    object [ "activeAddress" .= availableActiveAddress
           , "active_tx_ix" .= availableActiveTxIx
           , "utxo_assets" .= availableActiveAssets
           , "lender_id" .= availableActiveLenderID
           , "borrower_id" .= availableActiveBorrowerID
           , "loan_asset" .= availableActiveLoanAsset
           , "loan_principle" .= availableActiveLoanPrinciple
           , "length_of_loan_in_slots" .= availableActiveLoanTerm
           , "interest_numerator" .= availableActiveLoanInterestNumerator
           , "interest_denominator" .= availableActiveLoanInterestDenominator
           , "required_backing" .= availableActiveLoanBacking
           , "collateral_rates" .= availableActiveCollateralRates
           , "loan_expiration" .= availableActiveLoanExpiration
           , "loan_outstanding_numerator" .= availableActiveLoanOutstandingNumerator
           , "loan_outstanding_denominator" .= availableActiveLoanOutstandingDenominator
           ]

data LoanHistory = LoanHistory
  { defaultStatus :: Bool
  , loanInfo :: AvailableActive
  } deriving (Show)

instance ToJSON LoanHistory where
  toJSON LoanHistory{..} =
    object [ "default" .= defaultStatus
           , "loan_info" .= toJSON loanInfo
           ]