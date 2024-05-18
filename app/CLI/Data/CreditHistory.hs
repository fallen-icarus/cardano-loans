{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE StrictData #-}

module CLI.Data.CreditHistory where

import Relude
import Data.Aeson
import Prettyprinter
import Prettyprinter.Render.Terminal

import CLI.Data.Bech32Address
import CLI.Data.Asset
import CLI.Data.Network

import CardanoLoans

data CreditHistory = CreditHistory
  { _default :: Bool
  , _remainingLovelace :: Lovelace
  , _remainingNativeAssets :: [NativeAsset]
  , _loanTerms :: ActiveDatum
  } deriving (Show)

instance ToJSON CreditHistory where
  toJSON CreditHistory{..} =
    object [ "default" .= _default
           , "remaining_lovelace" .= unLovelace _remainingLovelace
           , "remaining_native_assets" .= _remainingNativeAssets
           , "loan_info" .= _loanTerms
           ]

prettyCreditHistory :: Network -> CreditHistory -> Doc AnsiStyle
prettyCreditHistory network CreditHistory{_loanTerms=_loanTerms@ActiveDatum{..},..} = 
  vsep [ annotate (colorDull Yellow) "loan_id:" <+> pretty _loanId
       , indent 4 $ annotate (colorDull Green) "default:" <+> pretty _default
       , indent 4 $ annotate (colorDull Green) "remaining_assets:"
       , indent 6 $ pretty _remainingLovelace <+> "lovelace"
       , indent 6 $ align $ vsep $ map pretty _remainingNativeAssets
       , indent 4 $ prettyActiveDatum _loanTerms
       , mempty
       ]
  where
    config = case network of
      Mainnet -> mainnetConfig
      PreProdTestnet -> preprodConfig

    prettyActiveDatum :: ActiveDatum -> Doc AnsiStyle
    prettyActiveDatum ActiveDatum{..} =
      vsep [ annotate (colorDull Cyan) "borrower_id:" <+> pretty _borrowerId
           , annotate (colorDull Cyan) "loan_id:" <+> pretty _loanId
           , annotate (colorDull Cyan) "lender_address:" <+> 
               pretty (either (const "failed to convert to bech32") fst $ plutusToBech32 network _lenderAddress)
           , annotate (colorDull Cyan) "loan_amount:" <+> pretty _loanPrincipal <+> pretty _loanAsset
           , annotate (colorDull Cyan) "interest:" <+> pretty _loanInterest
           , let time = getPOSIXTime _loanTerm in
               annotate (colorDull Cyan) "loan_term:" 
                 <+> pretty (time `div` 1000) <+> "slots"
                 <+> tupled [pretty time <+> "milliseconds"]
           , annotate (colorDull Cyan) "compound_frequency:" <+> (flip (maybe "none") _compoundFrequency $ 
               \(POSIXTime time) ->
                 pretty (time `div` 1000) <+> "slots" <+> tupled [pretty time <+> "milliseconds"])
           , let time = getPOSIXTime _lastCompounding in
               annotate (colorDull Cyan) "last_compounding:" 
                 <+> pretty (posixTimeToSlot config _lastCompounding)
                 <+> tupled [pretty time <+> "POSIXTime"]
           , annotate (colorDull Cyan) "minimum_payment:" <+> pretty _minPayment <+> pretty _loanAsset
           , annotate (colorDull Cyan) "penalty:" <+> case _penalty of
               NoPenalty -> "none"
               FixedFee fee -> pretty fee <+> tupled ["fixed-fee"] 
               PercentFee fee -> pretty fee <+> tupled ["precent-fee"] 
           , annotate (colorDull Cyan) "total_payments_this_epoch:" <+> pretty _totalEpochPayments
           , annotate (colorDull Cyan) "swappable_collateral:" <+> pretty _collateralIsSwappable
           , let time = getPOSIXTime _claimExpiration in
               annotate (colorDull Cyan) "claim_expiration:" 
                 <+> pretty (posixTimeToSlot config _claimExpiration)
                 <+> tupled [pretty time <+> "POSIXTime"]
           , let time = getPOSIXTime _loanExpiration in
               annotate (colorDull Cyan) "loan_expiration:" 
                 <+> pretty (posixTimeToSlot config _loanExpiration)
                 <+> tupled [pretty time <+> "POSIXTime"]
           , annotate (colorDull Cyan) "outstanding_balance:" <+> pretty _loanOutstanding
           , annotate (colorDull Cyan) "collateralization:"
           , indent 2 $ align $ vsep $ flip map (_unCollateralization _collateralization) $ 
               \(col,rate) -> pretty col <+> "@" <+> pretty rate
           ]
