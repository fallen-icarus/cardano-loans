{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE StrictData #-}

module CLI.Data.LoanHistory where

import Relude
import Data.Aeson
import Prettyprinter
import Prettyprinter.Render.Terminal

import CLI.Data.Bech32Address
import CLI.Data.Network

import CardanoLoans

data LoanHistory = LoanHistory
  { _loanStatus :: ActiveDatum
  , _action :: Text
  , _timeStamp :: POSIXTime
  } deriving (Show)

instance ToJSON LoanHistory where
  toJSON LoanHistory{..} =
    object [ "status" .= _loanStatus
           , "action" .= _action
           , "time_of_action" .= getPOSIXTime _timeStamp
           ]

prettyLoanHistory :: Network -> LoanHistory -> Doc AnsiStyle
prettyLoanHistory network LoanHistory{..} = 
  vsep [ annotate (colorDull Yellow) "action:" <+> pretty _action
       , indent 4 $ annotate (colorDull Green) "time_of_action:" <+> 
           pretty (getPOSIXTime _timeStamp) <+> "POSIXTime"
       , indent 4 $ prettyActiveDatum _loanStatus
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
           , annotate (colorDull Cyan) "epoch_duration:" <+> (flip (maybe "none") _epochDuration $ 
               \(POSIXTime time) ->
                 pretty (time `div` 1000) <+> "slots" <+> tupled [pretty time <+> "milliseconds"])
           , let time = getPOSIXTime _lastEpochBoundary in
               annotate (colorDull Cyan) "last_epoch_boundary:" 
                 <+> pretty (posixTimeToSlot config _lastEpochBoundary)
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
