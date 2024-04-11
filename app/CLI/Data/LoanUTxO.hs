{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE StrictData #-}

module CLI.Data.LoanUTxO where

import Relude
import Data.Aeson
import Prettyprinter
import Prettyprinter.Render.Terminal

import CLI.Data.Bech32Address
import CLI.Data.Asset
import CLI.Data.Network

import CardanoLoans

data LoanDatum
  = Ask AskDatum
  | Offer OfferDatum
  | Active ActiveDatum
  deriving (Show)

instance ToJSON LoanDatum where
  toJSON (Ask datum) =
    object [ "type" .= ("ask" :: Text)
           , "datum" .= datum
           ]
  toJSON (Offer datum) =
    object [ "type" .= ("offer" :: Text)
           , "datum" .= datum
           ]
  toJSON (Active datum) =
    object [ "type" .= ("active" :: Text)
           , "datum" .= datum
           ]

data LoanUTxO = LoanUTxO
  { _loanAddress :: PaymentAddress
  , _stakeCredential :: Credential
  , _utxoRef :: TxOutRef
  , _lovelaces :: Lovelace
  , _nativeAssets :: [NativeAsset]
  , _loanDatum :: Maybe LoanDatum
  } deriving (Show)

instance FromJSON LoanUTxO where
  parseJSON =
      withObject "LoanUTxO" $ \o ->
        LoanUTxO
          <$> o .: "address"
          <*> (o .: "stake_address" >>= 
                maybe mzero return . rightToMaybe . stakeAddressToPlutusCredential . StakeAddress)
          <*> ( (concatRef <$> o .: "tx_hash" <*> o .: "tx_index") >>= 
                  maybe mzero return . rightToMaybe . readTxOutRef)
          <*> o .: "value"
          <*> o .: "asset_list"
          <*> (o .: "inline_datum" >>= withObject "inlineDatum" (.: "value") >>= return . parseDatum)
    where
      concatRef :: String -> Integer -> String
      concatRef hash idx = hash <> "#" <> show idx

      parseDatum :: Value -> Maybe LoanDatum
      parseDatum v =
        (Ask <$> decodeDatum @AskDatum v)
        <|> (Offer <$> decodeDatum @OfferDatum v)
        <|> (Active <$> decodeDatum @ActiveDatum v)

instance ToJSON LoanUTxO where
  toJSON LoanUTxO{..} =
    object [ "loan_address" .= _loanAddress
           , "borrower_id" .= show @String _stakeCredential
           , "utxo_id" .= (\(TxOutRef hash idx) -> show @Text hash <> "#" <> show idx) _utxoRef
           , "native_assets" .= _nativeAssets
           , "lovelace" .= unLovelace _lovelaces
           , "loan_info" .= _loanDatum
           ]

prettyLoanUTxO :: Network -> LoanUTxO -> Doc AnsiStyle
prettyLoanUTxO network LoanUTxO{_utxoRef=(TxOutRef hash idx),..} = 
  vsep [ annotate (colorDull Yellow) "utxo_ref:" <+> show hash <> "#" <> show idx
       , indent 4 $ annotate (colorDull Green) "borrower_address:" <+> pretty _loanAddress 
       , indent 4 $ annotate (colorDull Green) "assets:"
       , indent 6 $ pretty _lovelaces <+> "lovelace"
       , indent 6 $ align $ vsep $ map pretty _nativeAssets
       , indent 4 $ 
           maybe (annotate (colorDull Cyan) "datum:" <+> "none") prettyLoanDatum _loanDatum
       , mempty
       ]
  where
    config = case network of
      Mainnet -> mainnetConfig
      PreProdTestnet -> preprodConfig

    prettyCredential :: Credential -> Doc a
    prettyCredential (PubKeyCredential pk) = pretty pk <+> "(pubkey)"
    prettyCredential (ScriptCredential sh) = pretty sh <+> "(script)"

    prettyLoanDatum :: LoanDatum -> Doc AnsiStyle
    prettyLoanDatum (Ask AskDatum{..}) =
      vsep [ annotate (colorDull Cyan) "type:" <+> pretty @Text "Ask"
           , annotate (colorDull Cyan) "borrower_id:" <+> prettyCredential _stakeCredential
           , annotate (colorDull Cyan) "loan_amount:" <+> pretty _loanPrinciple <+> pretty _loanAsset
           , let time = getPOSIXTime _loanTerm in
             annotate (colorDull Cyan) "loan_term:" 
               <+> pretty (time `div` 1000) <+> "slots"
               <+> tupled [pretty time <+> "milliseconds"]
           , annotate (colorDull Cyan) "collateral:"
           , indent 2 $ align $ vsep $ map pretty $ _unCollateral _collateral
           ]
    prettyLoanDatum (Offer OfferDatum{..}) =
      vsep [ annotate (colorDull Cyan) "type:" <+> pretty @Text "Offer"
           , annotate (colorDull Cyan) "borrower_id:" <+> prettyCredential _stakeCredential
           , annotate (colorDull Cyan) "lender_id:" <+> pretty _lenderId
           , annotate (colorDull Cyan) "lender_address:" <+> 
               pretty (either (const "failed to convert to bech32") fst $ plutusToBech32 network _lenderAddress)
           , annotate (colorDull Cyan) "loan_amount:" <+> pretty _loanPrinciple <+> pretty _loanAsset
           , annotate (colorDull Cyan) "interest:" <+> pretty _loanInterest
           , let time = getPOSIXTime _loanTerm in
               annotate (colorDull Cyan) "loan_term:" 
                 <+> pretty (time `div` 1000) <+> "slots"
                 <+> tupled [pretty time <+> "milliseconds"]
           , annotate (colorDull Cyan) "compound_frequency:" <+> (flip (maybe "none") _compoundFrequency $ 
               \(POSIXTime time) ->
                 pretty (time `div` 1000) <+> "slots" <+> tupled [pretty time <+> "milliseconds"])
           , annotate (colorDull Cyan) "minimum_payment:" <+> pretty _minPayment <+> pretty _loanAsset
           , annotate (colorDull Cyan) "penalty:" <+> case _penalty of
               NoPenalty -> "none"
               FixedFee fee -> pretty fee <+> tupled ["fixed-fee"] 
               PercentFee fee -> pretty fee <+> tupled ["precent-fee"] 
           , annotate (colorDull Cyan) "swappable_collateral:" <+> pretty _collateralIsSwappable
           , let time = getPOSIXTime _claimPeriod in
               annotate (colorDull Cyan) "claim_period:" 
                 <+> pretty (time `div` 1000) <+> "slots"
                 <+> tupled [pretty time <+> "milliseconds"]
           , annotate (colorDull Cyan) "offer_expiration:" <+> (flip (maybe "none") _offerExpiration $ 
               \(POSIXTime time) ->
                 pretty (time `div` 1000) <+> "slots" <+> tupled [pretty time <+> "milliseconds"])
           , annotate (colorDull Cyan) "offer_deposit:" <+> pretty _offerDeposit <+> "lovelace"
           , annotate (colorDull Cyan) "collateralization:"
           , indent 2 $ align $ vsep $ flip map (_unCollateralization _collateralization) $ 
               \(col,rate) -> pretty col <+> "@" <+> pretty rate
           ]
    prettyLoanDatum (Active ActiveDatum{..}) =
      vsep [ annotate (colorDull Cyan) "type:" <+> pretty @Text "Active"
           , annotate (colorDull Cyan) "borrower_id:" <+> prettyCredential _stakeCredential
           , annotate (colorDull Cyan) "loan_id:" <+> pretty _loanId
           , annotate (colorDull Cyan) "lender_address:" <+> 
               pretty (either (const "failed to convert to bech32") fst $ plutusToBech32 network _lenderAddress)
           , annotate (colorDull Cyan) "loan_amount:" <+> pretty _loanPrinciple <+> pretty _loanAsset
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
                 <+> tupled [pretty time <+> "posix"]
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
                 <+> tupled [pretty time <+> "posix"]
           , let time = getPOSIXTime _loanExpiration in
               annotate (colorDull Cyan) "loan_expiration:" 
                 <+> pretty (posixTimeToSlot config _loanExpiration)
                 <+> tupled [pretty time <+> "posix"]
           , annotate (colorDull Cyan) "outstanding_balance:" <+> pretty _loanOutstanding
           , annotate (colorDull Cyan) "collateralization:"
           , indent 2 $ align $ vsep $ flip map (_unCollateralization _collateralization) $ 
               \(col,rate) -> pretty col <+> "@" <+> pretty rate
           ]
