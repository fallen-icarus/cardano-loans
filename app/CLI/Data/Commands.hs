{-# LANGUAGE StrictData #-}

module CLI.Data.Commands where

import Relude

import CardanoLoans

import CLI.Data.ApiService
import CLI.Data.Network
import CLI.Data.Output
import CLI.Data.Bech32Address

data Command
  = ExportScript Script FilePath
  | CreateDatum NewDatum FilePath
  | CreateRedeemer NewRedeemer FilePath
  | BeaconName BeaconName Output
  | Time Time
  | Query Query
  | SubmitTx Network ApiService FilePath
  | EvaluateTx Network ApiService FilePath

data Time
  = ConvertTime ConvertTime Network
  | CalcNextBoundary POSIXTime POSIXTime POSIXTime

data Script 
  = NegotiationBeaconScript
  | ActiveBeaconScript
  | PaymentObserverScript
  | AddressUpdateObserverScript
  | LoanScript
  | ProxyScript

data NewDatum
  = NewAsk NewAskInfo
  | NewOffer NewOfferInfo
  | NewActiveManual NewActiveInfo
  | NewActiveAuto Network ApiService TxOutRef POSIXTime Credential
  | NewPayment LoanId
  | NewPostPaymentActiveManual NewPaymentInfo
  | NewPostPaymentActiveAuto Network ApiService TxOutRef Integer POSIXTime
  | NewPostAddressUpdateActiveManual NewAddressInfo
  | NewPostAddressUpdateActiveAuto Network ApiService TxOutRef Address

data NewRedeemer
  = NewNegotiation NegotiationBeaconsRedeemer
  | NewLoan LoanRedeemer
  | NewActive ActiveBeaconsRedeemer
  | NewPaymentObserver PaymentObserverRedeemer
  | NewAddressUpdateObserver AddressUpdateObserverRedeemer

data BeaconName
  = NegotiationPolicyId
  | ActivePolicyId
  | AssetBeaconName Asset
  | AskBeaconName
  | OfferBeaconName
  | LenderIdName Credential
  | ActiveBeaconName
  | LoanIdName TxOutRef

data ConvertTime
  = POSIXTimeToSlot POSIXTime
  | SlotToPOSIXTime Slot

data Query
  -- | Query a user's personal address. You can optionally filtering for only UTxOs with a Key NFT.
  = QueryPersonal Network ApiService PaymentAddress Bool Format Output
  -- | Query all open Ask UTxOs. You can optionally filter by loan asset and/or
  -- collateral used and/or by borrower.
  | QueryAsks Network ApiService (Maybe Asset) Collateral (Maybe Credential) Format Output
  -- | Query all open offers UTxOs. You can optionally filter by loan asset and/or
  -- a borrower id and/or a lender id.
  | QueryOffers Network ApiService (Maybe Asset) (Maybe Credential) (Maybe Credential) Format Output
  -- | Query all open Active UTxOs. You can optionally filter by loan asset and/or
  -- a borrower id and/or a loan id.
  | QueryActives Network ApiService (Maybe Asset) (Maybe Credential) (Maybe LoanId) Format Output
  -- | Query the latest slot number.
  | QueryCurrentSlot Network ApiService
  -- | Query the borrower's credit history.
  | QueryBorrowerCreditHistory Network ApiService BorrowerId Format Output
  -- | Query the loan's history.
  | QueryLoanHistory Network ApiService LoanId Format Output
  -- | Query the loan's outstanding balance.
  | QueryBalance Network ApiService TxOutRef
  -- | Query the current protocol parameters.
  | QueryParameters Network Output
