use aiken/time.{PosixTime}
use aiken/transaction/credential.{Address}
use aiken/transaction/value.{PolicyId,AssetName}
use cardano_loans/ratio.{Rational}

type LoanDatum {
  AskDatum { loan_beacon_sym: PolicyId
           , borrower_id: AssetName
           , loan_asset: (PolicyId,AssetName)
           , loan_principle: Int
           , loan_term: PosixTime
           , collateral: List<(PolicyId,AssetName)>
           }
  OfferDatum { loan_beacon_sym: PolicyId
             , lender_id: AssetName
             , lender_address: Address
             , loan_asset: (PolicyId,AssetName)
             , loan_principle: Int
             , loan_term: PosixTime
             , loan_interest: Rational
             , collateralization: List<((PolicyId,AssetName),Rational)>
             }
}