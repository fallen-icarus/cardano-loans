use aiken/time.{PosixTime}
use aiken/transaction/credential.{Address,Credential}
use aiken/transaction/value.{PolicyId,AssetName}
use aiken/transaction.{OutputReference}
use aiken/math/rational.{Rational}

pub type LoanDatum {
  AskDatum { 
    loan_beacon_sym: PolicyId,
    borrower_id: AssetName,
    loan_asset: (PolicyId,AssetName),
    loan_principle: Int,
    loan_term: PosixTime, // How long the loan is active once accepted.
    collateral: List<(PolicyId,AssetName)>
  }
  OfferDatum { 
    loan_beacon_sym: PolicyId,
    lender_id: AssetName,
    lender_address: Address,
    loan_asset: (PolicyId,AssetName),
    loan_principle: Int,
    rollover_frequency: Option<PosixTime>, // The frequency at which rollovers must occur.
    loan_term: PosixTime,
    loan_interest: Rational,
    min_payment: Int, // The minimum loan payment that can be made.
    collateralization: List<((PolicyId,AssetName),Rational)>,
    collateral_is_swappable: Bool, // Whether collateral can be swapped out.
    claim_period: PosixTime, // How long the lender will have to claim the expired UTxO once defaulted.
    offer_deposit: Int // How much ADA was used for the UTxO's minUTxOValue.
  }
  ActiveDatum { 
    loan_beacon_sym: PolicyId,
    borrower_id: AssetName,
    lender_address: Address,
    loan_asset: (PolicyId,AssetName),
    loan_principle: Int,
    rollover_frequency: Option<PosixTime>,
    last_checkpoint: PosixTime,
    loan_term: PosixTime, // Included for credit history
    loan_interest: Rational,
    min_payment: Int,
    collateralization: List<((PolicyId,AssetName),Rational)>,
    collateral_is_swappable: Bool,
    claim_expiration: PosixTime, // The time at which collateral is considered "Lost".
    loan_expiration: PosixTime,
    loan_outstanding: Rational,
    loan_id: AssetName
  }
}

pub type LoanRedeemer {
  CloseAsk
  CloseOffer
  AcceptOffer
  MakePayment
  Rollover
  ClaimExpired
  UpdateLenderAddress{new_address: Address}
  Unlock
}

pub type BeaconRedeemer {
  CreateAsk { 
    staking_credential: Credential, // Borrower's staking credential. Pubkey or script.
    loan_assets: List<(PolicyId,AssetName)> // Assets to be loaned.
  } 
  CreateOffer { 
    staking_credential: Credential, // Lender's staking credential. Pubkey or script.
    loan_assets: List<(PolicyId,AssetName)>
  }
  CreateActive { 
    staking_credential: Credential, // Borrower's staking credential.
    pairings: List<(OutputReference,OutputReference)> // The desired pairings of Ask and Offer UTxOs.
  }
  BurnBeacons
}