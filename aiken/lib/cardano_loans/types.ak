use aiken/hash.{Hash,Blake2b_224}
use aiken/time.{PosixTime}
use aiken/bytearray
use aiken/transaction/credential.{Address,Credential,Script}
use aiken/transaction/value.{PolicyId,AssetName}
use aiken/transaction.{OutputReference,ScriptContext}
use cardano_loans/ratio.{Rational}

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
    loan_checkpoints: List<PosixTime>, // The relative times at which rollovers must occur.
    loan_term: PosixTime,
    loan_interest: Rational,
    collateralization: List<((PolicyId,AssetName),Rational)>,
    claim_period: PosixTime // How long the lender will have to claim the expired UTxO once defaulted.
  }
  ActiveDatum { 
    loan_beacon_sym: PolicyId,
    borrower_id: AssetName,
    lender_address: Address,
    loan_asset: (PolicyId,AssetName),
    loan_principle: Int,
    next_checkpoints: List<PosixTime>,
    past_checkpoitns: List<PosixTime>, // Included for credit history.
    loan_term: PosixTime, // Included for credit history
    loan_interest: Rational,
    collateralization: List<((PolicyId,AssetName),Rational)>,
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
  UpdateLenderAddress
  UnlockLostCollateral
}

pub type BeaconRedeemer {
  MintAskBeacon { 
    staking_credential: Credential // Borrower's staking credential.
  } 
  MintOfferBeacon { 
    staking_credential: Credential // Lender's staking credential.
  }
  MintActiveBeacon { 
    staking_credential: Credential, // Borrower's staking credential.
    pairings: List<(OutputReference,OutputReference)> // The desired pairings of Ask and Offer UTxOs.
  }
  BurnBeacons
}