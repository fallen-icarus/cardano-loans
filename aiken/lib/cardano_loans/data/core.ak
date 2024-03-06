use aiken/bytearray as byte
use aiken/hash
use aiken/interval.{Interval,IntervalBound,Finite}
use aiken/time.{PosixTime}
use aiken/transaction/credential.{Credential,VerificationKeyCredential,ScriptCredential}
use aiken/transaction/value.{PolicyId,AssetName}
use aiken/transaction.{OutputReference,ValidityRange,TransactionId}
use aiken/string
use aiken/math

// Allows creating unique minting policies for testing purposes.
pub const app_name = @""

pub type Asset = (PolicyId,AssetName)

// Compare two assets. Equal is allowed.
pub fn compare_asset(asset1: Asset, asset2: Asset) -> Ordering {
  let (pol1,tok1) = asset1
  let (pol2,tok2) = asset2

  when byte.compare(pol1,pol2) is {
    Less -> Less
    Greater -> Greater
    Equal -> {
      when byte.compare(tok1,tok2) is {
        Less -> Less
        Greater -> Greater
        Equal -> Equal
      }
    }
  }
}

// Compare two assets. This is only used for sorting collateral lists so the assets should
// never be equal.
pub fn compare_collateral_asset(asset1: Asset, asset2: Asset) -> Ordering {
  let (pol1,tok1) = asset1
  let (pol2,tok2) = asset2

  when byte.compare(pol1,pol2) is {
    Less -> Less
    Greater -> Greater
    Equal -> {
      when byte.compare(tok1,tok2) is {
        Less -> Less
        Greater -> Greater
        Equal -> error @"Duplicate collateral found"
      }
    }
  }
}

// Compare two collateralizations. This is only used for sorting collateral lists so the assets 
// should never be equal.
pub fn compare_collateralization(
  collat1: (Asset,Rational), 
  collat2: (Asset,Rational)
) -> Ordering {
  let (asset1,_) = collat1
  let (asset2,_) = collat2

  compare_collateral_asset(asset1,asset2)
}

// A custom Rational type since the aiken version enforces invariants that are sometimes unecessary.
pub type Rational = (Int,Int)

pub fn apply_interest(balance: Rational, interest: Rational) -> Rational {
  let (bal_num,bal_den) = balance
  let (interest_num,interest_den) = interest

  let total_num = bal_num * (interest_den + interest_num) // Balance * (1 + interest)
  let total_den = bal_den * interest_den
  let new_gcd = math.gcd(total_num,total_den)

  (total_num / new_gcd, total_den / new_gcd)
}

pub fn subtract_payment(bal_num: Int, bal_den: Int, payment: Int) -> Rational {
  let new_num = bal_num - bal_den * payment

  (new_num, bal_den)
}

// pub fn subtract_payment(bal_num: Int, bal_den: Int, payment: Int) -> Rational {
//   let total_num = bal_num - bal_den * payment
//   let new_gcd = math.gcd(total_num,bal_den)
//
//   (total_num / new_gcd, bal_den / new_gcd)
// }

//-----------------------------------------------
// Negotiation PolicyId
//-----------------------------------------------
// A type representing the negotiation beacon script's policy id.
pub opaque type NegotiationID {
  inner: PolicyId
}

pub fn to_negotiation_id(policy_id: PolicyId) -> NegotiationID {
  NegotiationID(policy_id)
}

pub fn from_negotiation_id(policy_id: NegotiationID) -> PolicyId {
  policy_id.inner
}

//-----------------------------------------------
// Active PolicyId
//-----------------------------------------------
// A type representing the active beacon script's policy id.
pub opaque type ActiveID {
  inner: PolicyId
}

pub fn to_active_id(policy_id: PolicyId) -> ActiveID {
  ActiveID(policy_id)
}

pub fn from_active_id(policy_id: ActiveID) -> PolicyId {
  policy_id.inner
}

//-----------------------------------------------
// Borrower IDs
//-----------------------------------------------
// A type representing borrower ids. It is written as an opaque type to create a zero cost type
// that only exists at compile time.
pub opaque type BorrowerID {
  inner: AssetName
}

pub fn get_borrower_id_name(borrower_id: BorrowerID) -> AssetName {
  borrower_id.inner
}

// Convert a pubkey hash or script hash to a BorrowerID.
pub fn credential_to_borrower_id(borrower_cred: Credential) -> BorrowerID {
  when borrower_cred is {
    VerificationKeyCredential(pkh) -> {
        BorrowerID(pkh)
    }

    ScriptCredential(sh) -> {
        BorrowerID(sh)
    }
  }
}

//-----------------------------------------------
// Lender IDs
//-----------------------------------------------
// A type representing lender ids. It is written as an opaque type to create a zero cost type
// that only exists at compile time.
pub opaque type LenderID {
  inner: AssetName
}

// Get the inner AssetName from the LenderID.
pub fn get_lender_id_name(lender_id: LenderID) -> AssetName {
  lender_id.inner
}

// Convert a pubkey hash or script hash to a LenderID.
pub fn credential_to_lender_id(lender_cred: Credential) -> LenderID {
  when lender_cred is {
    VerificationKeyCredential(pkh) -> {
        LenderID(byte.concat(#"00",pkh))
    }

    ScriptCredential(sh) -> {
        LenderID(byte.concat(#"01",sh))
    }
  }
}

// Convert a LenderID to the respective credential.
pub fn lender_id_to_credential( lender_id: LenderID, ) -> Credential {
  // The first two characters of the lender_id signify what kind of credential the LenderID
  // is. "00" means the credential is a pubkey while "01" means the credential is a script.
  let full = lender_id.inner
  let id_1 = byte.take(full,1)
  let cred = byte.drop(full,1)
  if id_1 == #"00" {
    VerificationKeyCredential(cred)
  } else if id_1 == #"01" {
    ScriptCredential(cred)
  } else { error } // This branch should never be taken.
}

//-----------------------------------------------
// LoanIDs
//-----------------------------------------------
// A type representing loan ids. It is written as an opaque type to create a zero cost type
// that only exists at compile time.
pub opaque type LoanID {
  inner: AssetName
}

// Get the inner AssetName from the LoanID.
pub fn get_loan_id_name(loan_id: LoanID) -> AssetName {
  loan_id.inner
}

// Wrap an AssetName in the LoanID constructor. This is required when checking UTxOs contain the
// proper beacons.
pub fn asset_name_to_loan_id(loan_id: AssetName) -> LoanID {
  LoanID(loan_id)
}

// Create the LoanID from the Offer `OuptutReference` by hashing the combination of the tx hash
// and the output index. It uses the sha2_256 hash algorithm. Since the output reference input
// comes directly from the blockchain (as opposed to the user), outputs of this function
// should always be distinct from outputs of `generate_beacon_name` - no output references
// are prefixed with "Asset".
pub fn make_loan_id(offer_ref: OutputReference) -> LoanID {
  let OutputReference(TransactionId(tx_hash),out_index) = offer_ref
  hash.sha2_256(
    byte.concat(
      tx_hash,
      string.from_int(out_index) |> byte.from_string(_)
    )
  ) |> LoanID
}

//-----------------------------------------------
// AssetBeacon
//-----------------------------------------------
// A type representing asset beacons. It is written as an opaque type to create a zero cost type
// that only exists at compile time.
pub opaque type AssetBeacon {
  inner: AssetName
}

// Get the inner AssetName from the AssetBeacon.
pub fn get_asset_beacon_name(asset_beacon: AssetBeacon) -> AssetName {
  asset_beacon.inner
}

// Create the asset name for the Asset beacon by hashing the combination of the policy id and 
// asset name, prefixed with "Asset". 
pub fn make_asset_beacon(asset: Asset) -> AssetBeacon {
  let (policy_id,asset_name) = asset
  hash.sha2_256(
    byte.concat( policy_id, asset_name )
    |> byte.concat( byte.from_string(@"Asset"), _ )
  ) |> AssetBeacon
}

//-----------------------------------------------
// Time
//-----------------------------------------------
// A type respenting the start time for an action. Uses an opaque type to create a zero cost type.
pub opaque type StartTime {
  inner: PosixTime
}

// Get the time from the tx's invalid-before setting. The invalid-before setting is used for
// start times since it guarantees that the specified time has passed; the transaction would fail
// otherwise. It is the best approximation for the current time since users can always set it
// to the current time.
pub fn get_start_time(val_range: ValidityRange) -> StartTime {
  when val_range is {
    Interval(IntervalBound(Finite(x),_),_) -> StartTime(x)
    _ -> error @"invalid-before not specified"
  }
}

pub fn expiration_time_has_passed(
  lower_bound: StartTime, 
  expiration_time: ExpirationTime
) -> Bool {
  lower_bound.inner > expiration_time.inner
}

// A type respenting time at which an action can no longer be performed. Uses an opaque type 
// to create a zero cost type.
pub opaque type ExpirationTime {
  inner: PosixTime
}

// Get the time from the tx's invalid-hereafter setting. The invalid-hereafter setting is used
// for end times since it guarantees that the specified time has NOT passed yet; the transaction
// would fail otherwise. By having users set the invalid-hereafter to the closest expiration
// time, users can assert to the plutus script that the expiration indeed has not passed.
pub fn get_end_time(val_range: ValidityRange) -> ExpirationTime {
  when val_range is {
    Interval(IntervalBound(Finite(x),_),_) -> ExpirationTime(x)
    _ -> error @"invalid-hereafter not specified"
  }
}

pub fn expiration_time_has_not_passed(
  upper_bound: ExpirationTime, 
  expiration_time: ExpirationTime
) -> Bool {
  upper_bound.inner <= expiration_time.inner
}

// This is only used to check an offer expiration since that is the only expiration time
// set by a user.
pub fn is_valid_expiration_time(expiration_time: ExpirationTime) -> Bool {
  expiration_time.inner > 0
}

// This is used to check an offer expiration.
pub fn make_expiration_time(expiration_time: PosixTime) -> ExpirationTime {
  ExpirationTime(expiration_time)
}

// A type respenting how long something will last. Uses an opaque type to create a zero cost type.
pub opaque type Duration {
  inner: PosixTime
}

pub fn calc_expiration_time(start: StartTime, duration: Duration) -> ExpirationTime {
  ExpirationTime(start.inner + duration.inner)
}

pub fn add_duration(duration1: Duration, duration2: Duration) -> Duration {
  Duration(duration1.inner + duration2.inner)
}

// All durations must be > 0.
pub fn is_valid_duration(duration: Duration) -> Bool {
  duration.inner > 0
}

pub fn make_duration(duration: PosixTime) -> Duration {
  Duration(duration)
}

pub fn update_checkpoint(last_checkpoint: StartTime, rollover_frequency: Duration) -> StartTime {
  StartTime(last_checkpoint.inner + rollover_frequency.inner)
}

