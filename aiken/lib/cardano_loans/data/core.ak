use aiken/bytearray as byte
use aiken/hash.{Blake2b_224,Hash}
use aiken/interval.{Interval,IntervalBound,Finite}
use aiken/time.{PosixTime}
use aiken/transaction/credential.{
  Credential,
  VerificationKey,
  VerificationKeyCredential,
  ScriptCredential,
  Script
}
use aiken/transaction/value.{PolicyId,AssetName}
use aiken/transaction.{OutputReference,ValidityRange,TransactionId}
use aiken/string
use aiken/math

//-----------------------------------------------
// Type Synonyms
//-----------------------------------------------
pub type Asset = (PolicyId,AssetName)
pub type ScriptHash = Hash<Blake2b_224,Script>
pub type KeyHash = Hash<Blake2b_224,VerificationKey>
pub type Fraction = (Int,Int)

//-----------------------------------------------
// Negotiation PolicyId
//-----------------------------------------------
// A zero cost type representing the negotiation beacon script's policy id.
pub opaque type NegotiationId {
  inner: PolicyId
}

pub fn to_negotiation_id(policy_id: PolicyId) -> NegotiationId {
  NegotiationId(policy_id)
}

pub fn from_negotiation_id(policy_id: NegotiationId) -> PolicyId {
  policy_id.inner
}

//-----------------------------------------------
// Active PolicyId
//-----------------------------------------------
// A type representing the active beacon script's policy id.
pub opaque type ActiveId {
  inner: PolicyId
}

pub fn to_active_id(policy_id: PolicyId) -> ActiveId {
  ActiveId(policy_id)
}

pub fn from_active_id(policy_id: ActiveId) -> PolicyId {
  policy_id.inner
}

//-----------------------------------------------
// Borrower Ids
//-----------------------------------------------
// A zero cost type representing borrower ids. 
pub opaque type BorrowerId {
  inner: AssetName
}

pub fn get_borrower_id_name(borrower_id: BorrowerId) -> AssetName {
  borrower_id.inner
}

// Convert a pubkey hash or script hash to a BorrowerId.
pub fn credential_to_borrower_id(borrower_cred: Credential) -> BorrowerId {
  when borrower_cred is {
    VerificationKeyCredential(pkh) -> BorrowerId(pkh)
    ScriptCredential(sh) -> BorrowerId(sh)
  }
}

pub fn unsafe_to_borrower_id( borrower_id: AssetName ) -> BorrowerId {
  BorrowerId(borrower_id)
}

//-----------------------------------------------
// AssetBeacon
//-----------------------------------------------
// A zero cost type representing asset beacons. 
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

pub fn unsafe_to_asset_beacon( asset_beacon: AssetName ) -> AssetBeacon {
  AssetBeacon(asset_beacon)
}

//-----------------------------------------------
// Lender Ids
//-----------------------------------------------
// A zero cost type representing lender ids. 
pub opaque type LenderId {
  inner: AssetName
}

// Get the inner AssetName from the LenderId.
pub fn get_lender_id_name(lender_id: LenderId) -> AssetName {
  lender_id.inner
}

// Convert a pubkey hash or script hash to a LenderId.
pub fn credential_to_lender_id(lender_cred: Credential) -> LenderId {
  when lender_cred is {
    VerificationKeyCredential(pkh) -> LenderId(byte.concat(#"00",pkh))
    ScriptCredential(sh) -> LenderId(byte.concat(#"01",sh))
  }
}

// Convert a LenderId to the respective credential.
pub fn lender_id_to_credential( lender_id: LenderId, ) -> Credential {
  // The first two characters of the lender_id signify what kind of credential the LenderId
  // is. "00" means the credential is a pubkey while "01" means the credential is a script.
  let prefix = byte.take(lender_id.inner,1)
  let cred = byte.drop(lender_id.inner,1)
  if prefix == #"00" {
    VerificationKeyCredential(cred)
  } else if prefix == #"01" {
    ScriptCredential(cred)
  } else { error } // This branch should never be taken.
}

pub fn unsafe_to_lender_id( lender_id: AssetName ) -> LenderId {
  LenderId(lender_id)
}

//-----------------------------------------------
// LoanIds
//-----------------------------------------------
// A type representing loan ids. It is written as an opaque type to create a zero cost type
// that only exists at compile time.
pub opaque type LoanId {
  inner: AssetName
}

// Get the inner AssetName from the LoanId.
pub fn get_loan_id_name(loan_id: LoanId) -> AssetName {
  loan_id.inner
}

// Wrap an AssetName in the LoanId constructor. This is required when checking UTxOs contain the
// proper beacons.
pub fn asset_name_to_loan_id(loan_id: AssetName) -> LoanId {
  LoanId(loan_id)
}

// Create the LoanId from the Offer `OuptutReference` by hashing the combination of the tx hash
// and the output index. It uses the sha2_256 hash algorithm. Since the output reference input
// comes directly from the blockchain (as opposed to the user), outputs of this function
// should always be distinct from outputs of `make_asset_beacon` - no output references
// are prefixed with "Asset".
pub fn make_loan_id(offer_ref: OutputReference) -> LoanId {
  let OutputReference(TransactionId(tx_hash),out_index) = offer_ref
  hash.sha2_256(
    byte.concat(
      tx_hash,
      string.from_int(out_index) |> byte.from_string(_)
    )
  ) |> LoanId
}

pub fn unsafe_to_loan_id( loan_id: AssetName ) -> LoanId {
  LoanId(loan_id)
}

