use aiken/dict.{Dict}
use aiken/option
use aiken/hash.{Hash,Blake2b_224}
use aiken/list
use aiken/transaction.{
    Datum,
    InlineDatum,
    Redeemer,
    ScriptPurpose
  } as tx
use aiken/transaction/credential.{
    Address,
    Credential,
    Inline,
    StakeCredential,
    Script,
    ScriptCredential,
    VerificationKey,
    VerificationKeyCredential
  }
use aiken/transaction/value.{
    PolicyId
  }

use cardano_loans/data/datums.{
    LoanDatum,
  }

// If a test fails, provide a user friendly error message. This is useful
// when chaining if-then-elses.
pub fn trace_if_false(msg: String, predicate: Bool) {
  if !predicate {
    trace msg
    predicate
  } else {
    predicate
  }
}

// If a test fails, immediately crash the script with an error message. This is useful
// when some side-effect needs to be verified before continuing with the main function.
pub fn error_if_false(msg: String, predicate: Bool) {
  if !predicate {
    error msg
  } else {
    predicate
  }
}

// Check that either the staking pubkey signed or the staking script was executed.
pub fn staking_credential_approves(
  staking_cred: Option<StakeCredential>, 
  withdrawals: Dict<StakeCredential, Int>,
  extra_signatories: List<Hash<Blake2b_224, VerificationKey>>
) -> Bool {
  when staking_cred is {
    // This is to prevent permanent locking of funds. Beacons can never be minted to an address
    // without a valid staking credential.
    None -> True

    // If the address uses a staking pubkey, it must have signed the tx.
    Some(Inline(VerificationKeyCredential(skey))) -> list.has(extra_signatories,skey)

    // If the address uses a staking script, it must have been executed in the tx. In order for
    // the staking credential to show up in this dictionary, it must be registered.
    Some(svh) -> dict.has_key(withdrawals,svh)
  }
}

// Check if the address is using a pubkey or the proxy script for the payment credential.
// If the proxy script is used for the payment credential, the address MUST have a staking
// credential.
pub fn valid_lender_address(
  proxy_hash: Hash<Blake2b_224, Script>, 
  addr: Address
) -> Bool {
  when addr is {
    Address(VerificationKeyCredential(_),_) -> True
    
    Address(ScriptCredential(script_hash),stake) -> 
      script_hash == proxy_hash && option.is_some(stake)
  }
}

// This parses the datums attached to loan UTxOs. It will fail with an error unless
// the datum is an inline LoanDatum.
pub fn parse_datum(raw_datum: Datum) -> LoanDatum {
  when raw_datum is {
    InlineDatum(d) -> {
      expect loan_datum: LoanDatum = d
      loan_datum
    }
    _ -> error @"All datums must be inline datums"
  }
}

// Lookup the value by the first element in the tuple for a list of tuples.
pub fn find_by_key(self: List<(k,v)>, key: k) -> Option<v> {
  when self is {
    [] -> None
    [(k,v),..rest] -> {
      if k == key { Some(v) }
      else { find_by_key(rest, key) }
    }
  }
}

// Check if the beacon script was executed using either a minting execution or a staking
// execution, and with the proper redeemer.
pub fn beacon_script_executed(
  redeemers: Dict<ScriptPurpose, Redeemer>,
  req_redeemer: Data, 
  req_script_id: PolicyId
) -> Bool {
  let req_mint = tx.Mint(req_script_id)
  let req_stake = tx.WithdrawFrom(Inline(ScriptCredential(req_script_id)))
  let foo = fn(red: (ScriptPurpose, Redeemer)) -> Bool {
    let (purpose,red_used) = red
    and {
      purpose == req_mint || purpose == req_stake,
      red_used == req_redeemer
    }
  }

  list.find(dict.to_list(redeemers),foo) |> option.is_some(_)
}

pub fn is_pub_key_credential(cred: Credential) -> Bool {
  when cred is {
    VerificationKeyCredential(_) -> True
    _ -> False
  }
}
