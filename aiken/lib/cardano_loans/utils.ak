use aiken/bytearray as byte
use aiken/cbor
use aiken/dict.{Dict}
use aiken/option
use aiken/list
use aiken/math
use aiken/interval.{Interval,IntervalBound,Finite}
use aiken/time.{PosixTime}
use aiken/transaction.{
    Datum,
    InlineDatum,
    Redeemer,
    ScriptPurpose,
    ValidityRange,
    Mint,
    Spend
  } as tx
use aiken/transaction/credential.{
    Address,
    Credential,
    Inline,
    StakeCredential,
    ScriptCredential,
    VerificationKeyCredential
  }
use aiken/transaction/value.{AssetName, PolicyId, Value}

use cardano_loans/data/core.{Asset,Fraction,ScriptHash,KeyHash}
use cardano_loans/data/datums.{LoanDatum}

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
  extra_signatories: List<KeyHash>
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
pub fn valid_lender_address(proxy_hash: ScriptHash, addr: Address) -> Bool {
  when addr is {
    // Any payment pubkey can be used.
    Address(VerificationKeyCredential(_),_) -> True
    // The only payment script supported is the proxy script.
    Address(ScriptCredential(sh),stake) -> sh == proxy_hash && option.is_some(stake)
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
pub fn compare_collateral_asset_no_dups(asset1: Asset, asset2: Asset) -> Ordering {
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
pub fn compare_collateralization_no_dups(
  collat1: (Asset,Fraction), 
  collat2: (Asset,Fraction)
) -> Ordering {
  let (asset1,_) = collat1
  let (asset2,_) = collat2

  compare_collateral_asset_no_dups(asset1,asset2)
}

// Check if the target script was executed using either a minting execution or a staking
// execution, and with the proper redeemer.
pub fn script_executed_with_redeemer(
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

// Check if the target observer script was executed using a staking execution. This function is only
// safe to do in situations where the redeemer used for the execution does not matter.
pub fn observer_script_executed(
  withdrawals: Dict<StakeCredential, Int>,
  target_script_id: ByteArray
) -> Bool {
  dict.get(withdrawals,Inline(ScriptCredential(target_script_id))) |> option.is_some(_)
}

// Get the time from the tx's invalid-before setting. The invalid-before setting is used for
// start times since it guarantees that the specified time has passed; the transaction would fail
// otherwise. It is the best approximation for the current time since users can always set it
// to the current time.
pub fn get_lower_bound(val_range: ValidityRange) -> PosixTime {
  when val_range is {
    Interval(IntervalBound(Finite(x),_),_) -> x
    _ -> error @"invalid-before not specified"
  }
}

// Get the time from the tx's invalid-hereafter setting. The invalid-hereafter setting is used
// for end times since it guarantees that the specified time has NOT passed yet; the transaction
// would fail otherwise. By having users set the invalid-hereafter to the closest expiration
// time, users can assert to the plutus script that the expiration indeed has not passed. This
// returns an Option in case the end time is not actually needed, like when accepting offers.
pub fn get_upper_bound(val_range: ValidityRange) -> Option<PosixTime> {
  when val_range is {
    Interval(_,IntervalBound(Finite(x),_)) -> Some(x)
    _ -> None
  }
}

pub fn expiration_has_not_passed(expiration: PosixTime, hereafter: PosixTime) -> Bool {
  hereafter <= expiration
}

pub fn apply_interest(balance: Fraction, interest: Fraction) -> Fraction {
  let (bal_num,bal_den) = balance
  let (interest_num,interest_den) = interest

  let total_num = bal_num * (interest_den + interest_num) // Balance * (1 + interest)
  let total_den = bal_den * interest_den
  let new_gcd = math.gcd(total_num,total_den)

  (total_num / new_gcd, total_den / new_gcd)
}

pub fn subtract_payment(bal_num: Int, bal_den: Int, payment: Int) -> Fraction {
  (bal_num - bal_den * payment, bal_den)
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

pub fn sum_token_values(_key: AssetName, v1: Int, v2: Int) -> Option<Int> {
  Some(v1 + v2)
}

pub fn is_pub_key_credential(cred: Credential) -> Bool {
  when cred is {
    VerificationKeyCredential(_) -> True
    _ -> False
  }
}
