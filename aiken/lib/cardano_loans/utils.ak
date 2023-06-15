use aiken/transaction.{Transaction,Datum,InlineDatum,Output,Input}
use aiken/transaction/credential.{ 
  Credential, StakeCredential, Inline, VerificationKeyCredential, ScriptCredential, Script,
  Address, VerificationKey
}
use aiken/transaction/value.{AssetName,PolicyId,Value,MintedValue}
use aiken/hash.{Hash,Blake2b_224}
use aiken/list.{has,is_empty,all}
use aiken/option.{is_some}
use aiken/int
use aiken/string
use aiken/dict.{has_key,Dict}
use cardano_loans/types.{
  LoanDatum, LoanRedeemer, BeaconRedeemer, AskDatum, OfferDatum, ActiveDatum,
  CloseAsk, CloseOffer, AcceptOffer, MakePayment, Rollover, ClaimExpired, UpdateLenderAddress,
  UnlockLostCollateral, MintAskBeacon, MintOfferBeacon, MintActiveBeacon, BurnBeacons
}
use cardano_loans/ratio

pub fn staking_credential_approves(
  staking_cred: Option<StakeCredential>,
  extra_signatories: List<Hash<Blake2b_224, VerificationKey>>,
  withdrawals: Dict<StakeCredential, Int>
) -> Bool {
  when staking_cred is {
    // This is to prevent permanent locking of funds. Beacons can never be minted to an address
    // without a valid staking credential.
    None -> True

    // If the address uses a staking pubkey, it must have signed the tx.
    Some(Inline(VerificationKeyCredential(skey))) -> {
      has(extra_signatories,skey)
    }

    // If the address uses a staking script, it must have been executed in the tx.
    Some(svh) -> {
      has_key(withdrawals,svh)
    }
  }
}

pub fn lender_approves(
  lender_id: AssetName, 
  sigs: List<Hash<Blake2b_224, VerificationKey>>,
  wtdr: Dict<StakeCredential, Int>
) -> Bool {
  staking_credential_approves(Some(Inline(VerificationKeyCredential(lender_id))),sigs,wtdr) ||
  staking_credential_approves(Some(Inline(ScriptCredential(lender_id))),sigs,wtdr)
}

fn parse_datum(raw_datum: Datum) -> LoanDatum {
  when raw_datum is {
    InlineDatum(d) -> {
      expect datum: LoanDatum = d // This will error if datum is not LoanDatum
      datum
    }
    _ -> error @"All datums must be inline datums"
  }
}

fn credential_to_asset_name(cred: Credential) -> AssetName {
  when cred is {
    VerificationKeyCredential(pkh) -> pkh
    ScriptCredential(sh) -> sh
  }
}

fn address_uses_payment_pubkey(addr: Address) -> Bool {
  when addr is {
    Address(VerificationKeyCredential(_),_) -> {
      True
    }
    _ -> {
      False
    }
  }
}

pub fn total_input_value(ins: List<Input>) -> Value {
  let foo = fn (i: Input ,acc: Value) {
    let Input(_,Output(_,val,_,_)) = i
    value.merge(acc,val)
  }
  list.foldr(ins,value.zero(),foo)
}

pub fn all_beacons_burned(
  sym: PolicyId,
  name: AssetName, 
  ival: Value,
  minted: MintedValue
) -> Bool {
  if value.quantity_of(ival,sym,name) == -value.quantity_of(value.from_minted_value(minted),sym,name) {
    True
  } else {
    error @"Beacons not burned"
  }
}

// This function either throws an error or returns True.
pub fn mint_check(redeemer: BeaconRedeemer, mint: Dict<AssetName,Int>) -> Bool {
  when redeemer is {
    MintAskBeacon(_) -> {
      when dict.to_list(mint) is {
        [(beacon_name,mint_number)] -> {
          if beacon_name == "Ask" && mint_number >= 1 {
            True
          } else {
            error @"Only Ask beacons can be minted and they must have the token name 'Ask'"
          }
        }
        _ -> error @"Only Ask beacons can be minted and they must have the token name 'Ask'"
      }
    }
    MintOfferBeacon(lender_cred) -> {
      when dict.to_list(mint) is {
        [(name1,num1),(name2,num2)] -> {
          if name1 == "Offer" {
            if num1 != 1 {
              error @"Only one Offer beacon can be minted"
            } else if name2 != credential_to_asset_name(lender_cred) {
              error @"LenderID does not have lender's credential as token name"
            } else if num2 != 1 {
              error @"Only one LenderID can be minted"
            } else {
              True
            }
          } else if name2 == "Offer" {
            if num2 != 1 {
              error @"Only one Offer beacon can be minted"
            } else if name1 != credential_to_asset_name(lender_cred) {
              error @"LenderID does not have lender's credential as token name"
            } else if num1 != 1 {
              error @"Only one LenderID can be minted"
            } else {
              True
            }
          } else {
            error @"Only the Offer beacon and LenderID can be minted"
          }
        }
        _ -> {
          error @"Only the Offer beacon and LenderID can be minted"
        }
      }
    }
    MintActiveBeacon(_,_) -> {
      todo
    }
    BurnBeacons -> {
      if dict.foldl(mint,True,fn (_,v,r) {r && v < 0}) {
        True
      } else {
        error @"This redeemer can only be used to burn"
      }
    }
  }
}

fn valid_datum(sym: PolicyId, redeemer: BeaconRedeemer, datum: LoanDatum) -> Bool {
  when (redeemer,datum) is {
    ( MintAskBeacon(borrower_cred),
      AskDatum(loan_beacon_sym,borrower_id,_,loan_principle,loan_term,collateral)
    ) -> {
      if loan_beacon_sym != sym {
        error @"Invalid AskDatum loan_beacon_sym"
      } else if borrower_id != credential_to_asset_name(borrower_cred) {
        error @"Invalid AskDatum borrower_id"
      } else if loan_principle <= 0 {
        error @"Invalid AskDatum loan_principle"
      } else if loan_term <= 0 {
        error @"Invalid AskDatum loan_term"
      } else if is_empty(collateral) {
        error @"AskDatum collateral is empty"
      } else {
        True
      }
    }
    (MintAskBeacon(_),_) -> {
      error @"Ask beacon must be stored with an AskDatum"
    }
    ( MintOfferBeacon(lender_cred),
      OfferDatum(
        beacon_sym,lender_id,lender_address,_,principle,checkpoints,term,interest,
        collateralization,claim_period)
    ) -> {
      if beacon_sym != sym {
        error @"Invalid OfferDatum loan_beacon_sym"
      } else if lender_id != credential_to_asset_name(lender_cred) {
        error @"Invalid OfferDatum lender_id"
      } else if !address_uses_payment_pubkey(lender_address) {
        error @"Lender address must use payment pubkey"
      } else if principle <= 0 {
        error @"Invalid OfferDatum loan_principle"
      } else if term <= 0 {
        error @"Invalid OfferDatum loan_term"
      } else if ratio.lt(interest,ratio.zero()) {
        error @"Invalid OfferDatum loan_interest"
      } else if claim_period <= 0 {
        error @"Invalid OfferDatum claim_period"
      } else if is_empty(collateralization) {
        error @"OfferDatum collateralization list is empty"
      } else if !all(
          collateralization,
          fn(x) {
            let (_,n) = x 
            ratio.gte(n,ratio.zero())
          }) {
        error @"All collateralizations must be >= 0"
      } else if !all(checkpoints,fn(n) {n > 0}) {
        error @"All checkpoints must be > 0"
      } else if list.sort(checkpoints,int.compare) != checkpoints {
        error @"Checkpoints must be in ascending order"
      } else {
        True
      }
    }
    (MintOfferBeacon(_),_) -> {
      error @"Offer beacons must be stored with an OfferDatum"
    }
    _ -> {
      False // Never used with ActiveDatums
    }
  }
}

fn loan_principle_met(oval: Value, datum: LoanDatum) -> Bool {
  expect OfferDatum(_,_,_,(asset_sym,asset_name),principle,..) = datum
  if asset_sym == "" {
    if value.quantity_of(oval,asset_sym,asset_name) != 3_000_000 + principle {
      error @"Offer not stored with 3 ADA + loan amount"
    } else {
      True
    }
  } else {
    if value.lovelace_of(oval) != 3_000_000 &&
       value.quantity_of(oval,asset_sym,asset_name) != principle {
      error @"Offer not stored with 3 ADA + loan amount"
    } else {
      True
    }
  }
}

pub fn destination_check(
  dapp_hash: Hash<Blake2b_224,Script>,
  sym: PolicyId,
  redeemer: BeaconRedeemer,
  tx_info: Transaction,
  app_name: String
) -> Bool {
  let Transaction(_,_,outputs,..) = tx_info
  let foo = fn (out: Output, acc: Bool) {
    let Output(Address(pay,stake),val,d,_) = out
    when redeemer is {
      MintAskBeacon(borrower_cred) -> {
        let ask_num = value.quantity_of(val,sym,"Ask")
        // If the output has an Ask beacon:
        if ask_num > 0 {
          // It can only have one Ask beacon.
          if ask_num != 1 {
            error @"Ask beacons must be stored individually"
          } else {
            when (pay,stake) is {
              (ScriptCredential(vh),Some(Inline(s_cred))) -> {
                // It must be locked at a dApp address.
                if vh != dapp_hash {
                  let msg = string.concat(@"Beacon must go to a ",app_name) 
                         |> string.concat(_,@" dApp address")
                  error msg
                // It must be locked at the dApp address for the borrower in the redeemer.
                } else if s_cred != borrower_cred {
                  error @"Receiving address does not match redeemer staking credential"
                // It must have a valid AskDatum.
                } else {
                  acc && valid_datum(sym,redeemer,parse_datum(d))
                }
              }
              _ -> {
                error @"Beacon must go to a dapp address with a valid staking credential"
              }
            }
          }
        } else {
          acc
        }
      }
      MintOfferBeacon(lender_cred) -> {
        if value.quantity_of(val,sym,"Offer") == 1 {
          if value.quantity_of(val,sym,credential_to_asset_name(lender_cred)) != 1 {
            error @"Offer beacon and LenderID must be stored together"
          } else {
            when (pay,stake) is {
              (ScriptCredential(vh),Some(Inline(_))) -> {
                if vh != dapp_hash {
                  let msg = string.concat(@"Beacon must go to a ",app_name) 
                         |> string.concat(_,@" dApp address")
                  error msg
                } else {
                  let datum = parse_datum(d)
                  acc && valid_datum(sym,redeemer,datum) && loan_principle_met(val,datum)
                }
              }
              _ -> {
                error @"Beacons must go to a dapp address with a valid staking credential"
              }
            }
          }
        } else {
          acc
        }
      }
      _ -> {
        False // Not used for MintActiveBeacon and BurnBeacons.
      }
    }
  }
  list.foldl(outputs,True,foo)
}