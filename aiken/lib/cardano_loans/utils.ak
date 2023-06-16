use aiken/transaction.{
  Transaction, Datum, InlineDatum, Output, Input, OutputReference, TransactionId,
  ValidityRange
}
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
use aiken/time.{PosixTime}
use aiken/interval.{Interval,IntervalBound,Finite}
use aiken/bytearray
use aiken/dict.{has_key,Dict}
use cardano_loans/types.{
  LoanDatum, LoanRedeemer, BeaconRedeemer, AskDatum, OfferDatum, ActiveDatum,
  CloseAsk, CloseOffer, AcceptOffer, MakePayment, Rollover, ClaimExpired, UpdateLenderAddress,
  UnlockLostCollateral, MintAskBeacon, MintOfferBeacon, MintActiveBeacon, BurnBeacons
}
use cardano_loans/ratio.{Rational}

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
      error // Never used with this redeemer.
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
        when list.last(checkpoints) is {
          Some(x) -> {
            if x >= term {
              error @"Final checkpoint must be before expiration"
            } else {
              True
            }
          }
          _ -> {
            True
          }
        }
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
    if value.quantity_of(oval,asset_sym,asset_name) != 5_000_000 + principle {
      error @"Offer not stored with 5 ADA + loan amount"
    } else {
      True
    }
  } else {
    if value.lovelace_of(oval) != 5_000_000 ||
       value.quantity_of(oval,asset_sym,asset_name) != principle {
      error @"Offer not stored with 5 ADA + loan amount"
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

fn compare_out_ref(ref1: OutputReference, ref2: OutputReference) -> Ordering {
  let OutputReference(TransactionId(tx_id1),out_index1) = ref1
  let OutputReference(TransactionId(tx_id2),out_index2) = ref2
  when bytearray.compare(tx_id1,tx_id2) is {
    Less -> { Less }
    Greater -> { Greater }
    Equal -> { int.compare(out_index1,out_index2) }
  }
}

// They should never be equal so it will crash if two are equal.
fn compare_active_datums(datum1: LoanDatum, datum2: LoanDatum) -> Ordering {
  expect ActiveDatum(_,_,_,_,_,_,_,_,_,_,_,_,_,loan_id1) = datum1
  expect ActiveDatum(_,_,_,_,_,_,_,_,_,_,_,_,_,loan_id2) = datum2
  when bytearray.compare(loan_id1,loan_id2) is {
    Less -> { Less }
    Greater -> { Greater }
    Equal -> { error @"Duplicate ActiveDatum found" }
  }
}

// This is only used on values with a LoanID so it will either throw an error if the LoanID
// is in both values or return Less.
fn compare_value(loan_id: AssetName, sym: PolicyId, val1: Value, val2: Value) -> Ordering {
  if value.quantity_of(val1,sym,loan_id) == value.quantity_of(val2,sym,loan_id) {
    error @"LoanID used twice in lender payments"
  } else {
    Less
  }
}

// Get the loan start time from the tx's validity range. Based off the lower bound.
// This is also used when a lender is claiming a loan to get the claim time.
pub fn get_start_time(val_range: ValidityRange) -> PosixTime {
  when val_range is {
    Interval(IntervalBound(Finite(x),_),_) -> {
      x
    }
    _ -> {
      error @"invalid-before not specified"
    }
  }
}

// Returns what the loan output should have for the ActiveDatum based on the inputs that were paired.
fn expected_active_datum(
  start: PosixTime,
  ask_datum: LoanDatum,
  offer_datum: LoanDatum,
  loan_id: AssetName
) -> LoanDatum {
  expect AskDatum(
    ask_sym,
    ask_borrower_id,
    ask_asset,
    ask_principle,
    ask_term,
    ask_collateral
  ) = ask_datum
  expect OfferDatum(
    offer_sym,
    offer_lender_id,
    offer_lender_address,
    offer_asset,
    offer_principle,
    offer_checkpoints,
    offer_term,
    offer_interest,
    offer_collateralization,
    offer_claim_period
  ) = offer_datum
  if ask_asset != offer_asset {
    error @"Datums do not agree on loan asset"
  } else if ask_principle != offer_principle {
    error @"Datums do not agree on loan principle"
  } else if ask_term != offer_term {
    error @"Datums do not agree on loan term"
  } else if ask_collateral != list.map(offer_collateralization,fn(x) {let (a,_) = x a}) {
    error @"Datums do not agree on collateral"
  } else {
    ActiveDatum{
      loan_beacon_sym: offer_sym,
      borrower_id: ask_borrower_id,
      lender_address: offer_lender_address,
      loan_asset: offer_asset,
      loan_principle: offer_principle,
      next_checkpoints: list.map(offer_checkpoints,fn(x) { x + start }),
      past_checkpoitns: [],
      loan_term: offer_term,
      loan_interest: offer_interest,
      collateralization: offer_collateralization,
      claim_expiration: start + offer_claim_period,
      loan_expiration: start + offer_term,
      loan_outstanding: ratio.from_integer(offer_principle),
      loan_id: loan_id
    }
  }
}

fn is_enough_collateral(
  principle_taken: Rational,
  collats: List<((PolicyId,AssetName),Rational)>,
  oval: Value
) -> Bool {
  let foo = fn (collat: ((PolicyId,AssetName),Rational), acc: Rational) {
    let ((policy,name),price) = collat
    if price == ratio.zero() {
      acc
    } else {
      ratio.add(
        acc, 
        ratio.multiply(
          ratio.from_integer(value.quantity_of(oval,policy,name)),
          ratio.recip(price)
        )
      )
    }
  }
  ratio.gte(list.foldl(collats,ratio.zero(),foo),principle_taken)
}

// Goes through the inputs and builds up two maps: one Ask input map and one Offer input map.
// All inputs from the loan validator must come from the same address and all inputs must
// have the proper beacon token present. The integer returned is the size of that map.
fn input_maps (
  borrower_cred: Credential,
  ins: List<Input>,
  sym: PolicyId,
  dapp_hash: Hash<Blake2b_224,Script>,
) -> ((Dict<OutputReference,LoanDatum>,Int),(Dict<OutputReference,LoanDatum>,Int)) {
  let foo = fn (
    in: Input, 
    acc: ((Dict<OutputReference,LoanDatum>,Int),(Dict<OutputReference,LoanDatum>,Int))
  ) {
    let Input(out_ref,Output(Address(pay_cred,stake_cred),val,d,_)) = in
    let ((ask_map,asize),(offer_map,osize)) = acc
    // Check if input has an Ask beacon. This can only be from a dApp address.
    if value.quantity_of(val,sym,"Ask") == 1 {
      // Check if it comes from the correct dApp address.
      if stake_cred == Some(Inline(borrower_cred)) {
        let datum = parse_datum(d) // Guaranteed to be an AskDatum since the "Ask" beacon is present.
        let new_ask_map = dict.insert(ask_map,out_ref,datum,compare_out_ref)
        let new_asize = asize + 1
        ((new_ask_map,new_asize),(offer_map,osize))
      } else {
        error @"Input from wrong dApp address"
      }
    // Check if input has an Offer beacon. This can only be from a dApp address.
    } else if value.quantity_of(val,sym,"Offer") == 1 {
      // Check if it comes from the correct dApp address.
      if stake_cred == Some(Inline(borrower_cred)) {
        let datum = parse_datum(d) // Guaranteed to be an OfferDatum since the "Offer" beacon is present.
        let new_offer_map = dict.insert(offer_map,out_ref,datum,compare_out_ref)
        let new_osize = osize + 1
        ((ask_map,asize),(new_offer_map,new_osize))
      } else {
        error @"Input from wrong dApp address"
      }
    // Check if input comes from a dApp address. If it does and the previous two checks failed,
    // this must be an invalid input.
    } else if pay_cred == ScriptCredential(dapp_hash) {
      error @"Invalid loan input found"
    // Otherwise, it is a benign input and can be skipped.
    } else {
      acc
    }
  }
  list.foldl(ins,((dict.new(),0),(dict.new(),0)),foo)
}

// This compares the inputs against the pairings specified in the redeemer. It check:
// 1) length pairings == size ask map == size offer map
// 2) all ask and offer inputs are only paired up once
// 3) the input datums agree
// It returns a map of required outputs by ActiveDatums since LoanIDs guarantee uniqueness.
// Internally:
// - The pay_map is a map of outputs that must go to the lenders.
// - The loan_map is a map of collateral output datums that must go to the loan address.
pub fn acceptance_input_checks(
  borrower_cred: Credential,
  ins: List<Input>,
  sym: PolicyId,
  dapp_hash: Hash<Blake2b_224,Script>,
  start_time: PosixTime,
  pairings: List<(OutputReference,OutputReference)>
) -> ( Dict<Value,Address>, Int , Dict<LoanDatum,AssetName>, Int, Value ) {
  let ((ask_map,asize),(offer_map,osize)) = input_maps(borrower_cred,ins,sym,dapp_hash)
  let foo = fn (
    pair: (OutputReference,OutputReference),
    acc: ( 
      Dict<OutputReference,Int>, // This will be dropped before returning the overall result.
      Dict<Value,Address>, 
      Int,
      Dict<LoanDatum,AssetName>,
      Int, 
      Value
    )
  ) {
    let (ask_ref,offer_ref) = pair
    let (used_map,pay_map,pay_size,loan_map,loan_size,req_mint) = acc
    let OutputReference(TransactionId(offer_id),_) = offer_ref
    when (dict.get(used_map,ask_ref),dict.get(used_map,offer_ref)) is {
      // Neither have been used yet.
      (None,None) -> {
        when (dict.get(ask_map,ask_ref),dict.get(offer_map,offer_ref)) is {
          (Some(ask_datum),Some(offer_datum)) -> {
            expect OfferDatum(_,lender_id,lender_address,..) = offer_datum
            let pay_lender_val = value.zero() |> 
              value.add(_,value.ada_policy_id,value.ada_asset_name,5_000_000) |>
              value.add(_,sym,offer_id,1)
            let new_req_mint = req_mint |>
              value.add(_,sym,offer_id,2) |>
              value.add(_,sym,lender_id,-1) |>
              value.add(_,sym,"Ask",-1) |>
              value.add(_,sym,"Offer",-1) |>
              value.add(_,sym,"Active",1)
            let new_pay_map = dict.insert(
              pay_map,
              pay_lender_val,
              lender_address,
              compare_value(offer_id,sym,_,_)
            )
            let new_pay_size = pay_size + 1
            let req_datum = expected_active_datum(start_time,ask_datum,offer_datum,offer_id)
            let new_loan_map = dict.insert(loan_map,req_datum,lender_id,compare_active_datums)
            let new_loan_size = loan_size + 1
            let new_used_map = used_map |>
              dict.insert(_,offer_ref,1,compare_out_ref) |>
              dict.insert(_,ask_ref,1,compare_out_ref)
            (new_used_map,new_pay_map,new_pay_size,new_loan_map,new_loan_size,new_req_mint)
          }
          _ -> {
            error @"Paired input not found"
          }
        }
      }
      // The input has already been used.
      _ -> {
        error @"Input already paired"
      }
    }
  }
  // Check that there are the same number of Asks and Offers and at least one of each.
  if asize == osize && asize != 0 {
    let (_,pay_map,pay_size,loan_map,loan_size,req_mint) = list.foldl(
      pairings,
      (dict.new(),dict.new(),0,dict.new(),0,value.zero()),
      foo
    )
    // The loan_map size will be the same as the pay_map size.
    if pay_size == asize {
      (pay_map,pay_size,loan_map,loan_size,req_mint)
    } else {
      error @"Not all pairs found"
    }
  } else {
    error @"Wrong number of ask and offer inputs"
  }
}

// This checks the outputs to ensure:
// 1) All lenders get the proper LoanID tokens and their deposits back.
// 2) All loans are properly collateralized at the borrower's address.
// 3) All collateral UTxOs have the proper LoanID token.
pub fn acceptance_output_checks(
  borrower_cred: Credential,
  pairings: List<(OutputReference,OutputReference)>,
  sym: PolicyId,
  dapp_hash: Hash<Blake2b_224,Script>,
  start_time: PosixTime,
  pay_map: Dict<Value,Address>,
  pay_size: Int,
  loan_map: Dict<LoanDatum,AssetName>,
  loan_size: Int,
  outs: List<Output>
) -> Bool {
  let borrower_addr = Address(ScriptCredential(dapp_hash),Some(Inline(borrower_cred)))
  let borrower_token = credential_to_asset_name(borrower_cred)
  let foo = fn (out: Output, acc: (Int,Int)) {
    let (lenders_paid,collaterals_satisfied) = acc
    let Output(addr,val,d,_) = out
    // Check if output is a collateral output.
    if addr == borrower_addr {
      let datum = parse_datum(d)
      when datum is {
        ActiveDatum(_,_,_,_,_,_,_,_,_,collats,_,_,borrowed,loan_id) -> {
          // Check if output has enough collateral.
          if !is_enough_collateral(borrowed,collats,val) {
            error @"Not enough collateral posted"
          // Check if output has one LoanID.
          } else if value.quantity_of(val,sym,loan_id) != 1 {
            error @"Collateral output must have exactly one LoanID"
          // Check if output has one BorrowerID.
          } else if value.quantity_of(val,sym,borrower_token) != 1 {
            error @"Collateral output must have exactly one BorrowerID"
          // Check if output has one Active beacon.
          } else if value.quantity_of(val,sym,"Active") != 1 {
            error @"Collateral output must have exactly one Active beacon"
          } else {
            (lenders_paid,collaterals_satisfied + 1)
          }
        }
        _ -> {
          error @"Loan output has wrong datum type"
        }
      } 
    // Check if the value is supposed to be a lender payment.
    } else {
      when dict.get(pay_map,val) is {
        Some(lender_addr) -> {
          if lender_addr == addr {
            (lenders_paid + 1, collaterals_satisfied)
          // The above check can only fail if the LoanID was sent to the wrong address.
          } else {
            error @"LoanID sent to wrong address."
          }
        }
        _ -> acc
      }
    }
  }
  let (lenders_paid,collaterals_satisfied) = list.foldl(outs,(0,0),foo)
  if lenders_paid == pay_size && collaterals_satisfied == loan_size {
    True
  } else {
    error @"Missing required outputs"
  }
}