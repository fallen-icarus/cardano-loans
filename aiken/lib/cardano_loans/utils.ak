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
use aiken/int
use aiken/string
use aiken/time.{PosixTime}
use aiken/interval.{Interval,IntervalBound,Finite}
use aiken/dict.{has_key,Dict}
use aiken/bytearray
use cardano_loans/types.{
  LoanDatum, BeaconRedeemer, AskDatum, OfferDatum, ActiveDatum, 
  MintAskBeacon, MintOfferBeacon, MintActiveBeacon, BurnBeacons
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
    Some(Inline(VerificationKeyCredential(skey))) -> has(extra_signatories,skey) 

    // If the address uses a staking script, it must have been executed in the tx.
    Some(svh) -> has_key(withdrawals,svh)
  }
}

pub fn lender_approves(
  lender_id: AssetName, 
  sigs: List<Hash<Blake2b_224, VerificationKey>>,
  wtdr: Dict<StakeCredential, Int>
) -> Bool {
  // Since only the credential hash is captured by the LenderID, whether the credential is a pubkey
  // or a script is lost. Therefore, both must be tried. At least one of them should succeed.
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

// This is used to parse the datums attached to loan payments. It must include the beacon policy
// id and the LoanID for the loan being paid. This is to prevent double satisfaction by creating
// unique UTxOs for every loan payment output.
fn parse_payment_datum(raw_datum: Datum) -> (PolicyId,AssetName) {
  when raw_datum is {
    InlineDatum(d) -> {
      expect datum: (PolicyId,AssetName) = d
      datum
    }

    _ -> error @"All payment datums must be inline datums"
  }
}

// Convert a pubkey hash or script hash to an AssetName.
fn credential_to_asset_name(cred: Credential) -> AssetName {
  when cred is {
    VerificationKeyCredential(pkh) -> pkh
    ScriptCredential(sh) -> sh
  }
}

// Check if the address is using a payment pubkey. All lender addresses must use a payment pubkey
// since payments to script addresses must include a datum. This requirement simplifies the payment
// logic.
pub fn address_uses_payment_pubkey(addr: Address) -> Bool {
  when addr is {
    Address(VerificationKeyCredential(_),_) -> True
    _ -> False
  }
}

// Get the total value of a list of inputs.
pub fn total_input_value(ins: List<Input>) -> Value {
  let foo = fn (i: Input ,acc: Value) {
    let Input(_,Output(_,val,_,_)) = i
    value.merge(acc,val)
  }

  list.foldr(ins,value.zero(),foo)
}

// Checks that the target tokens are all burned (inputs == amount burned).
pub fn all_beacons_burned(
  sym: PolicyId,
  name: AssetName, 
  ival: Value,
  minted: MintedValue
) -> Bool {
  // The value of the token in the input must be the negation of the value of the same token
  // minted/burned.
  if value.quantity_of(ival,sym,name) == -value.quantity_of(value.from_minted_value(minted),sym,name) { 
    True 
  } 

  // Beacons were not all burned.
  else { 
    error @"Beacons not burned" 
  }
}

// This function either throws an error or returns True.
pub fn mint_check(redeemer: BeaconRedeemer, mint: Dict<AssetName,Int>) -> Bool {
  when redeemer is {

    MintAskBeacon(_) -> { 
      when dict.to_list(mint) is { 
        // Only one token can be minted/burned by the policy and it must be the Ask beacon.
        [(beacon_name,mint_number)] -> {
          // Any number of Ask beacons can be minted.
          if beacon_name == "Ask" && mint_number >= 1 { 
            True 
          }

          // If the previous check passed, this is a valid mint.
          else {
            error @"Only Ask beacons can be minted and they must have the token name 'Ask'" 
          }
        }

        // Fail if more than one token minted/burned by the policy.
        _ -> error @"Only Ask beacons can be minted and they must have the token name 'Ask'"
      }
    }

    MintOfferBeacon(lender_cred) -> {
      when dict.to_list(mint) is {
        // Only the Offer beacon and LenderID can be minted because its transaction hash is used
        // as the LoanID for the resulting loan. All Offer beacons must have unique transaction
        // hashes.
        [(name1,num1),(name2,num2)] -> {
          // Check if the first asset is the Offer beacon.
          if name1 == "Offer" {
            // Only one can be minted per tx.
            if num1 != 1 { 
              error @"Only one Offer beacon can be minted"
            } 

            // The other asset must be the LenderID.
            else if name2 != credential_to_asset_name(lender_cred) { 
              error @"LenderID does not have lender's credential as token name"
            }

            // Only one LenderID can be minted. 
            else if num2 != 1 { 
              error @"Only one LenderID can be minted" 
            } 

            // If the previous checks passed, this is a valid mint.
            else { 
              True 
            }
          } 

          // Check if the second asset is the Offer beacon.
          else if name2 == "Offer" {
            // Only one Offer beacon can be minted.
            if num2 != 1 { 
              error @"Only one Offer beacon can be minted" 
            }

            // The first asset must be the LenderID.
            else if name1 != credential_to_asset_name(lender_cred) { 
              error @"LenderID does not have lender's credential as token name" 
            } 

            // Only one LenderID can be minted.
            else if num1 != 1 { 
              error @"Only one LenderID can be minted" 
            } 

            // If the previous checks passed, this is a valid mint.
            else { 
              True 
            }
          } 

          // The wrong assets were minted/burned.
          else { 
            error @"Only the Offer beacon and LenderID can be minted" 
          }
        }

        // The wrong number of assets were minted/burned.
        _ -> error @"Only the Offer beacon and LenderID can be minted"
      }
    }

    MintActiveBeacon(_,_) -> error // Never used with this redeemer.

    BurnBeacons -> {
      // This redeemer can only be used to burn.
      if dict.foldl(mint,True,fn (_,v,r) {r && v < 0}) { 
        True 
      } 

      // Assets were minted.
      else { 
        error @"This redeemer can only be used to burn"
      }
    }
  }
}

// This function either returns True or crashes with an appropriate error message. Checks
// that the beacons are stored with the proper datums.
fn valid_datum(sym: PolicyId, redeemer: BeaconRedeemer, datum: LoanDatum) -> Bool {
  // The redeemers must be paired with the proper datum type.
  when (redeemer,datum) is {
    ( MintAskBeacon(borrower_cred),
      AskDatum(loan_beacon_sym,borrower_id,_,loan_principle,loan_term,collateral)
    ) -> {
      // The beacon symbol must be this policy id.
      if loan_beacon_sym != sym { 
        error @"Invalid AskDatum loan_beacon_sym" 
      } 

      // The borrower_id must match the credential in the redeemer.
      else if borrower_id != credential_to_asset_name(borrower_cred) { 
        error @"Invalid AskDatum borrower_id" 
      }

      // The loan principle must be greater than 0.
      else if loan_principle <= 0 { 
        error @"Invalid AskDatum loan_principle" 
      } 

      // The loan term must be greater than 0.
      else if loan_term <= 0 { 
        error @"Invalid AskDatum loan_term" 
      } 

      // The collateral list must not be empty.
      else if is_empty(collateral) { 
        error @"AskDatum collateral is empty" 
      } 

      // If the previous checks passed, this is a valid AskDatum.
      else 
        { True }
    }

    // Fail if the Ask beacon is not stored with an AskDatum.
    (MintAskBeacon(_),_) -> error @"Ask beacon must be stored with an AskDatum"

    ( MintOfferBeacon(lender_cred),
      OfferDatum(
        beacon_sym,lender_id,lender_address,_,principle,checkpoints,term,interest,
        collateralization,claim_period)
    ) -> {
      // Beacon symbol must be this policy id.
      if beacon_sym != sym { 
        error @"Invalid OfferDatum loan_beacon_sym"
      }

      // The LenderID must be for the credential in the redeemer.
      else if lender_id != credential_to_asset_name(lender_cred) { 
        error @"Invalid OfferDatum lender_id" 
      } 

      // The lender address must use a payment pubkey.
      else if !address_uses_payment_pubkey(lender_address) { 
        error @"Lender address must use payment pubkey"
      } 

      // The principle must be greater than 0.
      else if principle <= 0 { 
        error @"Invalid OfferDatum loan_principle"
      } 

      // The loan term must be greater than 0.
      else if term <= 0 { 
        error @"Invalid OfferDatum loan_term" 
      } 

      // The interest rate must be greater than or equal to 0.
      else if ratio.lt(interest,ratio.zero()) { 
        error @"Invalid OfferDatum loan_interest" 
      } 

      // The claim period must be greater than 0.
      else if claim_period <= 0 { 
        error @"Invalid OfferDatum claim_period" 
      } 

      // The collateralization list must not be empty.
      else if is_empty(collateralization) 
        { error @"OfferDatum collateralization list is empty" } 

      // All collateralization prices must be greater than or equal to 0.
      else if !all(
        collateralization,
        fn(x) {
          let (_,n) = x 
          ratio.gte(n,ratio.zero())
        }
      ) { 
        error @"All collateralizations must be >= 0" 
      }

      // All checkpoints must be greater than 0.
      else if !all(checkpoints,fn(n) {n > 0}) { 
        error @"All checkpoints must be > 0" 
      }

      // The checkpoints must be in ascending order. 
      else if list.sort(checkpoints,int.compare) != checkpoints { 
        error @"Checkpoints must be in ascending order" 
      } 

      // The last checkpoint must be less than the loan term. If the list is empty,
      // automatically returns True.
      else {
        when list.last(checkpoints) is {
          Some(x) -> {
            if x >= term { 
              error @"Final checkpoint must be before expiration" 
            } 

            // If all the previous checks passed, this is a valid OfferDatum.
            else { 
              True 
            }
          }

          // If all the previous checks passed, this is a valid OfferDatum.
          _ -> True
        }
      }
    }

    // Fail if the Offer beacon is not stored with an OfferDatum.
    (MintOfferBeacon(_),_) -> error @"Offer beacons must be stored with an OfferDatum"

    _ -> False // Never used with ActiveDatums
  }
}

// This returns True or crashes with the appropriate error message.
fn loan_principle_met(oval: Value, datum: LoanDatum) -> Bool {
  expect OfferDatum(_,_,_,(sym,name),principle,..) = datum
  // Handle the case where the loan asset is ADA since the deposit is also ADA.
  if sym == "" {
    // The quantity of ADA must be 5 + the loan amount.
    if value.quantity_of(oval,sym,name) != 5_000_000 + principle { 
      error @"Offer not stored with 5 ADA + loan amount" 
    } 

    // If the previous check passed, the principle was met.
    else { 
      True 
    }
  }

  // Handle the case where the loan asset is not ADA.
  else {
    // The UTxO must have 5 ADA and the loan amount.
    if value.lovelace_of(oval) != 5_000_000 || value.quantity_of(oval,sym,name) != principle { 
      error @"Offer not stored with 5 ADA + loan amount"
    }

    // If the previous check passed, the loan principle was met.
    else { 
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
          // It can only have one Ask beacon. Ask beacons must be stored individually.
          if ask_num != 1 { 
            error @"Ask beacons must be stored individually" 
          } 

          // Check that the output goes to the proper address.
          else {
            when (pay,stake) is {
              (ScriptCredential(vh),Some(Inline(s_cred))) -> {
                // It must be locked at a dApp address. This uses the app_name in the
                // error message to create unique beacons based on the setting of that
                // variable.
                if vh != dapp_hash {
                  let msg = 
                    string.concat(@"Beacon must go to a ",app_name) 
                    |> string.concat(_,@" dApp address")
                  error msg
                } 

                // It must be locked at the dApp address for the borrower in the redeemer.
                else if s_cred != borrower_cred { 
                  error @"Receiving address does not match redeemer staking credential"
                }

                // It must have a valid AskDatum. This will crash with an error unless True.
                else { 
                  acc && valid_datum(sym,redeemer,parse_datum(d))
                }
              }

              // Beacon went to the wrong address.
              _ -> error @"Beacon must go to a dapp address with a valid staking credential"
            }
          }
        }

        // If it does not have an Ask beacon, it is a benign output.
        else { 
          acc 
        }
      }

      MintOfferBeacon(lender_cred) -> {
        // Check if the output has an Offer beacon. Only one was minted in this transaction.
        if value.quantity_of(val,sym,"Offer") == 1 {
          // The Offer beacon and LenderID must be stored together.
          if value.quantity_of(val,sym,credential_to_asset_name(lender_cred)) != 1 {
            error @"Offer beacon and LenderID must be stored together"
          } 

          // The output must be locked at a dApp address with a staking credential.
          else {
            when (pay,stake) is {
              (ScriptCredential(vh),Some(Inline(_))) -> {
                // It must go to an address protected by the loan validator script.
                if vh != dapp_hash {
                  let msg = 
                    string.concat(@"Beacon must go to a ",app_name) 
                    |> string.concat(_,@" dApp address")
                  error msg
                }

                // The Offer beacon must be stored with the proper datum and value.
                else {
                  let datum = parse_datum(d)
                  // valid_datum and loan_principle_met fail with an error unless
                  // True.
                  acc && valid_datum(sym,redeemer,datum) && loan_principle_met(val,datum)
                }
              }

              // Offer beacon went to the wrong address.
              _ -> error @"Beacons must go to a dapp address with a valid staking credential"
            }
          }
        } 
        
        // If the output does not have an Offer beacon, it is a benign output.
        else { 
          acc 
        }
      }

      _ -> False // Not used for MintActiveBeacon and BurnBeacons.
    }
  }

  list.foldl(outputs,True,foo)
}

fn compare_out_ref(ref1: OutputReference, ref2: OutputReference) -> Ordering {
  let OutputReference(TransactionId(tx_id1),out_index1) = ref1
  let OutputReference(TransactionId(tx_id2),out_index2) = ref2
  when bytearray.compare(tx_id1,tx_id2) is {
    Less -> Less
    Greater -> Greater
    Equal -> int.compare(out_index1,out_index2)
  }
}

// They should never be equal since the LoanID is supposed to be unique so it 
// will crash if two are equal.
fn compare_active_datums(datum1: LoanDatum, datum2: LoanDatum) -> Ordering {
  expect ActiveDatum(_,_,_,_,_,_,_,_,_,_,_,_,_,loan_id1) = datum1
  expect ActiveDatum(_,_,_,_,_,_,_,_,_,_,_,_,_,loan_id2) = datum2
  when bytearray.compare(loan_id1,loan_id2) is {
    Less -> Less
    Greater -> Greater
    Equal -> error @"Duplicate LoanID found"
  }
}

// This is only used on values with a LoanID so it will either throw an error if the LoanID
// is in both values or return Less; the ordering does not matter.
fn compare_value(loan_id: AssetName, sym: PolicyId, val1: Value, val2: Value) -> Ordering {
  // Check if the values have the same LoanID.
  if value.quantity_of(val1,sym,loan_id) == value.quantity_of(val2,sym,loan_id) { 
    error @"LoanID used twice in lender payments" 
  } 

  // Otherwise, return Less.
  else { 
    Less 
  }
}

// Get the loan start time from the tx's validity range. Based off the lower bound.
// This is also used when a lender is claiming a loan to get the claim time.
pub fn get_start_time(val_range: ValidityRange) -> PosixTime {
  when val_range is {
    Interval(IntervalBound(Finite(x),_),_) -> x
    _ -> error @"invalid-before not specified"
  }
}

// Get the loan payment time from the tx's validity range. Based off the upper bound.
pub fn get_payment_time(val_range: ValidityRange) -> PosixTime {
  when val_range is {
    Interval(_,IntervalBound(Finite(x),_)) -> x
    _ -> error @"invalid-hereafter not specified"
  }
}

// Returns what the loan output should have for the ActiveDatum based on the inputs that were paired.
// Ensures that the inputs agree on the terms. The beacon policy already checked that the datums
// agreed on the beacon symbol.
fn expected_active_datum(
  start: PosixTime,
  ask_datum: LoanDatum,
  offer_datum: LoanDatum,
  loan_id: AssetName
) -> LoanDatum {
  // Pattern match out the required terms from the AskDatum.
  expect AskDatum(
    _,
    ask_borrower_id,
    ask_asset,
    ask_principle,
    ask_term,
    ask_collateral
  ) = ask_datum

  // Pattern match out the required terms from the OfferDatum.
  expect OfferDatum(
    offer_sym,
    _,
    offer_lender_address,
    offer_asset,
    offer_principle,
    offer_checkpoints,
    offer_term,
    offer_interest,
    offer_collateralization,
    offer_claim_period
  ) = offer_datum

  // The datums must agree on the loan asset.
  if ask_asset != offer_asset { 
    error @"Datums do not agree on loan asset" 
  } 
  
  // The datums must agree on the loan amount.
  else if ask_principle != offer_principle { 
    error @"Datums do not agree on loan principle" 
  } 

  // The datums must agree on the loan expiration.
  else if ask_term != offer_term { 
    error @"Datums do not agree on loan term" 
  }

  // The datums must agree on the allowed collateral. They must be in the same order.
  else if ask_collateral != list.map(offer_collateralization,fn(x) {let (a,_) = x a}) { 
    error @"Datums do not agree on collateral"
  }

  // If the previous checks passed, then the datums agree on the terms.
  // Produce the expected datum. Convert the times to absolute times based on the start time.
  else {
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
      claim_expiration: start + offer_term + offer_claim_period,
      loan_expiration: start + offer_term,
      loan_outstanding: ratio.from_integer(offer_principle),
      loan_id: loan_id
    }
  }
}

// Check if the newly accepted loan has enough collateral backing it.
fn is_enough_collateral(
  principle_taken: Rational,
  collats: List<((PolicyId,AssetName),Rational)>,
  oval: Value
) -> Bool {
  let foo = fn (collat: ((PolicyId,AssetName),Rational), acc: Rational) {
    // Pattern match out the info for that asset.
    let ((policy,name),price) = collat

    // If the price is zero, the accumulator will be unchanged. This must be explicitly skipped
    // since the reciprical of zero is undefined.
    if price == ratio.zero() { 
      acc 
    } 
    
    // Calculate the value of the collateral in terms of the loan asset. Add it to the
    // accumulator.
    else {
      value.quantity_of(oval,policy,name) // Get the quantity of that collateral asset.
      |> ratio.from_integer(_) // Convert it to Rational.
      |> ratio.multiply(_,ratio.recip(price)) // Divide it by the price.
      |> ratio.add(acc,_) // Add it to the accumulator.
    }
  }

  // Collateral posted >= principle taken
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
) -> (Dict<OutputReference,LoanDatum>,Int,Dict<OutputReference,LoanDatum>,Int) {
  let foo = fn (
    in: Input, 
    acc: (Dict<OutputReference,LoanDatum>,Int,Dict<OutputReference,LoanDatum>,Int)
  ) {
    let Input(out_ref,Output(Address(pay_cred,stake_cred),val,d,_)) = in
    let (ask_map,asize,offer_map,osize) = acc

    // Check if the input has an Ask beacon. This can only be from a dApp address.
    if value.quantity_of(val,sym,"Ask") == 1 {
      // Check if it comes from the correct dApp address.
      if stake_cred == Some(Inline(borrower_cred)) {
        // The datum is guaranteed to be an AskDatum since the "Ask" beacon is present.
        let datum = parse_datum(d)

        // Insert the new ask input into the ask_map.
        let new_ask_map = dict.insert(ask_map,out_ref,datum,compare_out_ref)

        // Increase the size of the ask_map by one.
        let new_asize = asize + 1

        // Return the accumulator.
        (new_ask_map,new_asize,offer_map,osize)
      } 

      // This Ask input comes from a different loan address. Loan addresses are not meant to
      // compose.
      else { 
        error @"Input from wrong dApp address" 
      }
    }

    // Check if input has an Offer beacon. This can only be from a dApp address.
    else if value.quantity_of(val,sym,"Offer") == 1 {
      // Check if it comes from the correct dApp address.
      if stake_cred == Some(Inline(borrower_cred)) {
        // The datum is guaranteed to be an OfferDatum since the "Offer" beacon is present.
        let datum = parse_datum(d) 

        // Insert the new offer input into the offer_map.
        let new_offer_map = dict.insert(offer_map,out_ref,datum,compare_out_ref)

        // Increase the size of the offer_map by one.
        let new_osize = osize + 1

        // Return the accumulator.
        (ask_map,asize,new_offer_map,new_osize)
      } 

      // The Offer input comes from a different loan address. Loan addresses are not meant to
      // compose.
      else { 
        error @"Input from wrong dApp address" 
      }
    }
    
    // Check if input comes from a dApp address. If it does and the previous two checks failed,
    // this must be an invalid input (missing beacon or has an Active beacon). It does not matter
    // if the loan address belongs to the borrower or not; it should fail regardless.
    else if pay_cred == ScriptCredential(dapp_hash) { 
      error @"Invalid loan input found" 
    }

    // Otherwise, it is a benign input and can be skipped.
    else { 
      acc 
    }
  }

  list.foldl(ins,(dict.new(),0,dict.new(),0),foo)
}

test input_maps_test1() {
  let borrower_cred = VerificationKeyCredential("00")
  let dapp_hash = "01"
  let offer_value = value.from_asset("00","Offer",1)
  let offer_datum = OfferDatum{
    loan_beacon_sym: "00",
    lender_id: "11",
    lender_address: Address(VerificationKeyCredential("11"),None),
    loan_asset: ("",""),
    loan_principle: 1000000,
    loan_checkpoints: [],
    loan_term: 10,
    loan_interest: ratio.unsafe_ratio(1,10),
    collateralization: [],
    claim_period: 10
  }
  let ask_value = value.from_asset("00","Ask",1)
  let ask_datum = AskDatum{
    loan_beacon_sym: "00",
    borrower_id: credential_to_asset_name(borrower_cred),
    loan_asset: ("",""),
    loan_principle: 1000000,
    loan_term: 10,
    collateral: []
  }
  let offer_input_ref = OutputReference(TransactionId("00"),0)
  let offer_input = Input(
    offer_input_ref,
    Output(
      Address(ScriptCredential(dapp_hash),Some(Inline(borrower_cred))),
      offer_value,
      InlineDatum(offer_datum),
      None
    )
  )
  let ask_input_ref = OutputReference(TransactionId("00"),1)
  let ask_input = Input(
    ask_input_ref,
    Output(
      Address(ScriptCredential(dapp_hash),Some(Inline(borrower_cred))),
      ask_value,
      InlineDatum(ask_datum),
      None
    )
  )

  input_maps(borrower_cred,[offer_input,ask_input],"00",dapp_hash) == (
    dict.from_list([(ask_input_ref,ask_datum)],compare_out_ref),
    1,
    dict.from_list([(offer_input_ref,offer_datum)],compare_out_ref),
    1
  )
}

// This compares the inputs against the pairings specified in the redeemer. It check:
// 1) length pairings == size ask map == size offer map
// 2) all ask and offer inputs are only paired up once
// 3) the input datums agree
// It returns a map of required outputs by ActiveDatums since LoanIDs guarantee uniqueness.
// Internally:
// - The pay_map is a map of outputs that must go to the lenders.
// - The loan_map is a map of collateral output datums that must go to the loan address.
// - The used_map keeps track of inputs that have already been used.
pub fn acceptance_input_checks(
  borrower_cred: Credential,
  ins: List<Input>,
  sym: PolicyId,
  dapp_hash: Hash<Blake2b_224,Script>,
  start_time: PosixTime,
  pairings: List<(OutputReference,OutputReference)>
) -> ( Dict<Value,Address>, Int, Dict<LoanDatum,Int>, Int, Value ) {
  // This will throw an error if any inputs are invalid.
  let (ask_map,asize,offer_map,osize) = input_maps(borrower_cred,ins,sym,dapp_hash)

  let foo = fn (
    pair: (OutputReference,OutputReference),
    acc: ( 
      Dict<OutputReference,Int>, // This will be dropped before returning the overall result.
      Dict<Value,Address>, 
      Int,
      Dict<LoanDatum,Int>,
      Int, 
      Value
    )
  ) {
    let (ask_ref,offer_ref) = pair
    let (used_map,pay_map,pay_size,loan_map,loan_size,req_mint) = acc
    let OutputReference(TransactionId(offer_id),_) = offer_ref

    // Lookup the paired inputs from the input maps.
    when (dict.get(used_map,ask_ref),dict.get(used_map,offer_ref)) is {
      // Neither inputs have been used yet.
      (None,None) -> {
        when (dict.get(ask_map,ask_ref),dict.get(offer_map,offer_ref)) is {
          (Some(ask_datum),Some(offer_datum)) -> {
            expect OfferDatum(_,lender_id,lender_address,..) = offer_datum

            // Calculate the amount owed to the lender upon acceptance:
            // 1) Offer deposit
            // 2) One of the two minted LoanIDs
            let pay_lender_val = 
              value.from_lovelace(5_000_000) 
              |> value.add(_,sym,offer_id,1)

            // Update the required mint:
            // 1) Burn 1 Ask beacon
            // 2) Burn 1 Offer beacon
            // 3) Mint 1 Active beacon
            // 4) Burn 1 LenderID - this isn't needed anymore
            // 5) Mint 2 LoanIDs with the name of the offer input tx hash
            // 6) Mint 1 BorrowerID
            let new_req_mint = 
              req_mint 
              |> value.add(_,sym,offer_id,2) 
              |> value.add(_,sym,lender_id,-1) 
              |> value.add(_,sym,"Ask",-1) 
              |> value.add(_,sym,"Offer",-1) 
              |> value.add(_,sym,"Active",1) 
              |> value.add(_,sym,credential_to_asset_name(borrower_cred),1)

            // Insert the lender payment information into the lender pay_map
            let new_pay_map = dict.insert(
              pay_map,
              pay_lender_val,
              lender_address,
              compare_value(offer_id,sym,_,_)
            )

            // Increment the size of the pay_map by one.
            let new_pay_size = pay_size + 1

            // Calculate the expected Active datum for the collateral output and check that
            // the inputs agree on the terms. This will crash with an error if the terms
            // do not agree.
            let req_datum = expected_active_datum(start_time,ask_datum,offer_datum,offer_id)

            // Add the collateral datum to the loan_map.
            let new_loan_map = dict.insert(loan_map,req_datum,0,compare_active_datums)

            // Increment the loan_map size by one.
            let new_loan_size = loan_size + 1

            // Add both used inputs to the used_map.
            let new_used_map = 
              used_map 
              |> dict.insert(_,offer_ref,1,compare_out_ref) 
              |> dict.insert(_,ask_ref,1,compare_out_ref)

            // Return the updated accumulator.
            (new_used_map,new_pay_map,new_pay_size,new_loan_map,new_loan_size,new_req_mint)
          }

          // At least one paired input was not found in the input maps.
          _ -> error @"Paired input not found"
        }
      }

      // At least one input has already been used.
      _ -> error @"Input already paired"
    }
  }

  // Check that there are the same number of Asks and Offers and at least one of each.
  if asize == osize && asize != 0 {
    let (_,pay_map,pay_size,loan_map,loan_size,req_mint) = list.foldl(
      pairings,
      (dict.new(),dict.new(),0,dict.new(),0,value.zero()),
      foo
    )

    // Check that all the inputs were paired up.
    // The loan_map size will be the same as the pay_map size.
    if pay_size == asize { 
      (pay_map,pay_size,loan_map,loan_size,req_mint) 
    } 

    // Not all inputs were paired up.
    else { 
      error @"Not all pairs found" 
    }
  } 

  // Wrong number of inputs.
  else { 
    error @"Wrong number of ask and offer inputs" 
  }
}

test acceptance_input_checks_test1() {
  let borrower_cred = VerificationKeyCredential("02")
  let dapp_hash = "01"
  let offer_value = value.from_asset("00","Offer",1)
  let offer_datum = OfferDatum{
    loan_beacon_sym: "00",
    lender_id: "11",
    lender_address: Address(VerificationKeyCredential("11"),None),
    loan_asset: ("",""),
    loan_principle: 1000000,
    loan_checkpoints: [],
    loan_term: 10,
    loan_interest: ratio.unsafe_ratio(1,10),
    collateralization: [],
    claim_period: 10
  }
  let ask_value = value.from_asset("00","Ask",1)
  let ask_datum = AskDatum{
    loan_beacon_sym: "00",
    borrower_id: credential_to_asset_name(borrower_cred),
    loan_asset: ("",""),
    loan_principle: 1000000,
    loan_term: 10,
    collateral: []
  }
  let offer_input_ref = OutputReference(TransactionId("00"),0)
  let offer_input = Input(
    offer_input_ref,
    Output(
      Address(ScriptCredential(dapp_hash),Some(Inline(borrower_cred))),
      offer_value,
      InlineDatum(offer_datum),
      None
    )
  )
  let ask_input_ref = OutputReference(TransactionId("00"),1)
  let ask_input = Input(
    ask_input_ref,
    Output(
      Address(ScriptCredential(dapp_hash),Some(Inline(borrower_cred))),
      ask_value,
      InlineDatum(ask_datum),
      None
    )
  )

  let lender_value = value.add(value.from_asset("00","00",1),"","",5000000)
  let active_datum = ActiveDatum{
    loan_beacon_sym: "00",
    borrower_id: credential_to_asset_name(borrower_cred),
    lender_address: Address(VerificationKeyCredential("11"),None),
    loan_asset: ("",""),
    loan_principle: 1000000,
    next_checkpoints: [],
    past_checkpoitns: [], // Included for credit history.
    loan_term: 10, // Included for credit history
    loan_interest: ratio.unsafe_ratio(1,10),
    collateralization: [],
    claim_expiration: 30, // The time at which collateral is considered "Lost".
    loan_expiration: 20,
    loan_outstanding: ratio.from_integer(1000000),
    loan_id: "00"
  }

  let req_mint = value.zero()
    |> value.add(_,"00","Offer",-1)
    |> value.add(_,"00","Ask",-1)
    |> value.add(_,"00","11",-1)
    |> value.add(_,"00","Active",1)
    |> value.add(_,"00","00",2)
    |> value.add(_,"00","02",1)

  acceptance_input_checks(
    borrower_cred,[offer_input,ask_input],"00",dapp_hash,10,[(ask_input_ref,offer_input_ref)]
  ) == (
    dict.from_list([(lender_value,Address(VerificationKeyCredential("11"),None))],compare_value("00","00",_,_)),
    // dict.new(),
    1,
    dict.from_list([(active_datum,0)],compare_active_datums),
    1,
    req_mint
  )
}

// This checks the outputs to ensure:
// 1) All lenders get the proper LoanID tokens and their deposits back.
// 2) All loans are properly collateralized at the borrower's address.
// 3) All collateral UTxOs have the proper LoanID token.
pub fn acceptance_output_checks(
  borrower_cred: Credential,
  sym: PolicyId,
  dapp_hash: Hash<Blake2b_224,Script>,
  pay_map: Dict<Value,Address>,
  pay_size: Int,
  loan_map: Dict<LoanDatum,Int>,
  loan_size: Int,
  outs: List<Output>
) -> Bool {
  let borrower_addr = Address(ScriptCredential(dapp_hash),Some(Inline(borrower_cred)))
  let borrower_token = credential_to_asset_name(borrower_cred)

  let foo = fn (out: Output, acc: (Int,Int)) {
    let (lenders_paid,collaterals_satisfied) = acc
    let Output(addr,val,d,_) = out

    // Check if output is a collateral output. Any output to the loan address is assumed
    // to be a collateral output. There is no other reason to output to the loan address.
    if addr == borrower_addr {
      let datum = parse_datum(d)
      // Check that the datum is the proper datum.

      when datum is {
        ActiveDatum(_,_,_,_,_,_,_,_,_,collats,_,_,borrowed,loan_id) -> {
          // Check if the output has enough collateral.
          if !is_enough_collateral(borrowed,collats,val) { 
            error @"Not enough collateral posted" 
          }
          
          // Check if the output has one LoanID.
          else if value.quantity_of(val,sym,loan_id) != 1 { 
            error @"Collateral output must have exactly one LoanID" 
          }
          
          // Check if the output has one BorrowerID. BorrowerIDs must be stored individually.
          else if value.quantity_of(val,sym,borrower_token) != 1 { 
            error @"Collateral output must have exactly one BorrowerID" 
          }

          // Check if the output has one Active beacon. Active beacons must be stored
          // individually.
          else if value.quantity_of(val,sym,"Active") != 1 { 
            error @"Collateral output must have exactly one Active beacon" 
          }

          // Check that the output has the proper datum.
          else { 
            // Check that the output has one of the expected datums. The value of the Map is a
            // dummy Int. The fact that the key is present means the datum is valid. The datum
            // was used to check the value of the output so the fact the previous checks passed
            // means the datum was stored with the proper value.
            when dict.get(loan_map,datum) is{
              None -> error @"Collateral ouptut has wrong datum"
              Some(_) -> 
                // Count the collateral as satisfied
                (lenders_paid, collaterals_satisfied + 1)
            }
          }
        }

        // Datum is not an ActiveDatum.
        _ -> error @"Loan output has wrong datum type"
      } 
    }

    // Check if the value is supposed to be a lender payment.
    else {
      when dict.get(pay_map,val) is {
        Some(lender_addr) -> {
          if lender_addr == addr {
            // Count the lender payment as satisfied.
            (lenders_paid + 1, collaterals_satisfied) 
          }

          // The above check can only fail if the LoanID was sent to the wrong address.
          else { 
            error @"LoanID sent to wrong address." 
          }
        }

        _ -> acc // Not meant to be a lender payment. Can be skipped.
      }
    }
  }

  let (lenders_paid,collaterals_satisfied) = list.foldl(outs,(0,0),foo)
  
  // All lenders must be paid and all loans must be satisfied with collateral.
  if lenders_paid == pay_size && collaterals_satisfied == loan_size { 
    True 
  } 
  
  // If the above check failed, then at least one required output is missing.
  else { 
    error @"Missing required outputs" 
  }
}

// Gets the amount paid to the lender, the flux for this loan's collateral, the new datum
// attached to the collateral output, and whether or not the BorrowerID is included in the
// collateral output. They are returned so they can be checked later. One loan is checked
// per execution. This also checks that outputs are valid.
pub fn payment_info(
  lender_addr: Address,
  sym: PolicyId,
  loan_asset: (PolicyId,AssetName),
  loan_id: AssetName,
  starting_val: Value, // Collateral starting value.
  loan_addr: Address,
  borrower_id: AssetName,
  outs: List<Output>
) -> (Int,Value,Option<(LoanDatum,Bool)>) {
  let (policy,name) = loan_asset
  let foo = fn (out: Output, acc: (Int,Value,Option<(LoanDatum,Bool)>)) {
    let Output(addr,val,d,_) = out
    let (paid,flux,collat_info) = acc

    // Check if this output is to the lender.
    if addr == lender_addr {
      // The output must have a unique datum that links it to the target loan.
      // This guarantees a unique output for every loan payment to prevent double satisfaction.
      // The payment will only count if the datum is correct. This flexibility allows the borrower
      // to make multiple payments to a given lender when they have multiple loans from that lender.
      // The policy id must be included since this protocol can compose with other p2p-DeFi protocols.
      // For example, one tx can mint both an Asset UTxO (from cardano-options) and an Offer UTxO.
      // Since they share a transaction hash, payments for one protocol can possibly double satisfy the
      // other. Using the full output reference instead of just the transaction hash can also be used
      // to prevent double satisfaction but that would make audibility harder. The policy id 
      // implicitly says what the payment is for; the same is not true for the full output reference.
      if parse_payment_datum(d) != (sym,loan_id) { 
        acc 
      } 
      
      else {
        (paid + value.quantity_of(val,policy,name), flux, collat_info) 
      }
    } 
    
    // Check if the output contains a LoanID.
    else if value.quantity_of(val,sym,loan_id) == 1 {
      // The LoanID must be re-output to the loan address.
      if addr != loan_addr { 
        error @"LoanID sent to wrong address" 
      }

      // The output must have the Active beacon.
      else if value.quantity_of(val,sym,"Active") != 1 { 
        error @"Collateral output missing Active beacon" 
      }

      // Return the info.
      else { 
        ( paid,
          value.merge(flux,value.negate(val)),
          // Return the datum used and whether or not the BorrowerID is present in output.
          Some(
            (parse_datum(d), value.quantity_of(val,sym,borrower_id) == 1)
          )
        )
      }
    }

    // Check if a BorrowerID is found in any other UTxO. This check is meant to make it so
    // that a BorrowerID from a fully paid loan cannot be found in any output UTxO.
    // It must be burned.
    else if value.quantity_of(val,sym,borrower_id) > 0 {
      // Check if the BorrowerID is grouped up with another loan. This prevents the BorrowerID
      // from being moved to another active loan.
      if value.quantity_of(val,sym,borrower_id) != 1 { 
        error @"One BorrowerID per loan"
      }
      
      // Check if BorrowerID is stored with a valid Active Loan. This prevents the BorrowerID
      // from being withdrawn from the loan address.
      else if value.quantity_of(val,sym,"Active") != 1 { 
        error @"BorrowerID not stored with valid loan" 
      }

      // If the above two checks passed, then any UTxO with a BorrowerID should be valid.
      // They cannot go anywhere and therefore extra IDs must be burned.
      else { 
        acc 
      }
    }

    // If the output does not contain a LoanID or a BorrowerID, this is a benign output.
    else { 
      acc 
    }
  }

  list.foldl(outs,(0,starting_val,None),foo)
}

// Returns True or throws an error.
pub fn collateral_taken_check(
  in_val: Value,
  collat_flux: Value,
  paid: Rational,
  outstanding: Rational,
  collateralization: List<((PolicyId,AssetName),Rational)>
) -> Bool {
  // Calculate the proportion of the loan that was repaid.
  let goal = ratio.multiply(paid,ratio.recip(outstanding))

  // Returns the the total amount of collateral taken (in units of the loan asset) and
  // the total starting balance for collateral (in units of the loan asset).
  let foo = fn (collat: ((PolicyId,AssetName),Rational), acc: (Rational,Rational)) {
    let ((policy,name),price) = collat
    let (collat_taken,starting_collat) = acc

    // If the price is zero, the accumulator will remain unchanged.
    // The lender does not care about this asset.
    if price == ratio.zero() { 
      acc 
    }

    // Calculate the proportion taken for this collateral asset (in terms of the loan asset).
    else {
      let recip_price = ratio.recip(price)
      let new_starting_collat = 
        value.quantity_of(in_val,policy,name) // Get quantity of collateral in starting balance.
        |> ratio.from_integer(_) // Convert to Rational.
        |> ratio.multiply(_,recip_price) // Convert to units of the loan asset.
        |> ratio.add(_,starting_collat) // Add to running total.

      // It is possible for the quantity of the collateral to be negative in the case where 
      // collateral is deposited. If this asset is deposited, more of another asset can be taken.
      let new_collat_taken = 
        value.quantity_of(collat_flux,policy,name) // Get quantity fo collateral taken.
        |> ratio.from_integer(_) // Convert to Rational.
        |> ratio.multiply(_,recip_price) // Convert to units of the loan asset.
        |> ratio.add(_,collat_taken) // Add to running total.

      (new_collat_taken,new_starting_collat)
    }
  }

  // The proportion of collateral taken must be less than or equal to the proportion of the
  // loan repaid.
  let (collat_taken,starting_collat) = 
    list.foldl(collateralization,(ratio.zero(),ratio.zero()),foo)

  let prop_collat_taken = ratio.multiply(collat_taken,ratio.recip(starting_collat))

  if ratio.gt(prop_collat_taken,goal) { 
    error @"Too much collateral taken" 
  }
  
  else { 
    True 
  }
}

// Returns True or False. Used for rollovers and updating lender address. It cannot throw an
// error in the case of composability. Therefore, the False must be handled differently.
pub fn update_datum_check(
  loan_addr: Address,
  req_val: Value,
  req_datum: LoanDatum,
  outs: List<Output>
) -> Bool {
  let foo = fn (out: Output, acc: Bool) {
    let Output(addr,oval,d,_) = out

    when addr == loan_addr is {
      // If this output is not to the loan address, it can't possibly be the proper output.
      False -> acc

      True -> {
        // Since there can be multiple outputs to the loan address in the case of composability,
        // this function must first check if the proper output has already been found.

        // If acc is True, the proper output has already been found and all other outputs can be
        // skipped.
        if acc {
          acc
        }

        // Since acc is still False, the proper output has yet to be found.
        else {
          // Both the datum and value must be correct.
          parse_datum(d) == req_datum && req_val == oval
        }
      }
    }
  }

  if !list.foldl(outs,False,foo) { 
    error @"Updated output not found - datum and value must be correct" 
  }

  else { 
    True 
  }
}