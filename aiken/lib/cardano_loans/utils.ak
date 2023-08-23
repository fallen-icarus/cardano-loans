use aiken/bytearray as byte
use aiken/dict.{Dict}
use aiken/hash.{Hash,Blake2b_224}
use aiken/list
use aiken/int
use aiken/math/rational.{Rational} as ratio
use aiken/option.{is_some}
use aiken/string
use aiken/interval.{Interval,IntervalBound,Finite}
use aiken/time.{PosixTime}
use aiken/transaction.{
  Datum,InlineDatum,Input,Output,OutputReference,TransactionId,ValidityRange
  } as tx
use aiken/transaction/credential.{
  Address,
  Credential,
  Inline,
  Script,
  ScriptCredential,
  StakeCredential,
  VerificationKey,
  VerificationKeyCredential
  }
use aiken/transaction/value.{PolicyId,AssetName,MintedValue,Value}

use cardano_loans/types.{
  ActiveDatum, AskDatum, BeaconRedeemer, BurnBeacons,
  CreateActive, CreateAsk, CreateOffer, LoanDatum, OfferDatum
}

const app_name = @"testing"

pub fn trace_if_false(msg: String, predicate: Bool) {
  if !predicate {
    trace msg
    predicate
  } else {
    predicate
  }
}

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
  wtdr: Dict<StakeCredential, Int>,
  sigs: List<Hash<Blake2b_224, VerificationKey>>
) -> Bool {
  when staking_cred is {
    // This is to prevent permanent locking of funds. Beacons can never be minted to an address
    // without a valid staking credential.
    None -> True

    // If the address uses a staking pubkey, it must have signed the tx.
    Some(Inline(VerificationKeyCredential(skey))) -> list.has(sigs,skey)

    // If the address uses a staking script, it must have been executed in the tx. In order for
    // the staking credential to show up in this dictionary, it must be registered.
    Some(svh) -> dict.has_key(wtdr,svh)
  }
}

// Check whether the lender approved the transaction. This assumes the lender is using a staking
// credential.
pub fn lender_approves(
  lender_id: AssetName, 
  wtdr: Dict<StakeCredential, Int>,
  sigs: List<Hash<Blake2b_224, VerificationKey>>
) -> Bool {
  // Since only the credential hash is captured by the LenderID, whether the credential is a pubkey
  // or a script is lost. Therefore, both must be tried. At least one of them should succeed.
  staking_credential_approves(Some(Inline(VerificationKeyCredential(lender_id))),wtdr,sigs) ||
  staking_credential_approves(Some(Inline(ScriptCredential(lender_id))),wtdr,sigs)
}

// Convert a pubkey hash or script hash to an AssetName.
fn credential_to_asset_name(cred: Credential) -> AssetName {
  when cred is {
    VerificationKeyCredential(pkh) -> pkh
    ScriptCredential(sh) -> sh
  }
}

// Create the asset name for the beacon by hashing the combination of the policy id and asset name.
// It uses the sha2_256 hash algorithm.
pub fn generate_beacon_name(asset: (PolicyId,AssetName)) -> AssetName {
  let (policy_id,asset_name) = asset
  hash.sha2_256(
    byte.concat( policy_id, asset_name )
  )
}

// Create the LoanID from the Offer OuptutReference by hashing the combination of the tx hash
// and the output index. It uses the sha2_256 hash algorithm.
pub fn generate_loan_id(offer_ref: OutputReference) -> AssetName {
  let OutputReference(TransactionId(tx_hash),out_index) = offer_ref
  hash.sha2_256(
    byte.concat(
      tx_hash,
      string.from_int(out_index) |> byte.from_string(_)
    )
  )
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

// This parses the datums attached to target UTxOs. It will fail with an error unless
// the datum is an inline LoanDatum.
fn parse_datum(raw_datum: Datum) -> LoanDatum {
  when raw_datum is {
    InlineDatum(d) -> {
      expect loan_datum: LoanDatum = d
      loan_datum
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

    _ -> error @"All datums must be inline datums"
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

// Check whether the proper beacons are properly minted/burned based on the redeemer.
pub fn mint_check(
  beacon_id: PolicyId,
  beacon_names: List<AssetName>, // sha2_256 hash of each loan asset in redeemer.
  redeemer: BeaconRedeemer,
  mint: MintedValue
) -> Bool {
  // Get the beacons minted/burned. Used no matter the redeemer.
  let beacons_minted = value.from_minted_value(mint) |> value.tokens(_,beacon_id)

  when redeemer is {
    CreateAsk(_,_) -> {
      // Checks:
      // 1) Only the Ask beacon and the Asset beacons are minted by this redeemer. All
      //    Asset beacons minted must be found in the redeemer.
      // 2) The Ask beacon must have the token name "Ask".
      // 3) The token name for the Asset beacons must be: sha2_256( asset_id ++ asset_name ).

      // Sorted expected names. Includes the Ask beacon name.
      let expected_names = 
        list.push(beacon_names,"Ask") 
        |> list.sort(_,byte.compare) // Must sort the list after adding the "Ask" name.

      // Sorted actual names. This will throw an error if any tokens are burned instead of minted.
      let actual_names = 
        dict.foldl(
          beacons_minted,
          [],
          fn(name,num,acc) { 
            // Crash if the token is burned instead of minted.
            expect True = error_if_false( @"Redeemer can only be used to mint", num > 0 )

            // The dictionary should already be sorted on the keys so add it to the end
            // to preserve ordering.
            list.concat(acc,[name]) 
          }
        )

      // Compare the expected token names (based on the redeemer) to the actual token names.
      trace_if_false(
        @"Only the Ask beacon and Asset beacons can/must be minted",
        expected_names == actual_names // Exact match.
      )
    }

    CreateOffer(lender_cred,_) -> {
      // Checks:
      // 1) Only the Offer beacon, LenderID, and Asset beacons can be minted with this redeemer.
      //    All assets whose Asset beacons will be minted must be found in the redeemer.
      // 2) The Offer beacon must have the token name "Offer".
      // 3) The LenderID must use the credential in the redeemer as the token name.
      // 4) The Asset beacon must have the token name: sha2_256( asset_policy_id ++ asset_name ).

      // Sorted expected names. Includes the Offer and LenderID beacon names.
      let expected_names = 
        list.concat(beacon_names,["Offer",credential_to_asset_name(lender_cred)]) 
        |> list.sort(_,byte.compare) // Must sort after adding the Offer beacon and LenderID.

      // Sorted actual names. This will throw an error if any tokens are burned instead of minted.
      let actual_names = 
        dict.foldl(
          beacons_minted,
          [],
          fn(name,num,acc) { 
            // Crash if the token is burned instead of minted.
            expect True = error_if_false( @"Redeemer can only be used to mint", num > 0 )

            // The dictionary should already be sorted on the keys so add it to the end
            // to preserve ordering.
            list.concat(acc,[name]) 
          }
        )

      // Compare the expected token names (based on the redeemer) to the actual token names.
      trace_if_false(
        @"Only the Offer beacon, LenderID, and Asset beacons can/must be minted",
        expected_names == actual_names // Exact match.
      )
    }

    CreateActive(_,_) -> error // Never used with this redeemer.

    BurnBeacons -> {
      // Always allowed as long as this redeemer is only used to burn.
      trace_if_false( 
        @"This redeemer can only be used to burn",
        dict.foldl( 
          beacons_minted,
          True, 
          fn(_,v,r) {r && v < 0} // Burn check.
        )
      )
    }
  }
}

// Check if the beacons are minted to the proper address and the UTxO is configured properly.
// This function will return True or False. It uses app_name in the error message to generate
// a unique beacon policy for testing. Create* redeemers are not meant to compose with each
// other.
pub fn destination_check(
  proxy_hash: Hash<Blake2b_224, Script>,
  dapp_hash: Hash<Blake2b_224, Script>,
  beacon_id: PolicyId,
  beacon_names: List<AssetName>,
  loan_assets: List<(PolicyId,AssetName)>,
  redeemer: BeaconRedeemer,
  outputs: List<Output>
) -> Bool {
  // Create a map from beacon names to loan assets. They are already in the same order.
  let name_map = list.zip(beacon_names,loan_assets) |> dict.from_list(_,byte.compare)

  let foo = fn (out: Output, acc: Bool) {
    // Extract out the output's information.
    let Output(Address(pay,stake),val,d,_) = out

    // All of the previous inputs need to be valid and this input needs to be valid.
    acc &&

    // The following checks depend on what redeemer is being used.
    when redeemer is {
      CreateAsk(req_borrower_id,_) -> {
        // Checks:
        // 1) The Ask UTxO must have exactly 1 Ask beacon and 1 Asset beacon. No other beacons
        //    can be in the UTxO.
        // 2) The Ask UTxO must be locked at a dapp address that is using the credential in the redeemer
        //    as the staking credential.
        // 3) The Ask UTxO must have a valid inline AskDatum:
        //  - loan_beacon_sym == this policy id.
        //  - borrower_id == credential in redeemer as a token name.
        //  - loan_asset == asset in redeemer that corresponds to the Asset beacon stored in UTxO.
        //  - loan_principle > 0.
        //  - loan_term > 0.
        //  - collateral list must not be empty.

        // Get the beacons in this UTxO and pattern match on it.
        when value.tokens(val,beacon_id) |> dict.to_list() is {
          // Valid Ask UTxOs should have exactly two types of beacons: 1 Ask and 1 Asset.
          [(name1,num1),(name2,num2)] -> {
            // Need to figure out which one is which so that the loan_asset field in the datum
            // can be checked to make sure it matches the Asset beacon.
            let loan =
              if name1 == "Ask" { name2 }
              else if name2 == "Ask" { name1 }
              else { error @"Ask beacon not found in beacon output" }

            // Get the required loan_asset based on the Asset beacon name.
            let req_loan_asset = when dict.get(name_map,loan) is {
              None -> error @"Asset beacon does not match any asset in redeemer"
              Some(x) -> x
            }

            // There can only be 1 Ask beacon and 1 Asset beacon in the UTxO.
            trace_if_false(
              @"UTxO has more than 1 of the phase beacon or the Asset beacon",
              num1 == 1 && num2 == 1
            ) &&
            
            // The beacons must be stored at a dApp address with the same staking credential
            // as in the redeemer. Uses app_name in the error message.
            trace_if_false( 
              string.concat(@"Beacons must go to a ", app_name) 
                |> string.concat(_,@" dApp address with the redeemer staking credential"),
              pay == ScriptCredential(dapp_hash) && stake == Some(Inline(req_borrower_id))
            ) &&

            // The beacons must be stored with the proper datum.
            when parse_datum(d) is {
              AskDatum(beacon_sym, borrower_id, loan_asset, principle, term, collateral) -> {
                // The AskDatum must be correct.
                trace_if_false(
                  @"Invalid AskDatum",

                  // The loan_beacon_sym must be this policy id.
                  beacon_sym == beacon_id &&

                  // The borrower_id must be the same as the credential in the redeemer.
                  borrower_id == credential_to_asset_name(req_borrower_id) &&

                  // The loan_asset must be the corresponding asset for the Asset beacon present.
                  // The loan asset associated with this beacon should be found in the map created
                  // from the redeemer.
                  loan_asset == req_loan_asset &&

                  // The loan principle must be > 0.
                  principle > 0 &&

                  // The loan term must be > 0.
                  term > 0 &&

                  // The collateral list must not be empty.
                  !list.is_empty(collateral)
                )
              }

              _ -> error @"Beacons not stored with AskDatum"
            }
          }

          // If no beacons are present, this output can be ignored.
          [] -> acc

          // Any other pattern is a sign of beacons being stored with the wrong value.
          _ -> error @"Beacons not minted to a valid Ask UTxO"
        }
      }

      CreateOffer(req_lender_id,_) -> {
        // Checks:
        // 1) The Offer UTxO must have exactly 1 Offer beacon, 1 LenderID, 1 Asset beacon, the loan
        //    amount, and the amount of ADA specified in in the offer_deposit field of the datum.
        //    No other assets are allowed.
        // 2) The Offer UTxO must be locked at a dapp address with a staking credential.
        // 3) The Offer UTxO must have a valid inline OfferDatum:
        //  - loan_beacon_sym == this policy id.
        //  - lender_id == credential in redeemer as token name.
        //  - lender_address must either use the proxy script or a pubkey as the payment credential,
        //    and if the proxy script is used, the address must have a staking credential.
        //  - loan_asset == asset in redeemer that corresponds to the Asset beacon stored in UTxO.
        //  - loan_principle > 0.
        //  - rollover_frequency > 0 if used.
        //  - loan_term > 0.
        //  - loan_interest must have a denominator > 0 and the interest must be >= 0.
        //  - min_payment >= 0.
        //  - collateralization list must not be empty, all relative prices must have a
        //    denominator > 0, and all relative prices >= 0.
        //  - claim_period > 0.
        //  - offer_deposit > 0.

        // Convert the lender credential to an asset name.
        let lender_name = credential_to_asset_name(req_lender_id)

        // Get the beacons in this UTxO and pattern match on it.
        when value.tokens(val,beacon_id) |> dict.to_list() is {
          // Valid Offer UTxOs should have exactly three types of beacons: 
          //  - 1 Offer beacon
          //  - 1 LenderID
          //  - 1 Asset beacon
          //
          // The actual number of each beacon will be checked at the end with an exact match
          // on the UTxO's value.
          [(name1,_),(name2,_),(name3,_)] -> {
            // Need to figure out which beacon is which so that the datums can be checked for
            // the proper loan_asset.
            let loan =
              if name1 == "Offer" {
                if name2 == lender_name { name3 }
                else if name3 == lender_name { name2 }
                else { error @"Offer beacon stored without LenderID" }
              } else if name2 == "Offer" {
                if name1 == lender_name { name3 }
                else if name3 == lender_name { name1 }
                else { error @"Offer beacon stored without LenderID" }
              } else if name3 == "Offer" {
                if name1 == lender_name { name2 }
                else if name2 == lender_name { name1 }
                else { error @"Offer beacon stored without LenderID" }
              } else { error @"Offer beacon not found with other beacons" }

            // Get the required loan_asset based on the Asset beacon name.
            let (asset_id,asset_name) as req_loan_asset = 
              when dict.get(name_map,loan) is {
                None -> error @"Asset beacon does not match any asset in redeemer"
                Some(x) -> x
              }

            // The beacons must be stored at a dApp address with staking. Uses app_name in
            // the error message.
            trace_if_false( 
              string.concat(@"Beacons must go to a ", app_name) 
                |> string.concat(_,@" dApp address with staking"),
              pay == ScriptCredential(dapp_hash) && is_some(stake)
            ) &&

            // The beacons must be stored with the proper datum and value.
            when parse_datum(d) is {
              OfferDatum(
                beacon_sym,
                lender_id,
                lender_address,
                loan_asset,
                principle,
                rollover_frequency,
                term,
                interest,
                min_payment,
                collateralization,
                _, // This is just True or False. No need to check validity.
                claim_period,
                offer_deposit
              ) -> {
                // The datum must be correct.
                trace_if_false(
                  @"Invalid OfferDatum",

                  // The loan_beacon_sym must be this policy id.
                  beacon_sym == beacon_id &&

                  // The lender_id must be the same as the credential in the redeemer.
                  lender_id == lender_name &&

                  // The lender_address must use a pubkey or the proxy script for the payment
                  // credential. If the proxy script is used, the address must have a staking
                  // credential.
                  valid_lender_address(proxy_hash,lender_address) &&

                  // The loan_asset must be the corresponding asset for the beacon present.
                  loan_asset == req_loan_asset &&

                  // The loan principle must be > 0.
                  principle > 0 &&

                  // The loan term must be > 0.
                  term > 0 &&

                  // If the rollover_frequency is set, it must be > 0.
                  when rollover_frequency is {
                    None -> True
                    Some(f) -> f > 0
                  } &&

                  // The loan interest must have a denominator > 0 and the interest must be > 0.
                  ratio.denominator(interest) > 0 && ratio.compare(interest,ratio.zero()) != Less &&

                  // The min_payment must be >=0.
                  min_payment >= 0 &&

                  // The collateralization list must not be empty, all prices must have a denominator
                  // > 0, and all prices must be >= 0.
                  !list.is_empty(collateralization) &&
                  list.all(
                    collateralization,
                    fn(x) {
                      let (_,n) = x
                      ratio.compare(n,ratio.zero()) != Less && ratio.denominator(n) > 0
                    }
                  ) &&

                  // The claim_period must be > 0.
                  claim_period > 0 &&

                  // The offer_deposit must be > 0.
                  offer_deposit > 0
                ) &&

                // The UTxO value must be exact:
                //      1 Offer beacon 
                //    + 1 LenderID 
                //    + 1 Asset beacon
                //    + offer_deposit 
                //    + loan principle
                trace_if_false(
                  @"Offer UTxO has wrong value",
                  val == (
                    value.from_lovelace(offer_deposit) // Offer deposit from datum.
                    |> value.add(_,asset_id,asset_name,principle) // Loan amount.
                    |> value.add(_,beacon_id,"Offer",1) // Offer beacon.
                    |> value.add(_,beacon_id,loan,1) // Loan asset beacon.
                    |> value.add(_,beacon_id,lender_name,1) // LenderID.
                  )
                )
              }

              _ -> error @"Beacons not stored with OfferDatum"
            }
          }

          // If no beacons are present, this output can be ignored.
          [] -> acc

          // Any other pattern is a sign of beacons being stored with the wrong value.
          _ -> error @"Beacons not minted to a valid Offer UTxO"
        }
      }

      _ -> error // Not used with the other redeemers.
    }
  }

  list.foldl(outputs,True,foo)
}

fn compare_out_ref(ref1: OutputReference, ref2: OutputReference) -> Ordering {
  let OutputReference(TransactionId(tx_id1),out_index1) = ref1
  let OutputReference(TransactionId(tx_id2),out_index2) = ref2
  when byte.compare(tx_id1,tx_id2) is {
    Less -> Less
    Greater -> Greater
    Equal -> int.compare(out_index1,out_index2)
  }
}

// This is only used on values that need to go into a Map. The map requires a comparison.
// The order does not matter but the values have a LoanID so it will either throw an error if the LoanID
// is in both values or return Less.
fn compare_value(loan_id: AssetName, sym: PolicyId, val1: Value, val2: Value) -> Ordering {
  // Check if the values have the same LoanID.
  if value.quantity_of(val1,sym,loan_id) == value.quantity_of(val2,sym,loan_id) { 
    error
  } 

  // Otherwise, return Less.
  else { 
    Less 
  }
}

// They should never be equal since the LoanID is supposed to be unique. It 
// will crash if two are equal.
fn compare_active_datums(datum1: LoanDatum, datum2: LoanDatum) -> Ordering {
  expect ActiveDatum(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,loan_id1) = datum1
  expect ActiveDatum(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,loan_id2) = datum2
  when byte.compare(loan_id1,loan_id2) is {
    Less -> Less
    Greater -> Greater
    Equal -> error
  }
}

// Get the time from the tx's invalid-before setting.
// This is used when accepting a loan and when a lender is claiming an expired loan.
pub fn get_invalid_before(val_range: ValidityRange) -> PosixTime {
  when val_range is {
    Interval(IntervalBound(Finite(x),_),_) -> x
    _ -> error @"invalid-before not specified"
  }
}

// Get the time from the tx's invalid-hereafter setting.
// This is used when making loan payments and rolling over a loan.
pub fn get_invalid_hereafter(val_range: ValidityRange) -> PosixTime {
  when val_range is {
    Interval(_,IntervalBound(Finite(x),_)) -> x
    _ -> error @"invalid-hereafter not specified"
  }
}

// Returns what the loan output should have for the ActiveDatum based on the inputs that were paired.
// Ensures that the inputs agree on the terms. The beacon policy implicitly checked that the datums
// agreed on the beacon symbol by using it to find the corresponding inputs.
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
    offer_rollover_freq,
    offer_term,
    offer_interest,
    offer_min_payment,
    offer_collateralization,
    offer_collateral_is_swappable,
    offer_claim_period,
    _
  ) = offer_datum

  // The datums must agree.
  expect True = error_if_false(
    @"Datums do not agree",

    // Must agree on loan asset.
    ask_asset == offer_asset &&

    // Must agree on principle.
    ask_principle == offer_principle &&

    // Must agree on loan expiration.
    ask_term == offer_term &&

    // Must agree on collateral.
    ask_collateral ==
      list.map(
        offer_collateralization,
        fn(x) {
          let (a,_) = x
          a
        }
      )
  )

  // If the previous checks passed, then the datums agree on the terms.
  // Produce the expected datum. Convert the times to absolute times based on the start time.
  ActiveDatum{
    loan_beacon_sym: offer_sym,
    borrower_id: ask_borrower_id,
    lender_address: offer_lender_address,
    loan_asset: offer_asset,
    loan_principle: offer_principle,
    rollover_frequency: offer_rollover_freq,
    last_checkpoint: start,
    loan_term: offer_term,
    loan_interest: offer_interest,
    min_payment: offer_min_payment,
    collateralization: offer_collateralization,
    collateral_is_swappable: offer_collateral_is_swappable,
    claim_expiration: start + offer_term + offer_claim_period,
    loan_expiration: start + offer_term,
    loan_outstanding: ratio.from_int(offer_principle),
    loan_id: loan_id
  }
}

// Goes through the inputs and builds up two maps: one Ask input map and one Offer input map.
// All inputs from the loan validator must come from the same address and all inputs must
// have the proper beacon token present. The integer returned is the size of that map.
fn input_maps (
  borrower_cred: Credential,
  inputs: List<Input>,
  sym: PolicyId,
  dapp_hash: Hash<Blake2b_224,Script>,
) -> (Dict<OutputReference,LoanDatum>,Int,Dict<OutputReference,LoanDatum>,Int) {
  let foo = fn (
    input: Input, 
    acc: (Dict<OutputReference,LoanDatum>,Int,Dict<OutputReference,LoanDatum>,Int)
  ) {
    let Input(out_ref,Output(Address(pay_cred,stake_cred),val,d,_)) = input
    let (ask_map,asize,offer_map,osize) = acc

    // Check if the input has an Ask beacon. This can only be from a dApp address.
    if value.quantity_of(val,sym,"Ask") == 1 {
      // Check if it comes from the correct dApp address.
      if stake_cred == Some(Inline(borrower_cred)) {
        // The datum is guaranteed to be an AskDatum since the "Ask" beacon is present.
        expect AskDatum(..) as datum = parse_datum(d)

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
        error @"Input from wrong loan address" 
      }
    }

    // Check if input has an Offer beacon. This can only be from a dApp address.
    else if value.quantity_of(val,sym,"Offer") == 1 {
      // Check if it comes from the correct dApp address.
      if stake_cred == Some(Inline(borrower_cred)) {
        // The datum is guaranteed to be an OfferDatum since the "Offer" beacon is present.
        expect OfferDatum(..) as datum = parse_datum(d) 

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
        error @"Input from wrong loan address" 
      }
    }
    
    // Check if input comes from a dApp address. If it does and the previous two checks failed,
    // this must be an invalid input (missing beacons or has an Active beacon). It does not matter
    // if the loan address belongs to the borrower or not; it should fail regardless.
    else if pay_cred == ScriptCredential(dapp_hash) { 
      error @"Invalid loan input found" 
    }

    // Otherwise, it is a benign input and can be skipped.
    else { 
      acc 
    }
  }

  list.foldl(inputs,(dict.new(),0,dict.new(),0),foo)
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
// - The value is the required mint/burn.
pub fn acceptance_input_checks(
  borrower_cred: Credential,
  ins: List<Input>,
  sym: PolicyId,
  dapp_hash: Hash<Blake2b_224,Script>,
  start_time: PosixTime,
  pairings: List<(OutputReference,OutputReference)>
) -> ( Dict<Value,Address>, Int, Dict<LoanDatum,Int>, Int, Value ) {
  // Checks:
  // Checks:
  // 1) All Ask UTxOs must have an Ask beacon.
  // 2) All Offer UTxOs must have an Offer beacon.
  // 3) All Ask UTxOs and Offer UTxOs must come from the same address.
  // 4) No other UTxOs from the loan address are present in the transaction.
  // 5) All Ask UTxOs must be paired up with exactly one Offer UTxO and vice, versa.
  // 6) The paired Ask UTxO and Offer UTxO must agree on the terms.

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

    // Check if the inputs have been used already.
    when (dict.get(used_map,ask_ref),dict.get(used_map,offer_ref)) is {
      // Neither inputs have been used yet.
      (None,None) -> {
        // Get the datums associated with the Ask and Offer.
        when (dict.get(ask_map,ask_ref),dict.get(offer_map,offer_ref)) is {
          (Some(ask_datum),Some(offer_datum)) -> {
            expect OfferDatum(_,lender_id,lender_address,loan_asset,_,_,_,_,_,_,_,_,deposit) = offer_datum

            // Generate the LoanID from the Offer input.
            let offer_id = generate_loan_id(offer_ref)
            let asset_beacon = generate_beacon_name(loan_asset)

            // Calculate the amount owed to the lender upon acceptance:
            // 1) Offer deposit
            // 2) One of the two minted LoanIDs
            let pay_lender_val = 
              value.from_lovelace(deposit) // from the datum
              |> value.add(_,sym,offer_id,1)

            // Update the required mint:
            // 1) Burn 1 Ask beacon
            // 2) Burn 1 Offer beacon
            // 3) Mint 1 Active beacon
            // 4) Burn 1 LenderID - this isn't needed anymore
            // 5) Burn 1 loan asset beacon - the other will be stored with the collateral.
            // 5) Mint 2 LoanIDs with the name of the hashed offer output reference.
            // 6) Mint 1 BorrowerID
            let new_req_mint = 
              req_mint 
              |> value.add(_,sym,offer_id,2) 
              |> value.add(_,sym,lender_id,-1) 
              |> value.add(_,sym,"Ask",-1) 
              |> value.add(_,sym,"Offer",-1) 
              |> value.add(_,sym,"Active",1)
              |> value.add(_,sym,asset_beacon,-1)
              |> value.add(_,sym,credential_to_asset_name(borrower_cred),1)

            // Insert the lender payment information into the lender pay_map
            let new_pay_map = dict.insert(
              pay_map,
              pay_lender_val,
              lender_address,
              compare_value(offer_id,sym,_,_) // Order doesn't matther but this is still needed.
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
    // since the reciprocal of zero is undefined.
    if price == ratio.zero() { 
      acc 
    } 
    
    // Calculate the value of the collateral in terms of the loan asset. Add it to the
    // accumulator.
    else {
      expect Some(recip_price) = ratio.reciprocal(price)

      value.quantity_of(oval,policy,name) // Get the quantity of that collateral asset.
      |> ratio.from_int(_) // Convert it to Rational.
      |> ratio.mul(_,recip_price) // Divide it by the price.
      |> ratio.add(acc,_) // Add it to the accumulator.
    }
  }

  // Collateral posted >= principle taken
  ratio.compare(list.foldl(collats,ratio.zero(),foo),principle_taken) != Less
}

// This checks the outputs to ensure:
// 1) All lenders get the proper LoanID tokens and their deposits back.
// 2) All loans are properly collateralized at the borrower's address.
// 3) All collateral UTxOs have the proper beacon tokens.
pub fn acceptance_output_checks(
  borrower_cred: Credential,
  sym: PolicyId,
  dapp_hash: Hash<Blake2b_224,Script>,
  pay_map: Dict<Value,Address>,
  pay_size: Int,
  loan_map: Dict<LoanDatum,Int>,
  loan_size: Int,
  outputs: List<Output>
) -> Bool {
  // Checks:
  // 1) The corresponding Active UTxO must have the proper inline ActiveDatum:
  //  - loan_beacon_sym == this policy id.
  //  - borrower_id == this borrower's staking credential as a token name.
  //  - lender_address == lender_address from Offer UTxO.
  //  - loan_asset == loan_asset from Offer UTxO.
  //  - loan_principle == loan_principle from Offer UTxO.
  //  - rollover_frequency == rollover_frequency from Offer UTxO.
  //  - last_checkpoint == invalid-before of this tx
  //  - loan_term == loan_term from Offer UTxO.
  //  - loan_interest == loan_interest from Offer UTxO.
  //  - min_payment == min_payment from Offer UTxO.
  //  - collateralization == collateralization from Offer UTxO.
  //  - collateral_is_swappable == collateral_is_swappable from Offer UTxO.
  //  - claim_expiration == 
  //      invalid-before of this tx + loan_term from Offer UTxO + claim_period from Offer UTxO.
  //  - loan_expiration == invalid-before of this tx + loan_term from Offer UTxO.
  //  - loan_outstanding == loan_principle from Offer UTxO * (1 + loan_interest from Offer UTxO)
  //  - loan_id == sha2_256( offer_tx_hash ++ offer_output_index ) as token name.
  // 2) The corresponding Active UTxO must have the proper value:
  //  - 1 Active beacon
  //  - 1 BorrowerID
  //  - 1 Asset beacon that corresponds to the loan_asset in the ActiveDatum.
  //  - 1 LoanID that corresponds to the loan_id in the ActiveDatum.
  //  - The proper amount of collateral.
  // 3) The lender must be paid their Offer UTxO deposit (offer_deposit in the OfferDatum) and
  //    1 LoanID that corresponds to the loan. The UTxO must include a payment datum since
  //    the output could be to the proxy script. The payment datum is (beacon_sym,"Accepted").

  let borrower_addr = Address(ScriptCredential(dapp_hash),Some(Inline(borrower_cred)))
  let borrower_token = credential_to_asset_name(borrower_cred)

  let foo = fn (output: Output, acc: (Int,Int)) {
    let (lenders_paid,collaterals_satisfied) = acc
    let Output(addr,val,d,_) = output

    // Check if output is a collateral output. Any output to the loan address is assumed
    // to be a collateral output. There is no other reason to output to the loan address.
    if addr == borrower_addr {
      let datum = parse_datum(d)
      // Check that the datum is the proper datum.

      when datum is {
        ActiveDatum(_,_,_,loan_asset,_,_,_,_,_,_,collats,_,_,_,borrowed,loan_id) -> {
          // Generate the loan asset beacon name from the asset being loaned.
          let asset_beacon = generate_beacon_name(loan_asset)

          // Check if the output has enough collateral.
          expect True = 
            error_if_false(
              @"Not enough collateral posted",
              is_enough_collateral(borrowed,collats,val)
            )

          expect True =
            error_if_false(
              @"Collateral output has wrong beacons",
              
              // Must have 1 LoanID.
              value.quantity_of(val,sym,loan_id) == 1 &&

              // Must have 1 of the proper Asset beacon.
              value.quantity_of(val,sym,asset_beacon) == 1 &&

              // Must have 1 BorrowerID.
              value.quantity_of(val,sym,borrower_token) == 1 &&

              // Must have 1 Active beacon.
              value.quantity_of(val,sym,"Active") == 1
            )
          
          // Check that the output has one of the proper datum. The value of the Map is a
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

        // Datum is not an ActiveDatum.
        _ -> error @"Loan output has wrong datum type"
      } 
    }

    // Check if the value is supposed to be a lender payment.
    else {
      when dict.get(pay_map,val) is {
        Some(lender_addr) -> {

          // Since the value can only exist in the map if the LoanID is present, the payment
          // must be to the proper lender's address.
          expect True = error_if_false (@"LoanID sent to wrong address", lender_addr == addr )

          // The LoanID must be stored with the proper datum. It is just a place holder datum
          // in case the proxy script is used. The Key NFT ensures uniqueness of UTxOs.
          let (policy_id,name) = parse_payment_datum(d)
          expect True = error_if_false(
            @"Lender's Key NFT stored with wrong datum",
            policy_id == sym && name == "Accepted"
          )
    
          // Count the lender payment as satisfied.
          (lenders_paid + 1, collaterals_satisfied) 
        }

        _ -> acc // Not meant to be a lender payment. Can be skipped.
      }
    }
  }

  let (lenders_paid,collaterals_satisfied) = list.foldl(outputs,(0,0),foo)
  
  // All lenders must be paid and all loans must be satisfied with collateral.
  error_if_false( 
    @"Missing required outputs",
    lenders_paid == pay_size && collaterals_satisfied == loan_size
  )
}

// Gets the amount paid to the lender, the flux for this loan's collateral, the new datum
// attached to the collateral output, and whether or not the BorrowerID is included in the
// collateral output. They are returned so they can be checked later. One loan is checked
// per execution. This also checks that outputs are valid. Only one UTxO paid to the lender
// counts towards the payment; this is to prevent UTxO dusting attacks.
pub fn payment_info(
  lender_addr: Address,
  sym: PolicyId,
  loan_asset: (PolicyId,AssetName),
  loan_id: AssetName,
  starting_val: Value, // Collateral starting value.
  loan_addr: Address,
  borrower_id: AssetName,
  outputs: List<Output>
) -> (Int,Value,Option<(LoanDatum,Bool)>) {

  let (policy,name) = loan_asset
  let asset_beacon = generate_beacon_name(loan_asset)

  let foo = fn (output: Output, acc: (Int,Value,Option<(LoanDatum,Bool)>)) {
    let Output(addr,val,d,_) = output
    let (paid,flux,collat_info) = acc

    // Check if this output is to the lender.
    if addr == lender_addr {
      // The output must have a unique datum that links it to the target loan.
      // This guarantees a unique output for every loan payment to prevent double satisfaction.
      // The payment will only count if the datum is correct. The datum must be the beacon policy id
      // with the LoanID. Since every LoanID is guaranteed to be unique, every output UTxO will be
      // unique. Using the full output reference can also be used to prevent double satisfaction but
      // that would make audibility harder - the policy id implicitly says what the payment is for;
      // the same is not true for the full output reference.
      if parse_payment_datum(d) != (sym,loan_id) { 
        // If it doesn't have the proper datum, this can't be a valid payment. Invalid payments
        // can be ignored since the collateral taken check will fail due to being greater than
        // 0.
        acc 
      } 
      
      else {
        ( value.quantity_of(val,policy,name), // Record how much was paid.
          flux, 
          collat_info
        ) 
      }
    } 
    
    // Check if the output contains a LoanID.
    else if value.quantity_of(val,sym,loan_id) == 1 {
      // The LoanID must be re-output to the loan address.
      expect True = error_if_false( @"LoanID sent to wrong address", addr == loan_addr )

      // The output must have the Active beacon.
      expect True = error_if_false(
        @"Collateral output must have 1 Active beacon",
        value.quantity_of(val,sym,"Active") == 1
      )

      // The output must have the Asset beacon.
      expect True = error_if_false(
        @"Collateral output must have 1 Asset beacon",
        value.quantity_of(val,sym,asset_beacon) == 1
      )

      // Return the info.
      ( paid,

        // Subtract the value from the starting value.
        value.merge(flux,value.negate(val)),

        // Return the datum used and whether or not the BorrowerID is present in output.
        Some((parse_datum(d), value.quantity_of(val,sym,borrower_id) == 1))
      )
    }

    // Check if a BorrowerID is found in any other UTxO. This check is meant to make it so
    // that a BorrowerID from a fully paid loan cannot be found in any output UTxO.
    // It must be burned.
    else if value.quantity_of(val,sym,borrower_id) > 0 {
      // Check if the BorrowerID is grouped up with another loan. This prevents the BorrowerID
      // from being moved to another active loan.
      expect True =
        error_if_false(
          @"1 BorrowerID per loan",
          value.quantity_of(val,sym,borrower_id) == 1
        )
      
      // Check if BorrowerID is stored with a valid Active UTxO. This prevents the BorrowerID
      // from being withdrawn from the loan address.
      expect True =
        error_if_false(
          @"BorrowerID not stored with a valid Active UTxO",
          value.quantity_of(val,sym,"Active") == 1
        )

      // If the above two checks passed, then any UTxO with a BorrowerID should be valid.
      // They cannot go anywhere and therefore extra IDs must be burned.
      acc
    }

    // If the output does not contain a LoanID or a BorrowerID, this is a benign output.
    else { 
      acc 
    }
  }

  list.foldl(outputs,(0,starting_val,None),foo)
}

// Returns True or throws an error.
pub fn collateral_taken_check(
  in_val: Value,
  collat_flux: Value,
  paid: Rational,
  outstanding: Rational,
  collateralization: List<((PolicyId,AssetName),Rational)>,
  swappable: Bool
) -> Bool {
  // Calculate the proportion of the loan that was repaid.
  let goal = when ratio.reciprocal(outstanding) is {
    None -> error @"Outstanding is already 0"
    Some(x) -> ratio.mul(paid,x)
  }

  // Returns the total amount of collateral taken (in units of the loan asset) and
  // the total starting balance for collateral (in units of the loan asset).
  let foo = fn (collat: ((PolicyId,AssetName),Rational), acc: (Rational,Rational)) {
    let ((policy,name),price) = collat
    let (total_taken,starting_collat) = acc

    // If the price is zero, the accumulator will remain unchanged.
    // The lender does not care about this asset.
    if price == ratio.zero() { 
      acc 
    }

    // Calculate the proportion taken for this collateral asset (in terms of the loan asset).
    else {
      expect Some(recip_price) = ratio.reciprocal(price)
      let new_starting_collat = 
        value.quantity_of(in_val,policy,name) // Get quantity of collateral in starting balance.
        |> ratio.from_int(_) // Convert to Rational.
        |> ratio.mul(_,recip_price) // Convert to units of the loan asset.
        |> ratio.add(_,starting_collat) // Add to running total.

      // It is possible for the quantity of the collateral to be negative in the case where 
      // collateral is deposited. If this asset is deposited, more of another asset can be taken.
      // This swapping is only allowed if the terms agreed to it.
      let collat_taken = 
        if swappable {
          value.quantity_of(collat_flux,policy,name) |> ratio.from_int(_) // Can be negative.
        } else {
          let taken = value.quantity_of(collat_flux,policy,name)
          expect True = error_if_false( @"Collateral cannot be deposited/swapped", taken >= 0 )
          ratio.from_int(taken)
        }

      let new_total_taken = 
        ratio.mul(collat_taken,recip_price) // Convert to units of the loan asset.
        |> ratio.add(_,total_taken) // Add to running total.

      (new_total_taken,new_starting_collat)
    }
  }

  // The proportion of collateral taken must be less than or equal to the proportion of the
  // loan repaid.
  let (collat_taken,starting_collat) = 
    list.foldl(collateralization,(ratio.zero(),ratio.zero()),foo)

  expect Some(recip_starting_collat) = ratio.reciprocal(starting_collat)

  let prop_collat_taken = ratio.mul(collat_taken,recip_starting_collat)

  trace_if_false(
    @"Too much collateral taken",
    ratio.compare(prop_collat_taken,goal) != Greater
  )
}

// Returns True or False. Used for rollovers and updating lender address. It cannot throw an
// error in the case of composability. Therefore, the False must be handled differently.
pub fn update_loan_check(
  loan_addr: Address,
  req_val: Value,
  req_datum: LoanDatum,
  outputs: List<Output>
) -> Bool {
  let foo = fn (output: Output, acc: Bool) {
    let Output(addr,oval,d,_) = output

    when addr == loan_addr is {
      // If this output is not to the loan address, it can't possibly be the proper output.
      False -> acc

      True -> {
        // Since there can be multiple outputs to the loan address in the case of composability,
        // this function must first check if the proper output has already been found.

        // If acc is True, the proper output has already been found and all other outputs can be
        // skipped. Otherwise, both the datum and value must be correct.
        acc || (parse_datum(d) == req_datum && req_val == oval)
      }
    }
  }
  
  trace_if_false(
    @"Updated output not found - datum and value must be correct",
    list.foldl(outputs,False,foo)
  )
}

// This function sums the total input value from the transaction while also looking for
// a target Key NFT.
pub fn extended_total_input_value(
  dapp_hash: Hash<Blake2b_224,Script>,
  sym: PolicyId,
  loan_id: AssetName,
  inputs: List<Input>
) -> (Value,Bool) {
  let foo = fn(input: Input, acc: (Value,Bool)) {
    let Input(_,Output(Address(pay,_),val,_,_)) = input
    let (acc_val,acc_bool) = acc
    ( value.merge(acc_val,val),
      acc_bool || (value.quantity_of(val,sym,loan_id) == 1 && pay != ScriptCredential(dapp_hash))
    )
  }

  list.foldl(inputs,(value.zero(),False),foo)
}