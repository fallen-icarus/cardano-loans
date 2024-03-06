use aiken/dict.{Dict}
use aiken/hash.{Hash,Blake2b_224}
use aiken/option
use aiken/list
use aiken/string
use aiken/transaction.{
    Datum,
    Output
  }
use aiken/transaction/credential.{
    Address,
    Script,
    ScriptCredential,
  }
use aiken/transaction/value.{
    AssetName,
    PolicyId,
    Value
  }

use cardano_loans/data/core.{
    Asset,
    NegotiationID,
    ActiveID,
    AssetBeacon,
    LenderID,
  }
use cardano_loans/data/datums.{
    OfferDatum
  }
use cardano_loans/utils.{
    trace_if_false,
    parse_datum,
    valid_lender_address
  }

// This is used to check that the value of Offer UTxOs are correct. It returns the value of the
// loan asset so that it can be checked against the principle in the datum. When ada is the
// loan asset, the first Int should be loan_principle + offer_deposit.
fn extract_offer_quantity(
  val: Value,
  negotiation_beacon_id: NegotiationID,
  asset_beacon: AssetBeacon,
  loan_asset: Asset,
  lender_id: LenderID
) -> (Int,Int) {
  // Extract out the loan asset info.
  let (loan_asset_id,loan_asset_name) = loan_asset

  // Extract out the asset beacon name.
  let asset_beacon_name = core.get_asset_beacon_name(asset_beacon)

  // Extract out the lender_id's token name.
  let lender_id_name = core.get_lender_id_name(lender_id)

  // A function to map over the value of this UTxO. It will return the total number of the loan
  // asset, the total amount of ada, and whether the UTxO has the proper beacons.
  let foo = fn(x: (PolicyId,Dict<AssetName,Int>), acc: (Int,Int,Bool)) {
    let (cn,tns_dict) = x
    let (loan_quantity,ada_quantity,valid_beacons) = acc

    // Check the beacons.
    if cn == core.from_negotiation_id(negotiation_beacon_id) {
      let tns_list = dict.to_list(tns_dict)

      // A function to check each beacon.
      let beacon_check = fn(y: (AssetName,Int), inner_acc: (Bool,Bool,Bool)) {
        let (tn,n) = y
        let (offer_check,asset_check,lender_check) = inner_acc
        if tn == "Offer" {
          // There must only be one Offer beacon.
          (n == 1, asset_check, lender_check)
        } else if tn == asset_beacon_name {
          // There must only be one Asset beacon.
          (offer_check, n == 1, lender_check)
        } else if tn == lender_id_name {
          // There must only be one LenderID beacon.
          (offer_check, asset_check, n == 1)
        } else {
          error @"UTxO has a beacon with the wrong name"
        }
      }

      when list.foldl(tns_list,(False,False,False),beacon_check) is {
        (True,True,True) -> (loan_quantity,ada_quantity,True)
        _ -> (loan_quantity,ada_quantity,False) // The UTxO does not have exactly one of each beacon.
      }
    } 

    // Check that the asset is the loan asset. Ada is the only asset that can be present without 
    // being the loan asset.
    else if cn == loan_asset_id {
      when dict.to_list(tns_dict) is {
        // Since this check assumes the loan asset is the only asset in the UTxO with that
        // policy id, beacons cannot be the loan asset. When ada is the loan asset, the 
        // ada_quantity field in the accumulator will not be used.
        [(tn,n)] -> {
          if tn == loan_asset_name {
            (loan_quantity+n,ada_quantity,valid_beacons)
          } else {
            error @"No extraneous assets allowed in the UTxO"
          }
        }
        _ -> error @"No extraneous assets allowed in the UTxO"
      }
    }

    // Check that the asset is ada. If this branch is taken, then ada is not the loan asset.
    else if cn == #"" {
      expect Some(n) = dict.get(tns_dict,#"")

      (loan_quantity,ada_quantity+n,valid_beacons)
    }

    else { error @"No extraneous assets allowed in the UTxO" }
  }

  let (loan_quantity,ada_quantity,valid_beacons) = list.foldl(
    value.to_dict(val) |> dict.to_list(_),
    (0,0,False),
    foo
  )

  if valid_beacons {
    // ada_quantity will be 0 when ada is the loan asset.
    (loan_quantity,ada_quantity)
  } else {
    // If the recursive function returned false instead of crashing, it is because the UTxO
    // has the right beacons but the wrong quantity of each beacon.
    error @"UTxO does not have exactly one of each beacon"
  }
}

// This checks the datum and calls extract_offer_loan_quantity to check the value.
fn valid_offer_output(
  negotiation_beacon_id: NegotiationID,
  active_beacon_id: ActiveID,
  lender_id: LenderID,
  proxy_hash: Hash<Blake2b_224, Script>,
  output_value: Value,
  output_datum: Datum
) -> Bool {
  // Extract the info from the OfferDatum. Crash the script if it is not an OfferDatum.
  expect OfferDatum(
    negotiation_beacon_id_,
    active_beacon_id_,
    lender_id_,
    lender_address_,
    loan_asset_,
    asset_beacon_,
    loan_principle_,
    rollover_frequency_,
    loan_term_,
    loan_interest_,
    min_payment_,
    collateralization_,
    _, // Don't need to check a true/false field.
    claim_period_,
    offer_deposit_,
    offer_expiration_
  ) = when parse_datum(output_datum) is {
    OfferDatum{..} as datum -> datum
    _ -> error @"UTxO not stored with an OfferDatum"
  }

  let expected_asset_beacon = core.make_asset_beacon(loan_asset_)

  let (loan_quantity,ada_quantity) = 
    extract_offer_quantity(
      output_value,
      negotiation_beacon_id,
      expected_asset_beacon,
      loan_asset_,
      lender_id
    )

  let (interest_num_,interest_den_) = loan_interest_

  // All of the following must be true.
  and {
    // Datum checks:
    trace_if_false(@"Datum has wrong negotiation_beacon_id", 
      negotiation_beacon_id_ == negotiation_beacon_id),
    trace_if_false(@"Datum has wrong active_beacon_id", 
      active_beacon_id_ == active_beacon_id),
    trace_if_false(@"Datum has wrong lender_id", 
      lender_id_ == lender_id),
    trace_if_false(@"Datum has wrong lender_address", 
      valid_lender_address(proxy_hash,lender_address_)),
    trace_if_false(@"Datum has wrong asset_beacon", 
      asset_beacon_ == expected_asset_beacon),
    trace_if_false(@"Datum has wrong principle", 
      loan_principle_ > 0),
    trace_if_false(@"Datum has wrong rollover_frequency", 
      // Set to 1 to ensure True when None.
      option.or_else(rollover_frequency_,core.make_duration(1)) |> core.is_valid_duration), 
    trace_if_false(@"Datum has wrong term", 
      core.is_valid_duration(loan_term_)),
    trace_if_false(@"Datum has wrong loan_interest", 
      interest_den_ > 0 && interest_num_ >= 0),
    trace_if_false(@"Datum has wrong min_payment", 
      min_payment_ >= 0 ),
    trace_if_false(@"Datum has wrong collateralization",
      and {
        // List can't be empty.
        !list.is_empty(collateralization_),
        // List must be sorted and not have duplicates.
        collateralization_ == list.sort(collateralization_,core.compare_collateralization),
        // All prices must be > 0 and the denominators must be > 0.
        list.all(
          collateralization_,
          fn(x) {
            let (_,(num,den)) = x
            num >= 0 && den > 0
          }
        )
      }
    ),
    trace_if_false(@"Datum has wrong claim_period", 
      core.is_valid_duration(claim_period_)),
    trace_if_false(@"Datum has wrong offer_deposit", 
      offer_deposit_ > 0),
    trace_if_false(@"Datum has wrong offer_expiration", 
      // Set to 1 to ensure True when None.
      option.or_else(offer_expiration_,core.make_expiration_time(1)) 
      |> core.is_valid_expiration_time), 

    // Value checks:
    trace_if_false(@"UTxO has wrong value",
      if loan_asset_ == (#"",#"") { 
        loan_quantity == loan_principle_ + offer_deposit_ 
      } else { 
        ada_quantity == offer_deposit_ && loan_quantity == loan_principle_
      }
    )
  }

}

// Check if the beacons are locked at the proper address and the Offer UTxO is configured properly.
// This function will return `True` or `False`. It uses `app_name` in the error message to generate
// a unique beacon policy for testing. 
pub fn offer_beacon_destination_check(
  loan_script_hash: Hash<Blake2b_224, Script>,
  lender_id: LenderID,
  negotiation_beacon_id: NegotiationID,
  active_beacon_id: ActiveID,
  proxy_hash: Hash<Blake2b_224, Script>,
  outputs: List<Output>
) -> Bool {

  // A function to map over the list of outputs.
  let foo = fn (out: Output, acc: Bool) {
    // Extract out the output's information.
    let Output(Address(pay,stake),val,d,_) = out

    // Get the beacons in this UTxO and pattern match on it.
    when value.tokens(val,core.from_negotiation_id(negotiation_beacon_id)) |> dict.to_list() is {
      // If beacons are not present, this output can be ignored.
      [] -> acc

      // The UTxO has three types of beacon. The exact beacons will be checked later.
      [(_,_),(_,_),(_,_)] -> {
        // All previous outputs must be valid.
        acc &&

        // The output must be at a DApp address with the borrower's credential as the
        // stake credential. Uses `app_name` in the error message.
        trace_if_false( 
          string.concat(@"Beacon must go to a ", core.app_name) 
            |> string.concat(_,@" DApp address with a valid staking credential"),
          pay == ScriptCredential(loan_script_hash) && option.is_some(stake)
        ) &&

        // The output must be properly configured. This has its own trace messages.
        valid_offer_output(negotiation_beacon_id,active_beacon_id,lender_id,proxy_hash,val,d)
      }

      // This path can only be taken if the above two matches failed which means the UTxO
      // has the wrong number of beacons.
      _ -> error @"Offer UTxOs must have exactly three kinds of beacons"
    }
  }

  // Map over the list. This will only return True if all the outputs are valid.
  list.foldl( outputs, True, foo )
}
