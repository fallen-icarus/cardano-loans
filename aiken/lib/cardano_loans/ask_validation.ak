use aiken/dict.{Dict}
use aiken/list
use aiken/string
use aiken/transaction.{
    Datum,
    Output
  }
use aiken/transaction/credential.{
    Address,
  }
use aiken/transaction/value.{
    AssetName,
    PolicyId,
    Value
  }

use cardano_loans/data/core.{
    NegotiationID,
    ActiveID,
    Asset,
    AssetBeacon,
    BorrowerID
  }
use cardano_loans/data/datums.{
    AskDatum
  }
use cardano_loans/utils.{
    trace_if_false,
    error_if_false,
    parse_datum,
  }

// This is used to check that the value of Ask UTxOs have the correct value. It can only return 
// false if it is missing required collateral. All other failures will result in error crashing
// the script.
fn check_ask_value(
  val: Value,
  negotiation_beacon_id: NegotiationID,
  asset_beacon: AssetBeacon,
  collateral: List<Asset>,
) -> Bool {
  // Get the asset beacon name from `AssetBeacon`.
  let asset_beacon_name = core.get_asset_beacon_name(asset_beacon)

  // A function to map over the value of this UTxO. It will return the total number of collateral
  // with at least one unit present and whether the UTxO has the proper beacons.
  let foo = fn(x: (PolicyId,Dict<AssetName,Int>), acc: (Int,Bool)) {
    let (cn,tns_dict) = x
    let (collateral_units_met,valid_beacons) = acc

    // Check the beacons.
    if cn == core.from_negotiation_id(negotiation_beacon_id) {
      let tns_list = dict.to_list(tns_dict)

      // A function to check each beacon.
      let beacon_check = fn(y: (AssetName,Int), inner_acc: (Bool,Bool)) {
        let (tn,n) = y
        let (ask_check,asset_check) = inner_acc
        if tn == "Ask" {
          // There must only be one Ask beacon.
          (n == 1, asset_check)
        } else if tn == asset_beacon_name {
          // There must only be one Asset beacon.
          (ask_check, n == 1)
        } else {
          error @"UTxO has a beacon with the wrong name"
        }
      }

      when list.foldl(tns_list,(False,False),beacon_check) is {
        (True,True) -> (collateral_units_met,True)
        _ -> (collateral_units_met,False) // The UTxO does not have exactly one of each beacon.
      }
    } 

    // Check that the asset is in the collateral list and there is at least one unit of it. It
    // is possible that collateral can share a policy id so this check must account for that.
    // Ada is the only asset that can be present without being in the collateral list.
    else {
      let non_beacon_toks = dict.to_list(tns_dict)

      // For each token in the list, it must be found in the collateral list and there must
      // be at least one unit of each. Ada must be allowed even if it is not used as collateral.
      let collat_check = fn(token_info: (AssetName,Int), inner_acc: Int) {
        let (tn,n) = token_info
        if list.has(collateral,(cn,tn)) {
          if n >= 1 { inner_acc + 1 }
          else { error @"Collateral unit missing from Ask UTxO" }
        } else if cn == #"" { 
          // If ada was used as collateral, it should have matched the previous if.
          // This branch is to ensure minUTxOValue can still be satisfied.
          inner_acc 
        } else {
          // If it is not in the list and it is not ada, this is an extraneous asset.
          error @"No extraneous assets allowed in the UTxO"
        }
      }

      // The starting accumulator should be the current count of satisfied collateral.
      ( list.foldl( non_beacon_toks, collateral_units_met, collat_check), valid_beacons )
    }
  }

  let (collateral_units_met,valid_beacons) = list.foldl(
    value.to_dict(val) |> dict.to_list(_),
    (0,False),
    foo
  )

  if valid_beacons {
    // The collateral_units_met must match the size of the collateral list.
    collateral_units_met == list.length(collateral)
  } else {
    // If the recursive function returned false instead of crashing, it is because the UTxO
    // has the right beacons but the wrong quantity of each beacon.
    error @"UTxO does not have exactly one of each beacon"
  }
}

// This checks the datum and calls extract_ask_loan_and_collateral_quantity to check the value.
fn valid_ask_output(
  negotiation_beacon_id: NegotiationID,
  active_beacon_id: ActiveID,
  borrower_id: BorrowerID,
  output_value: Value,
  output_datum: Datum
) -> Bool {
  // Extract the info from the AskDatum. Crash the script if it is not an AskDatum.
  expect AskDatum(
    negotiation_beacon_id_,
    active_beacon_id_,
    borrower_id_,
    loan_asset_,
    asset_beacon_,
    loan_principle_,
    loan_term_,
    collateral_
  ) = when parse_datum(output_datum) is {
     AskDatum{..} as datum -> datum
     _ -> error @"UTxO not stored with an AskDatum"
  }

  // Generate the required AssetBeacon for the loan asset.
  let expected_asset_beacon = core.make_asset_beacon(loan_asset_)

  expect True = error_if_false(
    @"Ask UTxO is missing required collateral",
    check_ask_value(
      output_value,
      negotiation_beacon_id,
      expected_asset_beacon,
      collateral_
    )
  )

  // All of the following must be true.
  and {
    trace_if_false(@"Datum has wrong negotiation_beacon_id", 
      negotiation_beacon_id_ == negotiation_beacon_id),
    trace_if_false(@"Datum has wrong active_beacon_id", active_beacon_id_ == active_beacon_id),
    trace_if_false(@"Datum has wrong borrower_id", borrower_id_ == borrower_id),
    trace_if_false(@"Datum has wrong asset_beacon", asset_beacon_ == expected_asset_beacon),
    trace_if_false(@"Datum has wrong principle", loan_principle_ > 0),
    trace_if_false(@"Datum has wrong term", core.is_valid_duration(loan_term_)),
    trace_if_false(@"Datum has wrong collateral", and {
      !list.is_empty(collateral_),
      collateral_ == list.sort(collateral_,core.compare_collateral_asset)
    })
  }
}

// Check if the beacons are locked at the proper address and the Ask UTxO is configured properly.
// This function will return `True` or `False`. It uses `app_name` in the error message to generate
// a unique beacon policy for testing. Since the negotiation beacon script can only be executed
// once in a given transaction, all outputs with negotiation beacons are assumed to be Ask UTxOs.
pub fn ask_beacon_destination_check(
  borrower_loan_address: Address,
  borrower_id: BorrowerID,
  negotiation_beacon_id: NegotiationID,
  active_beacon_id: ActiveID,
  outputs: List<Output>
) -> Bool {
  // A function to map over the list of outputs.
  let foo = fn (out: Output, acc: Bool) {
    // Extract out the output's information.
    let Output(address,val,d,_) = out

    // Get the beacons in this UTxO and pattern match on it.
    when value.tokens(val,core.from_negotiation_id(negotiation_beacon_id)) |> dict.to_list() is {
      // If beacons are not present, this output can be ignored.
      [] -> acc

      // The UTxO has two types of beacon. The exact beacons will be checked later. 
      [(_,_),(_,_)] -> {
        // All previous outputs must be valid.
        acc &&

        // The output must be at a DApp address with the borrower's credential as the
        // stake credential. Uses `app_name` in the error message.
        trace_if_false( 
          string.concat(@"Beacon must go to a ", core.app_name) 
            |> string.concat(_,@" DApp address with the borrower's staking credential"),
          address == borrower_loan_address
        ) &&

        // The output must be properly configured. This has its own trace messages.
        valid_ask_output(negotiation_beacon_id,active_beacon_id,borrower_id,val,d)
      }

      // This path can only be taken if the above two matches failed which means the UTxO
      // has the wrong number of beacons.
      _ -> error @"Ask UTxOs must have exactly two kinds of beacons"
    }
  }

  // Map over the list. This will only return True if all the outputs are valid.
  list.foldl( outputs, True, foo )
}
