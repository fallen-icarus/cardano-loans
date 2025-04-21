use aiken/dict.{Dict}
use aiken/list
use aiken/transaction.{Datum,Output}
use aiken/transaction/credential.{Address}
use aiken/transaction/value.{AssetName,PolicyId,Value}

use cardano_loans/data/core.{
    NegotiationId,
    ActiveId,
    Asset,
    AssetBeacon,
    BorrowerId
  }
use cardano_loans/data/datums.{AskDatum}
use cardano_loans/utils.{error_if_false,trace_if_false,parse_datum}

// This is used to check that the Ask UTxOs have the correct value. It can only return 
// false if it is missing required collateral. All other failures will result in an error crashing
// the script. Due to the way this function is written, beacon tokens do not count towards the 
// collateral quantities even if they are the loan asset. Therefore, beacon tokens cannot be used 
// as one of the collateral assets. If the collateral list is empty, then the Ask UTxO does not need
// any collateral asset.
fn check_ask_value(
  val: Value,
  negotiation_beacon_id: NegotiationId,
  asset_beacon: AssetBeacon,
  collateral: List<Asset>,
) -> Bool {
  // A function to map over the value of this UTxO. It will return the total number of collateral
  // with at least one unit present. While checking for collateral, it will also check that the
  // UTxO has the proper beacons.
  let foo = fn(x: (PolicyId,Dict<AssetName,Int>), collateral_units_met: Int) {
    let (cn,tns_dict) = x

    // Check the beacons.
    if cn == core.from_negotiation_id(negotiation_beacon_id) {
      let tns_list = dict.to_list(tns_dict)

      // A function to check each beacon.
      let check_beacons = fn(y: (AssetName,Int), inner_acc: (Bool,Bool)) {
        let (tn,n) = y
        let (ask_check,asset_check) = inner_acc
        if tn == "Ask" {
          // There must only be one Ask beacon. If n was zero, tn could not have matched "Ask".
          (error_if_false(@"Too many Ask beacons in UTxO", n == 1), asset_check)
        } else if tn == core.get_asset_beacon_name(asset_beacon) {
          // There must only be one Asset beacon. If n was zero, tn could not have matched the
          // asset beacon.
          (ask_check, error_if_false(@"Too many asset beacons in UTxO", n == 1))
        } else {
          error @"UTxO has a beacon with the wrong name"
        }
      }

      when list.foldl(tns_list,(False,False),check_beacons) is {
        (True,True) -> collateral_units_met

        // This path can be taken if the UTxO has either an Ask beacon or an asset beacon, but not
        // both. It should not actually possible to take this path since the script will crash at
        // an earlier point if it has too few or too many kinds of beacons.
        _ -> error @"UTxO does not have the right beacons"
      }
    } 

    // Check that the asset is in the collateral list and there is at least one unit of it. It
    // is possible that collateral can share a policy id so this check must account for that.
    // Ada is the only asset that can be present without being in the collateral list.
    else {
      // For each token in the list, it must be found in the collateral list and there must
      // be at least one unit of each. Ada must be allowed even if it is not used as collateral.
      let check_collateral = fn(token_info: (AssetName,Int), inner_acc: Int) {
        let (tn,_) = token_info
        if list.has(collateral,(cn,tn)) {
          // If the asset is present, it has at least one unit. Zero values are never present
          // in UTxO values.
          inner_acc + 1
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
      list.foldl(dict.to_list(tns_dict), collateral_units_met, check_collateral)
    }
  }

  // The collateral_units_met must match the size of the collateral list. If the collateral list is
  // empty, then there should not be any other assets (ADA is always allowed).
  list.foldl(value.to_dict(val) |> dict.to_list(_), 0, foo) == list.length(collateral)
}

// This checks the datum and calls check_ask_value to check the value.
fn valid_ask_output(
  negotiation_beacon_id_: NegotiationId,
  active_beacon_id_: ActiveId,
  borrower_id_: BorrowerId,
  output_value: Value,
  output_datum: Datum
) -> Bool {
  // Extract the info from the AskDatum. Crash the script if it is not an AskDatum.
  expect AskDatum{
    negotiation_beacon_id,
    active_beacon_id,
    borrower_id,
    loan_asset,
    asset_beacon,
    loan_principal,
    loan_term,
    collateral
  } = when parse_datum(output_datum) is {
     AskDatum{..} as datum -> datum
     _ -> error @"UTxO not stored with an AskDatum"
  }

  // Generate the required AssetBeacon for the loan asset.
  let expected_asset_beacon = core.make_asset_beacon(loan_asset)

  // All of the following must be true.
  and {
    trace_if_false(
      @"Datum has wrong negotiation beacon id", 
      negotiation_beacon_id_ == core.to_negotiation_id(negotiation_beacon_id)),
    trace_if_false(
      @"Datum has wrong active beacon id", 
      active_beacon_id_ == core.to_active_id(active_beacon_id)),
    trace_if_false(
      @"Datum has wrong borrower id", 
      core.get_borrower_id_name(borrower_id_) == borrower_id),
    trace_if_false(
      @"Datum has wrong asset beacon", 
      asset_beacon == core.get_asset_beacon_name(expected_asset_beacon)),
    trace_if_false(
      @"Datum has wrong principal", 
      loan_principal > 0),
    trace_if_false(
      @"Datum has wrong term", 
      loan_term > 0),
    trace_if_false(
      @"Datum has wrong collateral", 
      // It can be empty, but if it isn't:
      // It must be sorted and not have any duplicates.
      collateral == list.sort(collateral,utils.compare_collateral_asset_no_dups)),
    trace_if_false(
      @"Ask UTxO is missing required collateral",
      check_ask_value(
        output_value,
        negotiation_beacon_id_,
        expected_asset_beacon,
        collateral
      )
    )
  }
}

// Check if the beacons are locked at the proper address and the Ask UTxO is configured properly.
// All outputs with negotiation beacons are assumed to be Ask UTxOs. This function can return False
// if the Ask UTxO is stored at the wrong address.
pub fn ask_beacon_destination_check(
  borrower_loan_address: Address,
  borrower_id: BorrowerId,
  negotiation_beacon_id: NegotiationId,
  active_beacon_id: ActiveId,
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
        // stake credential.
        trace_if_false( 
          @"Beacons must go to a DApp address with the borrower's staking credential",
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

  // Map over the list. This will only return True if all of the outputs are valid.
  list.foldl(outputs, True, foo)
}
