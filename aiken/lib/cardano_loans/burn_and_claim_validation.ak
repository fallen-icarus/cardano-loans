use aiken/bytearray as byte
use aiken/list
use aiken/dict.{Dict}
use aiken/transaction.{
    Output,
    Input,
  }
use aiken/transaction/credential.{
    Address,
    Credential,
  }
use aiken/transaction/value.{
    AssetName,
  }

use cardano_loans/data/core.{
    StartTime,
    ActiveID
  }
use cardano_loans/data/datums.{
    ActiveDatum
  }
use cardano_loans/utils.{
    trace_if_false,
    error_if_false,
    parse_datum,
  }

// Go through the inputs and build up a list of required beacons to burn. All inputs are assumed
// to be expired loans. If any are not, this will crash the script.
fn get_beacons_to_burn(
  active_beacon_id: ActiveID,
  loan_script_cred: Credential,
  lower_bound: StartTime,
  inputs: List<Input>
) -> Dict<AssetName,Int> {
  // Helper functions declared only once.
  let subtract = fn (_k, a, b) { Some (a-b) }

  // A function to map over the list of inputs.
  let foo = fn(input: Input, req_burn: Dict<AssetName,Int>) {
    let Input(_,Output(Address(pay,_),val,d,_)) = input

    // All expired loans will have 4 beacons and be located at a loan address. If an input
    // has beacons but is not located at a loan address, it is not an expired loan. The
    // required beacons to burn can come exclusively from expired loans so the outputs
    // located outside of loan addresses can be ignored.

    if pay == loan_script_cred {
      // Get the beacons in this UTxO and pattern match on it.
      when value.tokens(val,core.from_active_id(active_beacon_id)) |> dict.to_list() is {
        // If beacons are not present, this is an invalid loan UTxO and custody belongs
        // to the borrower.
        [] -> error @"Invalid loan UTxO among inputs"

        // If there are four beacons, this should be an already active loan and other script will 
        // check it.
        [(_,_),(_,_),(_,_),(_,_)] -> {
          // Get the ActiveDatum from the input. The only way to figure out which beacon is the
          // LoanID is to check the datum. The LoanID requires two units to be burned.
          expect ActiveDatum(
            _,
            borrower_id,
            _,
            _,
            asset_beacon,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            loan_expiration,
            _,
            loan_id
          ) = parse_datum(d)

          expect True = error_if_false(
            @"Loan is not expired",
            core.expiration_time_has_passed(lower_bound,loan_expiration)
          )

          dict.insert_with(req_burn,"Active",1,subtract,byte.compare) |>
          dict.insert_with(_,core.get_borrower_id_name(borrower_id),1,subtract,byte.compare) |>
          dict.insert_with(_,core.get_asset_beacon_name(asset_beacon),1,subtract,byte.compare) |>
          dict.insert_with(_,core.get_loan_id_name(loan_id),2,subtract,byte.compare)
        }

        // This path can only be taken if the above two matches failed which means the UTxO
        // has the wrong number of beacons.
        _ -> error @"UTxO input is not an Active UTxO"
      }
    } else {
      req_burn
    }
  }

  list.foldl(inputs,dict.new(),foo)
}

// Check that the required beacons have been burned.
pub fn valid_expired_claim(
  active_beacon_id: ActiveID,
  loan_script_cred: Credential,
  lower_bound: StartTime,
  inputs: List<Input>,
  beacons_minted: Dict<AssetName,Int>
) -> Bool {
  let req_burn =
    get_beacons_to_burn(
      active_beacon_id,
      loan_script_cred,
      lower_bound,
      inputs
    )

  trace_if_false(
    @"The wrong active beacons were burned",
    beacons_minted == req_burn
  )
}
