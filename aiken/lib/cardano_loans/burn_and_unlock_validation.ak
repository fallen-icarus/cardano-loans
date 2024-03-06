use aiken/bytearray as byte
use aiken/list
use aiken/dict.{Dict}
use aiken/transaction.{
    Output,
    Input,
  }
use aiken/transaction/value.{
    AssetName,
  }

use cardano_loans/data/core.{
    StartTime,
    ActiveID,
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
// to be loans that need to be unlocked. If any are not, this will crash the script.
fn get_beacons_to_burn(
  active_beacon_id: ActiveID,
  lower_bound: StartTime,
  inputs: List<Input>
) -> Dict<AssetName,Int> {
  // Helper functions declared only once.
  let subtract = fn (_k, a, b) { Some (a-b) }

  // A function to map over the list of inputs.
  let foo = fn(input: Input, req_burn: Dict<AssetName,Int>) {
    let Input(_,Output(_,val,d,_)) = input

    // All loan inputs should come from a loan address. There is no need to check this, though,
    // since beacons can only be minted to the loan address. The only exception are the key NFTs
    // but this redeemer cannot be used with the key NFTs. 
    // 
    // All lost collateral will have 4 beacons, while all finished loans will have 3 beacons.

    // Get the beacons in this UTxO and pattern match on it.
    when value.tokens(val,core.from_active_id(active_beacon_id)) |> dict.to_list() is {
      // If beacons are not present, this output can be ignored.
      [] -> req_burn

      // The UTxO has three types of beacon. This is a finished loan.
      [(tn1,n1),(tn2,n2),(tn3,n3)] -> {
        // Burn all of the beacons.
        dict.insert_with(req_burn,tn1,n1,subtract,byte.compare) |>
        dict.insert_with(_,tn2,n2,subtract,byte.compare) |>
        dict.insert_with(_,tn3,n3,subtract,byte.compare)
      }

      // The UTxO has four types of beacon. This can only be an active loan.
      [(tn1,n1),(tn2,n2),(tn3,n3),(tn4,n4)] -> {
        // Get the ActiveDatum from the input. The only way to figure out if the claim period
        // has passed is to check the datum.
        expect ActiveDatum(
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          claim_expiration,
          _,
          _,
          _
        ) = parse_datum(d)

        expect True = error_if_false(
          @"Claim period has not passed",
          core.expiration_time_has_passed(lower_bound,claim_expiration)
        )

        // Burn all of the beacons.
        dict.insert_with(req_burn,tn1,n1,subtract,byte.compare) |>
        dict.insert_with(_,tn2,n2,subtract,byte.compare) |>
        dict.insert_with(_,tn3,n3,subtract,byte.compare) |>
        dict.insert_with(_,tn4,n4,subtract,byte.compare)
      }

      // This path can only be taken if the above two matches failed which means the UTxO
      // has the wrong number of beacons.
      _ -> error @"UTxO input is not an Active UTxO"
    }
  }

  list.foldl(inputs,dict.new(),foo)
}

// Check that the required beacons have been burned.
pub fn valid_unlock_check(
  active_beacon_id: ActiveID,
  lower_bound: StartTime,
  inputs: List<Input>,
  beacons_minted: Dict<AssetName,Int>
) -> Bool {
  let req_burn =
    get_beacons_to_burn(
      active_beacon_id,
      lower_bound,
      inputs
    )

  trace_if_false(
    @"The wrong active beacons were burned",
    beacons_minted == req_burn
  )
}
