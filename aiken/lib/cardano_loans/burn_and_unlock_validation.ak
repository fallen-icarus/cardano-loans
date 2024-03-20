use aiken/bytearray as byte
use aiken/list
use aiken/dict.{Dict}
use aiken/time.{PosixTime}
use aiken/transaction.{Output, Input, Mint, Redeemer, ScriptPurpose, Spend}
use aiken/transaction/credential.{Address, Credential}
use aiken/transaction/value.{AssetName}

use cardano_loans/data/core.{ActiveId}
use cardano_loans/data/datums.{ActiveDatum}
use cardano_loans/data/redeemers.{LoanRedeemer,Unlock}
use cardano_loans/utils.{trace_if_false, error_if_false, parse_datum}

// Go through the inputs and build up a list of required beacons to burn. Cross-reference
// the redeemers map to only check inputs being spent with Unlock.
fn get_beacons_to_burn(
  active_beacon_id: ActiveId,
  loan_script_cred: Credential,
  lower_bound: PosixTime,
  inputs: List<Input>,
  redeemers: List<(ScriptPurpose,Redeemer)>,
  req_burn: Dict<AssetName,Int>
) -> Dict<AssetName,Int> {
  when (inputs,redeemers) is {
    // No inputs are left, just return the result.
    ( [] , _ ) -> req_burn

    // Mints appear first in the redeemer map so just skip them.
    ( _ , [(Mint(_),_), ..rest_reds] ) -> 
      get_beacons_to_burn(
        active_beacon_id,
        loan_script_cred,
        lower_bound,
        inputs,
        rest_reds,
        req_burn
      )

    // Compare the next input to the next redeemer. They are both assumed to be in
    // lexicographical order.
    ( [Input(ref,Output(Address(pay_cred,_) as addr,val,datum,_)), ..rest_ins]
    , [(purpose,used_redeemer), ..rest_reds]
    ) -> {
      // If the input is a loan script input, do all of the checks.
      if pay_cred == loan_script_cred {
        expect True = error_if_false(
          @"Inputs are not in lexicographical order",
          purpose == Spend(ref)
        )

        expect used_redeemer_: LoanRedeemer = used_redeemer

        when used_redeemer_ is {
          // This execution only cares about inputs using the Unlock redeemer.
          Unlock -> {
            // Get the beacons in this UTxO and pattern match on it.
            when value.tokens(val,core.from_active_id(active_beacon_id)) |> dict.to_list() is {
              // If beacons are not present, this is an invalid loan UTxO and can be spent
              // by the borrower. Just skip this UTxO.
              [] -> req_burn

              // If there are four beacons, this is an active UTxO. The claim period must
              // have passed for this UTxO.
              [(tn1,n1),(tn2,n2),(tn3,n3),(tn4,n4)] -> {
                // Get the ActiveDatum from the input.
                expect ActiveDatum{
                  claim_expiration,
                  ..
                } = parse_datum(datum)

                expect True = error_if_false(
                  @"Loan's claim period has not passed",
                  !utils.expiration_has_not_passed(claim_expiration,lower_bound)
                )

                let new_req_burn = req_burn |>
                  dict.insert_with(_,tn1,-n1,utils.sum_token_values,byte.compare) |>
                  dict.insert_with(_,tn2,-n2,utils.sum_token_values,byte.compare) |>
                  dict.insert_with(_,tn3,-n3,utils.sum_token_values,byte.compare) |>
                  dict.insert_with(_,tn4,-n4,utils.sum_token_values,byte.compare)

                get_beacons_to_burn(
                  active_beacon_id,
                  loan_script_cred,
                  lower_bound,
                  rest_ins, // Move on to the next input.
                  rest_reds, // Move on to the next redeemer.
                  new_req_burn
                )
              }

              // If there are three beacons, this is either a finished Active UTxO or an Offer
              // UTxO. This UTxO must have an ActiveDatum.
              [(tn1,n1),(tn2,n2),(tn3,n3)] -> {
                expect True = error_if_false(
                  @"Loan input is not an Active UTxO",
                  when parse_datum(datum) is {
                    ActiveDatum{..} -> True
                    _ -> False
                  }
                )

                let new_req_burn = req_burn |>
                  dict.insert_with(_,tn1,-n1,utils.sum_token_values,byte.compare) |>
                  dict.insert_with(_,tn2,-n2,utils.sum_token_values,byte.compare) |>
                  dict.insert_with(_,tn3,-n3,utils.sum_token_values,byte.compare)

                get_beacons_to_burn(
                  active_beacon_id,
                  loan_script_cred,
                  lower_bound,
                  rest_ins, // Move on to the next input.
                  rest_reds, // Move on to the next redeemer.
                  new_req_burn
                )
              }

              // This path can only be taken if the above two matches failed which means the UTxO
              // has the wrong number of beacons. SpendWithKeyNFT can only be used on Active
              // UTxOs.
              _ -> error @"UTxO input is not an Active UTxO"
            }
          }

          // If the input is using a different loan redeemer, it will be checked by the other
          // redeemer's logic. It can be safely skipped in this execution.
          _ ->
            get_beacons_to_burn(
              active_beacon_id,
              loan_script_cred,
              lower_bound,
              rest_ins, // Move on to the next input.
              rest_reds, // Move on to the next redeemer.
              req_burn
            )
        }

      // If it is not a loan script input, check if it is a pubkey input.
      } else if utils.is_pub_key_credential(pay_cred) {
        // If the input is a pubkey input, the current spend redeemer's input must still
        // be found.
        get_beacons_to_burn(
          active_beacon_id,
          loan_script_cred,
          lower_bound,
          rest_ins, // Move on for inputs.
          redeemers, // Keep the current redeemer.
          req_burn
        )

      // If none of the above branches matched, then this must be a input for another script.
      } else {
        // If the input is a script input that is not a loan's input, move on for both
        // the inputs and the redeemers.
        get_beacons_to_burn(
          active_beacon_id,
          loan_script_cred,
          lower_bound,
          rest_ins, // Move on for inputs.
          rest_reds, // Move on for redeemers.
          req_burn
        )
      }
    }

    _ -> error "Unmatched input"
  }
}

// Check that the required beacons have been burned.
pub fn valid_unlock(
  active_beacon_id: ActiveId,
  loan_script_cred: Credential,
  lower_bound: PosixTime,
  inputs: List<Input>,
  redeemers: Dict<ScriptPurpose,Redeemer>,
  beacons_minted: Dict<AssetName,Int>
) -> Bool {
  let req_burn =
    get_beacons_to_burn(
      active_beacon_id,
      loan_script_cred,
      lower_bound,
      inputs,
      dict.to_list(redeemers),
      dict.new()
    )

  trace_if_false(
    @"The wrong active beacons were burned",
    beacons_minted == req_burn
  )
}
