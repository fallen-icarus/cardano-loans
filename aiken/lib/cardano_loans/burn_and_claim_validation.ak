use aiken/bytearray as byte
use aiken/dict.{Dict}
use aiken/time.{PosixTime}
use aiken/transaction.{Output, Input, Redeemer, ScriptPurpose, Spend}
use aiken/transaction/credential.{Address, Credential}
use aiken/transaction/value.{AssetName}

use cardano_loans/data/core.{ActiveId}
use cardano_loans/data/datums.{ActiveDatum}
use cardano_loans/data/redeemers.{LoanRedeemer,SpendWithKeyNFT}
use cardano_loans/utils.{trace_if_false, error_if_false, parse_datum}

// Go through the inputs and build up a list of required beacons to burn. Cross-reference
// the redeemers map to only check inputs being spent with SpendWithKeyNFT. The loan inputs
// do not need to come from the same loan address.
fn get_beacons_to_burn(
  active_beacon_id: ActiveId,
  loan_script_cred: Credential,
  lower_bound: PosixTime,
  inputs: List<Input>,
  redeemers: List<(ScriptPurpose,Redeemer)>,
  req_burn: Dict<AssetName,Int>
) -> Dict<AssetName,Int> {
  when (inputs,redeemers) is {
    // Compare the next input to the next redeemer. They are both assumed to be in
    // lexicographical order.
    ( [Input(in_ref,Output(Address(pay_cred,_),val,datum,_)), ..rest_ins]
    , [(Spend(red_ref),used_redeemer), ..rest_reds]
    ) -> {
      // If the input is a loan script input, do all of the checks.
      if pay_cred == loan_script_cred {
        expect True = error_if_false(
          @"Inputs are not in lexicographical order",
          in_ref == red_ref
        )

        expect used_redeemer_: LoanRedeemer = used_redeemer
        when used_redeemer_ is {
          // This execution only cares about inputs using the SpendWithKeyNFT redeemer.
          SpendWithKeyNFT -> {
            // Get the beacons in this UTxO and pattern match on it.
            when value.tokens(val,core.from_active_id(active_beacon_id)) |> dict.to_list() is {
              // If beacons are not present, this is an invalid loan UTxO and custody belongs
              // to the borrower.
              [] -> error @"Invalid loan UTxO among inputs"

              // If there are four beacons, this is an active UTxO.
              [(_,_),(_,_),(_,_),(_,_)] -> {
                // Get the ActiveDatum from the input. The only way to figure out which beacon is 
                // the LoanId is to check the datum. Two units of the LoanId must be burned.
                expect ActiveDatum{
                  borrower_id,
                  asset_beacon,
                  loan_expiration,
                  loan_id,
                  ..
                } = parse_datum(datum)

                // Check if the loan has expired using the invalid-before as proof that the time
                // has indeed passed.
                expect True = error_if_false(
                  @"Loan is not expired",
                  !utils.expiration_has_not_passed(loan_expiration,lower_bound)
                )

                let new_req_burn = req_burn |>
                  dict.insert_with(_,"Active",-1,utils.sum_token_values,byte.compare) |>
                  dict.insert_with(_,borrower_id,-1,utils.sum_token_values,byte.compare) |>
                  dict.insert_with(_,asset_beacon,-1,utils.sum_token_values,byte.compare) |>
                  dict.insert_with(_,loan_id,-2,utils.sum_token_values,byte.compare)

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
              // has the wrong number of beacons. This can only happen if the UTxO is a finished
              // Active UTxO (ie, missing the BorrowerId but still has the rest of the beacons). 
              // SpendWithKeyNFT can only be used on live Active UTxOs.
              _ -> error @"UTxO input is a finished loan"
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

    // If the redeemer is not a Spend purpose, skip it.
    (_ , [_, ..rest_reds]) -> 
      get_beacons_to_burn(
        active_beacon_id,
        loan_script_cred,
        lower_bound,
        inputs,
        rest_reds,
        req_burn
      )

    // There are no more redeemers to check.
    _ -> req_burn
  }
}

// Check that the required beacons have been burned.
pub fn valid_expired_claim(
  active_beacon_id: ActiveId,
  loan_script_cred: Credential,
  lower_bound: PosixTime,
  inputs: List<Input>,
  redeemers: Dict<ScriptPurpose,Redeemer>,
  beacons_minted: Dict<AssetName,Int>
) -> Bool {
  trace_if_false(
    @"The wrong active beacons were burned",
    beacons_minted ==
      get_beacons_to_burn(
        active_beacon_id,
        loan_script_cred,
        lower_bound,
        inputs,
        dict.to_list(redeemers),
        dict.new()
      )
  )
}
