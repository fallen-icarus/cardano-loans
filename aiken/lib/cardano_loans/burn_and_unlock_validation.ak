use aiken/bytearray as byte
use aiken/dict.{Dict}
use aiken/time.{PosixTime}
use aiken/transaction.{Output, Input, Redeemer, ScriptPurpose, Spend}
use aiken/transaction/credential.{Address, Credential}
use aiken/transaction/value.{AssetName}

use cardano_loans/data/core.{ActiveId}
use cardano_loans/data/datums.{ActiveDatum}
use cardano_loans/data/redeemers.{LoanRedeemer,Unlock}
use cardano_loans/utils.{error_if_false, parse_datum}

// Go through the inputs and build up a list of required beacons to burn. Cross-reference
// the redeemers map to only check inputs being spent with Unlock. It also returns the borrower's
// address so that approval can be checked.
fn get_beacons_to_burn(
  active_beacon_id: ActiveId,
  loan_script_cred: Credential,
  lower_bound: Option<PosixTime>,
  inputs: List<Input>,
  redeemers: List<(ScriptPurpose,Redeemer)>,
  req_burn: Dict<AssetName,Int>,
  borrower_addr: Option<Address>
) -> (Option<Address>,Dict<AssetName,Int>) {
  when (inputs,redeemers) is {
    // Compare the next input to the next redeemer. They are both assumed to be in
    // lexicographical order.
    ( [Input(in_ref,Output(Address(pay_cred,_) as addr,val,datum,_)), ..rest_ins]
    , [(Spend(red_ref),used_redeemer), ..rest_reds]
    ) -> {
      // If the input is a loan script input, do all of the checks.
      if pay_cred == loan_script_cred {
        expect True = error_if_false(
          @"Inputs are not in lexicographical order",
          in_ref == red_ref
        )

        expect True = error_if_false(
          @"All loan inputs must be for the same borrower",
          when borrower_addr is {
            None -> True
            Some(x) -> addr == x
          }
        )

        expect used_redeemer_: LoanRedeemer = used_redeemer
        when used_redeemer_ is {
          // This execution only cares about inputs using the Unlock redeemer.
          Unlock -> {
            // Get the beacons in this UTxO and pattern match on it.
            when value.tokens(val,core.from_active_id(active_beacon_id)) |> dict.to_list() is {
              // If beacons are not present, this is an invalid loan UTxO and can be spent
              // by the borrower. Just skip this UTxO. The spending script independently checks
              // that the input has an ActiveDatum and the required borrower approves.
              [] ->
                get_beacons_to_burn(
                  active_beacon_id,
                  loan_script_cred,
                  lower_bound,
                  rest_ins, // Move on to the next input.
                  rest_reds, // Move on to the next redeemer.
                  req_burn,
                  borrower_addr
                )

              // If there are four beacons, this is an active UTxO. The claim period must
              // have passed for this UTxO.
              [(tn1,n1),(tn2,n2),(tn3,n3),(tn4,n4)] -> {
                // Get the ActiveDatum from the input.
                expect ActiveDatum{
                  claim_expiration,
                  ..
                } = parse_datum(datum)

                // Check that the claim period has passed using invalid-before.
                expect True = error_if_false(
                  @"Loan's claim period has not passed",
                  when lower_bound is {
                    None -> error @"invalid-before not specified"
                    Some(x) -> !utils.expiration_has_not_passed(claim_expiration,x)
                  }
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
                  new_req_burn,
                  Some(addr)
                )
              }

              // This path can only be taken if the above two matches failed which should never
              // happen.
              _ -> error @"E2"
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
              req_burn,
              borrower_addr
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
          req_burn,
          borrower_addr
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
          req_burn,
          borrower_addr
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
        req_burn,
        borrower_addr
      )

    // There are no more redeemers to check.
    _ -> (borrower_addr,req_burn)
  }
}

// Check that the required beacons have been burned. Returns the borrower's address so that
// approval can be checked.
pub fn valid_unlock(
  active_beacon_id: ActiveId,
  loan_script_cred: Credential,
  lower_bound: Option<PosixTime>,
  inputs: List<Input>,
  redeemers: Dict<ScriptPurpose,Redeemer>,
  beacons_minted: Dict<AssetName,Int>
) -> Address {
  let (borrower_addr,req_burn) =
    get_beacons_to_burn(
      active_beacon_id,
      loan_script_cred,
      lower_bound,
      inputs,
      dict.to_list(redeemers),
      dict.new(),
      None
    )

  when borrower_addr is {
    None -> error @"No valid unlock inputs found"
    Some(addr) -> {
      expect True = error_if_false(
        @"The wrong active beacons were burned",
        beacons_minted == req_burn
      )

      addr // Return the address for checking approval.
    }
  }

}
