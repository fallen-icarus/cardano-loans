use aiken/dict.{Dict}
use aiken/list
use aiken/bytearray as byte
use aiken/option
use aiken/time.{PosixTime}
use aiken/transaction.{
    Datum,
    InlineDatum,
    Output,
    Input,
    ScriptPurpose,
    Redeemer,
    Spend
  }
use aiken/transaction/credential.{Address, ScriptCredential}
use aiken/transaction/value.{Value, PolicyId, AssetName}

use cardano_loans/data/core.{ScriptHash}
use cardano_loans/data/datums.{ActiveDatum}
use cardano_loans/data/redeemers.{LoanRedeemer, ApplyInterest}
use cardano_loans/utils.{error_if_false}

type RequiredCollateralOutput {
  req_value: List<(PolicyId,Dict<AssetName,Int>)>,
  req_datum: Datum
}

// Add the deposit increase to the ada balance and check that the BorrowerId is still present.
fn add_deposit_value(
  deposit_amount: Int,
  old_value: Value,
  active_beacon_id: PolicyId,
  borrower_id: AssetName,
) -> List<(PolicyId,Dict<AssetName,Int>)> {
  // A function to map over the value of this UTxO. 
  let foo = fn(x: (PolicyId,Dict<AssetName,Int>), acc: (List<(PolicyId,Dict<AssetName,Int>)>,Bool)) {
    let (cn,tns_dict) = x
    let (new_val,found_borrower_id) = acc

    // Check for the borrower_id.
    if cn == active_beacon_id {
      ( list.concat(new_val,[(cn,tns_dict)])
      , dict.get(tns_dict,borrower_id) |> option.is_some(_)
      )
    } else if cn == #"" {
      ( list.concat(
          new_val,
          [(cn,dict.insert_with(tns_dict,#"",deposit_amount,utils.sum_token_values,byte.compare))]
        )
      , found_borrower_id
      )
    } else {
      ( list.concat(new_val,[(cn,tns_dict)])
      , found_borrower_id
      )
    }
  }

  let (new_val,found_borrower_id) = 
    list.foldl(
      value.to_dict(old_value) |> dict.to_list(_),
      ([],False),
      foo
    )

  expect True = error_if_false(
    @"Interest input missing Borrower Id",
    found_borrower_id
  )

  new_val
}

// Get the required output information based on the input information.
fn process_input(
  old_datum: Datum, 
  deposit_amount: Int, 
  number_of_times: Int,
  upper_bound: Option<PosixTime>,
  old_value: Value
) -> RequiredCollateralOutput {
  when utils.parse_datum(old_datum) is {
    ActiveDatum{
      active_beacon_id,
      payment_observer_hash,
      interest_observer_hash,
      address_update_observer_hash,
      borrower_id,
      lender_address,
      loan_asset,
      asset_beacon,
      loan_principal,
      epoch_duration,
      last_epoch_boundary,
      loan_term,
      loan_interest,
      compounding_interest,
      min_payment,
      penalty,
      collateralization,
      collateral_is_swappable,
      claim_expiration,
      loan_expiration,
      loan_outstanding,
      total_epoch_payments,
      loan_id
    } -> {
      let upper = when upper_bound is {
        None -> error @"invalid-hereafter not specified"
        Some(x) -> x
      }

      expect True = error_if_false(
        @"Loan is expired",
        utils.expiration_has_not_passed(
          loan_expiration,
          upper
        )
      )

      expect True = error_if_false(
        @"Deposit increase not >= 0",
        deposit_amount >= 0
      )

      expect True = error_if_false(
        @"Number of times not >= 1",
        number_of_times >= 1
      )

      // Determine the required collateral datum given the payment amount.
      RequiredCollateralOutput( 
        add_deposit_value(deposit_amount,old_value,active_beacon_id,borrower_id),
        ActiveDatum{
          active_beacon_id: active_beacon_id,
          payment_observer_hash: payment_observer_hash,
          interest_observer_hash: interest_observer_hash,
          address_update_observer_hash: address_update_observer_hash,
          borrower_id: borrower_id,
          lender_address: lender_address,
          loan_asset: loan_asset,
          asset_beacon: asset_beacon,
          loan_principal: loan_principal,
          epoch_duration: epoch_duration,
          last_epoch_boundary: 
            last_epoch_boundary + number_of_times * option.or_else(epoch_duration,0),
          loan_term: loan_term,
          loan_interest: loan_interest,
          compounding_interest: compounding_interest,
          min_payment: min_payment,
          penalty: penalty,
          collateralization: collateralization,
          collateral_is_swappable: collateral_is_swappable,
          claim_expiration: claim_expiration,
          loan_expiration: loan_expiration,
          loan_outstanding: 
            utils.compound_n_times(
              min_payment > total_epoch_payments, // Penalize if min_payment not met.
              penalty,
              loan_outstanding,
              // Don't apply interest unless it is compounding interest.
              if compounding_interest {loan_interest} else {(0,1)},
              number_of_times
            ),
          total_epoch_payments: 0,
          loan_id: loan_id
        } |> InlineDatum
      )
    }

    _ -> error @"Input is not an Active UTxO"
  }
}

// Compare the inputs to the redeemer maps to get all inputs that are for interest applications.
// It returns the required information for checking the outputs. All loan inputs must be for
// the same borrower.
fn get_input_info(
  loan_script_hash: ScriptHash,
  upper_bound: Option<PosixTime>,
  inputs: List<Input>,
  redeemers: List<(ScriptPurpose,Redeemer)>,
  result: (Option<Address>,List<RequiredCollateralOutput>)
) -> (Option<Address>,List<RequiredCollateralOutput>) {
  let (target_borrower,req_outputs) = result

  when (inputs,redeemers) is {
    // Compare the next input to the next redeemer. They are both assumed to be in
    // lexicographical order.
    ( [ Input(in_ref,Output(Address(pay_cred,_) as addr,val,datum,_)), ..rest_ins ]
    , [ (Spend(red_ref),used_redeemer), ..rest_reds ]
    ) -> {
      // If the input is a loan script input, do all of the checks.
      if pay_cred == ScriptCredential(loan_script_hash) {
        expect True = error_if_false(
          @"All loan inputs must be for the same borrower",
          when target_borrower is {
            None -> True
            Some(x) -> addr == x
          }
        )

        expect True = error_if_false(
          @"Inputs are not in lexicographical order",
          in_ref == red_ref
        )

        expect used_redeemer_: LoanRedeemer = used_redeemer
        when used_redeemer_ is {
          // This execution only cares about inputs using the ApplyInterest redeemer.
          ApplyInterest{deposit_increase,number_of_times} -> {
            let req_collateral_output = 
              process_input(
                datum,
                deposit_increase,
                number_of_times,
                upper_bound,
                val
              )

            get_input_info(
              loan_script_hash,
              upper_bound,
              rest_ins,
              rest_reds,
              ( Some(addr), list.concat(req_outputs,[req_collateral_output]) )
            )
          }

          // If the input is using a different loan redeemer, it will be checked by the other
          // redeemer's logic. It can be safely skipped in this execution.
          _ ->
            get_input_info(
              loan_script_hash,
              upper_bound,
              rest_ins,
              rest_reds,
              result
            )
        }

      // If it is not a loan script input, check if it is a pubkey input.
      } else if utils.is_pub_key_credential(pay_cred) {
        // If the input is a pubkey input, the current spend redeemer's input must still
        // be found.
        get_input_info(
          loan_script_hash,
          upper_bound,
          rest_ins, // Move on for inputs.
          redeemers, // Keep the current redeemer.
          result
        )

      // If none of the above branches matched, then this must be a input for another script.
      } else {
        // If the input is a script input that is not a loan's input, move on for both
        // the inputs and the redeemers.
        get_input_info(
          loan_script_hash,
          upper_bound,
          rest_ins, // Move on for inputs.
          rest_reds, // Move on for redeemers.
          result
        )
      }
    }

    // If the redeemer is not a Spend purpose, skip it.
    (_ , [_, ..rest_reds]) -> 
      get_input_info(
        loan_script_hash,
        upper_bound,
        inputs,
        rest_reds,
        result
      )

    // There are no more redeemers to check.
    _ -> result
  }
}

// The outputs are also assumed to be in the same order as the inputs (ie, the outputs for loan
// input 1 are before the outputs for loan input 2). The script will fail if this assumption is not
// true. This assumption dramatically improves performance. This function will crash with an error 
// if any checks fail. 
fn get_output_info(
  borrower_address: Address,
  req_outputs: List<RequiredCollateralOutput>,
  outputs: List<Output>,
) -> Bool {
  when (req_outputs,outputs) is {
    // The collateral output still needs to be found for this loan input.
    ( [req_collateral_output, ..rest_req_outs]
    , [Output(addr,val,d,_), ..rest_outs]
    ) -> {
      let RequiredCollateralOutput{
        req_value,
        req_datum: req_collat_datum
      } = req_collateral_output

      // Check if this output is the next required collateral output.
      if addr == borrower_address && d == req_collat_datum {
        expect True = error_if_false(
          @"Interest output has the wrong value",
          req_value == (value.to_dict(val) |> dict.to_list(_))
        )

        get_output_info(
          borrower_address,
          rest_req_outs,
          rest_outs
        )
      } else {
        get_output_info(
          borrower_address,
          req_outputs,
          rest_outs
        )
      }
    }

    // All required outputs have been found.
    ([], _) -> True

    _ -> error "Not all required outputs found"
  }
}

// Check all interest applications for a borrower and return the borrower address used so 
// that the borrower's approval can be checked.
pub fn valid_interest_application(
  inputs: List<Input>,
  outputs: List<Output>,
  redeemers: Dict<ScriptPurpose,Redeemer>,
  loan_script_hash: ScriptHash,
  upper_bound: Option<PosixTime>
) -> Address {
  let (target_borrower,req_outputs) = get_input_info(
    loan_script_hash,
    upper_bound,
    inputs,
    dict.to_list(redeemers),
    (None,[])
  )

  when target_borrower is {
    None -> error @"No interest inputs found"
    Some(x) -> {
      // This has its own error messages.
      expect True = get_output_info(x,req_outputs, outputs)

      x // Return the borrower's address so that approval can be checked.
    }
  }
}
