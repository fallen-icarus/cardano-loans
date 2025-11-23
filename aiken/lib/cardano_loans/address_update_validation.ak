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

use cardano_loans/data/core.{ActiveId, LoanId, ScriptHash}
use cardano_loans/data/datums.{ActiveDatum}
use cardano_loans/data/redeemers.{LoanRedeemer, UpdateLenderAddress}
use cardano_loans/utils.{error_if_false}

type RequiredCollateralOutput {
  borrower_address: Address,
  req_value: List<(PolicyId,Dict<AssetName,Int>)>,
  req_datum: Datum
}

type RequiredKeyOutput {
  new_address: Address,
  loan_id: LoanId,
  active_beacon_id: ActiveId
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
    @"Address update input missing beacons",
    found_borrower_id
  )

  new_val
}

// Get the required output information based on the input information.
fn process_input(
  borrower_address,
  old_datum: Datum, 
  proxy_hash: ScriptHash,
  new_address: Address,
  deposit_amount: Int, 
  upper_bound: Option<PosixTime>,
  old_value: Value
) -> (RequiredCollateralOutput,RequiredKeyOutput) {
  when utils.parse_datum(old_datum) is {
    ActiveDatum{
      active_beacon_id,
      payment_observer_hash,
      address_update_observer_hash,
      borrower_id,
      loan_asset,
      asset_beacon,
      loan_principal,
      epoch_duration,
      last_epoch_boundary,
      loan_term,
      loan_interest,
      compounding_interest: compounding_interest,
      min_payment,
      penalty,
      max_consecutive_misses,
      collateralization,
      collateral_is_swappable,
      claim_expiration,
      loan_expiration,
      loan_outstanding,
      total_epoch_payments,
      current_consecutive_misses,
      loan_id,
      ..
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
        @"Invalid new lender address",
        utils.valid_lender_address(proxy_hash,new_address)
      )

      ( RequiredCollateralOutput( 
          borrower_address,
          add_deposit_value(deposit_amount,old_value,active_beacon_id,borrower_id),
          ActiveDatum{
            active_beacon_id: active_beacon_id,
            payment_observer_hash: payment_observer_hash,
            address_update_observer_hash: address_update_observer_hash,
            borrower_id: borrower_id,
            lender_address: new_address,
            loan_asset: loan_asset,
            asset_beacon: asset_beacon,
            loan_principal: loan_principal,
            epoch_duration: epoch_duration,
            last_epoch_boundary: last_epoch_boundary,
            loan_term: loan_term,
            loan_interest: loan_interest,
            compounding_interest: compounding_interest,
            min_payment: min_payment,
            penalty: penalty,
            max_consecutive_misses: max_consecutive_misses,
            collateralization: collateralization,
            collateral_is_swappable: collateral_is_swappable,
            claim_expiration: claim_expiration,
            loan_expiration: loan_expiration,
            loan_outstanding: loan_outstanding,
            total_epoch_payments: total_epoch_payments,
            current_consecutive_misses: current_consecutive_misses,
            loan_id: loan_id
          } |> InlineDatum
        )
      , RequiredKeyOutput{
          new_address: new_address,
          loan_id: core.unsafe_to_loan_id(loan_id),
          active_beacon_id: core.to_active_id(active_beacon_id)
        }
      )
    }

    _ -> error @"Input is not an Active UTxO"
  }
}

// Compare the inputs to the redeemer maps to get all inputs that are for address updates.
// It returns the required information for checking the outputs. 
fn get_input_info(
  loan_script_hash: ScriptHash,
  proxy_hash: ScriptHash,
  upper_bound: Option<PosixTime>,
  inputs: List<Input>,
  redeemers: List<(ScriptPurpose,Redeemer)>,
  result: (List<RequiredCollateralOutput>,List<RequiredKeyOutput>)
) -> (List<RequiredCollateralOutput>,List<RequiredKeyOutput>) {
  let (req_col_outputs,req_key_outputs) = result

  when (inputs,redeemers) is {
    // Compare the next input to the next redeemer. They are both assumed to be in
    // lexicographical order.
    ( [ Input(in_ref,Output(Address(pay_cred,_) as addr,val,datum,_)), ..rest_ins ]
    , [ (Spend(red_ref),used_redeemer), ..rest_reds ]
    ) -> {
      // If the input is a loan script input, do all of the checks.
      if pay_cred == ScriptCredential(loan_script_hash) {
        expect True = error_if_false(
          @"Inputs are not in lexicographical order",
          in_ref == red_ref
        )

        expect used_redeemer_: LoanRedeemer = used_redeemer
        when used_redeemer_ is {
          // This execution only cares about inputs using the UpdateLenderAddress redeemer.
          UpdateLenderAddress{new_address,deposit_increase} -> {
            let (new_req_collateral_output,new_req_key_output) = 
              process_input(
                addr,
                datum,
                proxy_hash,
                new_address,
                deposit_increase,
                upper_bound,
                val
              )

            get_input_info(
              loan_script_hash,
              proxy_hash,
              upper_bound,
              rest_ins,
              rest_reds,
              ( list.concat(req_col_outputs,[new_req_collateral_output]),
                list.concat(req_key_outputs,[new_req_key_output])
              )
            )
          }

          // If the input is using a different loan redeemer, it will be checked by the other
          // redeemer's logic. It can be safely skipped in this execution.
          _ ->
            get_input_info(
              loan_script_hash,
              proxy_hash,
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
          proxy_hash,
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
          proxy_hash,
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
        proxy_hash,
        upper_bound,
        inputs,
        rest_reds,
        result
      )

    // There are no more redeemers to check.
    _ -> result
  }
}

// The required collateral outputs are assumed to be in the same order as the
// inputs (ie, the collateral output for loan input 1 are before the collateral output for loan 
// input 2). Likewise, the required key outputs are also assumed to be in the same order as the
// inputs. The reqquired collateral output and the associated required key output do NOT need to be
// next to each other. The script will fail if either of these two assumptions are not true. 
// These assumptions dramatically improve performance.
//
// This function will crash with an error if any checks fail.
fn get_output_info(
  req_col_outputs: List<RequiredCollateralOutput>,
  req_key_outputs: List<RequiredKeyOutput>,
  outputs: List<Output>,
) -> Bool {
  when (req_col_outputs,req_key_outputs,outputs) is {
    // Only collateral outputs still need to be found.
    ( [req_collateral_output, ..rest_req_col_outs]
    , []
    , [Output(addr,val,d,_), ..rest_outs]
    ) -> {
      let RequiredCollateralOutput{
        borrower_address,
        req_value,
        req_datum: req_collat_datum
      } = req_collateral_output

      // Check if this output is the next required collateral output.
      if addr == borrower_address && d == req_collat_datum {
        expect True = error_if_false(
          @"Collateral output has wrong value",
          (value.to_dict(val) |> dict.to_list(_)) == req_value
        )

        get_output_info(
          rest_req_col_outs,
          [],
          rest_outs
        )
      } else {
        get_output_info(
          req_col_outputs,
          [],
          rest_outs
        )
      }
    }

    // Only the key output still needs to be found for this loan input.
    ( []
    , [ req_key_output, ..rest_req_key_outs ]
    , [ Output(addr,val,d,_), ..rest_outs ]
    ) -> {
      let RequiredKeyOutput{
        new_address,
        loan_id,
        active_beacon_id,
      } = req_key_output

      // Check if this output is the next required key output.
      if new_address == addr && d == InlineDatum((active_beacon_id,loan_id)) {
        expect True = error_if_false(
          @"Key output does not have exactly one Key NFT",
          value.quantity_of(
            val,
            core.from_active_id(active_beacon_id),
            core.get_loan_id_name(loan_id)
          ) == 1
        )

        // Move on to the required outputs for the next loan input.
        get_output_info(
          [],
          rest_req_key_outs, // Move on to the next req. payment output.
          rest_outs
        )
      } else {
        get_output_info(
          [],
          req_key_outputs,
          rest_outs
        )
      }
    }

    // Both of the required outputs still need to be found for this loan input.
    ( [req_collateral_output, ..rest_req_col_outs]
    , [req_key_output, ..rest_req_key_outs]
    , [Output(addr,val,d,_), ..rest_outs]
    ) -> {
      let RequiredCollateralOutput{
        borrower_address,
        req_value,
        req_datum: req_collat_datum
      } = req_collateral_output

      let RequiredKeyOutput{
        new_address,
        loan_id,
        active_beacon_id,
      } = req_key_output

      // Check if this output is the next required collateral output.
      if addr == borrower_address && d == req_collat_datum {
        expect True = error_if_false(
          @"Collateral output has wrong value",
          (value.to_dict(val) |> dict.to_list(_)) == req_value
        )

        get_output_info(
          rest_req_col_outs,
          req_key_outputs,
          rest_outs
        )

      // Check if this output is the next required key output.
      } else if new_address == addr && d == InlineDatum((active_beacon_id,loan_id)) {
        expect True = error_if_false(
          @"Key output does not have exactly one Key NFT",
          value.quantity_of(
            val,
            core.from_active_id(active_beacon_id),
            core.get_loan_id_name(loan_id)
          ) == 1
        )

        // Move on to the required outputs for the next loan input.
        get_output_info(
          req_col_outputs,
          rest_req_key_outs, // Move on to the next req. payment output.
          rest_outs
        )
      } else {
        get_output_info(
          req_col_outputs,
          req_key_outputs,
          rest_outs
        )
      }
    }

    // All required outputs have been found.
    ([], [], _) -> True

    _ -> error "Not all required outputs found"
  }
}

// Check all address updates for a lender.
pub fn valid_address_update(
  inputs: List<Input>,
  outputs: List<Output>,
  redeemers: Dict<ScriptPurpose,Redeemer>,
  loan_script_hash: ScriptHash,
  proxy_hash: ScriptHash,
  upper_bound: Option<PosixTime>
) -> Bool {
  let (req_col_outputs,req_key_outputs) = get_input_info(
    loan_script_hash,
    proxy_hash,
    upper_bound,
    inputs,
    dict.to_list(redeemers),
    ([],[])
  )

  // This has its own error messages.
  get_output_info(req_col_outputs,req_key_outputs,outputs)
}
