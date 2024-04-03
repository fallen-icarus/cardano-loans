use aiken/dict.{Dict}
use aiken/list
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

use cardano_loans/data/core.{
    ActiveId,
    BorrowerId,
    LoanId,
    AssetBeacon,
    Asset,
    Fraction,
    ScriptHash,
    NoPenalty
  }
use cardano_loans/data/datums.{ActiveDatum}
use cardano_loans/data/redeemers.{LoanRedeemer, MakePayment}
use cardano_loans/utils.{error_if_false}

type RequiredCollateralOutput {
  collateralization: List<(Asset,Fraction)>,
  collateral_is_swappable: Bool,
  active_beacon_id: ActiveId,
  loan_id: LoanId,
  asset_beacon: AssetBeacon,
  borrower_id: BorrowerId,
  is_full_payment: Bool,
  payment_amount: Int,
  relative_collteral_value: List<(Asset,Int)>,
  loan_outstanding: Fraction,
  req_datum: Datum
}

type RequiredPaymentOutput {
  active_beacon_id: ActiveId,
  lender_address: Address,
  loan_asset: Asset,
  loan_id: LoanId,
  payment_amount: Int
}

// Sum the relative collateral value lists and check that collateral_is_swappable is respected.
// In order to ensure collateral_is_swappable is respectively, this function just zips the 
// two lists to compare while independently summing them. 
fn sum_relative_collateral_values(
  in_values: List<(Asset,Int)>,
  out_values: List<(Asset,Int)>,
  collateral_is_swappable: Bool,
  acc: (Int,Int)
) -> (Int,Int) {
  when (in_values,out_values) is {
    ([(inAsset,inVal),..restIn],[(outAsset,outVal),..restOut]) -> {
      let (inRel,outRel) = acc

      // The lists are in reverse order of policy id and asset names. So Less means it should
      // appear later in the list and Greater means it should appear sooner.
      when utils.compare_asset(inAsset,outAsset) is {
        Equal -> {
          expect True = error_if_false(
            @"Collateral is not swappable",
            // Collateral can only be deposited if collateral_is_swappable is set to True.
            (outVal - inVal) <= 0 || collateral_is_swappable
          )

          sum_relative_collateral_values(
            restIn,
            restOut,
            collateral_is_swappable,
            (inRel + inVal,outRel + outVal)
          )
        }

        // The output is greater and needs to be processed separately before moving on.
        // This can only happen if a collateral asset was deposited.
        Less -> {
          expect True = error_if_false(
            @"Collateral is not swappable",
            // Collateral can only be deposited if collateral_is_swappable is set to True.
            collateral_is_swappable
          )

          sum_relative_collateral_values(
            in_values,
            restOut,
            collateral_is_swappable,
            (inRel,outRel + outVal)
          )
        }

        // The input is greater and needs to be processed separately before moving on.
        Greater -> {
          sum_relative_collateral_values(
            restIn,
            out_values,
            collateral_is_swappable,
            (inRel + inVal,outRel)
          )
        }
      }
    }

    ([(_,inVal),..restIn],[]) -> {
      let (inRel,outRel) = acc
      sum_relative_collateral_values(
        restIn,
        [],
        collateral_is_swappable,
        (inRel + inVal,outRel)
      )
    }

    // This scenario can only occur if a collateral asset was deposited.
    ([],[(_,outVal),..restOut]) -> {
      let (inRel,outRel) = acc

      expect True = error_if_false(
        @"Collateral is not swappable",
        // Collateral can only be deposited if collateral_is_swappable is set to True.
        collateral_is_swappable
      )

      sum_relative_collateral_values(
        [],
        restOut,
        collateral_is_swappable,
        (inRel,outRel + outVal)
      )
    }

    _ -> acc
  }
}

// Get the total relative value of the collateral. This also checks that the UTxO has the
// proper beacons. Whether the utxo should have the BorrowerId is toggleable so this function
// can be used with full payments. Returns the relative units of each collateral asset in the
// reverse order of the collateral list. Since this is also used to check the starting
// collateral value of loan inputs, all loans being paid must still contain a BorrowerId.
fn get_relative_collateral_values(
  utxo_value: Value,
  collateralization: List<(Asset,Fraction)>,
  active_beacon_id: ActiveId,
  loan_id: LoanId,
  asset_beacon: AssetBeacon,
  borrower_id: BorrowerId,
  should_have_borrower_id: Bool
) -> List<(Asset,Int)> {
  // A function to map over the value of this UTxO. It will return the relative value of
  // the collateral UTxO as well as whether the UTxO has the proper beacons. When
  // should_have_borrower_id is False, the UTxO should NOT have a BorrowerId.
  let foo = fn(x: (PolicyId,Dict<AssetName,Int>), acc:(List<(Asset,Int)>,Bool)) {
    let (cn,tns_dict) = x
    let (relative_collateral,has_beacons) = acc

    // Check the beacons.
    if cn == core.from_active_id(active_beacon_id) {
      let tns_list = dict.to_list(tns_dict)

      // A function to check each beacon.
      let beacon_check = fn(y: (AssetName,Int), inner_acc: (Bool,Bool,Bool,Bool)) {
        let (tn,n) = y
        let (active_check,asset_check,borrower_id_check,loan_id_check) = inner_acc
        if tn == "Active" {
          // There must only be one Active beacon.
          ( error_if_false(@"UTxO has too many Active beacons",n == 1)
          , asset_check
          , borrower_id_check
          , loan_id_check)
        } else if tn == core.get_asset_beacon_name(asset_beacon) {
          // There must only be one Asset beacon.
          ( active_check
          , error_if_false(@"UTxO has too many Asset beacons", n == 1)
          , borrower_id_check
          , loan_id_check)
        } else if tn == core.get_borrower_id_name(borrower_id) {
          // There must only be one BorrowerId beacon.
          (active_check, asset_check, n == 1, loan_id_check)
        } else if tn == core.get_loan_id_name(loan_id) {
          // There must only be one LoanId beacon.
          ( active_check
          , asset_check
          , borrower_id_check
          , error_if_false(@"UTxO has too many LoanIds", n == 1))
        } else {
          error @"UTxO has a beacon with the wrong name"
        }
      }

      when list.foldl(tns_list,(False,False,False,False),beacon_check) is {
        (True,True,False,True) -> {
          if should_have_borrower_id {
            error @"UTxO does not have exactly one BorrowerId"
          } else {
            (relative_collateral,True)
          }
        }

        (True,True,True,True) -> {
          if should_have_borrower_id {
            (relative_collateral,True)
          } else {
            error @"Finished loan's BorrowerId must be burned"
          }
        }

        _ -> error @"UTxO does not have the right beacons"
      }
    } 

    // Add up the collateral assets' relative values for this policy id.
    else {
      let non_beacon_toks = dict.to_list(tns_dict)

      let collat_check = fn(token_info: (AssetName,Int), inner_acc: List<(Asset,Int)>) {
        let (tn,n) = token_info
        let collatAsset = utils.find_by_key(collateralization,(cn,tn))
        when collatAsset is {
          None -> inner_acc
          Some((price_num,price_den)) -> {
            // If the price is zero, the accumulator will be unchanged. 
            if price_num == 0 { 
              inner_acc 
            } 
        
            // Calculate the value of the collateral in terms of the loan asset. Add it to the
            // accumulator. acc + collateral_quantity * reciprocal(price)
            else {
              list.push(inner_acc, ((cn,tn), n * price_den / price_num))
            }
          }
        }
      }

      // The starting accumulator should be the current count of relative collateral.
      (list.foldl(non_beacon_toks, relative_collateral, collat_check), has_beacons)
    }
  }

  let (relative_collateral,has_beacons) = 
    list.foldl( value.to_dict(utxo_value) |> dict.to_list(_),([],False),foo)

  expect True = error_if_false(
    @"Payment UTxO missing beacons",
    has_beacons
  )

  relative_collateral
}

// Determine what outputs are required based on the input.
fn process_input(
  old_datum: Datum, 
  payment_amount: Int, 
  upper_bound: Option<PosixTime>,
  old_value: Value
) -> (RequiredCollateralOutput,RequiredPaymentOutput) {
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
      loan_principle,
      compound_frequency,
      last_compounding,
      loan_term,
      loan_interest: (interest_num,_) as loan_interest,
      min_payment,
      penalty,
      collateralization,
      collateral_is_swappable,
      claim_expiration,
      loan_expiration,
      loan_outstanding: (outstanding_num,outstanding_den) as loan_outstanding,
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
        @"Next interest and/or penalty application required",
        // If the interest is set to zero and penalty is NoPenalty, the compound_frequency is irrelevant.
        (interest_num == 0 && penalty == NoPenalty) || when compound_frequency is {
          None -> True
          Some(freq) -> upper <= last_compounding + freq
        }
      )

      let is_full_payment = (payment_amount * outstanding_den) >= outstanding_num

      // Determine the required collateral datum given the payment amount.
      let req_collateral_output = RequiredCollateralOutput ( 
        collateralization, 
        collateral_is_swappable,
        core.to_active_id(active_beacon_id), 
        core.unsafe_to_loan_id(loan_id), 
        core.unsafe_to_asset_beacon(asset_beacon),
        core.unsafe_to_borrower_id(borrower_id), 
        is_full_payment,
        payment_amount,
        get_relative_collateral_values(
          old_value,
          collateralization,
          core.to_active_id(active_beacon_id),
          core.unsafe_to_loan_id(loan_id), 
          core.unsafe_to_asset_beacon(asset_beacon),
          core.unsafe_to_borrower_id(borrower_id), 
          True // The input must have a BorrowerId.
        ),
        loan_outstanding,
        ActiveDatum{
          active_beacon_id: active_beacon_id,
          payment_observer_hash: payment_observer_hash,
          interest_observer_hash: interest_observer_hash,
          address_update_observer_hash: address_update_observer_hash,
          borrower_id: borrower_id,
          lender_address: lender_address,
          loan_asset: loan_asset,
          asset_beacon: asset_beacon,
          loan_principle: loan_principle,
          compound_frequency: compound_frequency,
          last_compounding: last_compounding,
          loan_term: loan_term,
          loan_interest: loan_interest,
          min_payment: min_payment,
          penalty: penalty,
          collateralization: collateralization,
          collateral_is_swappable: collateral_is_swappable,
          claim_expiration: claim_expiration,
          loan_expiration: loan_expiration,
          loan_outstanding: utils.subtract_payment(outstanding_num,outstanding_den,payment_amount),
          total_epoch_payments: total_epoch_payments + payment_amount,
          loan_id: loan_id
        } |> InlineDatum
      )

      let req_payment_output = RequiredPaymentOutput(
        core.to_active_id(active_beacon_id),
        lender_address,
        loan_asset,
        core.unsafe_to_loan_id(loan_id),
        payment_amount

      )

      (req_collateral_output,req_payment_output)
    }

    _ -> error @"Input is not an Active UTxO"
  }
}

// Compare the inputs to the redeemer maps to get all inputs that are for making loan payments.
// It returns the required information for checking the outputs. All loan inputs must be for
// the same borrower.
fn get_input_info(
  loan_script_hash: ScriptHash,
  upper_bound: Option<PosixTime>,
  inputs: List<Input>,
  redeemers: List<(ScriptPurpose,Redeemer)>,
  result: (Option<Address>,List<RequiredCollateralOutput>,List<RequiredPaymentOutput>)
) -> (Option<Address>,List<RequiredCollateralOutput>,List<RequiredPaymentOutput>) {
  let (target_borrower,req_col_outputs,req_pay_outputs) = result

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
          // This execution only cares about inputs using the MakePayment redeemer.
          MakePayment(payment_amount) -> {
            let (req_collateral_output,req_payment_output) = process_input(
              datum,
              payment_amount,
              upper_bound,
              val
            )

            get_input_info(
              loan_script_hash,
              upper_bound,
              rest_ins,
              rest_reds,
              ( Some(addr),
                list.concat(req_col_outputs,[req_collateral_output]),
                list.concat(req_pay_outputs,[req_payment_output])
              )
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

// The collateral outputs are assumed to be in the same order as the inputs. The same is true
// for the payment outputs. The collateral output and corresponding payment output do not need
// to be next to each other. For example, it is possible to have all collateral outputs first,
// followed by the payment outputs.
//
// This function will crash with an error if any checks fail. The returned value is None if
// there were no full payments and Some(x,_,_) if there was at least one full payment, where x 
// is the number of full payments (and therefore BorrowerIds that must be burned. This function
// will crash if there are any full payments for a different borrower.
fn get_output_info(
  borrower_address: Address,
  req_col_outputs: List<RequiredCollateralOutput>,
  req_pay_outputs: List<RequiredPaymentOutput>,
  outputs: List<Output>,
  req_burn: Option<(Int,ActiveId,BorrowerId)>
) -> Option<(Int,ActiveId,BorrowerId)> {
  when (req_col_outputs,req_pay_outputs,outputs) is {
    // Only collateral outputs still need to be found.
    ( [req_collateral_output, ..rest_req_col_outs]
    , []
    , [Output(addr,val,d,_), ..rest_outs]
    ) -> {
      let RequiredCollateralOutput{
        collateralization, 
        collateral_is_swappable,
        active_beacon_id, 
        loan_id, 
        asset_beacon,
        borrower_id, 
        is_full_payment,
        payment_amount,
        relative_collteral_value: old_collateral_relative_values,
        loan_outstanding: (outstanding_num,outstanding_den),
        req_datum: req_collat_datum
      } = req_collateral_output

      // Check if this output is the next required collateral output.
      if addr == borrower_address && d == req_collat_datum {
        let new_collateral_relative_values = get_relative_collateral_values(
          val,
          collateralization,
          active_beacon_id,
          loan_id,
          asset_beacon,
          borrower_id,
          !is_full_payment // If it is a full payment, the output should not have a BorrowerId.
        )

        let (old_collateral_relative_value,new_collateral_relative_value) =
          sum_relative_collateral_values(
            old_collateral_relative_values,
            new_collateral_relative_values,
            collateral_is_swappable,
            (0,0)
          )

        // If this is a full payment, the target borrower id must be burned.
        if is_full_payment {
          when req_burn is {
            None -> 
              get_output_info(
                borrower_address,
                rest_req_col_outs, // Move on to the next req. collateral output.
                [],
                rest_outs,
                Some((1,active_beacon_id,borrower_id))
              )
            Some((num,pol,name)) -> {
              // All loans in this transaction must be for the same borrower.
              expect True = error_if_false(
                @"Full payment found for a different borrower",
                pol == active_beacon_id && name == borrower_id
              )

              get_output_info(
                borrower_address,
                rest_req_col_outs, // Move on to the next req. collateral output.
                [],
                rest_outs,
                Some((num+1,pol,name))
              )
            }
          }

        // If this is not a full payment, the collateral taken must be checked.
        } else {
          // The proportion of the collateral taken must be <= the proportion of the loan repaid.
          // Formula using only Int and no division (division is more expensive).
          expect True = error_if_false(
            @"Too much collateral taken",
            outstanding_num * (old_collateral_relative_value - new_collateral_relative_value) <=
              old_collateral_relative_value * payment_amount * outstanding_den
          )

          get_output_info(
            borrower_address,
            rest_req_col_outs, // Move on to the next req. collateral output.
            [],
            rest_outs,
            req_burn
          )
        }

      // This output can be ignored.
      } else {
        get_output_info(
          borrower_address,
          req_col_outputs, // Keep the current req. collateral output.
          [],
          rest_outs,
          req_burn
        )
      }
    }

    // Only the payment output still needs to be found for this loan input.
    ( []
    , [ req_payment_output, ..rest_req_pay_outs ]
    , [ Output(addr,val,d,_), ..rest_outs ]
    ) -> {
      let RequiredPaymentOutput{
        active_beacon_id,
        lender_address,
        loan_asset: (loan_asset_id,loan_asset_name),
        loan_id,
        payment_amount
      } = req_payment_output

      // Check if this output is the next required payment output.
      if addr == lender_address && d == InlineDatum((active_beacon_id,loan_id)) {
        expect True = error_if_false(
          @"Lender payment amount does not match redeemer",
          value.quantity_of(val,loan_asset_id,loan_asset_name) >= payment_amount
        )

        // Move on to the required outputs for the next loan input.
        get_output_info(
          borrower_address,
          [],
          rest_req_pay_outs, // Move on to the next req. payment output.
          rest_outs,
          req_burn
        )

      } else {
        get_output_info(
          borrower_address,
          [], 
          req_pay_outputs, // Keep the current req. payment output.
          rest_outs,
          req_burn
        )
      }
    }

    // Both of the required outputs still need to be found for this loan input.
    ( [req_collateral_output, ..rest_req_col_outs]
    , [req_payment_output, ..rest_req_pay_outs]
    , [Output(addr,val,d,_), ..rest_outs]
    ) -> {
      let RequiredCollateralOutput{
        collateralization, 
        collateral_is_swappable,
        active_beacon_id,
        loan_id: col_loan_id, // This may be a different one than the next req. payment.
        asset_beacon,
        borrower_id, 
        is_full_payment,
        payment_amount: col_payment_amount, // This may be different, too.
        relative_collteral_value: old_collateral_relative_values,
        loan_outstanding: (outstanding_num,outstanding_den),
        req_datum: req_collat_datum
      } = req_collateral_output

      let RequiredPaymentOutput{
        lender_address,
        loan_asset: (loan_asset_id,loan_asset_name),
        loan_id: pay_loan_id, // This may be a different one than the next req. collateral.
        payment_amount: pay_payment_amount, // This may be different, too.
        ..
      } = req_payment_output

      // Check if this output is the next required collateral output.
      if addr == borrower_address && d == req_collat_datum {
        let new_collateral_relative_values = get_relative_collateral_values(
          val,
          collateralization,
          active_beacon_id,
          col_loan_id,
          asset_beacon,
          borrower_id,
          !is_full_payment // If it is a full payment, the output should not have a BorrowerId.
        )

        let (old_collateral_relative_value,new_collateral_relative_value) =
          sum_relative_collateral_values(
            old_collateral_relative_values,
            new_collateral_relative_values,
            collateral_is_swappable,
            (0,0)
          )

        // If this is a full payment, the target borrower id must be burned.
        if is_full_payment {
          when req_burn is {
            None -> 
              get_output_info(
                borrower_address,
                rest_req_col_outs, // Move on to the next req. collateral output.
                req_pay_outputs,
                rest_outs,
                Some((1,active_beacon_id,borrower_id))
              )
            Some((num,pol,name)) -> {
              // All loans in this transaction must be for the same borrower.
              expect True = error_if_false(
                @"Full payment found for a different borrower",
                pol == active_beacon_id && name == borrower_id
              )

              get_output_info(
                borrower_address,
                rest_req_col_outs, // Move on to the next req. collateral output.
                req_pay_outputs,
                rest_outs,
                Some((num+1,pol,name))
              )
            }
          }

        // If this is not a full payment, the collateral taken must be checked.
        } else {
          // The proportion of the collateral taken must be <= the proportion of the loan repaid.
          // Formula using only Int and no division (division is more expensive): 
          expect True = error_if_false(
            @"Too much collateral taken",
            outstanding_num * (old_collateral_relative_value - new_collateral_relative_value) <=
              old_collateral_relative_value * col_payment_amount * outstanding_den
          )

          // The required payment output must still be found for this input.
          get_output_info(
            borrower_address,
            rest_req_col_outs, // Move on to the next req. collateral output.
            req_pay_outputs, // Keep the current req. payment output.
            rest_outs,
            req_burn
          )
        }

      // Check if this output is the next required payment output.
      } else if addr == lender_address && d == InlineDatum((active_beacon_id,pay_loan_id)) {
        expect True = error_if_false(
          @"Lender payment amount does not match redeemer",
          value.quantity_of(val,loan_asset_id,loan_asset_name) >= pay_payment_amount
        )

        get_output_info(
          borrower_address,
          req_col_outputs, // Keep the current req. collateral output.
          rest_req_pay_outs, // Move on to the next req. payment output.
          rest_outs,
          req_burn
        )

      // This output can be ignored.
      } else {
        get_output_info(
          borrower_address,
          req_col_outputs, // Keep the current req. collateral output.
          req_pay_outputs, // Keep the current req. payment output.
          rest_outs,
          req_burn
        )
      }
    }

    // All required outputs have been found.
    ([], [], _) -> req_burn

    _ -> error "Not all required outputs found"
  }
}

// Check all payments for a borrower and return the borrower address used so that the borrower's
// approval can be checked.
pub fn valid_payment(
  inputs: List<Input>,
  outputs: List<Output>,
  redeemers: Dict<ScriptPurpose,Redeemer>,
  mint: Value,
  loan_script_hash: ScriptHash,
  upper_bound: Option<PosixTime>
) -> Address {
  let (target_borrower,req_col_outputs,req_pay_outputs) = get_input_info(
    loan_script_hash,
    upper_bound,
    inputs,
    dict.to_list(redeemers),
    (None,[],[])
  )

  when target_borrower is {
    None -> error @"No payment inputs found"
    Some(addr) -> {
      // If there were any full payments, the BorrowerIds must be burned in isolation.
      when get_output_info(addr,req_col_outputs,req_pay_outputs,outputs,None) is {
        // All required outputs were found and there were no full payments.
        None -> addr // Return the borrower's address so approval can be checked.

        // All required outputs were found and there was at least one full payment.
        Some((num,active_beacon_id,borrower_id)) -> {
          when value.tokens(mint,core.from_active_id(active_beacon_id)) |> dict.to_list(_) is {
            [] -> error @"BorrowerId(s) not burned"

            [(tn,n)] -> {
              expect True = error_if_false(
                @"BorrowerId(s) not burned",
                and {
                  tn == core.get_borrower_id_name(borrower_id),
                  n == -num
                }
              )

              addr // Return the borrower's address so approval can be checked.
            }

            _ -> error @"BorrowerId(s) not burned in isolation"
          }
        }
      }
    }
  }
}
