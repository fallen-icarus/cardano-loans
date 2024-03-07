use aiken/bytearray as byte
use aiken/dict.{Dict}
use aiken/option
use aiken/list
use aiken/transaction.{
    Datum,
    InlineDatum,
    Output
  }
use aiken/transaction/credential.{
    Address,
  }
use aiken/transaction/value.{
    Value,
    PolicyId,
    AssetName
  }

use cardano_loans/data/core.{
    ExpirationTime,
    ActiveID,
    BorrowerID
  }
use cardano_loans/data/datums.{
    LoanDatum,
    ActiveDatum
  }
use cardano_loans/utils.{
    error_if_false,
  }

// Get the required output datum. Also check that the loan is not expired yet. Returns the
// active_beacon_id and borrower_id so that the presence of the BorrowerID can be checked.
fn get_required_datum(
  upper_bound: ExpirationTime, 
  old_datum: LoanDatum
) -> (ActiveID,BorrowerID,Datum) {
  when old_datum is {
    ActiveDatum(
      active_beacon_id,
      payment_observer_hash,
      borrower_id,
      lender_address,
      loan_asset,
      asset_beacon,
      loan_principle,
      rollover_frequency,
      last_checkpoint,
      loan_term,
      loan_interest,
      min_payment,
      collateralization,
      collateral_is_swappable,
      claim_expiration,
      loan_expiration,
      loan_outstanding,
      loan_id
    ) -> {
      expect True = error_if_false(
        @"Loan is expired",
        core.expiration_time_has_not_passed(
          upper_bound,
          loan_expiration
        )
      )

      ( active_beacon_id,
        borrower_id,
        ActiveDatum{
          active_beacon_id: active_beacon_id,
          payment_observer: payment_observer_hash,
          borrower_id: borrower_id,
          lender_address: lender_address,
          loan_asset: loan_asset,
          asset_beacon: asset_beacon,
          loan_principle: loan_principle,
          rollover_frequency: rollover_frequency,
          last_checkpoint: // Update the checkpoint
            core.update_checkpoint(
              last_checkpoint,
              option.or_else(rollover_frequency,core.make_duration(0))),
          loan_term: loan_term,
          loan_interest: loan_interest,
          min_payment: min_payment,
          collateralization: collateralization,
          collateral_is_swappable: collateral_is_swappable,
          claim_expiration: claim_expiration,
          loan_expiration: loan_expiration,
          loan_outstanding: // Must apply the interest
            core.apply_interest(loan_outstanding,loan_interest),
          loan_id: loan_id
        } |> InlineDatum // Cast to Data for easy checking against outputs.
      )
    }

    _ -> error @"Input is not an Active UTxO"
  }
}

// Determine the required output value based on the input value and the deposit increase.
// This also checks that the BorrowerID is still present.
fn get_required_value(
  starting_value: Value,
  deposit_increase: Int,
  active_beacon_id: ActiveID,
  borrower_id: BorrowerID
) -> List<(PolicyId,Dict<AssetName,Int>)> {
  expect True = error_if_false(
    @"Deposit increase must be >= 0",
    deposit_increase >= 0
  )

  let sum = fn (_k, a, b) { Some (a+b) }

  list.map(
    value.to_dict(starting_value) |> dict.to_list(_),
    fn(n) {
      let (cn,tns) = n
      if cn == #"" {
        (cn, dict.insert_with(tns,#"",deposit_increase,sum,byte.compare))
      } else if cn == core.from_active_id(active_beacon_id) {
        expect True = error_if_false(
          @"Active UTxO missing BorrowerID",
          dict.get(tns,core.get_borrower_id_name(borrower_id)) |> option.is_some(_)
        )

        n
      } else {
        n
      }
    }
  )
}

// Check if there is an output to the borrower_loan_address with the proper configuration.
fn rollover_output_present(
  borrower_loan_address: Address,
  req_value: List<(PolicyId,Dict<AssetName,Int>)>,
  req_datum: Datum,
  outputs: List<Output>
) -> Bool {
  // A function to map over the list of outputs.
  let foo = fn (out: Output) -> Bool {
    // Extract out the output's information.
    let Output(address,val,d,_) = out
    and {
      address == borrower_loan_address,
      req_value == (value.to_dict(val) |> dict.to_list(_)),
      req_datum == d
    }
  }

  // Find the first output that satisfies the rollover requirements.
  list.find(outputs,foo) |> option.is_some(_)
}

pub fn valid_rollover(
  starting_value: Value,
  old_datum: LoanDatum,
  deposit_increase: Int,
  borrower_loan_address: Address,
  outputs: List<Output>,
  upper_bound: ExpirationTime
) -> Bool {
  let (active_beacon_id,borrower_id,req_datum) = get_required_datum(upper_bound,old_datum)
  let req_value = get_required_value(starting_value,deposit_increase,active_beacon_id,borrower_id)

  rollover_output_present(borrower_loan_address,req_value,req_datum,outputs)
}
