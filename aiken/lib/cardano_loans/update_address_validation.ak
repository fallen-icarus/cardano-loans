use aiken/bytearray as byte
use aiken/dict.{Dict}
use aiken/hash.{Hash,Blake2b_224}
use aiken/option
use aiken/list
use aiken/transaction.{
    Datum,
    InlineDatum,
    Output
  }
use aiken/transaction/credential.{
    Address,
    Script,
  }
use aiken/transaction/value.{
    Value,
    PolicyId,
    AssetName
  }

use cardano_loans/data/core.{
    ActiveID,
    BorrowerID,
    LoanID,
  }
use cardano_loans/data/datums.{
    LoanDatum,
    ActiveDatum
  }
use cardano_loans/utils.{
    error_if_false
  }

// Get the required output datum. Also checks that the new address is a valid lender address. 
// Returns the active_beacon_id, borrower_id, and loan_id so that the presence of the beacons
// can be checked.
fn get_required_datum( 
  old_datum: LoanDatum, 
  new_address: Address,
  proxy_hash: Hash<Blake2b_224, Script>
) -> (ActiveID,BorrowerID,LoanID,Datum) {
  when old_datum is {
    ActiveDatum(
      active_beacon_id,
      payment_observer_hash,
      borrower_id,
      _,
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
      // The new address must be able to accept the payment datums.
      expect True = error_if_false(
        @"Invalid lender address",
        utils.valid_lender_address(proxy_hash,new_address)
      )

      ( active_beacon_id,
        borrower_id,
        loan_id,
        ActiveDatum{
          active_beacon_id: active_beacon_id,
          payment_observer: payment_observer_hash,
          borrower_id: borrower_id,
          lender_address: new_address,
          loan_asset: loan_asset,
          asset_beacon: asset_beacon,
          loan_principle: loan_principle,
          rollover_frequency: rollover_frequency,
          last_checkpoint: last_checkpoint,
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

// Check if there is an output to the borrower_loan_address with the proper configuration
// as well as an output to the new address with the Key NFT.
fn update_outputs_present(
  borrower_loan_address: Address,
  new_address: Address,
  active_beacon_id: ActiveID,
  loan_id: LoanID,
  req_value: List<(PolicyId,Dict<AssetName,Int>)>,
  req_datum: Datum,
  outputs: List<Output>,
  acc: (Bool,Bool)
) -> Bool {
  let (collat_output_found,key_output_found) = acc

  if collat_output_found && key_output_found {
    True
  } else {
    when outputs is {
      [] -> error @"Proper outputs not found"
      [Output(address,val,d,..), ..rest] -> {
        update_outputs_present(
          borrower_loan_address,
          new_address,
          active_beacon_id,
          loan_id,
          req_value,
          req_datum,
          rest,
          ( collat_output_found || and {
              address == borrower_loan_address,
              req_value == (value.to_dict(val) |> dict.to_list(_)),
              req_datum == d
            },
            key_output_found || and {
              address == new_address,
              1 == value.quantity_of(
                val,
                core.from_active_id(active_beacon_id),
                core.get_loan_id_name(loan_id)
              )
            }
          )
        )     
      }
    }
  }
}

pub fn valid_address_update(
  starting_value: Value,
  deposit_increase: Int,
  old_datum: LoanDatum,
  new_address: Address,
  borrower_loan_address: Address,
  proxy_hash: Hash<Blake2b_224, Script>,
  outputs: List<Output>
) -> Bool {
  let (active_beacon_id,borrower_id,loan_id,req_datum) = 
    get_required_datum(old_datum,new_address,proxy_hash)
  let req_value = 
    get_required_value(starting_value,deposit_increase,active_beacon_id,borrower_id)
  
  update_outputs_present(
    borrower_loan_address,
    new_address,
    active_beacon_id,
    loan_id,
    req_value,
    req_datum,
    outputs,
    (False,False)
  )
}
