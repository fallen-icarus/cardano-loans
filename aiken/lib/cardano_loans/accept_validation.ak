use aiken/bytearray as byte
use aiken/dict.{Dict}
use aiken/option
use aiken/list
use aiken/time.{PosixTime}
use aiken/transaction.{Datum, InlineDatum, Output, Input, Redeemer, ValidityRange}
use aiken/transaction/credential.{Address}
use aiken/transaction/value.{Value, PolicyId, AssetName}

use aiken/cbor

use cardano_loans/data/core.{
    ScriptHash,
    ActiveId,
    BorrowerId,
    LoanId,
    AssetBeacon,
    Asset,
    Fraction,
    NegotiationId
  }
use cardano_loans/data/datums.{LoanDatum, OfferDatum, ActiveDatum}
use cardano_loans/utils.{error_if_false, trace_if_false, parse_datum}

type RequiredCollateralOutput {
  borrower_address: Address,
  collateralization: List<(Asset,Fraction)>,
  active_beacon_id: ActiveId,
  loan_principle: Int,
  loan_id: LoanId,
  loan_asset: Asset,
  asset_beacon: AssetBeacon,
  borrower_id: BorrowerId,
  req_datum: Datum
}

type RequiredLenderOutput {
  active_beacon_id: ActiveId,
  lender_address: Address,
  offer_deposit: Int,
  loan_id: LoanId
}

// Check if the newly accepted loan has enough collateral backing it. This also checks that
// the output has the proper beacons and fails with an error if it does not.
fn has_enough_collateral(
  active_beacon_id: ActiveId,
  borrower_id: BorrowerId,
  asset_beacon: AssetBeacon,
  loan_id: LoanId,
  principle_taken: Int,
  collateralization: List<(Asset,Fraction)>,
  output_value: Value
) -> Bool {

  // A function to map over the value of this UTxO. It will return the relative value of
  // the collateral in the output as well as whether the output has the proper beacons.
  let foo = fn(x: (PolicyId,Dict<AssetName,Int>), acc: (Int,Bool)) {
    let (cn,tns_dict) = x
    let (relative_collateral,valid_beacons) = acc

    // Check the beacons.
    if cn == core.from_active_id(active_beacon_id) {
      let tns_list = dict.to_list(tns_dict)

      // A function to check each beacon.
      let beacon_check = fn(y: (AssetName,Int), inner_acc: (Bool,Bool,Bool,Bool)) {
        let (tn,n) = y
        let (active_check,asset_check,borrower_id_check,loan_id_check) = inner_acc
        if tn == "Active" {
          // There must only be one Active beacon.
          (n == 1, asset_check, borrower_id_check, loan_id_check)
        } else if tn == core.get_asset_beacon_name(asset_beacon) {
          // There must only be one Asset beacon.
          (active_check, n == 1, borrower_id_check, loan_id_check)
        } else if tn == core.get_borrower_id_name(borrower_id) {
          // There must only be one BorrowerId beacon.
          (active_check, asset_check, n == 1, loan_id_check)
        } else if tn == core.get_loan_id_name(loan_id) {
          // There must only be one LoanId beacon.
          (active_check, asset_check, borrower_id_check, n == 1)
        } else {
          error @"UTxO has a beacon with the wrong name"
        }
      }

      when list.foldl(tns_list,(False,False,False,False),beacon_check) is {
        (True,True,True,True) -> (relative_collateral,True)
        _ -> (relative_collateral,False) // The UTxO does not have exactly one of each beacon.
      }
    } 

    // Check that the asset is in the collateral list add its relative value to the accumulator. 
    // It does not care if there are extraneous assets.
    else {
      let non_beacon_toks = dict.to_list(tns_dict)

      let collat_check = fn(token_info: (AssetName,Int), inner_acc: Int) {
        let (tn,n) = token_info
        let (price_num,price_den) = 
          utils.find_by_key(collateralization,(cn,tn)) |> option.or_else(_,(0,1))

        // If the price is zero, the accumulator will be unchanged. 
        if price_num == 0 { 
          inner_acc 
        } 
        
        // Calculate the value of the collateral in terms of the loan asset. Add it to the
        // accumulator. acc + collateral_quantity * reciprocal(price)
        else {
          inner_acc + n * price_den / price_num
        }
      }

      // The starting accumulator should be the current count of relative collateral.
      ( list.foldl( non_beacon_toks, relative_collateral, collat_check), valid_beacons )
    }
  }

  let (relative_collateral,valid_beacons) = list.foldl(
    value.to_dict(output_value) |> dict.to_list(_),
    (0,False),
    foo
  )

  if valid_beacons {
    // Collateral posted >= principle taken
    relative_collateral >= principle_taken
  } else {
    // If the recursive function returned false instead of crashing, it is because the UTxO
    // has the right beacons but the wrong quantity of each beacon.
    error @"UTxO does not have exactly one of each beacon"
  }
}

// Returns what information the collateral output and lender output should have based on the
// offer. It will also check if the offer has expired. It returns the AssetBeacon so that it can
// be marked for minting an active version.
fn process_input(
  lower_bound: PosixTime,
  upper_bound: Option<PosixTime>,
  borrower_id: BorrowerId,
  offer_datum: LoanDatum,
  borrower_address: Address,
  payment_observer_hash: ScriptHash,
  rollover_observer_hash: ScriptHash,
  loan_id: LoanId // This is taken from the output reference for the Offer UTxO.
) -> (RequiredCollateralOutput,RequiredLenderOutput,AssetName) {
  expect OfferDatum{
    active_beacon_id,
    lender_address,
    loan_asset,
    asset_beacon,
    loan_principle,
    rollover_frequency,
    loan_term,
    loan_interest,
    min_payment,
    collateralization,
    collateral_is_swappable,
    claim_period,
    offer_deposit,
    offer_expiration,
    ..
  } = offer_datum

  // The offer must not be expired.
  expect True = error_if_false(
    @"Offer has expired",
    when (offer_expiration,upper_bound) is {
      (None,_) -> True
      (Some(_),None) -> error "invalid-hereafter not set"
      (Some(expir),Some(after)) -> utils.expiration_has_not_passed(expir,after)
    }
  )

  // Produce the expected datum. Calculate the expiration times.
  let req_datum = ActiveDatum{
    active_beacon_id: active_beacon_id,
    payment_observer_hash: payment_observer_hash,
    rollover_observer_hash: rollover_observer_hash,
    borrower_id: core.get_borrower_id_name(borrower_id),
    lender_address: lender_address,
    loan_asset: loan_asset,
    asset_beacon: asset_beacon,
    loan_principle: loan_principle,
    rollover_frequency: rollover_frequency,
    last_checkpoint: lower_bound,
    loan_term: loan_term,
    loan_interest: loan_interest,
    min_payment: min_payment,
    collateralization: collateralization,
    collateral_is_swappable: collateral_is_swappable,
    claim_expiration: loan_term + claim_period + lower_bound,
    loan_expiration: loan_term + lower_bound,
    loan_outstanding: // Must apply the first interest calculation.
      utils.apply_interest((loan_principle,1),loan_interest),
    loan_id: core.get_loan_id_name(loan_id)
  } |> InlineDatum

  let req_collateral = RequiredCollateralOutput{
    borrower_address: borrower_address,
    collateralization: collateralization,
    active_beacon_id: core.to_active_id(active_beacon_id),
    loan_principle: loan_principle,
    loan_id: loan_id,
    loan_asset: loan_asset,
    asset_beacon: core.unsafe_to_asset_beacon(asset_beacon),
    borrower_id: borrower_id,
    req_datum: req_datum
  }

  let req_lender_output = RequiredLenderOutput{
    active_beacon_id: core.to_active_id(active_beacon_id),
    lender_address: lender_address,
    offer_deposit: offer_deposit,
    loan_id: loan_id
  }

  (req_collateral, req_lender_output, asset_beacon)
}

// Go through the inputs and build up a list of required beacons to mint/burn as well as required
// output information to expect.
fn get_input_info(
  negotiation_beacon_id: NegotiationId,
  payment_observer_hash: ScriptHash,
  rollover_observer_hash: ScriptHash,
  borrower_id: BorrowerId,
  borrower_loan_address: Address,
  validity_range: ValidityRange,
  inputs: List<Input>
) -> ( 
  List<(Option<RequiredCollateralOutput>,Option<RequiredLenderOutput>)>,
  Int, // Total number of offers accepted.
  Int, // Total number of asks closed.
  Dict<AssetName,Int>, // Required active beacon tokens to mint.
  Dict<AssetName,Int> // Required negotiation beacon tokens to burn.
) {
  let lower_bound = utils.get_lower_bound(validity_range)
  let upper_bound = utils.get_upper_bound(validity_range)

  // A function to map over the list of inputs.
  let foo = fn(
    input: Input,
    acc: (
      List<(Option<RequiredCollateralOutput>,Option<RequiredLenderOutput>)>,
      Int, 
      Int, 
      Dict<AssetName,Int>, 
      Dict<AssetName,Int>
    )
  ) {
    let Input(out_ref,Output(address,val,d,_)) = input
    let (req_outputs,offers_accepted,asks_closed,req_mint,req_burn) = acc

    // Only inputs with negotiation beacons need to be checked. Ask inputs have two beacons 
    // (Ask, and Asset beacon) while Offers have three beacons (Offer, LenderId, and Asset beacon). 

    // Get the negotiation beacons in this UTxO and pattern match on it.
    when value.tokens(val,core.from_negotiation_id(negotiation_beacon_id)) |> dict.to_list() is {
      // If beacons are not present, this input can be ignored.
      [] -> acc

      // If there are two beacons, this should be an Ask UTxO.
      [(tn1,n1),(tn2,n2)] -> {
        // All loan inputs must come from the borrower's loan address.
        expect True = error_if_false(
          @"Found loan input from different borrower",
          address == borrower_loan_address
        )

        // Add the beacons to be burned.
        let new_req_burn = req_burn |>
          dict.insert_with(_,tn1,-n1,utils.sum_token_values,byte.compare) |>
          dict.insert_with(_,tn2,-n2,utils.sum_token_values,byte.compare)

        // Increment the number of asks closed.
        ( req_outputs, offers_accepted, asks_closed + 1, req_mint, new_req_burn )
      }

      // If there are three beacons, this is an Offer UTxO.
      [(tn1,n1),(tn2,n2),(tn3,n3)] -> {
        // All loan inputs must come from the borrower's loan address.
        expect True = error_if_false(
          @"Found loan input from different borrower",
          address == borrower_loan_address
        )

        let loan_id = core.make_loan_id(out_ref)
        let (req_collateral_out,req_lender_out,asset_beacon) = process_input(
          lower_bound,
          upper_bound,
          borrower_id,
          parse_datum(d),
          address,
          payment_observer_hash,
          rollover_observer_hash,
          loan_id
        )

        let new_req_burn = req_burn |>
          dict.insert_with(_,tn1,-n1,utils.sum_token_values,byte.compare) |>
          dict.insert_with(_,tn2,-n2,utils.sum_token_values,byte.compare) |>
          dict.insert_with(_,tn3,-n3,utils.sum_token_values,byte.compare)

        let new_req_mint = req_mint |>
          // Mint an Active beacon.
          dict.insert_with(_,"Active",1,utils.sum_token_values,byte.compare) |>
          // Mint a BorrowerId.
          dict.insert_with(_,core.get_borrower_id_name(borrower_id),1,utils.sum_token_values,byte.compare) |>
          // Mint two LoanIds.
          dict.insert_with(_,core.get_loan_id_name(loan_id),2,utils.sum_token_values,byte.compare) |>
          // Mint an Asset beacon.
          dict.insert_with(_,asset_beacon,1,utils.sum_token_values,byte.compare)

        // Add the outputs to the end.
        let new_outs = list.concat(req_outputs,[(Some(req_collateral_out),Some(req_lender_out))])

        ( new_outs,
          offers_accepted + 1, 
          asks_closed, 
          new_req_mint, 
          new_req_burn
        )
      }

      // This path can only be taken if the above matches failed which means the UTxO
      // has the wrong number of beacons. This should never happen.
      _ -> error @"The impossible happened"
    }
  }
  
  list.foldl(inputs,([],0,0,dict.new(),dict.new()),foo)
}

// Get the negotiation beacons burned and the active beacons minted.
fn get_beacon_mint_and_burn(
  negotiation_beacon_id: NegotiationId,
  active_beacon_id: ActiveId,
  acc: (Option<Dict<AssetName,Int>>,Option<Dict<AssetName,Int>>),
  xs: List<(PolicyId,Dict<AssetName,Int>)>
) -> (Option<Dict<AssetName,Int>>,Option<Dict<AssetName,Int>>) {
  // A custom function to collect both the negotiation beacons and the active beacons in a single
  // pass. It will recurse until it finds both.
  when acc is {
    (Some(_),Some(_)) -> acc
    _ -> {
      when xs is {
        [] -> acc
        [(cn,tns), ..rest] -> {
          let (negs,acts) = acc
          if cn == core.from_negotiation_id(negotiation_beacon_id) {
            get_beacon_mint_and_burn(negotiation_beacon_id,active_beacon_id,(Some(tns),acts),rest)
          } else if cn == core.from_active_id(active_beacon_id) {
            get_beacon_mint_and_burn(negotiation_beacon_id,active_beacon_id,(negs,Some(tns)),rest)
          } else {
            get_beacon_mint_and_burn(negotiation_beacon_id,active_beacon_id,acc,rest)
          }
        }
      }
    }
  }
}

// Check if the lender's output value is correct. It should have exactly 1 LoanId and the
// offer deposit amount of ADA. No other assets should be present.
fn lender_output_has_proper_value(
  val: Value,
  offer_deposit: Int,
  loan_id: LoanId,
  active_beacon_id: ActiveId
) -> Bool {
  when value.flatten(val) is {
    [(cn1,tn1,n1),(cn2,tn2,n2)] -> {
      // The empty bytestring (ADA's on-chain name) should always be first.
      and {
        error_if_false(
          @"Lender's deposit not returned exactly",
          cn1 == #"" && tn1 == #"" && n1 == offer_deposit
        ),
        error_if_false(
          @"Lender payment must have exactly one LoanId",
          cn2 == core.from_active_id(active_beacon_id) && 
          tn2 == core.get_loan_id_name(loan_id) && 
          n2 == 1
        )
      }
    }
    
    _ -> error @"Lender output can/must only contain ADA and the LoanId"
  }
}

// Go through the outputs and check that all required outputs are satisfied. The loan outputs are
// assumed to be in the order of the inputs, and be paired up (collateral output and lender output
// are next to each other). This function can never actually return False; it will crash with
// an error at the first failure.
fn check_outputs(
  req_outputs: List<(Option<RequiredCollateralOutput>,Option<RequiredLenderOutput>)>,
  outputs: List<Output>,
  result: Bool
) -> Bool {
  when (req_outputs,outputs) is {
    ( [ (Some(req_collateral_output),None), ..rest_req_outs ]
    , [ Output(addr,val,d,_), ..rest_outs ]
    ) -> {
      let RequiredCollateralOutput{
        borrower_address,
        collateralization, 
        active_beacon_id, 
        loan_principle,
        loan_id, 
        loan_asset,
        asset_beacon,
        borrower_id, 
        req_datum
      } = req_collateral_output

      if addr == borrower_address && d == req_datum {
        // has_enough_collateral checks that the output contains the proper beacons.
        check_outputs(
          rest_req_outs, 
          rest_outs, 
          error_if_false(
            @"Collateral output does not have enough collateral",
            has_enough_collateral(
              active_beacon_id,
              borrower_id,
              asset_beacon,
              loan_id,
              loan_principle,
              collateralization,
              val
            )
          )
        )

      // The next collateral output must be processed before moving on other required outputs.
      // So move on to the next output until this required output is found.
      } else {
        check_outputs(req_outputs,rest_outs,result)
      }
    }

    ( [ (None,Some(req_lender_output)), ..rest_req_outs ]
    , [ Output(addr,val,d,_), ..rest_outs ]
    ) -> {
      let RequiredLenderOutput{
        active_beacon_id,
        lender_address,
        offer_deposit,
        loan_id
      } = req_lender_output

      // Check if the output is the next required lender output.
      if addr == lender_address && d == InlineDatum((active_beacon_id,loan_id)) {
        check_outputs(
          rest_req_outs, 
          rest_outs, 
          // This will either crash or return true.
          lender_output_has_proper_value(
            val,
            offer_deposit,
            loan_id,
            active_beacon_id
          )
        )

      // The next lender output must be processed before moving on other required outputs.
      // So move on to the next output until this required output is found.
      } else {
        check_outputs(req_outputs,rest_outs,result)
      }
    }

    ( [ (Some(req_collateral_output) as col,Some(req_lender_output) as pay), ..rest_req_outs ]
    , [ Output(addr,val,d,_), ..rest_outs ]
    ) -> {
      let RequiredCollateralOutput{
        borrower_address,
        collateralization, 
        active_beacon_id, 
        loan_principle,
        loan_id, 
        loan_asset,
        asset_beacon,
        borrower_id, 
        req_datum
      } = req_collateral_output

      let RequiredLenderOutput{
        lender_address,
        offer_deposit,
        ..
      } = req_lender_output

      // Check if it is the next required collateral output.
      if addr == borrower_address && d == req_datum {
        // has_enough_collateral checks that the output contains the proper beacons.
        check_outputs(
          [(None,pay), ..rest_req_outs], 
          rest_outs, 
          error_if_false(
            @"Collateral output does not have enough collateral",
            has_enough_collateral(
              active_beacon_id,
              borrower_id,
              asset_beacon,
              loan_id,
              loan_principle,
              collateralization,
              val
            )
          )
        )

      // Check if it is the next required lender output.
      } else if addr == lender_address && d == InlineDatum((active_beacon_id,loan_id)) {
        check_outputs(
          [(col,None), ..rest_req_outs], 
          rest_outs, 
          // This will either crash or return true.
          lender_output_has_proper_value(
            val,
            offer_deposit,
            loan_id,
            active_beacon_id
          )
        )

      // Move on to the next output in the transaction.
      } else {
        check_outputs(req_outputs,rest_outs,result)
      }
    }

    ([], _) -> result

    _ -> error "Not all required outputs found"
  }
}

// Check that the new loans have the proper outputs and the proper beacons were minted/burned.
pub fn valid_loan_acceptances(
  negotiation_beacon_id: NegotiationId,
  active_beacon_id: ActiveId,
  payment_observer_hash: ScriptHash,
  rollover_observer_hash: ScriptHash,
  borrower_id: BorrowerId,
  borrower_loan_address: Address,
  validity_range: ValidityRange,
  inputs: List<Input>,
  outputs: List<Output>,
  minted: Value
) -> Bool {
  let (req_outputs,offers_accepted,asks_closed,req_mint,req_burn) =
    get_input_info(
      negotiation_beacon_id,
      payment_observer_hash,
      rollover_observer_hash,
      borrower_id,
      borrower_loan_address,
      validity_range,
      inputs
    )

  let (negs,acts) = get_beacon_mint_and_burn(
    negotiation_beacon_id,
    active_beacon_id,
    (None,None),
    value.to_dict(minted) |> dict.to_list(_)
  )

  and {
    check_outputs(
      req_outputs,
      outputs,
      True
    ),
    trace_if_false(
      @"The wrong negotiation beacons were burned",
      negs == Some(req_burn) 
    ),
    trace_if_false(
      @"The wrong active beacons were minted",
      acts == Some(req_mint)
    ),
    trace_if_false(
      @"# Ask UTxOs closed /= # Offer UTxOs accepted",
      asks_closed == offers_accepted
    )
  }
}
