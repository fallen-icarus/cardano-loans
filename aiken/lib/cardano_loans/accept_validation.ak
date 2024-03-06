use aiken/bytearray as byte
use aiken/list
use aiken/option
use aiken/dict.{Dict}
use aiken/transaction.{
    InlineDatum,
    Output,
    Input,
    ValidityRange
  }
use aiken/transaction/credential.{
    Address,
  }
use aiken/transaction/value.{
    AssetName,
    PolicyId,
    Value
  }

use cardano_loans/data/core.{
    NegotiationID,
    ActiveID,
    StartTime,
    ExpirationTime,
    LoanID,
    Asset,
    Rational,
    BorrowerID,
    AssetBeacon
  }
use cardano_loans/data/datums.{
    LoanDatum,
    OfferDatum,
    ActiveDatum
  }
use cardano_loans/utils.{
    trace_if_false,
    error_if_false,
    parse_datum,
  }

// Returns what information the collateral output and lender output should have based on the
// offer. It will also check if the offer has expired. It returns the AssetBeacon so that it can
// be marked for minting an active version.
fn extract_output_info(
  lower_bound: StartTime,
  upper_bound: ExpirationTime,
  borrower_id: BorrowerID,
  offer_datum: LoanDatum,
  loan_id: LoanID // This is taken from the output reference for the Offer UTxO.
) -> (LoanDatum,(LoanID,(Int,Address)),AssetBeacon) {
  // Pattern match out the required terms from the OfferDatum.
  expect OfferDatum(
    _,
    active_beacon_id_,
    _,
    lender_address_,
    loan_asset_,
    asset_beacon_,
    loan_principle_,
    rollover_frequency_,
    loan_term_,
    loan_interest_,
    min_payment_,
    collateralization_,
    collateral_is_swappable_,
    claim_period_,
    offer_deposit_,
    offer_expiration_
  ) = offer_datum

  // The offer must not be expired.
  expect True = error_if_false(
    @"Offer has expired",
    when offer_expiration_ is {
      None -> True
      Some(x) -> core.expiration_time_has_not_passed(upper_bound,x)
    }
  )

  // Produce the expected datum. Calculate the expiration times.
  let req_datum = ActiveDatum{
    active_beacon_id: active_beacon_id_,
    borrower_id: borrower_id,
    lender_address: lender_address_,
    loan_asset: loan_asset_,
    asset_beacon: asset_beacon_,
    loan_principle: loan_principle_,
    rollover_frequency: rollover_frequency_,
    last_checkpoint: lower_bound,
    loan_term: loan_term_,
    loan_interest: loan_interest_,
    min_payment: min_payment_,
    collateralization: collateralization_,
    collateral_is_swappable: collateral_is_swappable_,
    claim_expiration: 
      core.add_duration(loan_term_,claim_period_) |> core.calc_expiration_time(lower_bound,_),
    loan_expiration: core.calc_expiration_time(lower_bound,loan_term_),
    loan_outstanding: // Must apply the first interest calculation.
      core.apply_interest((loan_principle_,1),loan_interest_),
    loan_id: loan_id
  }

  ( req_datum, (loan_id,(offer_deposit_,lender_address_)), asset_beacon_ )
}

// Go through the inputs and build up a list of required beacons to burn as well as required
// output information to expect.
fn get_input_info(
  negotiation_beacon_id: NegotiationID,
  borrower_id: BorrowerID,
  borrower_loan_address: Address,
  validity_range: ValidityRange,
  inputs: List<Input>
) -> ( 
  List<LoanDatum>, // A list of required collateral output information.
  List<(LoanID,(Int,Address))>, // A list of required payment output information.
  Int, // Total number of offers accepted.
  Int, // Total number of asks closed.
  Dict<AssetName,Int>, // Required active beacon tokens to mint.
  Dict<AssetName,Int> // Required negotiation beacon tokens to burn.
) {
  let lower_bound = core.get_start_time(validity_range)
  let upper_bound = core.get_end_time(validity_range)

  // Helper functions declared only once.
  let sum = fn (_k, a, b) { Some (a+b) }

  // A function to map over the list of inputs.
  let foo = fn(
    input: Input,
    acc: (
      List<LoanDatum>, 
      List<(LoanID,(Int,Address))>, 
      Int, 
      Int, 
      Dict<AssetName,Int>, 
      Dict<AssetName,Int>
    )
  ) {
    let Input(out_ref,Output(address,val,d,_)) = input
    let (req_collateral,req_payments,offers_accepted,asks_closed,req_mint,req_burn) = acc

    // Only inputs with negotiation beacons need to be checked. Ask inputs have two beacons 
    // (Ask, and Asset beacon) while Offers have three beacons (Offer, LenderID, and Asset beacon). 

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
          dict.insert_with(_,tn1,-n1,sum,byte.compare) |>
          dict.insert_with(_,tn2,-n2,sum,byte.compare)

        // Increment the number of asks closed.
        ( req_collateral, req_payments, offers_accepted, asks_closed + 1, req_mint, new_req_burn )
      }

      // If there are three beacons, this is an Offer UTxO.
      [(tn1,n1),(tn2,n2),(tn3,n3)] -> {
        // All loan inputs must come from the borrower's loan address.
        expect True = error_if_false(
          @"Found loan input from different borrower",
          address == borrower_loan_address
        )

        let loan_id = core.make_loan_id(out_ref)
        let (req_datum,req_pay,asset_beacon) = extract_output_info(
          lower_bound,
          upper_bound,
          borrower_id,
          parse_datum(d),
          loan_id
        )

        let new_req_collateral = list.push(req_collateral,req_datum)
        let new_req_payments = list.push(req_payments,req_pay)
        let new_req_burn = req_burn |>
          dict.insert_with(_,tn1,-n1,sum,byte.compare) |>
          dict.insert_with(_,tn2,-n2,sum,byte.compare) |>
          dict.insert_with(_,tn3,-n3,sum,byte.compare)
        let new_req_mint = req_mint |>
          // Mint an Active beacon.
          dict.insert_with(_,"Active",1,sum,byte.compare) |>
          // Mint a BorrowerID.
          dict.insert_with(_,core.get_borrower_id_name(borrower_id),1,sum,byte.compare) |>
          // Mint two LoanIDs.
          dict.insert_with(_,core.get_loan_id_name(loan_id),2,sum,byte.compare) |>
          // Mint an Asset beacon.
          dict.insert_with(_,core.get_asset_beacon_name(asset_beacon),1,sum,byte.compare)

        ( new_req_collateral, 
          new_req_payments, 
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

  list.foldl(inputs,([],[],0,0,dict.new(),dict.new()),foo)
}

// Check if the newly accepted loan has enough collateral backing it. This also checks that
// the output has the proper beacons and fails with an error if it does not.
fn has_enough_collateral(
  active_beacon_id: ActiveID,
  borrower_id: BorrowerID,
  asset_beacon: AssetBeacon,
  loan_id: LoanID,
  principle_taken: Int,
  collateralization: List<(Asset,Rational)>,
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
          // There must only be one BorrowerID beacon.
          (active_check, asset_check, n == 1, loan_id_check)
        } else if tn == core.get_loan_id_name(loan_id) {
          // There must only be one LoanID beacon.
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

// Go through the list of outputs and check that:
// 1) All outputs with beacons are locked at the borrower's address.
// 2) All outputs with beacons have 4 different beacons.
// 3) The proper number of collateral outputs and payment outputs are satisfied. The uniqueness
//    of the LoanIDs prevents double satisfaction.
fn offer_inputs_all_satisfied(
  active_beacon_id: ActiveID,
  borrower_id: BorrowerID,
  borrower_loan_address: Address,
  req_collateral: List<LoanDatum>,
  req_payments: List<(LoanID,(Int,Address))>,
  offers_accepted: Int,
  outputs: List<Output>
) -> Bool {
  // A function to map over the list of outputs. Returns the total number of payment outputs
  // and collateral outputs satisfied.
  let foo = fn (out: Output, acc: (Int,Int)) {
    let (payments_satisfied,collateral_satisfied) = acc

    // Extract out the output's information.
    let Output(address,val,d,_) = out

    // Get the beacons in this UTxO and pattern match on it.
    when value.tokens(val,core.from_active_id(active_beacon_id)) |> dict.to_list() is {
      // If beacons are not present, this output can be ignored.
      [] -> acc

      // This is a collateral UTxO and has four types of beacon. The exact beacons will be 
      // checked later. 
      [(_,_),(_,_),(_,_),(_,_)] -> {
        // All outputs with beacons in this transaction must have ActiveDatums. 
        let datum = when parse_datum(d) is {
          ActiveDatum(..) as datum -> datum
          _ -> error @"Loan address output does not have an ActiveDatum."
        }

        if list.has(req_collateral,datum) {
          // If the datum is in the req_collateral list, this UTxO must be checked for the
          // proper value and location.
          expect ActiveDatum(
            _,
            _,
            _,
            _,
            asset_beacon,
            principle_taken,
            _,
            _,
            _,
            _,
            _,
            collateralization,
            _,
            _,
            _,
            _,
            loan_id
          ) = datum

          expect True = error_if_false(
            @"Collateral output locked at wrong address",
            address == borrower_loan_address
          )

          expect True = error_if_false(
            @"Collateral output does not have enough collateral",
            has_enough_collateral(
              active_beacon_id,
              borrower_id,
              asset_beacon,
              loan_id,
              principle_taken,
              collateralization,
              val
            )
          )

          // If the above checks passed, a collateral output is satisfied.
          (payments_satisfied, collateral_satisfied + 1)
        } else {
          // If the datum is not in the req_collateral list, it won't satisfy the required
          // count. This is okay as long as this output is not from a newly accepted loan.
          acc
        }
      }

      // This is a lender payment output and has one beacon which should be a LoanID.
      // The only type of output that can have a single UTxO is a lender payment upon
      // acceptance; the beacon must be a LoanID.
      [(tn,n)] -> {
        when utils.find_by_key(req_payments,core.asset_name_to_loan_id(tn)) is {
          None -> error @"Beacon found in invalid UTxO"
          Some((deposit,req_lender_address)) -> {
            expect True = error_if_false(
              @"Too many LoanIDs found in UTxO",
              n == 1
            )

            expect True = error_if_false(
              @"Lender deposit not returned",
              value.lovelace_of(val) >= deposit
            )

            expect True = error_if_false(
              @"LoanID sent to wrong lender address",
              address == req_lender_address
            )

            expect True = error_if_false(
              @"Lender output stored with wrong datum",
              InlineDatum((active_beacon_id,"Accepted")) == d
            )

            // If the above checks passed, a payment output is satisfied.
            (payments_satisfied + 1, collateral_satisfied)
          }
        }
      }

      // This path can only be taken if the above two matches failed which means the UTxO
      // has the wrong number of beacons.
      _ -> error @"UTxO output has the wrong number of beacons"
    }
  }

  // Map over the list and return the total number of payment outputs and collateral outputs
  // satisfied.
  let (payments_satisfied,collateral_satisfied) = list.foldl(outputs,(0,0),foo)

  and {
    trace_if_false(
      @"Not all lender outputs found", 
      payments_satisfied == offers_accepted
    ),
    trace_if_false(
      @"Not all collateral outputs found", 
      collateral_satisfied == offers_accepted
    )
  }
}

fn get_beacon_mint_and_burn(
  negotiation_beacon_id: NegotiationID,
  active_beacon_id: ActiveID,
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

// Check that the new loans have the proper outputs and the proper beacons were minted/burned.
pub fn valid_loan_acceptances(
  negotiation_beacon_id: NegotiationID,
  active_beacon_id: ActiveID,
  borrower_id: BorrowerID,
  borrower_loan_address: Address,
  validity_range: ValidityRange,
  inputs: List<Input>,
  outputs: List<Output>,
  minted: Value
) -> Bool {
  let (req_collateral,req_payments,offers_accepted,asks_closed,req_mint,req_burn) =
    get_input_info(
      negotiation_beacon_id,
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
    offer_inputs_all_satisfied(
      active_beacon_id,
      borrower_id,
      borrower_loan_address,
      req_collateral,
      req_payments,
      offers_accepted,
      outputs
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
