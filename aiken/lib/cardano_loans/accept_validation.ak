use aiken/bytearray as byte
use aiken/dict.{Dict}
use aiken/option
use aiken/list
use aiken/time.{PosixTime}
use aiken/transaction.{Datum, InlineDatum, Output, Input, ValidityRange}
use aiken/transaction/credential.{Address,ScriptCredential,Inline}
use aiken/transaction/value.{Value, PolicyId, AssetName}

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
use cardano_loans/utils.{error_if_false, parse_datum}

type RequiredCollateralOutput {
  collateralization: List<(Asset,Fraction)>,
  active_beacon_id: ActiveId,
  loan_principal: Int,
  loan_id: LoanId,
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
    (negots,acts) -> {
      when xs is {
        [] -> acc
        [(cn,tns), ..rest] -> {
          if cn == core.from_negotiation_id(negotiation_beacon_id) {
            get_beacon_mint_and_burn(negotiation_beacon_id,active_beacon_id,(Some(tns),acts),rest)
          } else if cn == core.from_active_id(active_beacon_id) {
            get_beacon_mint_and_burn(negotiation_beacon_id,active_beacon_id,(negots,Some(tns)),rest)
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
    // It should only ever have exactly two assets.
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

// Check if the newly accepted loan has enough collateral backing it. This also checks that
// the output has the proper beacons and fails with an error if it does not.
fn has_enough_collateral(
  active_beacon_id: ActiveId,
  borrower_id: BorrowerId,
  asset_beacon: AssetBeacon,
  loan_id: LoanId,
  principal_taken: Int,
  collateralization: List<(Asset,Fraction)>,
  output_value: Value
) -> Bool {
  // A function to map over the value of this UTxO. It will return the relative value of
  // the collateral in the output. It will also check the UTxO has the proper beacons.
  let foo = fn(x: (PolicyId,Dict<AssetName,Int>), acc: (Int,Bool)) {
    let (cn,tns_dict) = x
    let (relative_collateral,has_proper_beacons) = acc

    // Check the beacons.
    if cn == core.from_active_id(active_beacon_id) {
      let tns_list = dict.to_list(tns_dict)

      // A function to check each beacon.
      let check_beacons = fn(y: (AssetName,Int), inner_acc: (Bool,Bool,Bool,Bool)) {
        let (tn,n) = y
        let (active_check,asset_check,borrower_id_check,loan_id_check) = inner_acc
        if tn == "Active" {
          // There must only be one Active beacon.
          ( error_if_false(@"UTxO has too many Active beacons", n == 1)
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
          ( active_check
          , asset_check
          , error_if_false(@"UTxO has too many BorrowerIds", n == 1)
          , loan_id_check)
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

      when list.foldl(tns_list,(False,False,False,False),check_beacons) is {
        (True,True,True,True) -> (relative_collateral,True)

        // This path can be taken if the UTxO has only some of the required beacons.
        _ -> error @"UTxO does not have the right beacons"
      }
    } 

    // Check that the asset is in the collateral list and add its relative value to the accumulator. 
    // It does not care if there are extraneous assets since the borrower is incentivized not to
    // risk unnecessary assets.
    else {
      let non_beacon_toks = dict.to_list(tns_dict)

      let check_collat = fn(token_info: (AssetName,Int), inner_acc: Int) {
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
      (list.foldl(non_beacon_toks, relative_collateral, check_collat), has_proper_beacons)
    }
  }

  let (relative_collateral,has_proper_beacons) = 
    list.foldl(value.to_dict(output_value) |> dict.to_list(_), (0,False), foo) 

  expect True = error_if_false(
    @"Collateral UTxO missing beacons",
    has_proper_beacons
  )

  relative_collateral >= principal_taken
}

// Returns what information the collateral output and lender output should have based on the
// offer. It will also check if the offer has expired. It returns the AssetBeacon so that it can
// be marked for minting an active version.
fn process_input(
  lower_bound: PosixTime,
  upper_bound: Option<PosixTime>,
  borrower_id: BorrowerId,
  offer_datum: LoanDatum,
  payment_observer_hash: ScriptHash,
  interest_observer_hash: ScriptHash,
  address_update_observer_hash: ScriptHash,
  loan_id: LoanId // This is taken from the output reference for the Offer UTxO.
) -> (RequiredCollateralOutput,RequiredLenderOutput,AssetName) {
  expect OfferDatum{
    active_beacon_id,
    lender_address,
    loan_asset,
    asset_beacon,
    loan_principal,
    compound_frequency,
    loan_term,
    loan_interest,
    min_payment,
    penalty,
    collateralization,
    collateral_is_swappable,
    claim_period,
    offer_deposit,
    offer_expiration,
    ..
  } = offer_datum

  // The offer must not be expired. If the Offer does not have a set expiration, this check
  // can be skipped. There is no need for invalid-hereafter if the offer does not expire.
  expect True = error_if_false(
    @"Offer has expired",
    when (offer_expiration,upper_bound) is {
      (None,_) -> True
      (Some(_),None) -> error "invalid-hereafter required but not set"
      (Some(expir),Some(after)) -> utils.expiration_has_not_passed(expir,after)
    }
  )

  // Produce the expected datum. Calculate the expiration times using invalid-before.
  let req_datum = ActiveDatum{
    active_beacon_id: active_beacon_id,
    payment_observer_hash: payment_observer_hash,
    interest_observer_hash: interest_observer_hash,
    address_update_observer_hash: address_update_observer_hash,
    borrower_id: core.get_borrower_id_name(borrower_id),
    lender_address: lender_address,
    loan_asset: loan_asset,
    asset_beacon: asset_beacon,
    loan_principal: loan_principal,
    compound_frequency: compound_frequency,
    last_compounding: lower_bound, // Initialized to the invalid-before time.
    loan_term: loan_term,
    loan_interest: loan_interest,
    min_payment: min_payment,
    penalty: penalty,
    collateralization: collateralization,
    collateral_is_swappable: collateral_is_swappable,
    claim_expiration: loan_term + claim_period + lower_bound,
    loan_expiration: loan_term + lower_bound,
    loan_outstanding: // Must apply the first interest calculation.
      utils.apply_interest((loan_principal,1),loan_interest),
    total_epoch_payments: 0,
    loan_id: core.get_loan_id_name(loan_id)
  } |> InlineDatum

  let req_collateral = RequiredCollateralOutput{
    collateralization: collateralization,
    active_beacon_id: core.to_active_id(active_beacon_id),
    loan_principal: loan_principal,
    loan_id: loan_id,
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
// output information to expect. The order of the inputs determines the required order for the
// corresponding loan outputs. Only inputs with negotiation beacons need to be checked. Ask inputs
// have two beacons (Ask, and Asset beacon) while Offers have three beacons (Offer, LenderId, and
// Asset beacon). All inputs must come from the same loan address.
fn get_input_info(
  negotiation_beacon_id: NegotiationId,
  payment_observer_hash: ScriptHash,
  interest_observer_hash: ScriptHash,
  address_update_observer_hash: ScriptHash,
  loan_script_hash: ScriptHash,
  validity_range: ValidityRange,
  inputs: List<Input>
) -> ( 
  Option<Address>, // The required borrower's address.
  List<RequiredCollateralOutput>,
  List<RequiredLenderOutput>,
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
      Option<Address>,
      List<RequiredCollateralOutput>,
      List<RequiredLenderOutput>,
      Int, 
      Int, 
      Dict<AssetName,Int>, 
      Dict<AssetName,Int>
    )
  ) {
    let Input(out_ref,Output(Address(pay,stake) as addr,val,d,_)) = input
    let (req_addr,req_col_outputs,req_pay_outputs,offers_accepted,asks_closed,req_mint,req_burn) = acc

    // Get the negotiation beacons in this UTxO and pattern match on it.
    when value.tokens(val,core.from_negotiation_id(negotiation_beacon_id)) |> dict.to_list() is {
      // If beacons are not present, this input can be ignored. This enables closing invalid
      // Ask and Offer UTxOs with this redeemer. However, invalid Ask and Offer UTxOs still need
      // to be checked for whether they come from the current borrower's address.
      [] -> {
        if pay == ScriptCredential(loan_script_hash) {
          // All loan inputs must come from the borrower's loan address.
          expect True = error_if_false(
            @"Found loan input from different borrower",
            when req_addr is {
              None -> True
              Some(x) -> x == addr
            }
          )

          // Return this address since all loan inputs must come from the same address.
          (Some(addr),req_col_outputs,req_pay_outputs,offers_accepted,asks_closed,req_mint,req_burn)
        } else {
          // If it doesn't come from a loan address, it can be ignored.
          acc
        }
      }

      // If there are two beacons, this should be an Ask UTxO.
      [(tn1,n1),(tn2,n2)] -> {
        // Beacons should only ever be found at loan addresses. If this is ever False, there is a
        // bug in the negotiation beacon script.
        expect True = error_if_false(
          @"E3",
          pay == ScriptCredential(loan_script_hash)
        )

        // All loan inputs must come from the borrower's loan address.
        expect True = error_if_false(
          @"Found loan input from different borrower",
          when req_addr is {
            None -> True
            Some(x) -> x == addr
          }
        )

        // Add the beacons to be burned.
        let new_req_burn = req_burn |>
          dict.insert_with(_,tn1,-n1,utils.sum_token_values,byte.compare) |>
          dict.insert_with(_,tn2,-n2,utils.sum_token_values,byte.compare)

        // Increment the number of asks closed.
        (Some(addr),req_col_outputs, req_pay_outputs,offers_accepted, asks_closed + 1, req_mint, new_req_burn)
      }

      // If there are three negotiation beacons, this is an Offer UTxO.
      [(tn1,n1),(tn2,n2),(tn3,n3)] -> {
        // Beacons should only ever be found at loan addresses. If this is ever False, there is a
        // bug in the negotiation beacon script.
        expect True = error_if_false(
          @"E4",
          pay == ScriptCredential(loan_script_hash)
        )

        // Generate the new LoanId from the offer.
        let loan_id = core.make_loan_id(out_ref)
        let borrower_id = when req_addr is {
          None -> {
            expect Some(Inline(cred)) = stake
            core.credential_to_borrower_id(cred)
          }

          Some(Address(_,Some(Inline(cred))) as x) -> {
            // All loan inputs must come from the same borrower.
            expect True = error_if_false(
              @"Found loan input from different borrower",
              x == addr
            )

            core.credential_to_borrower_id(cred)
          }

          // All addresses with beacons should have a valid stake credential. If this ever happens,
          // there is a bug in the negotiation beacon script.
          _ -> error "E5" 
        }

        // Determine the required output information and what AssetBeacon needs to be minted based
        // on this input. The negotiation asset beacon must be replaced with the active version.
        let (new_req_collateral_out,new_req_lender_out,asset_beacon) = process_input(
          lower_bound,
          upper_bound,
          borrower_id,
          // If this UTxO has the three beacons, the datum is guaranteed to be an OfferDatum.
          parse_datum(d), 
          payment_observer_hash,
          interest_observer_hash,
          address_update_observer_hash,
          loan_id
        )

        // Burn all negotiation beacons.
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
          // Mint the active version of the AssetBeacon.
          dict.insert_with(_,asset_beacon,1,utils.sum_token_values,byte.compare)

        ( Some(addr),
          // Add the new required collateral output to the END of the required collateral list.
          list.concat(req_col_outputs,[new_req_collateral_out]),
          // Add the new required lender output to the END of the required lender output list.
          list.concat(req_pay_outputs,[new_req_lender_out]),
          offers_accepted + 1, 
          asks_closed, 
          new_req_mint, 
          new_req_burn
        )
      }

      // This path can only be taken if the above matches failed which means the UTxO
      // has the wrong number of negotiation beacons. This should never happen. If it does,
      // there is a bug in the negotiation beacon script.
      _ -> error @"E1"
    }
  }
  
  list.foldl(inputs,(None,[],[],0,0,dict.new(),dict.new()),foo)
}

// Go through the outputs and check that all required outputs are satisfied. The loan outputs are
// assumed to be in the order of the inputs. The next required collateral output and the next
// required lender output do not need to be next to each other. The only requirement is that all
// collateral outputs are in the order of the inputs and all lender payment outputs are in the order
// of the inputs. Any combination that satisfies this requirement is valid. For example, all
// collateral outputs can appear first, followed by all of the lender payment outputs. This function
// can never actually return False; it will crash with an error at the first failure.
fn check_outputs(
  borrower_address: Address,
  req_col_outputs: List<RequiredCollateralOutput>,
  req_pay_outputs: List<RequiredLenderOutput>,
  outputs: List<Output>,
) -> Bool {
  when (req_col_outputs,req_pay_outputs,outputs) is {
    // Only the collateral outputs still need to be found.
    ( [ req_collateral_output, ..rest_req_col_outs ]
    , []
    , [ Output(addr,val,d,_), ..rest_outs ]
    ) -> {
      let RequiredCollateralOutput{
        collateralization, 
        active_beacon_id, 
        loan_principal,
        loan_id, 
        asset_beacon,
        borrower_id, 
        req_datum
      } = req_collateral_output

      if addr == borrower_address && d == req_datum {
        // has_enough_collateral checks that the output contains the proper beacons.
        expect True = error_if_false(
          @"Collateral output does not have enough collateral",
          has_enough_collateral(
            active_beacon_id,
            borrower_id,
            asset_beacon,
            loan_id,
            loan_principal,
            collateralization,
            val
          )
        )

        check_outputs(
          borrower_address,
          rest_req_col_outs, // Move on to the next collateral output.
          [],
          rest_outs // Move on to the next output.
        )

      // This collateral output still needs to be found.
      } else {
        check_outputs(
          borrower_address,
          req_col_outputs, // Keep the current required collateral output.
          [],
          rest_outs // Move on to the next output.
        )
      }
    }

    // Only the lender payment outputs still need to be found.
    ( []
    , [ req_lender_output, ..rest_req_pay_outs ]
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
        // This will either crash or return true.
        expect True = lender_output_has_proper_value(
          val,
          offer_deposit,
          loan_id,
          active_beacon_id
        )

        check_outputs(
          borrower_address,
          [],
          rest_req_pay_outs, // Move on to the next payment output.
          rest_outs // Move on to the next output.
        )

      // This lender payment output still needs to be found.
      } else {
        check_outputs(
          borrower_address,
          [],
          req_pay_outputs, // Keep the current required payment output.
          rest_outs // Move on to the next output.
        )
      }
    }

    // Both collaleral outputs and lender payment outputs still need to be found. The next ones
    // to find are not necessarily for the same offer.
    ( [ req_collateral_output, ..rest_req_col_outs ]
    , [ req_lender_output, ..rest_req_pay_outs ]
    , [ Output(addr,val,d,_), ..rest_outs ]
    ) -> {
      let RequiredCollateralOutput{
        collateralization, 
        active_beacon_id, 
        loan_principal,
        loan_id: col_loan_id, // This may be a different one than the next req. payment.
        asset_beacon,
        borrower_id, 
        req_datum
      } = req_collateral_output

      let RequiredLenderOutput{
        lender_address,
        offer_deposit,
        loan_id: pay_loan_id, // This may be a different one than the next req. collateral.
        ..
      } = req_lender_output

      // Check if it is the next required collateral output.
      if addr == borrower_address && d == req_datum {
        // has_enough_collateral checks that the output contains the proper beacons.
        expect True = error_if_false(
          @"Collateral output does not have enough collateral",
          has_enough_collateral(
            active_beacon_id,
            borrower_id,
            asset_beacon,
            col_loan_id,
            loan_principal,
            collateralization,
            val
          )
        )

        check_outputs(
          borrower_address,
          rest_req_col_outs, // Move on to the next collateral output.
          req_pay_outputs, // This payment output still needs to be found.
          rest_outs // Move on to the next output.
        )

      // Check if it is the next required lender output.
      } else if addr == lender_address && d == InlineDatum((active_beacon_id,pay_loan_id)) {
        // This will either crash or return true.
        expect True = lender_output_has_proper_value(
          val,
          offer_deposit,
          pay_loan_id,
          active_beacon_id
        )

        check_outputs(
          borrower_address,
          req_col_outputs, // This collateral output still needs to be found.
          rest_req_pay_outs, // Move on to the next payment output.
          rest_outs, // Move on to the next output.
        )

      // Move on to the next output in the transaction. Both required outputs still need to
      // be found.
      } else {
        check_outputs(
          borrower_address,
          req_col_outputs, // Keep the current required collateral output.
          req_pay_outputs, // Keep the current required payment output.
          rest_outs // Move on to the next output.
        )
      }
    }

    ([], [], _) -> True

    _ -> error "Not all required outputs found"
  }
}

// Check that the new loans have the proper outputs and the proper beacons were minted/burned.
pub fn valid_loan_acceptances(
  negotiation_beacon_id: NegotiationId,
  active_beacon_id: ActiveId,
  payment_observer_hash: ScriptHash,
  interest_observer_hash: ScriptHash,
  address_update_observer_hash: ScriptHash,
  loan_script_hash: ScriptHash,
  validity_range: ValidityRange,
  inputs: List<Input>,
  outputs: List<Output>,
  minted: Value
) -> Address {
  let (req_addr,req_col_outputs,req_pay_outputs,offers_accepted,asks_closed,req_mint,req_burn) =
    get_input_info(
      negotiation_beacon_id,
      payment_observer_hash,
      interest_observer_hash,
      address_update_observer_hash,
      loan_script_hash,
      validity_range,
      inputs
    )

  when req_addr is {
    None -> error "No loan inputs found" // Don't ever mint active beacons without loan inputs.
    Some(borrower_addr) -> {
      let (negs,acts) = get_beacon_mint_and_burn(
        negotiation_beacon_id,
        active_beacon_id,
        (None,None),
        value.to_dict(minted) |> dict.to_list(_)
      )

      expect True = error_if_false(
        @"The wrong negotiation beacons were burned",
        negs == Some(req_burn))

      expect True = error_if_false(
        @"The wrong active beacons were minted",
        acts == Some(req_mint))

      expect True = error_if_false(
        @"# Ask UTxOs closed /= # Offer UTxOs accepted",
        asks_closed == offers_accepted)

      expect True = check_outputs(
        borrower_addr,
        req_col_outputs,
        req_pay_outputs,
        outputs
      )

      borrower_addr
    }
  }
}
