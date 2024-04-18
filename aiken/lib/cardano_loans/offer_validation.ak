use aiken/dict.{Dict}
use aiken/list
use aiken/option
use aiken/transaction.{Datum,Output}
use aiken/transaction/credential.{Address,ScriptCredential}
use aiken/transaction/value.{AssetName,PolicyId,Value}

use cardano_loans/data/core.{
    ScriptHash,
    NegotiationId,
    ActiveId,
    Asset,
    AssetBeacon,
    LenderId,
    NoPenalty,
    FixedFee,
    PercentFee
  }
use cardano_loans/data/datums.{OfferDatum}
use cardano_loans/utils.{error_if_false,trace_if_false,valid_lender_address,parse_datum}

// This is used to check that the value of Offer UTxOs are correct. It returns the value of the
// loan asset so that it can be checked against the principle in the datum. When ada is the
// loan asset, the first Int should be loan_principle + offer_deposit. Due to the way this
// function is written, beacon tokens do not count towards the offer quantities even if they
// are the loan asset. Therefore, beacon tokens cannot be used as the loan asset.
fn extract_offer_quantity(
  val: Value,
  negotiation_beacon_id: NegotiationId,
  asset_beacon: AssetBeacon,
  loan_asset: Asset,
  lender_id: LenderId
) -> (Int,Int) {
  // Extract out the loan asset info.
  let (loan_asset_id,loan_asset_name) = loan_asset

  // A function to map over the value of this UTxO. It will return the total number of the loan
  // asset and the total amount of ada.
  let foo = fn(x: (PolicyId,Dict<AssetName,Int>), acc: (Int,Int)) {
    let (cn,tns_dict) = x
    let (loan_quantity,ada_quantity) = acc

    // Check the beacons.
    if cn == core.from_negotiation_id(negotiation_beacon_id) {
      let tns_list = dict.to_list(tns_dict)

      // A function to check each beacon.
      let beacon_check = fn(y: (AssetName,Int), inner_acc: (Bool,Bool,Bool)) {
        let (tn,n) = y
        let (offer_check,asset_check,lender_check) = inner_acc
        if tn == "Offer" {
          // There must only be one Offer beacon. If n was zero, tn could not have matched "Offer".
          (error_if_false(@"UTxO has too many Offer beacons", n == 1), asset_check, lender_check)
        } else if tn == core.get_asset_beacon_name(asset_beacon) {
          // There must only be one Asset beacon. If n was zero, tn could not have matched the
          // asset beacon.
          (offer_check, error_if_false(@"UTxO has too many asset beacons", n == 1), lender_check)
        } else if tn == core.get_lender_id_name(lender_id) {
          // There must only be one LenderID beacon. If n was zero, tn could not have matched the
          // lender_id.
          (offer_check, asset_check, error_if_false(@"UTxO has too many lender ids", n == 1))
        } else {
          error @"UTxO has a beacon with the wrong name"
        }
      }

      when list.foldl(tns_list,(False,False,False),beacon_check) is {
        (True,True,True) -> (loan_quantity,ada_quantity)

        // This path can be taken if the UTxO has only some of the required beacons. This path
        // should never be taken since the script should fail earlier if the UTxO has too few
        // or too many types of beacons.
        _ -> error @"UTxO does not have the right beacons"
      }
    } 

    // Check that the asset is the loan asset. Ada is the only asset that can be present without 
    // being the loan asset.
    else if cn == loan_asset_id {
      when dict.to_list(tns_dict) is {
        // Since this check assumes the loan asset is the only asset in the UTxO with that
        // policy id, beacons cannot be the loan asset. When ada is the loan asset, the 
        // ada_quantity field in the accumulator will not be used.
        [(tn,n)] -> {
          if tn == loan_asset_name {
            (loan_quantity+n,ada_quantity)
          } else {
            error @"No extraneous assets allowed in the UTxO"
          }
        }
        _ -> error @"No extraneous assets allowed in the UTxO"
      }
    }

    // Check that the asset is ada. If this branch is taken, then ada is not the loan asset.
    // Ada must be explicitly allowed to satisfy the minUTxOValue requirement.
    else if cn == #"" {
      expect Some(n) = dict.get(tns_dict,#"")

      (loan_quantity,ada_quantity+n)
    }

    else { error @"No extraneous assets allowed in the UTxO" }
  }

  // ada_quantity will be 0 when ada is the loan asset.
  list.foldl(value.to_dict(val) |> dict.to_list(_), (0,0), foo)
}

// This checks the datum and calls extract_offer_quantity to check the value.
fn valid_offer_output(
  negotiation_beacon_id_: NegotiationId,
  active_beacon_id_: ActiveId,
  lender_id_: LenderId,
  proxy_hash: ScriptHash,
  output_value: Value,
  output_datum: Datum
) -> Bool {
  // Extract the info from the OfferDatum. Crash the script if it is not an OfferDatum.
  expect OfferDatum{
    negotiation_beacon_id,
    active_beacon_id,
    lender_id,
    lender_address,
    loan_asset,
    asset_beacon,
    loan_principle,
    compound_frequency,
    loan_term,
    loan_interest,
    min_payment,
    penalty,
    collateralization,
    claim_period,
    offer_deposit,
    offer_expiration,
    .. // Don't need to check collateral_is_swappable
  } = when parse_datum(output_datum) is {
    OfferDatum{..} as datum -> datum
    _ -> error @"UTxO not stored with an OfferDatum"
  }

  // Generate the required asset beacon name based on the loan asset.
  let expected_asset_beacon = core.make_asset_beacon(loan_asset)

  // Get the amount of the loan asset and the amount of ada in the UTxO. When ada is the loan,
  // the ada_quantity will be unused.
  let (loan_quantity,ada_quantity) = 
    extract_offer_quantity(
      output_value,
      negotiation_beacon_id_,
      expected_asset_beacon,
      loan_asset,
      lender_id_
    )

  // Pattern match out the numerator and denominator for the interest so that they can be
  // checked.
  let (interest_num,interest_den) = loan_interest

  // All of the following must be true.
  and {
    // Datum checks:
    trace_if_false(
      @"Datum has wrong negotiation beacon id", 
      negotiation_beacon_id_ == core.to_negotiation_id(negotiation_beacon_id)),
    trace_if_false(
      @"Datum has wrong active beacon id", 
      active_beacon_id_ == core.to_active_id(active_beacon_id)),
    trace_if_false(
      @"Datum has wrong lender id", 
      core.get_lender_id_name(lender_id_) == lender_id),
    trace_if_false(
      @"Datum has wrong lender address", 
      valid_lender_address(proxy_hash,lender_address)),
    trace_if_false(
      @"Datum has wrong asset beacon", 
      asset_beacon == core.get_asset_beacon_name(expected_asset_beacon)),
    trace_if_false(
      @"Datum has wrong principle", 
      loan_principle > 0),
    trace_if_false(
      @"Datum has wrong compound frequency", 
      // Set to 1 to ensure True when None.
      option.or_else(compound_frequency,1) |> fn(x) { x > 0 }), 
    trace_if_false(
      @"Datum has wrong term", 
      loan_term > 0),
    trace_if_false(
      @"Datum has wrong loan interest", 
      interest_den > 0 && interest_num >= 0),
    trace_if_false(
      @"Datum has wrong min payment", 
      min_payment >= 0),
    trace_if_false(
      @"Datum has wrong collateralization",
      and {
        // List can't be empty.
        !list.is_empty(collateralization),
        // List must be sorted and not have duplicates.
        collateralization == list.sort(collateralization,utils.compare_collateralization_no_dups),
        // All prices must be > 0 and the denominators must be > 0.
        list.all(
          collateralization,
          fn(x) {
            let (_,(num,den)) = x
            num >= 0 && den > 0
          }
        )
      }
    ),
    trace_if_false(
      @"Datum has wrong claim period", 
      claim_period > 0),
    trace_if_false(
      @"Datum has wrong offer deposit", 
      offer_deposit > 0),
    trace_if_false(
      @"Datum has wrong offer expiration", 
      // Set to 1 to ensure True when None.
      option.or_else(offer_expiration,1) |> fn(x) { x > 0 }), 
    trace_if_false(
      @"Datum has wrong penalty",
      when penalty is {
        NoPenalty -> True
        FixedFee(fee) -> fee > 0 && min_payment > 0
        PercentFee((fee_num,fee_den)) -> fee_num > 0 && fee_den > 0 && min_payment > 0
      }
    ),

    // Value checks:
    trace_if_false(
      @"UTxO has wrong value",
      if loan_asset == (#"",#"") { 
        loan_quantity == loan_principle + offer_deposit
      } else { 
        ada_quantity == offer_deposit && loan_quantity == loan_principle
      }
    )
  }
}

// Check if the beacons are locked at the proper address and the Offer UTxO is configured properly.
// This function can return False.
pub fn offer_beacon_destination_check(
  loan_script_hash: ScriptHash,
  lender_id: LenderId,
  negotiation_beacon_id: NegotiationId,
  active_beacon_id: ActiveId,
  proxy_hash: ScriptHash,
  outputs: List<Output>
) -> Bool {
  // A function to map over the list of outputs.
  let foo = fn (out: Output, acc: Bool) {
    // Extract out the output's information.
    let Output(Address(pay,stake),val,d,_) = out

    // Get the beacons in this UTxO and pattern match on it.
    when value.tokens(val,core.from_negotiation_id(negotiation_beacon_id)) |> dict.to_list() is {
      // If beacons are not present, this output can be ignored.
      [] -> acc

      // The UTxO has three types of beacon. The exact beacons will be checked later.
      [(_,_),(_,_),(_,_)] -> {
        // All previous outputs must be valid.
        acc &&

        // The output must be at a DApp address with the borrower's credential as the
        // stake credential.
        trace_if_false( 
          @"Beacon must go to a DApp address with a valid staking credential",
          pay == ScriptCredential(loan_script_hash) && option.is_some(stake)
        ) &&

        // The output must be properly configured. This has its own trace messages.
        valid_offer_output(negotiation_beacon_id,active_beacon_id,lender_id,proxy_hash,val,d)
      }

      // This path can only be taken if the above two matches failed which means the UTxO
      // has the wrong number of beacons.
      _ -> error @"Offer UTxOs must have exactly three kinds of beacons"
    }
  }

  // Map over the list. This will only return True if all the outputs are valid.
  list.foldl(outputs, True, foo)
}
