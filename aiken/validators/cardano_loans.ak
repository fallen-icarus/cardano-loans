use aiken/hash.{Hash,Blake2b_224}
use aiken/transaction/credential.{Address,Script,Inline}
use aiken/transaction/value
use aiken/transaction.{ScriptContext,Transaction,Input,Output} as tx
use aiken/list
use aiken/option
use cardano_loans/ratio
use cardano_loans/types.{
  LoanDatum, LoanRedeemer, BeaconRedeemer, AskDatum, OfferDatum, ActiveDatum,
  CloseAsk, CloseOffer, AcceptOffer, MakePayment, Rollover, ClaimExpired, UpdateLenderAddress,
  UnlockLostCollateral, MintAskBeacon, MintOfferBeacon, MintActiveBeacon, BurnBeacons
}
use cardano_loans/utils
// use aiken/cbor

// test cbor_test() {
//   let offer_datum = OfferDatum {
//     loan_beacon_sym: "",
//     lender_id: "",
//     lender_address: Address(credential.VerificationKeyCredential("00"),None),
//     loan_asset: ("",""),
//     loan_principle: 10,
//     loan_checkpoints: [1,2,3],
//     loan_term: 20,
//     loan_interest: ratio.unsafe_ratio(1,2),
//     collateralization: [(("",""),ratio.unsafe_ratio(1,1))],
//     claim_period: 10
//   }
//   cbor.serialise(offer_datum) == #""
// }

const app_name = @"testing"

validator() {
  fn spend(datum: LoanDatum, redeemer: LoanRedeemer, ctx: ScriptContext) -> Bool {
    let ScriptContext(tx_info,purpose) = ctx
    expect tx.Spend(out_ref) = purpose
    when redeemer is 
      {
        CloseAsk -> 
          {
            // The datum must be an AskDatum.
            when datum is {
              AskDatum(sym,..) -> 
                {
                  let Transaction(ins,_,_,_,mint,_,wdrw,_,sigs,..) = tx_info
                  expect Some(Input(_, Output(Address(_,scred), _, _, _))) = 
                    tx.find_input(ins,out_ref)
                  
                  // The address' staking credential must signal approval.
                  if !utils.staking_credential_approves(scred,sigs,wdrw) 
                    { error @"Staking credential did not approve" }

                  // All Ask beacons among inputs must be burned.
                  else 
                    {
                      let ival = utils.total_input_value(ins)
                      utils.all_beacons_burned(sym,"Ask",ival,mint)
                    }
                }

              _ -> error @"Datum is not an AskDatum"
            }
          }
        CloseOffer -> 
          {
            // The datum must be an OfferDatum.
            when datum is 
              {
                OfferDatum(sym,lender_id,..) -> 
                  {
                    let Transaction(ins,_,_,_,mint,_,wdrw,_,sigs,..) = tx_info
                    expect Some(Input(_, Output(Address(_,scred), ival, _, _))) = 
                      tx.find_input(ins,out_ref)
                    let total_ival = utils.total_input_value(ins)

                    // If the Offer beacon is present, Lender has custody:
                    if value.quantity_of(ival,sym,"Offer") == 1 
                      {
                        // The lender must approve the tx.
                        if !utils.lender_approves(lender_id,sigs,wdrw) 
                          { error @"Lender did not approve" } 
                        else 
                          { 
                            // All Offer beacons among inputs must be burned.
                            utils.all_beacons_burned(sym,"Offer",total_ival,mint) &&
                            // All LenderIDs among inputs must be burned.
                            utils.all_beacons_burned(sym,lender_id,total_ival,mint)
                          }
                      }
                    
                    // Otherwise, the address owner has custody and the staking credential
                    // must approve.
                    else if !utils.staking_credential_approves(scred,sigs,wdrw) 
                      { error @"Address owner did not approve" } 
                    else 
                      { True }
                  }

                _ -> error @"Datum is not an OfferDatum"
              }
          }
        AcceptOffer -> 
          {
            let Transaction(_,_,_,_,mint,..) = tx_info
            let mint_as_val = value.from_minted_value(mint)

            // The inputs must either have an AskDatum or an OfferDatum.
            when datum is 
              {
                AskDatum(sym,..) -> 
                  {
                    // At least one Active beacon must be minted. The minting policy will do the
                    // rest of the checks.
                    if value.quantity_of(mint_as_val,sym,"Active") <= 0 
                      { error @"Active beacon not minted" } 
                    else 
                      { True }
                  }

                OfferDatum(sym,..) -> 
                  {
                    // At least one Active beacon must be minted. The minting policy will do the
                    // rest of the checks.
                    if value.quantity_of(mint_as_val,sym,"Active") <= 0 
                      { error @"Active beacon not minted" } 
                    else 
                      { True }
                  }

                _ -> error @"Invalid input type"
              }
          }
        MakePayment -> 
          {
            // The datum must be an ActiveDatum.
            when datum is 
              {
                ActiveDatum(
                  sym,
                  borrower_id,
                  lender_address,
                  loan_asset,
                  principle,
                  next_checkpoints,
                  past_checkpoints,
                  term,
                  interest,
                  collateralization,
                  claim,
                  expiration,
                  outstanding,
                  loan_id
                  ) -> 
                    {
                      let Transaction(ins,_,outs,_,mint,_,wdrw,validity_range,sigs,..) = tx_info
                      expect Some(Input(_, Output(Address(pcred,scred), ival, _, _))) = 
                        tx.find_input(ins,out_ref)
                      
                      // The address' staking credential must signal approval.
                      if !utils.staking_credential_approves(scred,sigs,wdrw) 
                        { error @"Borrower did not approve" } 
                      // If the Active beacon is present, this is a valid loan UTxO:
                      else if value.quantity_of(ival,sym,"Active") == 1 
                        {
                          let payment_time = utils.get_payment_time(validity_range)
                          let next = list.head(next_checkpoints)
                          
                          // Get the total amount paid to the lender.
                          let (total_paid_,collat_flux,collat_datum) = 
                            utils.payment_info(
                              lender_address,
                              sym,
                              loan_asset,
                              loan_id,
                              ival,
                              Address(pcred,scred),
                              borrower_id,
                              outs
                            )
                          let total_paid = ratio.from_integer(total_paid_)

                          // The loan must not be expired.
                          if payment_time >= expiration 
                            { error @"Loan is expired" } 
                          // The next checkpoint must not have passed.
                          else if option.map(next,fn(x) {payment_time >= x}) == Some(True) 
                            { error @"The next checkpoint has passed" } 
                          else 
                            {
                              // If the total amount paid is greater than or equal to the outstanding
                              // balance, the loan is fully paid off:
                              if ratio.gte(total_paid,outstanding) 
                                {
                                  // All BorrowerIDs among tx inputs must be burned.
                                  // No other tokens can be minted/burned in this tx.
                                  // Successful loan repayments require only the BorrowerID is 
                                  // minted/burned in the transaction.
                                  let in_val = utils.total_input_value(ins)

                                  when value.flatten(value.from_minted_value(mint)) is 
                                    {
                                      [(cn,tn,n)] -> 
                                        {
                                          if cn != sym || tn != borrower_id || 
                                            n != value.quantity_of(in_val,sym,borrower_id) 
                                            { error @"All BorrowerIDs must be burned and no other assets minted/burned in tx" } 
                                          else 
                                            { True }
                                        }
                                      _ -> 
                                        { error @"Only BorrowerIDs can be minted/burned in this tx" }
                                    }

                                  // All remaining collateral is unlocked.
                                } 
                              
                              // Otherwise, this is a partial payment.
                              else 
                                {
                                  when collat_datum is 
                                    {
                                      None -> error @"Corresponding collateral output not found"
                                      Some(d) -> 
                                        {
                                          // The datum must be the same as the input except the
                                          // outstanding balance must be updated.
                                          let expected_datum = ActiveDatum(
                                            sym,
                                            borrower_id,
                                            lender_address,
                                            loan_asset,
                                            principle,
                                            next_checkpoints,
                                            past_checkpoints,
                                            term,
                                            interest,
                                            collateralization,
                                            claim,
                                            expiration,
                                            ratio.subtract(outstanding,total_paid),
                                            loan_id
                                          )
                                          if d != expected_datum 
                                            { error @"Collateral output has wrong datum" }

                                          // for all collateral:
                                          // proportion of collateral reclaimed <= proportion of loan repaid
                                          else 
                                            { utils.collateral_taken_check(
                                                ival,
                                                collat_flux,
                                                total_paid,
                                                outstanding,
                                                collateralization
                                              )
                                            }
                                        }
                                    }
                                }
                            }
                        } 

                      // This is an invalid Active UTxO and the address owner can spend it. The
                      // approval was already checked.
                      else 
                        { True }
                    }

                _ -> error @"Datum is not an ActiveDatum"
              }
          }

        Rollover -> 
          {
            // The datum must be an ActiveDatum.
            when datum is 
              {
                ActiveDatum(
                  sym,
                  borrower_id,
                  lender_address,
                  loan_asset,
                  principle,
                  next_checkpoints,
                  past_checkpoints,
                  term,
                  interest,
                  collateralization,
                  claim,
                  expiration,
                  outstanding,
                  loan_id
                  ) ->
                  {
                    let Transaction(ins,_,outs,_,_,_,wdrw,validity_range,sigs,..) = tx_info
                    expect Some(Input(_, Output(Address(pcred,scred), ival, _, _))) = 
                      tx.find_input(ins,out_ref)
                    let payment_time = utils.get_payment_time(validity_range)

                    // The address' staking credential must signal approval.
                    if !utils.staking_credential_approves(scred,sigs,wdrw) 
                      { error @"Borrower did not approve" }
                    // The UTxO must have an Active beacon.
                    else if value.quantity_of(ival,sym,"Active") != 1 
                      { error @"UTxO missing Active beacon" }
                    // The loan must not be expired.
                    else if payment_time >= expiration 
                      { error @"Loan is expired" }
                    // There must be an output to this address with:
                      // An updated datum:
                        // The loan outstanding must acrue interest.
                        // The passed checkpoint must be removed from next_checkpoints and added to
                        //   past_checkpoints.
                      // The exact same value as the input.
                    else
                      {
                        expect [x,..later_checks] = next_checkpoints
                        let new_past = list.push(past_checkpoints,x)
                        let expected_datum = ActiveDatum(
                          sym,
                          borrower_id,
                          lender_address,
                          loan_asset,
                          principle,
                          later_checks, // remove current checkpoint
                          new_past, // add current checkpoint to history
                          term,
                          interest,
                          collateralization,
                          claim,
                          expiration,
                          // accrue interest
                          ratio.multiply(outstanding,ratio.add(ratio.from_integer(1),interest)),
                          loan_id
                        )
                        utils.update_datum_check(sym,Address(pcred,scred),loan_id,ival,expected_datum,outs)
                      }
                  }

                _ -> error @"Datum is not an ActiveDatum"
              }
          }

        ClaimExpired -> 
          {
            // The datum must be an ActiveDatum.
            when datum is
              {
                ActiveDatum(
                  sym,
                  borrower_id,
                  _,
                  _,
                  _,
                  _,
                  _,
                  _,
                  _,
                  _,
                  _,
                  expiration,
                  outstanding,
                  loan_id
                ) ->
                {
                  let Transaction(ins,_,_,_,mint,_,_,validity_range,_,..) = tx_info
                  expect Some(Input(_, Output(_, ival, _, _))) = 
                    tx.find_input(ins,out_ref)
                  let claim_time = utils.get_start_time(validity_range)
                  // The input must have an Active beacon.
                  if value.quantity_of(ival,sym,"Active") != 1
                    { error @"Missing Active beacon" }
                  // The loan must be expired or fully repaid.
                  else if claim_time <= expiration && ratio.gt(outstanding,ratio.zero())
                    { error @"Loan is still active" }
                  // Two LoanIDs must be burned: lock and key.
                  else if value.quantity_of(value.from_minted_value(mint),sym,loan_id) != -2
                    { error @"Two LoanIDs not burned" }
                  else
                    {
                      let total_ival = utils.total_input_value(ins)
                      // All Active beacons among inputs must be burned.
                      utils.all_beacons_burned(sym,"Active",total_ival,mint) &&
                      // If the BorrowerID is still present, it must also be burned.
                      utils.all_beacons_burned(sym,borrower_id,total_ival,mint)
                    }
                }

                _ -> error @"Datum is not an ActiveDatum"
              }
          }

        UpdateLenderAddress(new_address) -> 
          {
            // The datum must be an ActiveDatum.
            when datum is
              {
                ActiveDatum(
                  sym,
                  borrower_id,
                  _,
                  loan_asset,
                  principle,
                  next_checkpoints,
                  past_checkpoints,
                  term,
                  interest,
                  collateralization,
                  claim,
                  expiration,
                  outstanding,
                  loan_id
                ) -> 
                {
                  let Transaction(ins,_,outs,..) = tx_info
                  expect Some(Input(_, Output(Address(pcred,scred), ival, _, _))) = 
                    tx.find_input(ins,out_ref)
                  // The input must have a LoanID.
                  if value.quantity_of(ival,sym,loan_id) != 1
                    { error @"Input missing LoanID" }
                  // There must be two LoanIDs among the tx inputs.
                  else if value.quantity_of(utils.total_input_value(ins),sym,loan_id) != 2
                    { error @"Must have two LoanIDs among inputs" }
                  // The new address must use a payment pubkey.
                  else if !utils.address_uses_payment_pubkey(new_address)
                    { error @"Lender address must use payment pubkey" }
                  // There must be an output to this address with:
                    // The same exact value as the input.
                    // The same datum except the new address.
                  else 
                    {
                      let req_datum = ActiveDatum(
                          sym,
                          borrower_id,
                          new_address,
                          loan_asset,
                          principle,
                          next_checkpoints,
                          past_checkpoints,
                          term,
                          interest,
                          collateralization,
                          claim,
                          expiration,
                          outstanding,
                          loan_id
                        )
                      utils.update_datum_check(sym,Address(pcred,scred),loan_id,ival,req_datum,outs)
                    }
                }

                _ -> error @"Datum is not an ActiveDatum"
              }
          }

        UnlockLostCollateral -> 
          {
            // The datum must be an ActiveDatum.
            when datum is
              {
                ActiveDatum(
                  sym,
                  borrower_id,
                  _,
                  _,
                  _,
                  _,
                  _,
                  _,
                  _,
                  _,
                  claim_expiration,
                  _,
                  _,
                  loan_id
                ) ->
                {
                  let Transaction(ins,_,_,_,mint,_,wdrw,validity_range,sigs,..) = tx_info
                  expect Some(Input(_, Output(Address(_,scred), ival, _, _))) = 
                    tx.find_input(ins,out_ref)
                  let claim_time = utils.get_start_time(validity_range)
                  // The claim period must have passed.
                  if claim_time <= claim_expiration
                    { error @"Claim period has not passed" }
                  // The address' staking credential must signal approval.
                  else if !utils.staking_credential_approves(scred,sigs,wdrw)
                    { error @"Address owner did not approve" }
                  // Input must have an Active beacon.
                  else if value.quantity_of(ival,sym,"Active") != 1
                    { error @"Input missing Active beacon" }
                  else 
                    {
                      let total_ival = utils.total_input_value(ins)
                      // All Active beacons among tx inputs must be burned.
                      utils.all_beacons_burned(sym,"Active",total_ival,mint) &&
                      // All LoanIDs among tx inputs must be burned.
                      utils.all_beacons_burned(sym,loan_id,total_ival,mint) &&
                      // All BorrowerIDs among tx inputs must be burned.
                      utils.all_beacons_burned(sym,borrower_id,total_ival,mint)
                    }
                }

                _ -> error "Datum is not an ActiveDatum"
              }
          }
      }
  }
}

validator(dapp_hash: Hash<Blake2b_224, Script>) {
  fn mint(redeemer: BeaconRedeemer, ctx: ScriptContext) -> Bool {
    let ScriptContext(tx_info,purpose) = ctx
    expect tx.Mint(policy_id) = purpose
    when redeemer is 
      {
        MintAskBeacon(borrower_cred) -> 
          {
            let Transaction(_,_,_,_,mint,_,wdrw,_,sigs,..) = tx_info

            // Only Ask beacons can be minted and they must use the token name 'Ask'.
            utils.mint_check(redeemer,value.tokens(value.from_minted_value(mint),policy_id)) &&

            // The Ask beacons must be minted to an address protected by the dapp_hash.
            // The Ask beacons must be minted to an address using a staking credential that matches the
            //   credential passed with the redeemer.
            // The Ask beacons must be stored with the proper inline AskDatum:
              // loan_beacon_sym == this policy id.
              // borrower_id == credential in redeemer as a token name.
              // loan_principle > 0.
              // loan_term > 0.
              // collateral list must not be empty.
            // The Ask beacons must be stored individually at the loan address.
            utils.destination_check(dapp_hash,policy_id,redeemer,tx_info,app_name) && 

            // The receiving staking credential must signal approval.
            if !utils.staking_credential_approves(Some(Inline(borrower_cred)),sigs,wdrw) 
              { error @"Borrower did not approve" } 
            else 
              { True }
          }

        MintOfferBeacon(lender_cred) -> 
          {
            let Transaction(_,_,_,_,mint,_,wdrw,_,sigs,..) = tx_info

            // Only one Offer beacon and one LenderID can be minted. The Offer beacon must have the
            //   token name 'Offer'. The LenderID must use the credential hash in the redeemer as 
            //   the token name.
            utils.mint_check(redeemer,value.tokens(value.from_minted_value(mint),policy_id)) &&

            // The beacons must be minted to an address protected by the dapp_hash.
            // The beacons must be minted to an address with a staking credential.
            // The beacons must be stored together in the same UTxO.
            // The beacons must be stored with the proper inline OfferDatum:
              // loan_beacon_sym == this policy id.
              // lender_id == credential hash as token name.
              // lender_address must use a payment script.
              // loan_principle > 0.
              // loan_checkpoints must all be >= 0 and in ascending order. It can be empty. No duplicates.
              // loan_term > 0.
              // 0 <= loan_interest <= 1.
              // collateralization list must not be empty and all relative prices >= 0.
              // claim_period > 0.
            // The Offer UTxO must be stored with 5 ADA + the amount of the loan asset.
            utils.destination_check(dapp_hash,policy_id,redeemer,tx_info,app_name) &&

            // The credential in the redeemer must signal approval.
            if !utils.staking_credential_approves(Some(Inline(lender_cred)),sigs,wdrw) 
              { error @"Lender did not approve" } 
            else 
              { True }
          }

        MintActiveBeacon(borrower_cred,pairs) -> 
          {
            // All Offer beacons must be burned.
            // All LenderIDs must be burned.
            // All Ask beacons must be burned.
            // One Active beacon, one BorrowerID, and 2 LoanIDs must be minted for every loan accepted.
            // Only one loan address has inputs in this tx.
            // All loan address inputs must either have an Ask beacon or an Offer beacon.
            // The datums of the paired inputs must agree.
            // One of the newly minted LoanIDs must be paid to the corresponding lender + 5 ADA.
            // The borrower must approve the transaction.
            let Transaction(ins,_,outs,_,mint,_,wdrw,validity_range,sigs,..) = tx_info
            let start_time = utils.get_start_time(validity_range)
            let (pay_map,pay_size,loan_map,loan_size,req_mint) = utils.acceptance_input_checks(
              borrower_cred,
              ins,
              policy_id,
              dapp_hash,
              start_time,
              pairs
            )
            if value.tokens(req_mint,policy_id) != value.tokens(value.from_minted_value(mint),policy_id) 
              { error @"Wrong beacons minted/burned" } 
            else if !utils.staking_credential_approves(Some(Inline(borrower_cred)),sigs,wdrw) 
              { error @"Borrower did not approve" } 
            else 
              {
                utils.acceptance_output_checks(
                  borrower_cred,
                  policy_id,
                  dapp_hash,
                  pay_map,
                  pay_size,
                  loan_map,
                  loan_size,
                  outs
                )
              }
          }

        BurnBeacons -> 
          {
            let Transaction(_,_,_,_,mint,..) = tx_info

            // Allowed as long as the redeemer is only used to burn.
            utils.mint_check(redeemer,value.tokens(value.from_minted_value(mint),policy_id))
          }
      }
  }
}