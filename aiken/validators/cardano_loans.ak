use aiken/hash.{Blake2b_224, Hash}
use aiken/list
use aiken/math/rational as ratio
use aiken/transaction.{Input, Output, ScriptContext, Transaction} as tx
use aiken/transaction/credential.{Address, Inline, Script, ScriptCredential}
use aiken/transaction/value
use cardano_loans/types.{
  AcceptOffer, ActiveDatum, AskDatum, BeaconRedeemer, BurnBeacons, ClaimExpired,
  CloseAsk, CloseOffer, CreateActive, CreateAsk, CreateOffer, LoanDatum,
  LoanRedeemer, MakePayment, OfferDatum, Rollover, Unlock, UpdateLenderAddress,
}
use cardano_loans/utils.{trace_if_false,error_if_false}

validator { 
  fn proxy(_datum: Data, _redeemer: Data, ctx: ScriptContext) -> Bool {
    
    // Since using the datums to guarantee uniqueness of loan payments is currently the cheapest
    // option, all lender addresses must be able to accept this datum. In the future, it may
    // be possible to cheaply use the reference script field of the UTxO for uniqueness instead
    // which would free up the datum. For now, all lender addresses must be able to accept UTxOs
    // with the datum enforced by the protocol.

    // One option would be allow addresses with any arbitrary payment script and trust the lender
    // to ensure that the script can accept the required datum. However, mistakes happen and these
    // are financial transactions so safe guards should be used whenever possible.

    // Another option is to disallow plutus scripts. Unfortunately, since a plutus script cannot
    // distinguish between a native script and a plutus script, the only way to actually do this is
    // to disallow all payment scripts altogether. Not allowing any payment script would prevent the
    // use of multisig - a desirable feature for pooled lenders. Therefore, this option is too
    // strict.

    // A different approach would be to allow the lenders to set up a personal, "pre-approved" proxy
    // script address that can have arbitrary logic for what to do next. It can accept any datum and
    // redeemer. The proxy payment script would simply delegate spending authority to the staking
    // credential for the proxy address. This has a few benefits: 1) There can be a single payment
    // script hash that the loan validator can look for which make the check cheap. 2) The proxy
    // script can still have arbitrary logic since it can accept any datum and redeemer, and just
    // delegates to the staking script, which can be a pubkey, native script, or plutus script. 3)
    // The datum in loan payment outputs can still be determined by the loan validator which is more
    // convenient than using a receipt token. This approach still allows lenders to use multisig
    // despite being strict on the type of datum used for loan payments. This is the approach that
    // will be taken until it is possible to use the reference script field for uniqueness. This
    // proxy validator is the "pre-approved" scripts.
    
    // It is important to note that a staking script can be executed by withdrawing 0 ADA from the
    // rewards address. In order to use this method, the rewards address must be
    // registered/delegated.

    expect ScriptContext(Transaction(inputs,_,_,_,_,_,wdrw,_,sigs,..),tx.Spend(out_ref)) = ctx
    expect Some(Input(_,Output(Address(_,stake),..))) = tx.find_input(inputs,out_ref)

    trace_if_false(
      @"Staking credential did not approve", 
      utils.staking_credential_approves(stake,wdrw,sigs)
    )
  }
}

validator(proxy_hash: Hash<Blake2b_224,Script>) {
  fn spend(datum: LoanDatum, redeemer: LoanRedeemer, ctx: ScriptContext) -> Bool {
    expect ScriptContext(
      Transaction(inputs,_,outputs,_,mint,_,wdrw,validity_range,sigs,..), 
      tx.Spend(out_ref)
    ) = ctx

    when redeemer is {
      CloseAsk -> {

        // This redeemer is dedicated to closing any Ask UTxOs, valid or invalid (i.e., UTxOs with
        // an AskDatum but without the proper beacons). The borrower has custody of both valid and
        // invalid Ask UTxOs.
         
        // In order to minimize the attack surface of this protocol, updating Ask UTxOs in place is
        // not permitted. Instead, one transaction will be needed to close the current one and a new
        // transaction will be needed to open a new one with the new terms. Considering how cheap it
        // is to open/close Ask UTxOs, adding the extra attack surface does not seem worth the
        // potential benefit.
        
        // In order to spend a UTxO with this redeemer, all of the following must be true:
        // 1) The input being spent must have an AskDatum. If it doesn't, it is not an Ask UTxO.
        // 2) The address' staking credential (aka the borrower) must signal approval.
        // 3) All Ask beacons among the transaction inputs must be burned.
        // 4) All Asset beacons among the transaction inputs must be burned.
        
        // Since Active UTxOs require an Asset beacon to be kept, requirement 4 prevents the
        // CloseAsk redeemer from being used in the same transaction where a loan is accepted.
        
        expect Some(
          Input(
            _,
            Output(
              Address(ScriptCredential(_),stake),
              _,
              ..))) = tx.find_input(inputs,out_ref)

        // The datum must be an AskDatum. Requirement 1.
        when datum is {
          AskDatum(sym,_,loan_asset,..) -> {
            let ival = utils.total_input_value(inputs)
            let mint_as_val = value.from_minted_value(mint)
            let asset_beacon_name = utils.generate_beacon_name(loan_asset)

            // The address' staking credential must signal approval. Requirement 2.
            trace_if_false( 
              @"Borrower did not approve",
              utils.staking_credential_approves(stake,wdrw,sigs)
            ) &&

            // All Ask beacons among inputs must be burned. Requirement 3.
            trace_if_false(
              @"Not all Ask beacons burned",
              value.quantity_of(ival,sym,"Ask") == -value.quantity_of(mint_as_val,sym,"Ask")
            ) &&

            // All Asset beacons among inputs must be burned. Since all Asset beacons
            // must be burned, this redeemer cannot be composed with the CloseOffer beacon to
            // trick the minting script into creating an Active UTxO since the Active UTxO requires
            // a Asset beacon to be kept. Requirement 4.
            trace_if_false(
              @"Not all Asset beacons burned",
              value.quantity_of(ival,sym,asset_beacon_name) ==
                -value.quantity_of(mint_as_val,sym,asset_beacon_name)
            )

          }

          _ -> error @"Datum is not an AskDatum"
        }
      }

      CloseOffer -> {

        // This redeemer is dedicated to closing any Offer UTxOs. Valid Offer UTxOs belong to the
        // lender and invalid UTxOs (ie, UTxOs with an OfferDatum but missing the proper beacons)
        // belong to the address owner (aka the borrower).
        
        // In order to minimize the attack surface of this protocol, updating Offer UTxOs in place
        // is not permitted. See the reason under the `CloseAsk` redeemer.
        
        // In order to spend a UTxO with this redeemer, all of the following must be true:
        // 1) The input being spent must have an OfferDatum.
        // 2) If the input has an Offer beacon:
        //      a) All Offer beacons among the transaction inputs must be burned.
        //      b) All LenderIDs among the transaction inputs must be burned.
        //      c) All Asset beacons among the transaction inputs must be burned.
        //      d) The lender must approve the transaction.
        //    Otherwise, the address owner has custody:
        //      a') The address' staking credential must signal approval.
        
        // Since Active UTxOs require an Asset beacon to be kept, requirement 2c prevents the
        // `CloseOffer` redeemer from being used in the same transaction where a loan is accepted.
        
        // Since the lender_id in the datum has a prefix that dictates whether to check for a
        // pubkey or a script, the protocol knows where to look for the lender's approval.
        
        expect Some(
          Input(
            _,
            Output(
              Address(ScriptCredential(_),stake),
              utxo_value,
              ..))) = tx.find_input(inputs,out_ref)

        // The datum must be an OfferDatum. Requirement 1.
        when datum is {
          OfferDatum(sym,lender_id,_,loan_asset,..) -> {
            // If the Offer beacon is present, the Lender has custody.
            if value.quantity_of(utxo_value,sym,"Offer") > 0 {
              let ival = utils.total_input_value(inputs)
              let mint_as_val = value.from_minted_value(mint)
              let asset_beacon_name = utils.generate_beacon_name(loan_asset)

              // All Offer beacons among inputs must be burned. Requirement 2a.
              trace_if_false(
                @"Not all Offer beacons burned",
                value.quantity_of(ival,sym,"Offer") == -value.quantity_of(mint_as_val,sym,"Offer")
              ) &&

              // All LenderIDs among inputs must be burned. Requirement 2b.
              trace_if_false(
                @"Not all LenderIDs burned",
                value.quantity_of(ival,sym,lender_id) == -value.quantity_of(mint_as_val,sym,lender_id)
              ) &&

              // All Asset beacons among inputs must be burned. Requirment 2c.
              trace_if_false(
                @"Not all Asset beacons burned",
                value.quantity_of(ival,sym,asset_beacon_name) ==
                  -value.quantity_of(mint_as_val,sym,asset_beacon_name)
              ) &&

              // The lender must approve the tx. Requirement 2d.
              trace_if_false(
                @"Lender did not approve",
                utils.lender_approves(lender_id,wdrw,sigs)
              )
            }

            // Otherwise the borrower has custody. Requirement 2a'.
            else {
              trace_if_false(
                @"Borrower did not approve",
                utils.staking_credential_approves(stake,wdrw,sigs)
              )
            }
          }

          _ -> error @"Datum is not an OfferDatum"
        }
      }

      AcceptOffer -> {

        // This redeemer is dedicated to accepting a loan. Multiple loans can be accepted in a
        // single transaction.
        
        // Since accepting a loan requires spending both an Ask UTxO and an Offer UTxO, having the
        // spending script do the checks would result in a lot of expensive redundant executions.
        // Instead, the minting policy will do the checks. The spending script just needs to ensure
        // the minting policy is executed properly.
        
        // Since an Active beacon can only be minted when the proper redeemer is used with the
        // minting policy, this spending script can use the minting of the Active beacon as a proxy
        // for whether the minting policy was executed correctly. This invariant of only being able
        // to mint the Active beacon with the proper redeemer is critical to this approach being
        // secure.
        
        // To spend a UTxO with this redeemer, all of the following must be true:
        // 1) The input must have either an AskDatum or an OfferDatum.
        // 2) At least one Active beacon must be minted.
        
        // The minting policy will check the rest.
        
        let mint_as_val = value.from_minted_value(mint)

        // The input must have either an AskDatum or an OfferDatum. Requirement 1.
        when datum is {
          AskDatum(sym,..) -> {
            // At least one Active beacon must be minted. The minting policy will check the rest.
            // Requirement 2.
            error_if_false( 
              @"Active beacon not minted",
              value.quantity_of(mint_as_val,sym,"Active") > 0
            )
          }

          OfferDatum(sym,..) -> {
            // At least one Active beacon must be minted. The minting policy will check the rest.
            // Requirement 2.
            error_if_false( 
              @"Active beacon not minted",
              value.quantity_of(mint_as_val,sym,"Active") > 0
            )
          }

          _ -> error @"Invalid input type"
        }
      }

      MakePayment -> {

        // This redeemer is used to make partial/full loan payments. The script can tell whether
        // the payment is a partial or full loan payment and will validate accordingly. Partial
        // and full payments can be made in the same transaction.
        
        // Due to the way the on-chain credit history works, when a loan is successfully paid off,
        // the BorrowerID must be burned in isolation (ie, no other tokens can be minted/burned, by
        // any policy, in the transaction).
        
        // In order to ensure compound interest accrues, the borrower cannot make the next payment
        // if a rollover is due. The script uses the invalid-hereafter flag as a proxy for the time.
        // For convenience, the borrower can set the invalid-hereafter flag to the slot when the
        // next rollover is due (or expiration if no more rollovers are required).
        
        // This redeemer does not allow "spending" a UTxO from the address; it only allows updating
        // the state of the UTxO. In other words, every input spent with this redeemer must have a
        // corresponding output to this address with the updated state. The LoanIDs are used to link
        // inputs with outputs and prevent double satisfaction.
        
        // To successfully make a payment, all of the following must be true:
        // 1) The input must have an ActiveDatum.
        // 2) The address' staking credential must signal approval.
        // 3) The Active beacon must be present in the input.
        // 4) The BorrowerID must be present in the input.
        // 5) The loan must not be expired.
        // 6) The next rollover must not be required yet.
        // 7) There must be an Active UTxO output to the same address as the input with the proper 
        //    datum:
        //      - Exactly the same as the input's datum except the outstanding balance is updated
        //        to reflect payment.
        // 8) Payments to the lender's address must include the proper datum: 
        //      - (beacon policy id, this loanId) 
        // 9) If the total amount paid to the lender is >= outstanding balance, the 
        //    loan is fully repaid:
        //      a) The BorrowerID for this loan must be burned.
        //      b) No other tokens can be minted/burned in this tx.
        //      c) The Active UTxO must still have 1 Active beacon, 1 Asset beacon, and 1 LoanID.
        //    Otherwise, it is a partial payment:
        //      a') The amount paid must be >= min_payment in the ActiveDatum.
        //      b') The Active UTxO must have 1 Active beacon, 1 BorrowerID, 1 LoanID, and 1 Asset beacon.
        //      c') The proportion of collateral taken must be <= the proportion of the loan repaid.
        
        // If the ActiveDatum says the collateral is swappable, the borrower can swap out collateral
        // when making a payment as long as the relative value of the collateral is still valid.
        
        // In order to allow mixing partial and full payments in a single transaction, this redeemer
        // cannot explicitly check that the proper number of BorrowerIDs are burned. There is no way
        // for a single script execution to cheaply know how many BorrowerIDs should be burned.
        // Instead, the script will make it so that any UTxO with a BorrowerID must also have an
        // Active beacon. Since fully repaid loans do not allow BorrowerIDs to be re-output to
        // Active UTxOs and Active UTxO inputs that are already missing a BorrowerID are not allowed
        // in this transaction (this is due to the way all of the spending redeemers work), these
        // BorrowerIDs cannot possibly be re-output to a UTxO with an Active beacon. Therefore,
        // these BorrowerIDs have no choice but to be burned.
        
        // Any remaining collateral is unlocked during a full payment of a loan. If it is not taken in
        // in this transaction, it can still be claimed with the `Unlock` spending redeemer.
        
        expect Some(
          Input(
            _,
            Output(
              Address(ScriptCredential(_),stake) as addr,
              utxo_value,
              ..))) = tx.find_input(inputs,out_ref)

        // The datum must be an ActiveDatum. Requirement 1.
        when datum is {
          ActiveDatum(
            sym,
            borrower_id,
            lender_address,
            loan_asset,
            principle,
            rollover_frequency,
            last_checkpoint,
            term,
            interest,
            min_payment,
            collateralization,
            swappable,
            claim,
            expiration,
            outstanding,
            loan_id
          ) -> {
            // Get the payment time from invalid-hereafter.
            let payment_time = utils.get_invalid_hereafter(validity_range)

            // Get the total amount paid to the lender. This checks requirement 8, 9b, and 9b'.
            let (total_paid_,collat_flux,collat_info) = utils.payment_info(
                lender_address,
                sym,
                loan_asset,
                loan_id,
                utxo_value,
                addr, // This address.
                borrower_id,
                outputs,
              )
            
            let total_paid = ratio.from_int(total_paid_)

            // The address' staking credential must signal approval. Requirement 2.
            trace_if_false(
              @"Borrower did not approve",
              utils.staking_credential_approves(stake,wdrw,sigs)
            ) &&

            // The Active beacon must be present in the UTxO. Requirment 3.
            trace_if_false(
              @"Input missing Active beacon",
              value.quantity_of(utxo_value,sym,"Active") == 1
            ) &&

            // The BorrowerID must be present in the UTxO. Requirement 4.
            trace_if_false(
              @"Input missing BorrowerID",
              value.quantity_of(utxo_value,sym,borrower_id) == 1
            ) &&

            // The loan must not be expired. Requirement 5.
            trace_if_false(
              @"Loan is expired",
              payment_time <= expiration
            ) &&

            // The rollover must not be required yet. Requirement 6.
            trace_if_false(
              @"Next rollover is required",
              when rollover_frequency is {
                None -> True
                Some(f) -> payment_time <= last_checkpoint + f
              }
            ) &&

            // Check the collateral information.
            when collat_info is {
              None -> error @"Collateral output not found"

              Some((datum,id_present)) -> {
                // The datum must be exactly the same as the input except the
                // outstanding balance must be updated.
                let expected_datum = ActiveDatum(
                  sym,
                  borrower_id,
                  lender_address,
                  loan_asset,
                  principle,
                  rollover_frequency,
                  last_checkpoint,
                  term,
                  interest,
                  min_payment,
                  collateralization,
                  swappable,
                  claim,
                  expiration,
                  ratio.sub(outstanding,total_paid) |> ratio.reduce(_),
                  loan_id
                )
                
                // Regardless of full/partial payment, the datum must be correct.
                // Requirement 7.
                trace_if_false(
                  @"Collateral output has wrong datum",
                  datum == expected_datum
                ) &&

                // If the total amount paid is greater than or equal to the outstanding balance,
                // the loan is fully paid off.
                if ratio.compare(total_paid,outstanding) != Less {
                  // The BorrowerID from the fully paid loan must be burned. Requirement 9a.
                  trace_if_false(
                    @"BorrowerID from fully paid loan must be burned",
                    !id_present
                  ) &&

                  // Only the BorrowerIDs can be minted/burned in this tx. Requirement 9b.
                  when value.flatten(value.from_minted_value(mint)) is {
                    [(cn,tn,n)] -> {
                      trace_if_false(
                        @"BorrowerIDs not burned",
                        cn == sym && tn == borrower_id && n < 0
                      )
                    }

                    _ -> error @"BorrowerIDs must be burned in isolation"
                  }

                  // All remaining collateral is unlocked.
                }

                // Otherwise, this is a partial payment.
                else {
                  // The amount paid must be >= min_payment. Requirement 9a'.
                  trace_if_false(
                    @"Minimum payment not met",
                    total_paid_ >= min_payment
                  ) &&

                  // The BorrowerID must still be stored with the collateral UTxO.
                  // Requirement 9b'.
                  trace_if_false(
                    @"Collateral output must have one BorrowerID",
                    id_present
                  ) &&

                  // proportion of collateral taken <= proportion of loan repaid.
                  // Requirement 9c'.
                  utils.collateral_taken_check(
                    utxo_value,
                    collat_flux,
                    total_paid,
                    outstanding,
                    collateralization,
                    swappable
                  )
                }
              }
            }
          }

          _ -> error @"Datum is not an ActiveDatum"
        }
      }

      Rollover(deposit_increase) -> {
        // This redeemer is used to have the loan accrue interest at set intervals.
        
        // The Active UTxO must be re-output to this address with the proper value and the properly
        // updated datum.
        
        // The invalid-hereafter flag is used as a proxy for the time. For convenience, borrowers
        // can set the invalid-hereafter flag to the expiration slot for the loan.
        
        // To successfully rollover a loan, all of the following must be true:
        // 1) The datum must be an ActiveDatum.
        // 2) The input must have a BorrowerID. This implies the other beacons are also present.
        // 3) The loan must not be expired.
        // 4) The address' staking credential must signal approval.
        // 5) The deposit_increase must be >= 0.
        // 6) There must be an corresponding Active UTxO output to this address with the exact 
        //    same value + the amount of ADA specified by the redeemer and proper datum. The 
        //    datum must be the same as input except:
        //      - loan_outstanding == outstanding * (1 + loan_interest)
        //      - last_checkpoint == last_checkpoint + rollover_frequency
        
        // Technically, the borrower can rollover the loan as many times as they like before the
        // expiration. However, since it means the interest is unnecessarily applied to the balance,
        // they are strongly disincentivized to do this. Therefore, this redeemer assumes it is only
        // executed when it actually needs to be executed.
        
        // If a rollover is missed (ie, the next rollover is required before the previous one was
        // made), this redeemer can only be used to do one rollover at a time. The `MakePayment`
        // redeemer will know that an additional rollover is needed so there is no risk in interest
        // not being applied appropriately.
        
        // Since this redeemer requires that the UTxO being evaluated has a BorrowerID and since the
        // corresponding output must have the same number of BorrowerIDs as the input, this redeemer
        // cannot interfere with the credit history. It can safely be composed with the
        // `MakePayment` redeemer. If the BorrowerID was not required, this redeemer could be used
        // on finished loans in the same transaction where another loan is paid off. This would add
        // another output with an Active beacon but missing the BorrowerID even though the loan was
        // already finished. In other words, it could result in loans being counted multiple times
        // in the credit history. It is possible to ignore the double counting but this requires
        // more involved data analyses of the credit history. To keep the data analyses simple, the
        // protocol just prevents this scenario in the first place.
        
        // Because the datum can grow with this redeemer, the minUTxOValue requirement can increase.
        // Parameter changes can also make this requirement grow. Therefore, requiring the value of
        // the new UTxO to be exactly the same as the input could be too strict in some situations.
        // To relax this restriction slightly, the output UTxO can have the input value + the amount
        // of ADA specified by the redeemer. 
        
        expect Some(
          Input(
            _,
            Output(
              Address(ScriptCredential(_),stake) as addr,
              utxo_value,
              ..))) = tx.find_input(inputs,out_ref)

        // The datum must be an ActiveDatum. Requirement 1.
        when datum is {
          ActiveDatum(
            sym,
            borrower_id,
            lender_address,
            loan_asset,
            principle,
            rollover_frequency,
            last_checkpoint,
            term,
            interest,
            min_payment,
            collateralization,
            swappable,
            claim,
            expiration,
            outstanding,
            loan_id
          ) -> {
            // Get the invalid-hereafter time. This is used to check if the loan is expired.
            // The invalid-hereafter can be set to the loan expiration for convenience.
            let rollover_time = utils.get_invalid_hereafter(validity_range)

            // Log new checkpoint.
            let updated_checkpoint = when rollover_frequency is {
              None -> last_checkpoint
              Some(f) -> last_checkpoint + f
            }

            let expected_datum = ActiveDatum(
              sym,
              borrower_id,
              lender_address,
              loan_asset,
              principle,
              rollover_frequency,
              updated_checkpoint,
              term,
              interest,
              min_payment,
              collateralization,
              swappable,
              claim,
              expiration,
              ratio.reduce(
                ratio.mul(outstanding,ratio.add(ratio.from_int(1),interest)) // outstanding * (1 + interest)
              ),
              loan_id
            )

            // The UTxO must have a Borrower ID. This would symbolize the loan is active. The
            // Active beacon would also be present. Requirement 2.
            trace_if_false(
              @"UTxO missing BorrowerID",
              value.quantity_of(utxo_value,sym,borrower_id) == 1
            ) &&

            // The loan must not be expired. Requirement 3.
            trace_if_false(
              @"Loan is expired",
              rollover_time <= expiration
            ) &&

            // The address' staking credential must signal approval. Requirement 4.
            trace_if_false(
              @"Borrower did not approve",
              utils.staking_credential_approves(stake,wdrw,sigs)
            ) &&

            // The deposit_increase must be >= 0.
            trace_if_false(
              @"Int not >= 0",
              deposit_increase >= 0
            ) &&

            // There must be an output to this address with:
              // An updated datum:
                // 1) The loan outstanding must acrue interest.
                // 2) The last_checkpoint must be updated.
              // The exact same value as the input.
            // Requirement 6.
            utils.update_loan_check(addr,utxo_value,deposit_increase,expected_datum,outputs)
          }

          _ -> error @"Datum is not an ActiveDatum"
        }
      }

      ClaimExpired -> {

        // This redeemer allows anyone with the Key NFT to claim the collateral of expired loans.
        
        // This redeemer only respects Key NFTs located OUTSIDE the dapp address and Lock NFTs
        // stored with an Active beacon.
        
        // The invalid-before flag is used as a proxy for the current time.
        
        // To successfully claim the collateral for an expired loan, all of the following must be true:
        // 1) The input must have an ActiveDatum.
        // 2) The input must have a BorrowerID.
        // 3) The loan must be expired.
        // 4) There must be 1 LoanID among the tx inputs that is NOT located at a dapp address.
        // 5) All of the beacons among the tx inputs must be burned.
        
        // The BorrowerID is required so that only the borrower can claim the minUTxOValue deposit
        // that was stored with the Active UTxO. This deposit belongs to the borrower. Finished
        // loans should not be claimable with the Key NFT for this reason.
        
        // Since the Key NFT is actually being moved in this transaction (the UTxO is being
        // consumed), this can be used as a proxy for the lender's approval.
        
        // The lender is able to claim the collateral for multiple expired loans in a given
        // transaction as long as they control all the required Key NFTs.
        
        // All loans claimed with this redeemer will count as defaults against the borrower.

        // The datum must be an ActiveDatum. Requirement 1.
        when datum is {
          ActiveDatum(
            sym,
            borrower_id,
            _,
            loan_asset,
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
            _,
            loan_id
          ) -> {
            expect (
              Some(Input(_,Output(_,utxo_value,..))),
              total_value,
              key_present
            ) = utils.find_input_and_info(inputs,out_ref,sym,loan_id)

            let claim_time = utils.get_invalid_before(validity_range)
            let asset_name = utils.generate_beacon_name(loan_asset)
            let mint_as_val = value.from_minted_value(mint)

            // The input must have a BorrowerID. Requirement 2.
            trace_if_false(
              @"Input missing BorrowerID",
              value.quantity_of(utxo_value,sym,borrower_id) == 1
            ) &&

            // The loan must be expired. Requirement 3.
            trace_if_false(
              @"Loan is not expired",
              claim_time > expiration
            ) &&

            // There must be 1 LoanID among the tx inputs that is NOT located at a dapp address.
            // Requirement 4.
            trace_if_false(
              @"Key NFT not found",
              key_present
            ) &&
            
            // All beacons among inputs must be burned. Requirement 5.
            trace_if_false(
              @"Not all beacons burned",
              // Active beacons burned.
              value.quantity_of(total_value,sym,"Active") ==
                -value.quantity_of(mint_as_val,sym,"Active") &&
              // BorrowerIDs burned.
              value.quantity_of(total_value,sym,borrower_id) ==
                -value.quantity_of(mint_as_val,sym,borrower_id) &&
              // LoanIDs burned.
              value.quantity_of(total_value,sym,loan_id) ==
                -value.quantity_of(mint_as_val,sym,loan_id) &&
              // Asset beacons burned.
              value.quantity_of(total_value,sym,asset_name) ==
                -value.quantity_of(mint_as_val,sym,asset_name)
            )
          }

          _ -> error @"Datum is not an ActiveDatum"
        }
      }

      UpdateLenderAddress(new_address,deposit_increase) -> {

        // Since the Key NFT can be traded among lenders, there needs to be a way for the new owner
        // of the Key NFT to update the lender_address field of the corresponding loan. That is the
        // purpose of this redeemer.
        
        // This redeemer only respects Key NFTs located OUTSIDE the dapp address and
        // Lock NFTs stored with an Active beacon.
        
        // To successfully update the loan_address for an active loan, all of the following must be
        // true:
        // 1) The input must have an ActiveDatum.
        // 2) The input must have a BorrowerID.
        // 3) There must be 1 LoanID among the tx inputs that is NOT located at a dapp address.
        // 4) The new address must use either the pubkey or the proxy script for the payment
        //    credential. If the proxy script is used, the address must have a staking credential.
        // 5) The deposit_increase must be >= 0.
        // 6) There must be an Active UTxO output to this address with the exact same value and
        //    proper datum, same as input datum except for the new lender_address. The address
        //    in the datum must be the same address in the redeemer. The value must be the same
        //    as the input value + the amount of lovelace specified in the redeemer.
        
        // Since the datum can grow with this redeemer, the minUTxOValue requirement can increase.
        // This requirement can also increase due to parameter changes. Therefore, requiring the
        // value of the new UTxO to be exactly the same as the input could be too strict in some
        // situations. To relax this restriction slightly, the output UTxO can have the input value
        // + the amount of ADA specified by the redeemer. IT MUST BE EXACTLY THIS AMOUNT OF ADA.

        // The dominant use case for this redeemer is when a Key NFT is purchased on the secondary
        // market and the new owner needs to change the address. This redeemer therefore prioritizes
        // composing the purchase of the Key NFT with the changing of the lender address. In this
        // scenario, the Key NFT would be "moving" to a new UTxO which means it would be found in
        // the transaction inputs, as opposed to the reference inputs.
        
        // The datum must be an ActiveDatum. Requirement 1.
        when datum is {
          ActiveDatum(
            sym,
            borrower_id,
            _,
            loan_asset,
            principle,
            rollover_frequency,
            last_checkpoint,
            term,
            interest,
            min_payment,
            collateralization,
            swappable,
            claim,
            expiration,
            outstanding,
            loan_id
          ) -> {
            expect (
              Some(Input(_,Output(addr,utxo_value,..))),
              _,
              key_present
            ) = utils.find_input_and_info(inputs,out_ref,sym,loan_id)

            let expected_datum = ActiveDatum(
              sym,
              borrower_id,
              new_address,
              loan_asset,
              principle,
              rollover_frequency,
              last_checkpoint,
              term,
              interest,
              min_payment,
              collateralization,
              swappable,
              claim,
              expiration,
              outstanding,
              loan_id
            )

            // The input must have a BorrowerID. Requirement 2.
            trace_if_false(
              @"Input missing BorrowerID",
              value.quantity_of(utxo_value,sym,borrower_id) == 1
            ) &&

            // There must be 1 LoanID among the tx inputs that is NOT located at a dapp address.
            // Requirement 3.
            trace_if_false(
              @"Key NFT not found",
              key_present
            ) &&

            // The new address uses a pubkey or the proxy script for the payment credential.
            // Requirement 4.
            trace_if_false(
              @"Invalid new address",
              utils.valid_lender_address(proxy_hash,new_address)
            ) &&

            // The deposit_increase increase must be >= 0. Requirement 5.
            trace_if_false(
              @"Int not >= 0",
              deposit_increase >= 0
            ) &&

            // There must be the proper output to this address. Requirement 6.
            utils.update_loan_check(addr,utxo_value,deposit_increase,expected_datum,outputs)
          }

          _ -> error @"Datum is not an ActiveDatum"
        }
      }

      Unlock -> {

        // This redeemer has three purposes:
        // 1) Spend invalid Active UTxOs (ie, UTxO with an ActiveDatum but missing an Active Beacon).
        //    None of the other redeemers allow spending invalid ActiveDatums. One is required to
        //    prevent permanent locking in case of accidents. 
        // 2) Unlock expired collateral if the Key NFT is lost. Since the Key NFT can be 
        //    accidentally lost forever, it would be possible for the corresponding collateral to 
        //    also be lost forever without an escape hatch. This redeemer can be used by the 
        //    borrower to claim lost collateral but only after the agreed upon claim period has 
        //    passed. Any collateral claimed with this redeemer will still count as a default 
        //    against the borrower.
        // 3) Clean up finished Active UTxOs since not all of the beacons could be burned during the
        // full payment.
        
        // The invalid-before flag is used as a proxy for the current time.
         
        // To successfully spend Active UTxOs with this redeemer, all of the following must be true:
        // 1) The input must have an ActiveDatum.
        // 2) The address' staking credential (aka the borrower) must signal approval.
        // 3) All beacons among the tx inputs must be burned.
        // 4) If the BorrowerID is present in this input:
        //      a) The claim period must have passed.
        //    Otherwise, conditions have already been satisfied: borrower's approval and beacons 
        //    burned.
        
        expect Some(
          Input(
            _,
            Output(
              Address(ScriptCredential(_),stake),
              utxo_value,
              ..))) = tx.find_input(inputs,out_ref)

        // The datum must be an ActiveDatum. Requirement 1.
        when datum is {
          ActiveDatum(
            sym,
            borrower_id,
            _,
            loan_asset,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            claim,
            _,
            _,
            loan_id
          ) -> {
            let asset_name = utils.generate_beacon_name(loan_asset)
            let total_value = utils.total_input_value(inputs)
            let mint_as_val = value.from_minted_value(mint)
            
            // The borrower must approve. Requirement 2.
            trace_if_false( 
              @"Borrower did not approve",
              utils.staking_credential_approves(stake,wdrw,sigs)
            ) &&

            // All beacons among inputs must be burned. Requirement 3.
            trace_if_false(
              @"Not all beacons burned",
              // Active beacons burned.
              value.quantity_of(total_value,sym,"Active") ==
                -value.quantity_of(mint_as_val,sym,"Active") &&
              // BorrowerIDs burned.
              value.quantity_of(total_value,sym,borrower_id) ==
                -value.quantity_of(mint_as_val,sym,borrower_id) &&
              // LoanIDs burned.
              value.quantity_of(total_value,sym,loan_id) ==
                -value.quantity_of(mint_as_val,sym,loan_id) &&
              // Asset beacons burned.
              value.quantity_of(total_value,sym,asset_name) ==
                -value.quantity_of(mint_as_val,sym,asset_name)
            ) &&

            // If the BorrowerID is present in the input, the claim period must have
            // passed. Requirement 4.
            trace_if_false(
              @"Claim period has not passed",
              if value.quantity_of(utxo_value,sym,borrower_id) == 1 {
                let unlock_time = utils.get_invalid_before(validity_range)
                unlock_time > claim
              } else {
                True
              }
            )
          }

          _ -> error @"Datum is not an ActiveDatum"
        }
      }
    }
  }
}

validator(proxy_hash: Hash<Blake2b_224,Script>, dapp_hash: Hash<Blake2b_224, Script>) {
  fn mint(redeemer: BeaconRedeemer, ctx: ScriptContext) -> Bool {
    when redeemer is {
      CreateAsk(borrower_cred, loan_assets) -> {

        // An Ask UTxO is used to broadcast the borrower's wish to borrow an asset. The Ask UTxO
        // must be valid so that only valid asks are ever broadcast to potential lenders.
        
        // An Ask UTxO has two beacons:
        //  - An Ask beacon that signifies that this UTxO is a borrower asking for a loan.
        //  - An Asset beacon that signifies what asset is to be borrowed.
        
        // Using these beacons, two queries are possible:
        //  - All asks, no matter what asset will be loaned out.
        //  - All asks for a specific loan asset.
        
        // The datum attached to the Ask UTxO details what terms the borrower is looking for.
        // All datums are inline datums that way other users can easily check the terms.
        
        // This redeemer allows creating multiple Ask UTxOs in a single transaction. They do not need
        // to be for the same loan asset. THIS REDEEMER IS THE ONLY WAY TO CREATE A VALID ASK UTXO.
        
        // Only the borrower is allowed to create an Ask UTxO at their dapp address. This prevents
        // identity theft.
         
        // For an Ask UTxO to successfully be created, all of the following must be true:
        // 1) Only the Ask beacon and Asset beacons can be minted with this redeemer. All assets
        //    whose Asset beacon will be minted must be found in the redeemer.
        // 2) The Ask beacon must have the token name "Ask".
        // 3) The Asset beacon must have the token name: 
        //      sha2_256( "Asset" ++ asset_policy_id ++ asset_name ).
        // 4) Each Ask UTxO must have exactly 1 Ask beacon and 1 Asset beacon. No other beacons
        //    can be found in the UTxO.
        // 5) Each Ask UTxO must be locked at a dapp address that is using the credential in the 
        //    redeemer as the staking credential.
        // 6) The Ask UTxO must have a valid inline AskDatum:
        //  - loan_beacon_sym == this policy id.
        //  - borrower_id == credential in redeemer as a token name.
        //  - loan_asset == asset in redeemer that corresponds to the Asset beacon stored in the 
        //    UTxO and the loan asset cannot be one of the protocol's beacons.
        //  - loan_principle > 0.
        //  - loan_term > 0.
        //  - collateral list must not be empty.
        // 7) The receiving staking credential must signal approval.
        
        // The prefix of "Asset" when creating the name for the Asset beacon is used to ensure
        // that Asset beacons can never be counterfeit LoanIDs. Since the Ask UTxO does not
        // require the loan asset to be stored in the UTxO, there is no way for the policy
        // to know whether or not the loan asset is a real asset. What would happen if the
        // "asset" passed in with the redeemer is actually an OutputReference for a UTxO? This
        // could possibly result in a counterfeit LoanID being created. By adding this prefix,
        // the generation of the Asset beacon name will always be different than the generation
        // of the LoanID. This means it is not possible to create counterfeit LoanIDs despite
        // the policy not knowing whether or not the asset is real. It is important to note
        // that there are other ways of dealing with counterfeit LoanIDs but this method seems
        // to have the best trade-off profile.

        // The fact that the borrower must approve the transaction means nobody but the borrower can
        // create an Ask under that credential.
        
        expect ScriptContext( 
          Transaction(_,_,outputs,_,mint,_,wdrw,_,sigs,..), 
          tx.Mint(policy_id) 
        ) = ctx

        let beacon_names = list.map(loan_assets,utils.generate_beacon_name)

        // This function checks requirements 1-3.
        utils.mint_check( policy_id, beacon_names, redeemer, mint ) &&

        // This function checks requirements 3-6.
        utils.destination_check(
          proxy_hash,
          dapp_hash,
          policy_id,
          beacon_names,
          loan_assets,
          redeemer, 
          outputs
        ) &&

        // This function checks requirement 7.
        trace_if_false( 
          @"Borrower did not approve", 
          utils.staking_credential_approves(Some(Inline(borrower_cred)),wdrw,sigs)
        )
      }

      CreateOffer(lender_cred, loan_assets) -> {

        // An Offer UTxO is used to broadcast to borrowers that a lender is willing to give them
        // a loan. The Offer UTxOs must be valid (i.e., have the proper beacons) so that the
        // borrower will only be able to accept offers that have been deemed valid by the protocol.
        
        // The borrower will get custody of any Offer UTxOs that are missing the proper beacons.
        
        // An Offer UTxO has three beacons:
        //  - An Offer beacon that signifies this UTxO is a loan offer.
        //  - A LenderID that signifies what lender the offer is from.
        //  - An Asset beacon that signifies what asset is to be loaned out.

        // Queries can be made using any combination of the above beacons. The main queries are:
        //  - All offers made to the borrower.
        //  - All offers that belong to a specific lender.
        //  - All offers made for a specific asset.
        
        // The datum attached to the Offer UTxO has the terms that the lender is offering to the
        // borrower. It is an inline datum for easy access by the borrower.
        
        // This redeemer allows creating multiple Offer UTxOs in a single transaction. They do
        // not need to be for the same loan asset. THIS REDEEMER IS THE ONLY WAY TO CREATE A VALID
        // OFFER UTXO.
        
        // For an Offer UTxO to successfully be created, all of the following must be true:
        // 1) Only the Offer beacon, LenderID, and Asset beacons can be minted with this redeemer.
        //    All assets whose Asset beacons will be minted must be found in the redeemer.
        // 2) The Offer beacon must have the token name "Offer".
        // 3) The LenderID must use the credential in the redeemer as the token name prefixed
        //    with either "00" or "01" if the credential is a pubkey or script, respectively.
        //    If a script is used as the credential, it must be a staking script.
        // 4) The Asset beacon must have the token name: 
        //      sha2_256( "Asset" ++ asset_policy_id ++ asset_name ).
        // 5) The Offer UTxO must have exactly 1 Offer beacon, 1 LenderID, 1 Asset beacon, the loan
        //    amount, and the amount of ADA specified in in the offer_deposit field of the datum.
        //    No other assets are allowed.
        // 6) The Offer UTxO must be locked at a dapp address with a staking credential.
        // 7) The Offer UTxO must have a valid inline OfferDatum:
        //  - loan_beacon_sym == this policy id.
        //  - lender_id == credential in redeemer as token name with the proper prefix.
        //  - lender_address must either use the proxy script or a pubkey as the payment credential, 
        //    and if the proxy script is used, the address must have a staking credential.
        //  - loan_asset == asset in redeemer that corresponds to the Asset beacon stored in the
        //    UTxO and the loan asset cannot be one of the protocol's beacons.
        //  - loan_principle > 0.
        //  - rollover_frequency > 0 if used.
        //  - loan_term > 0.
        //  - loan_interest must have a denominator > 0 and the interest must be >= 0.
        //  - min_payment >= 0.
        //  - collateralization list must not be empty, all relative prices must have a
        //    denominator > 0, and all relative prices >= 0.
        //  - claim_period > 0.
        //  - offer_deposit > 0.
        // 8) The lender credential must signal approval.

        // See the proxy-script ADR for an explanation as to why the lender address can only use a
        // proxy script if a payment script is desired.
        
        // The rollover_frequency field is how often interest should be applied to the outstanding
        // balance. The first interest calculation is made upon accepting the loan. If this field is
        // not used (None is specified), then that is the only time interest will be applied - this
        // would be a non-compounding interest loan. 
        
        // Since the `Rational` type assumes all denominators are > 0, the datum fields that use
        // the `Rational` type must explicitly check this. Once the Offer UTxO is created, the user
        // doesn't enter a `Rational` type again for any action, the protocol handles `Rational`
        // types post Offer UTxO creation. Therefore, the only time the `Rational`s need to be
        // checked is here.
        
        // An interest-free loan can be given by setting the interest rate == 0. This protocol does
        // not allow negative interest rates.
        
        // The min_payment field specifies the minimum payment size that the borrower must make with
        // each payment. The borrower can always pay off the remaining balance, even if it is less
        // than the mininum payment. If the lender does not want to enforce a minimum payment, this
        // field can be set to 0. This field offers another way for lenders to manage risk.
        
        // If a lender does not want a certain collateral asset to be used, they can set the
        // relative price for that asset to zero. In a similar fashion, the lender can offer
        // over/under-collateralized loans by setting the relative prices of the collateral
        // to above/below the market price.
        
        // Since it is possible for Key NFTs to be lost, and therefore, it is possible for
        // collateral to be locked forever, the lender must say how much time they need to claim
        // collateral in the case of a default. That is what the claim_period field is for. If the
        // claim_period passes, then the protocol considers the collateral "Lost" and the borrower
        // can unlock it (the default will still appear on the borrower's credit history). This
        // field should have the relative time for the claim period. The protocol will produce the
        // absolute claim period expiration when the offer is accepted.
        
        // The offer_deposit field is how much ADA the lender used to satisfy the minUTxOValue
        // requirement for the Cardano blockchain. It will be returned to the lender when the
        // borrower accepts the offer.

        // The collateral_is_swappable field is just a Bool so it does not need to be checked.
        // If it is not a Bool, the protocol will say the datum is the wrong type. This field
        // allows the borrower to swap out collateral during the loan period. This could be useful
        // to the borrower if the market values of the collateral change during the lifetime of
        // the loan. While this would not be good for the lender, the lender can offer this option
        // to make their offer more competitive than offers from other lenders. Since the lender
        // must explicitly enable this feature, there is no way the borrower can abuse it.

        // Since the lender must approve the transaction, nobody but the lender can create an
        // offer under that credential.
        
        expect ScriptContext( 
          Transaction(_,_,outputs,_,mint,_,wdrw,_,sigs,..), 
          tx.Mint(policy_id) 
        ) = ctx
        
        let beacon_names = list.map(loan_assets,utils.generate_beacon_name)
        
        // This function checks requirements 1-4.
        utils.mint_check( policy_id, beacon_names, redeemer, mint ) &&

        // This function checks requirements 5-7.
        utils.destination_check(
          proxy_hash,
          dapp_hash,
          policy_id,
          beacon_names,
          loan_assets,
          redeemer,
          outputs 
        ) &&

        // This function checks requirement 8.
        trace_if_false( 
          @"Lender did not approve", 
          utils.staking_credential_approves(Some(Inline(lender_cred)),wdrw,sigs)
        )
      }

      CreateActive(borrower_cred, pairs) -> {

        // This redeemer is the heart of the loan acceptance step. THE ONLY WAY TO CREATE A VALID
        // ACTIVE UTXO IS WITH THIS REDEEMER.
        
        // A valid Active UTxO has 4 beacons:
        //  - 1 Active beacon
        //  - 1 BorrowerID
        //  - 1 Asset beacon
        //  - 1 LoanID (Lock NFT)

        // These beacons allow for very expressive queries. For example:
        //  - All open loans for a specific borrower.
        //  - All open loans no matter the loan asset.
        //  - All open loans for a specific loan asset.
        //  - The corresponding loan for a specific Key NFT.
        
        // As alluded to already, this protocol uses a Lock/Key NFT mechanism for handling loans.
        // They are just two copies of the LoanID: one gets stored with the collateral (Lock NFT)
        // while the other is freely tradable between lenders (Key NFT). The LoanID token
        // name is: sha2_256( offer_tx_hash ++ offer_output_index ). It is not prefixed with 
        // anything. This makes the tokens generated as Asset beacons always distinct from tokens
        // generated as LoanIDs.
        
        // Since it does not make sense for two borrowers to share a transaction for accepting
        // loans, this redeemer requires that the transaction is dedicated to a single borrower. The
        // transaction can still be used to accept multiple loans; as long as they all belong to a
        // single borrower.
        
        // When a loan is accepted, the first interest calculation is applied to the loan. This
        // allows for non-compounding interest, as well as compounding interest.
        
        // This redeemer relies on the invalid-before flag as a proxy for the current time.

        // At a high-level, accepting a loan involves consuming an Ask UTxO and an Offer UTxO
        // to produce one Active UTxO.
        
        // To accept loans, all of the following must be true:
        // 1) All Ask UTxOs must have an Ask beacon.
        // 2) All Offer UTxOs must have an Offer beacon.
        // 3) All Ask UTxOs and Offer UTxOs must come from the same address.
        // 4) No other UTxOs from the loan address are present in the transaction.
        // 5) All Ask UTxOs must be paired up with exactly one Offer UTxO and vice, versa.
        // 6) The paired Ask UTxO and Offer UTxO must agree on the terms.
        // 7) The corresponding Active UTxO must have the proper inline ActiveDatum:
        //  - loan_beacon_sym == this policy id.
        //  - borrower_id == this borrower's staking credential as a token name.
        //  - lender_address == lender_address from Offer UTxO.
        //  - loan_asset == loan_asset from Offer UTxO.
        //  - loan_principle == loan_principle from Offer UTxO.
        //  - rollover_frequency == rollover_frequency from Offer UTxO.
        //  - last_checkpoint == invalid-before of this tx
        //  - loan_term == loan_term from Offer UTxO.
        //  - loan_interest == loan_interest from Offer UTxO.
        //  - min_payment == min_payment from Offer UTxO.
        //  - collateralization == collateralization from Offer UTxO.
        //  - collateral_is_swappable == collateral_is_swappable from Offer UTxO.
        //  - claim_expiration == 
        //      invalid-before of this tx + 
        //      loan_term from Offer UTxO + 
        //      claim_period from Offer UTxO.
        //  - loan_expiration == invalid-before of this tx + loan_term from Offer UTxO.
        //  - loan_outstanding == 
        //      loan_principle from Offer UTxO * (1 + loan_interest from Offer UTxO)
        //  - loan_id == sha2_256( offer_tx_hash ++ offer_output_index ) as token name.
        // 8) The corresponding Active UTxO must have the proper value:
        //  - 1 Active beacon
        //  - 1 BorrowerID
        //  - 1 Asset beacon that corresponds to the loan_asset in the ActiveDatum.
        //  - 1 LoanID that corresponds to the loan_id in the ActiveDatum.
        //  - The proper amount of collateral.
        // 9) The lender must be paid their Offer UTxO deposit (offer_deposit in the OfferDatum) and
        //    1 LoanID that corresponds to the loan. The UTxO must include a payment datum since
        //    the output could be to the proxy script. The payment datum is (beacon_sym,"Accepted").
        // 10) The beacons minted must exactly match what the policy expects:
        //  - All Ask beacons burned.
        //  - All Offer beacons burned.
        //  - All LenderIDs burned.
        //  - All Asset beacons from Offer UTxOs must be burned (the Ask UTxO's is transferred to
        //    the Active UTxO).
        //  - 1 Active beacon must be minted for every loan accepted.
        //  - 1 BorrowerID must be minted for every loan accepted.
        //  - 2 LoanIDs must be minted for every loan accepted (a unique pair per loan).
        // 11) The borrower must approve of the transaction.
        
        // To ensure that the `AcceptLoan` spending redeemer cannot be used to spend invalid UTxOs,
        // this redeemer must fail the transaction whenever any are present among the inputs.
        
        // The LoanID is the sha2_256 hash of the Offer UTxO's tx hash concatenated with its
        // output reference. Since the output references for UTxOs are guaranteed to be unique,
        // every loan is guaranteed to get a unique LoanID. Since the pre-hash string is not
        // prefixed with "Asset", LoanIDs will always be distinct from Asset beacons. This prevents
        // the creation of counterfeit LoanIDs with the `CreateAsk` redeemer.
        
        // Since the proxy payment script can be used for the lender's address, the Key NFT and
        // offer deposit must be sent to the lender with a datum. The datum does not matter since
        // the proxy script can be used with any datum and the Key NFT ensures all outputs to the
        // address in this transaction are unique. For convenience of the logic for this script, the
        // datum will just be (PolicyId,AssetName) where the policy id is the beacon minting policy
        // id and the asset name will be "Accepted".
        
        // Since this redeemer requires an exact match for the minting, it cannot be composed
        // with the other policy redeemers.
        
        // Requirement 11 ensures that only the borrower can open a loan under their credential.
        
        expect ScriptContext( 
          Transaction(inputs,_,outputs,_,mint,_,wdrw,validity_range,sigs,..), 
          tx.Mint(policy_id) 
        ) = ctx
        
        // Get the start time from invalid-before.
        let start_time = utils.get_invalid_before(validity_range)

        // Get the information from the inputs. This function checks requirements 1-6.
        let (pay_map,pay_size,loan_map,loan_size,req_mint) = utils.acceptance_input_checks(
          borrower_cred,
          inputs,
          policy_id,
          dapp_hash,
          start_time,
          pairs
        )

        // Check that the outputs satisfy the inputs. This will provide its own error messages.
        // This checks requirements 7-9.
        utils.acceptance_output_checks(
          borrower_cred,
          policy_id,
          dapp_hash,
          pay_map,
          pay_size,
          loan_map,
          loan_size,
          outputs
        ) &&

        // Check if the proper beacons were minted/burned. This checks requirement 10.
        trace_if_false(
          @"Wrong beacons minted/burned",
          value.tokens(req_mint,policy_id) == value.tokens(value.from_minted_value(mint),policy_id)
        ) &&

        // Check if the borrower approved the transaction. This checks requirement 11.
        trace_if_false(
          @"Borrower did not approve",
          utils.staking_credential_approves(Some(Inline(borrower_cred)),wdrw,sigs)
        )
      }

      BurnBeacons -> {
        // This redeemer will always succeed as long as it is only used to burn beacons.

        expect ScriptContext( 
          Transaction(_,_,_,_,mint,..), 
          tx.Mint(policy_id) 
        ) = ctx

        utils.mint_check( policy_id, [], redeemer, mint )
      }
    }
  }
}

// There are a few edge case scenarios that need to be mentioned:

// 1) It is technically possible to create an Asset beacon for something that is not an asset by
// using the `CreateAsk` redeemer. For example, if the supplied policy id was a tx hash and the
// supplied asset name was an output index, it would be possible to mint a beacon that is a
// counterfeit LoanID. However, due to the way this protocol works, any LoanIDs minted this way will
// always be different than actual LoanIDs. This means the protocol does not need to guard against
// counterfeit LoanIDs.

// 2) The borrower is their own lender and they use the same credential. Despite using the same
// credential, the LenderID and BorrowerID will be different tokens since LenderIDs are the
// credential prefixed with either "00" or "01". Therefore, all of the rules governing the tokens
// still apply. While the protocol allows the borrower to be their own lender, the borrower still
// cannot fake a full payment. They must still repay the loan truthfully despite being their own
// lender. This approach can be used by the borrower to build up good credit history. This is not a
// bad thing - traditional finance has similar mechanisms where new borrowers can borrower money
// from themselves for the sole reason of building up a credit history. Since most lenders will
// likely require several years of good credit to qualify for under-collateralized loans, there is a
// huge incentive for borrowers to keep the credit history, even if they "gamed" it in the
// beginning. The dilemma is basically between: 
//   - Build up several years of good credit for a few big scores of ripping off
//     a lender that gave you an under-collateralized loan. 
//   - Build up several years of good credit for lifetime access to under-collateralized loans.
// If the borrower chooses the former, other lenders will see that they did this and will not
// offer them under-collateralized loans going forward. If the borrower chooses the latter, they
// can retain access to under-collateralized loans now and in the future. The economic benefits
// of under-collaterized loans strongly incentivize borrowers to pick the second option. The
// fact that this protocol uses psuedonymous identities does not alter the game theory.

// 3) The borrower is their own lender and they use different credentials. There is no difference
// between this scenario and the previous one since the protocol treats the IDs separately
// anyway.
