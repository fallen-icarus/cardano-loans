use aiken/hash.{
    Blake2b_224, 
    Hash
  }
use aiken/dict
use aiken/transaction.{
    Input,
    Output,
    Transaction,
    ScriptContext,
    Mint,
  } as tx
use aiken/transaction/credential.{
    Address, 
    Inline,
    Script,
    ScriptCredential
  }
use aiken/transaction/certificate.{
    CredentialRegistration
  }
use aiken/transaction/value.{PolicyId} as value

use cardano_loans/data/core
use cardano_loans/data/datums.{
    LoanDatum,
    AskDatum,
    OfferDatum,
    ActiveDatum
  }
use cardano_loans/data/redeemers.{
    NegotiationBeaconsRedeemer,
    CreateCloseOrUpdateAsk, 
    CreateCloseOrUpdateOffer, 
    BurnNegotiationBeacons,
    RegisterNegotiationScript,

    ActiveBeaconsRedeemer,
    CreateActive, 
    BurnKeyAndClaimExpired,
    BurnRemainderOrUnlockLost,
    BurnActiveBeacons,

    PaymentObserverRedeemer,
    ObservePayment, 
    RegisterPaymentObserverScript,

    LoanRedeemer, 
    CloseOrUpdateAsk, 
    CloseOrUpdateOffer, 
    AcceptOffer, 
    MakePayment, 
    Rollover, 
    SpendWithKeyNFT, 
    UpdateLenderAddress, 
    Unlock,
  }
use cardano_loans/utils.{
    trace_if_false,
  }
use cardano_loans/ask_validation as ask
use cardano_loans/offer_validation as offer
use cardano_loans/accept_validation as accept
use cardano_loans/payment_validation as payment
use cardano_loans/observe_payment
use cardano_loans/rollover_validation as rollover
use cardano_loans/update_address_validation as update_address
use cardano_loans/burn_and_claim_validation as burn_and_claim
use cardano_loans/burn_and_unlock_validation as burn_and_unlock

validator { 
  fn proxy_script(_datum: Data, _redeemer: Data, ctx: ScriptContext) -> Bool {
    // Since using the datums to guarantee uniqueness of loan payments is currently the cheapest
    // option, all lender addresses must be able to accept this datum. In the future, it may
    // be possible to cheaply use another method to guarantee payment uniqueness, but for now, all 
    // lender addresses must be able to accept UTxOs with the datum enforced by the protocol. If
    // a payment is made to a script address that cannot use the datum, the payment output would
    // be locked forever. To make matters worse, borrowers are incentivized to deliberately
    // lock payment outputs, if possible, because it decreases the circulating supply of the locked
    // assets.
    //
    // One option would be to take a hands-off approach, and allow lender's to specify addresses 
    // with any arbitrary payment script. It would be up to the lender to ensure that the script 
    // can accept the required datum. However, mistakes happen and these are financial transactions 
    // so safe guards should be used whenever possible.
    //
    // Another option is to disallow plutus scripts, entirely. Unfortunately, since a plutus script 
    // cannot distinguish between a native script and a plutus script, the only way to actually do 
    // this is to disallow all payment scripts altogether, including native scripts. Not allowing 
    // any payment script would prevent the use of multisig - a desirable feature for pooled 
    // lenders and likely a requirement for adoption by corporations. Therefore, this option is too 
    // restrictive.
    //
    // A different approach would be to allow the lenders to set up a personal, "pre-approved" proxy
    // script address that can have arbitrary logic for what to do next. This proxy script can 
    // accept any datum and redeemer. The proxy payment script would simply delegate spending 
    // authority to the staking credential for the proxy address. This has a few benefits: 1) There 
    // can be a single payment script hash that the loan validator can look for which makes the 
    // check cheap. 2) Since all users can still get the same spending script for the DApp, beacon
    // tokens can still be used. 3) The proxy script can still have arbitrary logic since it can 
    // accept any datum and redeemer, and just delegates to the staking script, which can be a 
    // pubkey, native script, or plutus script. 4) The datum in loan payment outputs can still be 
    // determined by the loan validator which is more convenient than using a receipt token. This 
    // approach still allows lenders to use multisig despite being strict on the type of datum used 
    // for loan payments. This is the approach that will be taken until it is possible to use 
    // another method to guarantee uniqueness. 
    //
    // This proxy validator is the "pre-approved" scripts. It is completely general and can be used
    // with any DApp that requires a specific datum output. This means all users can use a single
    // address while accepting payments from many different DApps.
    //
    // It is important to note that a staking script can be executed by withdrawing 0 ADA from the
    // rewards address. In order to use this method, the rewards address must be
    // registered.

    expect ScriptContext(Transaction(inputs,_,_,_,_,_,wdrw,_,sigs,..),tx.Spend(out_ref)) = ctx
    expect Some(Input(_,Output(Address(_,stake),..))) = tx.find_input(inputs,out_ref)

    trace_if_false(
      @"Staking credential did not approve", 
      utils.staking_credential_approves(stake,wdrw,sigs)
    )
  }
}

// The loan validator script.
validator(proxy_hash: Hash<Blake2b_224, Script>) {
  fn loan_script(datum: LoanDatum, redeemer: LoanRedeemer, ctx: ScriptContext) -> Bool {
    expect ScriptContext(
      Transaction(inputs,_,outputs,_,mint,_,wdrw,validity_range,sigs,redeemers,..), 
      tx.Spend(out_ref)
    ) = ctx

    when redeemer is {
      CloseOrUpdateAsk -> {
        // This redeemer is dedicated to closing any Ask UTxOs, valid or invalid (i.e., UTxOs with
        // an AskDatum but without the proper beacons). The borrower has custody of both valid and
        // invalid Ask UTxOs.
        //
        // In order to spend a UTxO with this redeemer, all of the following must be true:
        // 1) The input being spent must have an AskDatum. 
        // 2) The negotiation beacon script must be executed with CreateOrCloseAsk(borrower_cred) 
        //    where the borrower_cred is the staking credential of this address.
        //
        // To optimize the performance of this redeemer, most of the checks are delegated to
        // the negotiation beacon script since beacons must be burned anyway. To ensure the 
        // negotiation beacon script is doing the right checks for this redeemer, the beacon script 
        // must be executed with the CreateOrCloseAsk redeemer. This is the only check that needs to 
        // be done by this redeemer since the beacon script will also check that the borrower 
        // approved.

        expect Some( Input( _, Output( Address(_,stake), ..))) = tx.find_input(inputs,out_ref)

        // Get the negotiation beacon id from the ask datum.
        let negotiation_beacon_id = when datum is {
          AskDatum(negotiation_beacon_id,..) -> core.from_negotiation_id(negotiation_beacon_id)
          _ -> error @"UTxO is not an Ask UTxO"
        }

        when stake is {
          Some(Inline(stake_cred)) -> {
            // The negotiation beacon script must be executed using CreateOrCloseAsk(stake_cred).
            let req_redeemer: Data = CreateCloseOrUpdateAsk(stake_cred) // Cast to Data type.
            trace_if_false(
              @"Negotiation beacon script not executed with proper redeemer",
              utils.beacon_script_executed(redeemers,req_redeemer,negotiation_beacon_id)
            )
          }

          // This address cannot possibly have beacons. This is just to prevent permanent locking.
          _ -> True
        }
      }
       
      CloseOrUpdateOffer -> {
        // This redeemer is dedicated to closing any Offer UTxOs, valid or invalid (i.e., UTxOs with
        // an OfferDatum but without the proper beacons). The lender has custody of valid Offer
        // UTxOs and the borrower has custody of invalid Offer UTxOs.
        //
        // In order for this redeemer to successfully execute, all of the following must be true:
        // 1) The input must have an OfferDatum.
        // 2) If the input has an Offer beacon, the negotiation beacon script must be executed 
        //    with the CreateOrCloseOffer(lender_id_minus_prefix) redeemer. Otherwise, the 
        //    address' staking credential must signal approval.
        //
        // To optimize the performance of this redeemer, most of the checks are delegated to
        // the beacon script since beacons must be burned anyway. To ensure the beacon script
        // is doing the right checks for this redeemer, the beacon script must be executed with
        // the CreateOrCloseOffer redeemer. 

        expect Some( Input( _, Output( Address(_,stake), val, ..))) = tx.find_input(inputs,out_ref)

        // Get the beacon id and lender id from the offer datum.
        let (negotiation_beacon_id,lender_id) = when datum is {
          OfferDatum(negotiation_beacon_id,_,lender_id,..) -> 
            (core.from_negotiation_id(negotiation_beacon_id),lender_id)
          _ -> error @"UTxO is not an Offer UTxO"
        }

        
        if value.quantity_of(val,negotiation_beacon_id,"Offer") > 0 {
          // If the Offer beacon is present, the lender has custody and the logic must be
          // delegated to the beacon script with the CreateOrCloseOffer(lender_id_minus_prefix)
          // redeemer.
          
          let req_redeemer: Data = // Cast to Data type.
            CreateCloseOrUpdateOffer(core.lender_id_to_credential(lender_id))
          trace_if_false(
            @"Negotiation beacon script not executed with proper redeemer",
            utils.beacon_script_executed(redeemers,req_redeemer,negotiation_beacon_id)
          )
        } else {
          // Otherwise, the borrower has custody.
          trace_if_false( 
            @"Borrower did not approve",
            utils.staking_credential_approves(stake,wdrw,sigs)
          )
        }
      }

      AcceptOffer -> {
        // This redeemer is dedicated to accepting any loan offers. Since accepting a loan requires 
        // spending both an Ask UTxO and an Offer UTxO, having the spending script do the checks 
        // would result in a lot of expensive redundant executions. Instead, the active beacon 
        // script will do the checks. The spending script just needs to ensure the active beacon 
        // script is executed properly.
        //
        // In order for this redeemer to successfully execute, all of the following must be true:
        // 1) The input must have either an OfferDatum or an AskDatum.
        // 2) The active beacon script must be executed using the 
        //    CreateActive(borrower_cred,negotiation_beacon_id) redeemer where the borrower_cred is 
        //    this address's staking credential and the negotiation_beacon_id is the 
        //    negotiation_beacon_id from the input's datum.
        //
        // Key security assumptions:
        // 1) The beacon script can only be executed once in a given transaction, and that 
        //    execution must be dedicated to a specific redeemer. 
        // 2) The beacon script will crash if any UTxOs come from a different loan address than
        //    this one.
        expect Some( Input( _, Output( Address(_,stake), ..))) = tx.find_input(inputs,out_ref)

        // Get the beacon id from the ask or offer datum.
        let (negotiation_beacon_id,active_beacon_id) = when datum is {
          AskDatum(negotiation_beacon_id,active_beacon_id,..) -> 
            (negotiation_beacon_id,core.from_active_id(active_beacon_id))
          OfferDatum(negotiation_beacon_id,active_beacon_id,..) ->
            (negotiation_beacon_id,core.from_active_id(active_beacon_id))
          _ -> error @"UTxO is not an Ask or Offer UTxO"
        }

        when stake is {
          Some(Inline(stake_cred)) -> {
            // The beacon script must be executed using CreateActive(stake_cred).
            let req_redeemer: Data = // Cast to Data type.
              CreateActive(stake_cred,negotiation_beacon_id)
            trace_if_false(
              @"Active beacon script not executed with proper redeemer",
              dict.get(redeemers, Mint(active_beacon_id)) == Some( req_redeemer )
            )
          }

          // This address cannot possibly have beacons. This is just to prevent permanent locking.
          _ -> True
        }
      }

      MakePayment(payment_amount) -> {
        // Make a payment on a loan. Both partial payments and full payments are supported.
        // 
        // In order to allow simple logic and composition, the spending script will do these
        // checks. Using an observer script instead dramatically complicates the logic or
        // simply sacrifices composability. This is due to the global perspective of the observer
        // script. Figuring out which inputs are payments vs other actions (like rollovers) requires
        // cross-referencing the transaction's redeemer list which can easily negate most of the
        // performance gains. Using the redeemer to tell the script which inputs are payments
        // is not better since the script still needs to validate the redeemer list is
        // correct; so it winds up doing the same checks anyway. The simplest method to
        // enable composition is to just have the spending script do the checks.
        //
        // For this redeemer to successfully execute, all of the following must be true:
        // 1) The input must have an ActiveDatum.
        // 2) The input must still have the BorrowerID.
        // 3) The loan must not be expired.
        // 4) The next rollover must not be required.
        // 5) There must be an output to the lender_address with a payment output:
        //    a) The datum must be inline (active_beacon_id,loan_id).
        //    b) It must contain the amount of the loan asset specified in the redeemer.
        // 6) There must be an active output to this address with the proper datum:
        //    a) Exactly the same as the input's datum except the outstanding_balance is
        //       updated to reflect payment.
        // 7) If the remaining balance is paid off:
        //    a) The BorrowerID for this loan must be burned.
        //    b) No other kinds of tokens can be minted/burned in this tx.
        //    c) The Active UTxO must still have the remaining beacons.
        //    d) All collateral is unlocked.
        // 8) Otherwise:
        //    a) The amount paid must be >= min_payment.
        //    b) All of the beacons must still be in the Active UTxO.
        //    c) The proportion of collateral taken must be <= the proportion of the amount
        //       paid. 
        // 9) All BorrowerIDs must be found with Active beacons.
        // 10) The borrower must approve the transaction.
        //
        // This redeemer does not allow "spending" a UTxO from the address; it only allows updating
        // the state of the UTxO. In other words, every input spent with this redeemer must have a
        // corresponding output to this address with the updated state. The LoanIDs are used to link
        // inputs with outputs and prevent double satisfaction.
        //
        // In order to ensure compound interest accrues, the borrower cannot make the next payment
        // if a rollover is due. The script uses the invalid-hereafter flag to assert a time
        // has not passed. For convenience, the borrower can set the invalid-hereafter flag to 
        // the slot when the next rollover is due (or expiration if no more rollovers are required).
        //
        // If the ActiveDatum says the collateral is swappable, the borrower can swap out collateral
        // when making a payment as long as the relative value of the collateral is still valid.
        //
        // In order to allow mixing partial and full payments in a single transaction, this redeemer
        // cannot explicitly check that the proper number of BorrowerIDs are burned. There is no way
        // for a single spending script execution to cheaply know how many BorrowerIDs should be
        // burned. Instead, the script will make it so that any UTxO with a BorrowerID must also
        // have an Active beacon. Since fully repaid loans do not allow BorrowerIDs to be re-output
        // to Active UTxOs and Active UTxO inputs that are already missing a BorrowerID are not
        // allowed in this transaction (this is due to the way all of the spending redeemers work),
        // these BorrowerIDs cannot possibly be re-output to a UTxO with an Active beacon.
        // Therefore, these BorrowerIDs have no choice but to be burned.
        //
        // To account for interest, collateral can only be taken proportional to the amount of
        // the outstanding_balance paid off. This ensures there will always be collateral until
        // the very end of the loan which helps incentivize proper behavior from borrowers.

        expect Some( Input( _, Output( Address(_,stake), ..))) = tx.find_input(inputs,out_ref)

        // Get the observer_hash from the Active datum.
        let observer_hash = when datum is {
          ActiveDatum(_,observer_hash,..) -> observer_hash
          _ -> error @"UTxO is not an Active UTxO"
        }

        // The negotiation beacon script must be executed using CreateOrCloseAsk(stake_cred).
        let req_redeemer: Data = ObservePayment // Cast to Data type.
        trace_if_false(
          @"Observe payment script not executed with proper redeemer",
          utils.beacon_script_executed(redeemers,req_redeemer,observer_hash)
        )
      }
      
      Rollover(deposit_increase) -> {
        // This redeemer is used to have the loan accrue interest. It involves evolving this
        // Active UTxO into an new Active UTxO where everything is the same except for the
        // updated outstanding_balance in the datum. Ada can be deposited in case the minUTxOValue 
        // increased due to a larger datum. The LoanIDs guarantee no double satisfaction occurs
        // during this step.
        //
        // The invalid-hereafter flag is used to prove the loan's expiration has not passed.
        //
        // In order for this redeemer to successfully execute, all of the following must be true:
        // 1) The input must have an ActiveDatum.
        // 2) The loan must not be expired.
        // 3) The BorrowerID must still present.
        // 4) The address' staking credential must signal approval.
        // 5) The deposit_increase must be >= 0.
        // 6) There must be a corresponding Active UTxO output to this address with:
        //    a) The value must be starting value + ada deposit_increase
        //    b) loan_outstanding == outstanding * (1 + loan_interest)
        //    c) last_checkpoint == last_checkpoint + rollover_frequency
        //
        // Technically, the borrower can rollover the loan as many times as they like before the
        // expiration. However, since it means the interest is unnecessarily applied to the balance,
        // they are strongly incentivized NOT to do this. Therefore, this redeemer assumes it is 
        // only executed when it actually needs to be executed.
        //
        // If a rollover is missed (ie, the next rollover is required before the previous one was
        // made), this redeemer can only be used to do one rollover at a time. This is a limitation
        // of the way time works on Cardano; you can assert that some time has or has not passed
        // but you cannot say "how much" time has passed since x. This makes it impossible for
        // the script to tell how many rollovers are required. The `MakePayment` redeemer will know 
        // that an additional rollover is needed so there is no risk in interest not being applied 
        // appropriately.
        //
        // Since this redeemer requires that the UTxO being evaluated is not expired (and therefore,
        // still has a BorrowerID), and since the corresponding output must have the same number of
        // BorrowerIDs as the input, this redeemer cannot interfere with the credit history. It can
        // safely be composed with the `MakePayment` redeemer. If the BorrowerID was not required,
        // this redeemer could be used on finished loans in the same transaction where another loan
        // is paid off. This would add another output with an Active beacon but missing the
        // BorrowerID even though the loan was already finished. In other words, it could result in
        // loans being counted multiple times in the credit history. It is possible to ignore the
        // double counting but this requires more involved data analyses of the credit history. To
        // keep the data analyses simple, the protocol just prevents this scenario in the first
        // place. There is no valid use case for rolling over a finished loan.
        //
        // Because the datum can grow with this redeemer, the minUTxOValue requirement can increase.
        // Parameter changes can also make this requirement grow. Therefore, requiring the value of
        // the new UTxO to be exactly the same as the input could be too strict in some situations.
        // To relax this restriction slightly, the output UTxO can have the input value + the amount
        // of ADA specified by the redeemer. 

        expect Some(Input(_,Output(Address(_,stake) as addr,utxo_value,..))) = 
          tx.find_input(inputs,out_ref)

        and {
          trace_if_false(
            @"Rollover output not found",
            rollover.valid_rollover(
              utxo_value,
              datum, 
              deposit_increase, 
              addr, 
              outputs,
              core.get_end_time(validity_range)
            )
          ),
          trace_if_false( 
            @"Borrower did not approve",
            utils.staking_credential_approves(stake,wdrw,sigs)
          )
        }
      }

      SpendWithKeyNFT -> {
        // Claiming expired Active UTxOs involves burning all beacons attached to the expired
        // UTxOs as well as the Key NFT for each loan. Because of this burning
        // requirement, the logic is outsourced to the beacon script. This redeemer just needs
        // to ensure the beacon script is executed with the proper redeemer.

        // Get the active_beacon_id from the datum.
        let active_beacon_id = when datum is {
          ActiveDatum(active_beacon_id,..) -> core.from_active_id(active_beacon_id)
          _ -> error @"UTxO is not an Active UTxO"
        }

        let req_redeemer: Data = BurnKeyAndClaimExpired
        trace_if_false(
          @"Active beacon script not executed with proper redeemer",
          dict.get(redeemers, Mint(active_beacon_id)) == Some( req_redeemer )
        )
      }

      UpdateLenderAddress(new_address,deposit_increase) -> {
        // Since the Key NFT can be traded among lenders, there needs to be a way for the new owner
        // of the Key NFT to update the lender_address field of the corresponding loan. That is the
        // purpose of this redeemer.
        //
        // As a proxy for the lender's approval, it is enough to check that there were two LoanIDs
        // (both the Lock and the Key NFT) moved during this transaction: one must still be 
        // attached to the collateral UTxO and the other one can go anywhere. The new 
        // lender_address will be set to wherever the Key NFT ends up. The address is set to the
        // destination of the Key NFT because, if it was enough for the Key NFT to just move,
        // the malicious frontends could deliberately choose the UTxO with the Key NFT in another
        // transaction for Alice. Alice's would still own the Key NFT but the malicious frontend
        // could have updated the lender_address for Alice's loan without Alice noticing. By
        // requiring the new address to be the destination of the Key NFT, it is much more likely
        // to notice malicious behavior since it is much easier to detect the withdrawal of assets
        // from an address; the malicious frontend would have to steal the Key NFT to change
        // Alice's lender address. The redeemer is used to tell the script where to expect the Key 
        // NFT; this improves performance.
        //
        // The reason reference inputs are not used for the location of the Key NFT is because
        // the dominant use case for this redeemer is when a Key NFT is purchased on the secondary
        // market, and the new owner needs to change the address. This redeemer therefore 
        // prioritizes composing the purchase of the Key NFT with the changing of the lender 
        // address. In this scenario, the Key NFT would be "moving" to a new UTxO which means it 
        // would be found in the transaction inputs, as opposed to the reference inputs.
        //
        // Because the datum can grow with this redeemer, the minUTxOValue requirement can increase.
        // Parameter changes can also make this requirement grow. Therefore, requiring the value of
        // the new UTxO to be exactly the same as the input could be too strict in some situations.
        // To relax this restriction slightly, the output UTxO can have the input value + the amount
        // of ADA specified by the redeemer. 
        expect Some(Input(_,Output(addr,utxo_value,..))) = 
          tx.find_input(inputs,out_ref)

        trace_if_false(
          @"Update outputs not found",
          update_address.valid_address_update(
            utxo_value,
            deposit_increase, 
            datum, 
            new_address,
            addr, 
            proxy_hash,
            outputs
          )
        )
      }

      Unlock -> {
        // This redeemer has three purposes:
        // 1) Spend invalid Active UTxOs (ie, UTxO with an ActiveDatum but missing an Active Beacon).
        // 2) Unlock expired collateral if the Key NFT is lost. Since the Key NFT can be 
        //    accidentally lost forever, it would be possible for the corresponding collateral to 
        //    also be lost forever without an escape hatch. This redeemer can be used by the 
        //    borrower to claim lost collateral but only after the agreed upon claim period has 
        //    passed. Any collateral claimed with this redeemer will still count as a default 
        //    against the borrower.
        // 3) Clean up finished Active UTxOs since not all of the beacons could be burned during the
        //    full payment.
        //
        // 2 of the 3 purposes involve burning, and the only outlier just requires the borrower's
        // approval. Therefore, this script will look for the borrower's approval, and if the
        // input being spent has a beacon, it will check that the beacon script is executed
        // with the BurnAndUnlock redeemer. The beacon script will check the beacons are properly
        // burned.

        expect Some(Input(_,Output(Address(_,stake),utxo_value,..))) = 
          tx.find_input(inputs,out_ref)

        // Get the active_beacon_id from the datum.
        let active_beacon_id = when datum is {
          ActiveDatum(active_beacon_id,..) -> core.from_active_id(active_beacon_id)
          _ -> error @"UTxO is not an Active UTxO"
        }

        and {
          trace_if_false(
            @"Borrower credential did not approve", 
            utils.staking_credential_approves(stake,wdrw,sigs)
          ),
          if value.tokens(utxo_value,active_beacon_id) |> dict.is_empty(_) {
            True
          } else {
            let req_redeemer: Data = BurnRemainderOrUnlockLost
            trace_if_false(
              @"Active beacon script not executed with proper redeemer",
              dict.get(redeemers, Mint(active_beacon_id)) == Some( req_redeemer )
            )
          }
        }
      }
    }
  }
}

validator( loan_script_hash: Hash<Blake2b_224, Script> ) {
  fn payment_observer_script(redeemer: PaymentObserverRedeemer, ctx: ScriptContext) -> Bool {
    when redeemer is {
      RegisterPaymentObserverScript -> {
        // Only allow registering the script. It cannot be delegated or deregistered.
        expect ScriptContext( _, tx.Publish(CredentialRegistration(_))) = ctx

        True
      }

      ObservePayment -> {
        let ScriptContext( 
          Transaction(inputs,_,outputs,_,mint,_,_,validity_range,_,redeemers,..), 
          purpose
        ) = ctx

        // This redeemer must be used with either a staking execution or a minting execution.
        expect True = utils.error_if_false(
          @"This redeemer must be used with minting or staking execution",
          when purpose is {
            tx.Mint(_) -> True
            tx.WithdrawFrom(_) -> True
            _ -> False
          }
        )

        observe_payment.valid_payment(
          inputs,
          outputs,
          redeemers,
          value.from_minted_value(mint),
          loan_script_hash,
          core.get_end_time(validity_range),
        )
      }
    }
  }
}

validator( 
  loan_script_hash: Hash<Blake2b_224, Script>,
  payment_observer_script_hash: Hash<Blake2b_224, Script>
) {
  fn active_beacon_script(redeemer: ActiveBeaconsRedeemer, ctx: ScriptContext) -> Bool {
    when redeemer is {
      CreateActive(borrower_cred,negotiation_beacon_id) -> {
        // Since a minting script can only be executed once in a given transaction, when this
        // redeemer is used, it will be assumed that all outputs with these beacons are meant to be
        // Active UTxOs locked at a borrower's address. 
        // 
        // This redeemer can only be used to mint Active beacons, BorrowerIDs, and LoanIDs;
        // this redeemer cannot burn these three beacons. It will check an exact match on
        // the expected mints for the active beacons and burns for the negotiation beacons.
        // Therefore, this redeemer cannot be composed with any redeemer where more flexible
        // minting/burning are required (such as fully paying off a loan and burning the BorrwerID).
        // It can still be used with any action on Active UTxOs that does not require burning 
        // (eg, rolling over a loan or making a partial payment). 
        //
        // For this redeemer to successfully execute, all of the following must be true:
        // 1) All loan inputs must come from the loan address associated with the borrower
        //    credential in the redeemer.
        // 2) No finished loans (ie, those with an Active beacon but no BorrowerID) are allowed
        //    among the inputs.
        // 3) The number of valid Offer UTxO inputs must equal the number of valid Ask UTxO
        //    inputs.
        // 4) All valid offers among inputs must not be expired. Uses invalid-hereafter
        //    to prove the time has not passed.
        // 5) Every offer must have a corresponding collateral output:
        //    a) Locked at the borrower's loan address.
        //    b) The datum must have the proper terms:
        //         - active_beacon_id == this policy id.
        //         - borrower_id == this borrower's staking credential as a token name.
        //         - lender_address == lender_address from Offer UTxO.
        //         - loan_asset == loan_asset from Offer UTxO.
        //         - asset_beacon == asset_beacon from Offer UTxO.
        //         - loan_principle == loan_principle from Offer UTxO.
        //         - rollover_frequency == rollover_frequency from Offer UTxO.
        //         - last_checkpoint == invalid-before of this tx
        //         - loan_term == loan_term from Offer UTxO.
        //         - loan_interest == loan_interest from Offer UTxO.
        //         - min_payment == min_payment from Offer UTxO.
        //         - collateralization == collateralization from Offer UTxO.
        //         - collateral_is_swappable == collateral_is_swappable from Offer UTxO.
        //         - claim_expiration == invalid-before of this tx 
        //                             + loan_term from Offer UTxO 
        //                             + claim_period from Offer UTxO.
        //         - loan_expiration == invalid-before of this tx + loan_term from Offer UTxO.
        //         - loan_outstanding == 
        //              loan_principle from Offer UTxO * (1 + loan_interest from Offer UTxO)
        //         - loan_id == sha2_256( offer_tx_hash ++ offer_output_index ) as token name.
        //    c) It must have enough relative collateral to equal the loan_principle amount.
        //    d) It must have exactly 1 BorrowerID, exactly 1 Active beacon, exactly 1 Asset Beacon
        //       that corresponds to the loan asset, and exactly 1 LoanID that matches the loan_id
        //       field in the datum.
        // 6) Every offer must have a corresponding lender payment output:
        //    a) Locked at the lender_address in the ActiveDatum.
        //    b) Contains the offer's deposit used with the Offer UTxO as well as exactly
        //       1 LoanID token for that loan (it should match the loan_id field in the 
        //       corresponding ActiveDatum).
        //    c) A hard-coded datum in case the lender_address is a proxy plutus script address.
        // 7) The negotiation beacon script must burn all negotiation beacons attached to the 
        //    Ask and Offer UTxOs.
        // 8) The active beacon script must mint exactly:
        //    a) 1 Active beacon for every Offer UTxO among the inputs.
        //    b) 1 BorrowerID for every Offer UTxO accepted.
        //    c) 2 LoanIDs for each Offer UTxO accepted.
        //    d) 1 Asset beacon for every Offer UTxO accepted.
        // 9) All loan outputs must be to the borrower's loan address.
        // 10) The borrower's staking credential must approve the transaction.
        //
        // This redeemer requires both the use of the invalid-before flag and the invalid-hereafter
        // flag. The invalid-before should be set to the current time while invalid-hereafter flag
        // should be set to the earliest Offer UTxO expiration. For example, if the first offer
        // expires at slot 10 while the second offer expires at slot 20, slot 10 should be used
        // for invalid-hereafter.
        //
        // Every offer has two required outputs: a lender output with the required LoanID and
        // a collateral output with the required collateral.
        //
        // The ask inputs are not compared against the offer inputs because the borrower must
        // manually accept the offer. This action means the borrower is already explicitly agreeing
        // to the lender's terms. Plus, if the lender made a counter-offer, the borrower can
        // immediately accept it; this would not be possible if the borrower's ask had to match
        // the offer accepted (the borrower would first have to update their Ask UTxO in one
        // transaction and then accept the offer in the next transaction).
        //
        // Even though Offer inputs are not compared against Ask inputs, it would be bad for the
        // beacon querying if borrowers could accept offers but leave the Ask UTxO open. This
        // Ask UTxO would continue to be broadcast to other lenders even though the borrower
        // has already satisfied their ask. Therefore, by requiring the borrower to close the
        // same amount of asks as offers accepted, this issue is mitigated. This is why the
        // CreateActive redeemer also tightly controls what the negotiation beacon script
        // can do in this transaction.
        //
        // The interest is applied for the first time upon immediately accepting the loan. This is
        // so that the protocal can support non-compounding interest on loans. For non-compounding
        // interest, set an loan_interest > 0 and set the rollover_frequency to None.
        //
        // This redeemer does not need to check any other output. All it needs to focus on are where
        // the newly minted beacons end up. The other redeemers will check all continuing Active
        // UTxOs (ie, Active loans present in both the inputs and outputs). By tightly controlling
        // what beacons can be minted and burned, this redeemer cannot be used to cheat on any
        // other active loan. 
        expect ScriptContext( 
          Transaction(inputs,_,outputs,_,mint,_,wdrw,validity_range,sigs,..), 
          tx.Mint(active_beacon_id) 
        ) = ctx

        and {
          // This has its own trace messages.
          accept.valid_loan_acceptances(
            negotiation_beacon_id,
            core.to_active_id(active_beacon_id),
            payment_observer_script_hash,
            core.credential_to_borrower_id(borrower_cred),
            Address(ScriptCredential(loan_script_hash),Some(Inline(borrower_cred))),
            validity_range,
            inputs,
            outputs,
            value.from_minted_value(mint)
          ),

          trace_if_false(
            @"Borrower credential did not approve", 
            utils.staking_credential_approves(Some(Inline(borrower_cred)),wdrw,sigs)
          )
        }
      }

      BurnKeyAndClaimExpired -> {
        // Since a minting script can only be executed once in a given transaction, when this
        // redeemer is used, it will be assumed that all inputs from loan addresses are expired
        // Active UTxOs. Therefore, this redeemer can only be used to burn active beacons.
        //
        // The invalid-before flag is used to prove that the expiration time has actually
        // passed. When claiming multiple expired loans, this flag should be set to the latest
        // expiration time.
        //
        // To successfully claim the collateral for an expired loan, all of the following must
        // be true:
        // 1) All loan inputs must be Active loans that have expired.
        // 2) All loan inputs must still contain the BorrowerID for each loan.
        // 2) Both the lock & key NFT for each loan must be burned.
        // 3) All beacons must be burned.
        //
        // The BorrowerID is required so that only the borrower can claim any remaining collateral
        // in the UTxO as well as the minUTxOValue deposit that was stored with the Active UTxO. 
        // This deposit belongs to the borrower. Finished loans should not be claimable with the 
        // Key NFT for this reason.
        //
        // Since the Key NFT is actually being burned in this transaction (the UTxO is being
        // consumed), this can be used as a proxy for the lender's approval.
        //
        // The lender is able to claim the collateral for multiple expired loans in a given
        // transaction as long as they control all the required Key NFTs.
        //
        // All loans claimed with this redeemer will count as defaults against the borrower.
        expect ScriptContext( 
          Transaction(inputs,_,_,_,mint,_,_,validity_range,_,..), 
          tx.Mint(active_beacon_id) 
        ) = ctx

        // This has its own trace messages.
        burn_and_claim.valid_expired_claim(
          core.to_active_id(active_beacon_id),
          ScriptCredential(loan_script_hash),
          core.get_start_time(validity_range),
          inputs,
          value.from_minted_value(mint) |> value.tokens(_,active_beacon_id)
        )
      }

      BurnRemainderOrUnlockLost -> {
        // Since a minting script can only be executed once in a given transaction, when this
        // redeemer is used, it will be assumed that all inputs from loan addresses are
        // Active UTxOs that must be unlocked (eg, finished loans or lost collateral). Therefore, 
        // this redeemer can only be used to burn beacons.
        //
        // The invalid-before flag is used to prove that the required claim time has actually
        // passed. When claiming multiple lost collateral, this flag should be set to the latest
        // claim time.
        //
        // To successfully claim lost collateral, all of the following must
        // be true:
        // 1) All loan inputs must be Active loans that have either finished (ie, are missing
        //    BorrowerIDs) or have had the claim period passed.
        // 2) All beacons must be burned.
        //
        // All lost collateral claimed with this redeemer will count as defaults against the 
        // borrower.
        expect ScriptContext( 
          Transaction(inputs,_,_,_,mint,_,_,validity_range,_,..), 
          tx.Mint(active_beacon_id) 
        ) = ctx

        // This has its own trace messages.
        burn_and_unlock.valid_unlock_check(
          core.to_active_id(active_beacon_id),
          core.get_start_time(validity_range),
          inputs,
          value.from_minted_value(mint) |> value.tokens(_,active_beacon_id)
        )
      }

      BurnActiveBeacons -> {
        // This redeemer will always succeed as long as it is only used to burn beacons. This
        // is the cheapest option when beacons only need to be burned and another script
        // checks the proper beacons are burned. 
        expect ScriptContext( Transaction(_,_,_,_,mint,..), tx.Mint(beacon_id) ) = ctx

        trace_if_false( 
          @"This redeemer can only be used to burn active beacons",
          dict.foldl( 
            value.from_minted_value(mint) |> value.tokens(_,beacon_id),
            True, 
            fn(_,v,r) {r && v < 0} // Burn check.
          )
        )
      }
    }
  }
}

validator(
  proxy_hash: Hash<Blake2b_224,Script>, 
  loan_script_hash: Hash<Blake2b_224, Script>,
  active_beacon_id: PolicyId
) {
  fn negotiation_beacon_script(redeemer: NegotiationBeaconsRedeemer, ctx: ScriptContext) -> Bool {
    when redeemer is {
      CreateCloseOrUpdateAsk(borrower_cred) -> {
        // Since a minting script can only be executed once in a given transaction, when this
        // redeemer is used, it will be assumed that all outputs with negotiation beacons are 
        // meant to be Ask UTxOs locked at the respective borrower's address. By explicitly 
        // stating what valid UTxOs with Ask beacons are, it implicitly enforces that the proper 
        // beacons are minted/burned. This design allows for closing and updating Ask UTxOs in 
        // the same transaction while also improving performance.
        //
        // If no beacons need to be minted/burned, this redeemer can be used with a staking
        // execution. It uses the exact same logic as the minting execution.
        //
        // For this redeemer to successfully execute, all of the following must be true:
        // 1) All beacons must go to a loan script address using the borrower's credential
        //    as the staking credential.
        // 2) All outputs with beacons must have exacly two beacons:
        //    a) an Ask beacon with the token name "Ask"
        //    b) an Asset beacon with the token name 
        //       sha2_256( "Asset" ++ asset_policy_id + asset_name )
        // 3) All outputs with beacons must have a proper AskDatum:
        //    a) negotiation_beacon_id == this policy id
        //    b) active_beacon_id == hard-coded active_beacon_id
        //    c) borrower_id == borrower's credential as a token name
        //    d) loan_asset == asset that corresponds to the Asset beacon in the output
        //    e) asset_beacon == Asset beacon name
        //    f) loan_principle > 0
        //    g) loan_term > 0
        //    h) collateral list must not be empty, be sorted, and not have any duplicates.
        // 4) The output must contain at least one unit of each asset in the collateral list.
        // 5) No other assets are allowed in the output.
        // 6) The borrower's credential must signal approval.
        //
        // The Asset beacon name's pre-hash is prefixed with "Asset" to prevent the creation of
        // counterfeit loan IDs since the script cannot easily tell if the loan asset in the
        // datum is actually an asset or an output reference for a UTxO. The only way to prove
        // something is an asset is to use it as a value in a UTxO, but it is not safe to assume
        // the borrower will already have some of the loan asset on hand when creating an Ask UTxO.
        // This pre-hash method ensures Asset beacons are distinct from the LoanID beacons
        // without placing an unreasonable burden on the borrower. The Asset beacon names
        // are deliberately the same for both the negotiation beacons and the active beacons to
        // improve beacon usability (just swap out the policy id); this is why the negotiation 
        // Asset beacon still uses the pre-hash even though the negotiation beacon script cannot 
        // possibly mint a LoanID.
        //
        // One unit of each asset in the collateral list must be stored in the Ask UTxO to
        // allow the lender to filter Ask UTxOs based off the loan asset AND a desired collateral 
        // asset. In other words, the collateral itself can compliment the beacon queries. This
        // requirement also has the added benefit of protecting against denial-of-service attacks
        // against the beacon queries by: 1) invalidating Ask UTxOs with fake collateral
        // assets, and 2) pushing up the required minUTxOValue for each Ask UTxO. The minUTxOValue
        // makes it expensive to create millions of Ask UTxOs with real but worthless collateral
        // just to polute the beacon queries.
        //
        // The borrower's credential must signal approval so that only the borrower can initiate
        // negotiations under that identity. This cryptographically prevents identity theft.
        //
        // The loan asset is deliberately allowed to be the same as a collateral asset to enable
        // secured loans. Secured loans are very useful for bootstrapping a borrower's credit
        // history.
        let ScriptContext( 
          Transaction(_,_,outputs,_,_,_,wdrw,_,sigs,..), 
          purpose
          // tx.Mint(negotiation_beacon_id) 
        ) = ctx

        // This redeemer must be used with either a staking execution or a minting execution.
        let negotiation_beacon_id = when purpose is {
          tx.Mint(negotiation_beacon_id) -> negotiation_beacon_id
          tx.WithdrawFrom(Inline(ScriptCredential(negotiation_beacon_id))) -> negotiation_beacon_id
          _ -> error @"This redeemer must be used with minting or staking execution"
        }

        and {
          // This has its own trace messages.
          ask.ask_beacon_destination_check(
            Address(ScriptCredential(loan_script_hash),Some(Inline(borrower_cred))),
            core.credential_to_borrower_id(borrower_cred),
            core.to_negotiation_id(negotiation_beacon_id),
            core.to_active_id(active_beacon_id),
            outputs
          ),
          trace_if_false(
            @"Borrower credential did not approve", 
            utils.staking_credential_approves(Some(Inline(borrower_cred)),wdrw,sigs)
          )
        }
      }

      CreateCloseOrUpdateOffer(lender_cred) -> {
        // Since a minting script can only be executed once in a given transaction, when this
        // redeemer is used, it will be assumed that all outputs with these beacons are meant to be
        // Offer UTxOs locked at a borrower's address on behalf of the respective lender. By 
        // explicitly stating what valid UTxOs with beacons are, it implicitly enforces that the 
        // proper beacons are minted/burned. This design allows for closing and updating Offer UTxOs 
        // in the same transaction while also improving performance.
        //
        // If no beacons need to be minted/burned, this redeemer can be used with a staking
        // execution. It uses the exact same logic as the minting execution.
        //
        // For this redeemer to successfully execute, all of the following must be true:
        // 1) All beacons must go to a loan script address with a valid staking credential.
        // 2) All outputs with beacons must have exacly three beacons:
        //    a) an Offer beacon with the token name "Offer"
        //    b) an Asset beacon with the token name 
        //       sha2_256( "Asset" ++ asset_policy_id + asset_name )
        //    c) A LenderID where the token name is the lender's stake credential, prefixed 
        //       with "00". 
        // 3) All outputs with beacons must have a proper AskDatum:
        //    a) negotiation_beacon_id == this policy id
        //    b) active_beacon_id == hard-coded active_beacon_id
        //    c) lender_id == the token name for the LenderID token in the output
        //    d) lender_address == either a payment pubkey address or a proxy script address
        //       with a valid staking credential
        //    e) loan_asset == asset that corresponds to the Asset beacon in the output
        //    f) loan_principle > 0
        //    g) rollover_frequency > 0 if used
        //    h) loan_term > 0
        //    i) loan_interest must have a denominator > 0 and the interest must be >=0
        //    j) min_payment >= 0
        //    k) collateralization list must not be empty, be sorted, not have any duplicates, and 
        //       all relative prices must be >=0 and have denominators > 0
        //    l) claim_period > 0
        //    m) offer_deposit > 0
        //    n) offer_expiration > 0
        // 4) The output must contain the principle for the loan + the offer deposit.
        // 5) No other assets are allowed in the output.
        // 6) The lender's credential must signal approval.
        //
        // The LenderID is prefixed with either "00" or "01" if it is a pubkey or script
        // credential, respectively. This allows the scripts to know whether the lender_id
        // is for a pubkey or script just from its asset name.
        //
        // The rollover_frequency field determines how often interest must be applied. If the
        // rollover_frequency is set to None but the interest is > 0, then the loan would have
        // non-compounding interest.
        //
        // The interest can be set to 0 to allow interest-free loans.
        //
        // If the lender does not want a particular collateral asset used, the relative value
        // for that asset can be set to 0 to prevent it from counting as collateral.
        //
        // The min_payment is the minimum required payment a borrower must make each time they
        // make a payment. The only exception is when the outstanding balance is less than the
        // set minimum payment. This field can be set to 0 to allow any payment size.
        //
        // The collateral_is_swappable field does not need to be checked since the only possible
        // values are true and false; anything else will cause the script to crash.
        //
        // The lender's credential must approve so that the LenderID is cryptographically paired
        // to that lender. No one can polute the LenderID queries for that lender. This also
        // checks that the lender did not mistakenly specify the wrong credential type.
        //
        // Checking the offer_expiration may not be necessary from a security perspective but it
        // helps filter out some badly configured Offer UTxOs. The check is cheap so it is probably
        // fine. The check only ensures the offer_expiration is > 0 if used.
        let ScriptContext( 
          Transaction(_,_,outputs,_,_,_,wdrw,_,sigs,..), 
          purpose
        ) = ctx

        // This redeemer must be used with either a staking execution or a minting execution.
        let negotiation_beacon_id = when purpose is {
          tx.Mint(negotiation_beacon_id) -> negotiation_beacon_id
          tx.WithdrawFrom(Inline(ScriptCredential(negotiation_beacon_id))) -> negotiation_beacon_id
          _ -> error @"This redeemer must be used with minting or staking execution"
        }

        and {
          // This has its own trace messages.
          offer.offer_beacon_destination_check(
            loan_script_hash,
            core.credential_to_lender_id(lender_cred),
            core.to_negotiation_id(negotiation_beacon_id),
            core.to_active_id(active_beacon_id),
            proxy_hash,
            outputs
          ),

          trace_if_false(
            @"Lender credential did not approve", 
            utils.staking_credential_approves(Some(Inline(lender_cred)),wdrw,sigs)
          )
        }
      }

      BurnNegotiationBeacons -> {
        // This redeemer will always succeed as long as it is only used to burn beacons. This
        // is the cheapest option when beacons only need to be burned and another script
        // checks the proper beacons are burned. 
        expect ScriptContext( Transaction(_,_,_,_,mint,..), tx.Mint(beacon_id) ) = ctx

        trace_if_false( 
          @"This redeemer can only be used to burn negotiation beacons",
          dict.foldl( 
            value.from_minted_value(mint) |> value.tokens(_,beacon_id),
            True, 
            fn(_,v,r) {r && v < 0} // Burn check.
          )
        )
      }

      RegisterNegotiationScript -> {
        // Only allow registering the script. It cannot be delegated or deregistered.
        expect ScriptContext( _, tx.Publish(CredentialRegistration(_))) = ctx

        True
      }
    }
  }
}
