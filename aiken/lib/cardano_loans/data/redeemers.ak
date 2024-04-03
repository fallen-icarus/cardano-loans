use aiken/transaction/credential.{Address,Credential}
use aiken/transaction/value.{PolicyId}

// Redeemers for the loan validator script.
pub type LoanRedeemer {
  // Close or update an Ask UTxO.
  CloseOrUpdateAsk

  // Close or update an Offer UTxO.
  CloseOrUpdateOffer

  // Convert an Ask UTxO and an Offer UTxO into an Active UTxO.
  AcceptOffer

  // Make a payment on a loan. The amount tells the script how much to expect. 
  MakePayment{payment_amount: Int}

  // Apply interest and penalty to a loan and optionally deposit more ada if the minUTxOValue 
  // increased. The interest is applied n times where n is number_of_times. Penalties are only
  // applied when appropriate.
  ApplyInterest{deposit_increase: Int, number_of_times: Int}

  // Claim collateral for an expired loan.
  SpendWithKeyNFT

  // Update the address where loan payments must go. The new_address is the address the lender
  // wishes to use for this loan.
  UpdateLenderAddress{new_address: Address, deposit_increase: Int}

  // Clean up remaining beacons or claim "Lost" collateral.
  Unlock
}

// Redeemers for the negotiation beacon script.
pub type NegotiationBeaconsRedeemer {
  // Create, close, or update some Ask UTxOs (1 or more). Creating and closing uses minting
  // execution while updating uses staking execution.
  CreateCloseOrUpdateAsk { 
    borrower_credential: Credential, // Borrower's staking credential. Pubkey or script.
  } 

  // Create, close, or update some Offer UTxOs (1 or more). Creating and closing uses minting
  // execution while updating uses staking execution.
  CreateCloseOrUpdateOffer { 
    lender_credential: Credential, // Lender's staking credential. Pubkey or script.
  }

  // Burn any beacons. This is only used when accepting offers.
  BurnNegotiationBeacons

  // Register the negotiation beacon script to allow it to be used as a staking script.
  RegisterNegotiationScript
}

// Redeemers for the active beacon script.
pub type ActiveBeaconsRedeemer {
  // Accept some loans (1 or more). The spending script will force the redeemer to use
  // the proper negotiation_beacon_id.
  CreateActive { 
    negotiation_beacon_id: PolicyId // PolicyId for the negotiation_beacons_script.
  }

  // Burn the lock and key NFTs, and claim expired collateral.
  BurnKeyAndClaimExpired

  // Burn all remaining beacons. If the BorrowerId is present, the loan's claim period must
  // have passed.
  BurnRemainderOrUnlockLost

  // Burn any beacons.
  BurnActiveBeacons
}

// Redeemers for the payment observer script.
pub type PaymentObserverRedeemer {
  // Observe a borrower's loan payment transaction.
  ObservePayment

  // Register the payment observer script allow it to be used as a staking script.
  RegisterPaymentObserverScript
}

// Redeemers for the interest observer script.
pub type InterestObserverRedeemer {
  // Observe a borrower's loan interest application transaction.
  ObserveInterest

  // Register the interest observer script to be used as a staking script.
  RegisterInterestObserverScript
}

// Redeemers for the address update observer script.
pub type AddressUpdateObserverRedeemer {
  // Observe a lender's address update transaction.
  ObserveAddressUpdate

  // Register the address update observer script to allow it to be used as a staking script.
  RegisterAddressUpdateObserverScript
}
