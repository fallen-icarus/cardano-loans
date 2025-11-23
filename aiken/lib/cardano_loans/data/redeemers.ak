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

  // Make a payment on a loan. The amount tells the script how much to expect. This applies any
  // required interest/penalties to the outstanding balance before processing the payment.
  MakePayment{payment_amount: Int}

  // Claim collateral for an expired loan. This can also be used to terminate a loan early if the
  // borrower has missed too many payments.
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

  // Burn the lock and key NFTs, and claim defaulted collateral.
  BurnKeyAndClaimDefaulted

  // Burn all remaining beacons and claim lost collateral. The loan's claim period must
  // have passed.
  BurnAndUnlockLost

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

// Redeemers for the address update observer script.
pub type AddressUpdateObserverRedeemer {
  // Observe a lender's address update transaction.
  ObserveAddressUpdate

  // Register the address update observer script to allow it to be used as a staking script.
  RegisterAddressUpdateObserverScript
}
