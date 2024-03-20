use aiken/transaction/credential.{Address,Credential}
use aiken/transaction/value.{PolicyId}

pub type LoanRedeemer {
  // Close or update an Ask UTxO.
  CloseOrUpdateAsk

  // Close or update an Offer UTxO.
  CloseOrUpdateOffer

  // Convert an Ask UTxO and an Offer UTxO into an Active UTxO.
  AcceptOffer

  // Make a payment on a loan. The amount tells the script how much to expect. This dramatically
  // improves performance.
  MakePayment{payment_amount: Int}

  // Apply interest to a loan and optionally deposit more ada if the minUTxOValue increased.
  Rollover{deposit_increase: Int}

  // Claim collateral for an expired loan.
  SpendWithKeyNFT

  // Update the address where loan payments must go.
  UpdateLenderAddress{new_address: Address, deposit_increase: Int}

  // Clean up remaining beacons or claim "Lost" collateral.
  Unlock
}

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

  // Register the negotiation beacon script as a staking script to allow it to be used
  // as a staking script.
  RegisterNegotiationScript
}

pub type ActiveBeaconsRedeemer {
  // Accept some loans (1 or more). The spending script will force the redeemer to use
  // the proper borrower_credential and negotiation_beacon_id.
  CreateActive { 
    borrower_credential: Credential, // Borrower's staking credential.
    negotiation_beacon_id: PolicyId // PolicyId for the negotiation_beacons_script.
  }

  // Burn the lock and key NFTs, and claim expired collateral.
  BurnKeyAndClaimExpired

  // Burn all remaining beacons. If the BorrowerID is present, the loan's claim period must
  // have passed.
  BurnRemainderOrUnlockLost

  // Burn any beacons.
  BurnActiveBeacons
}

pub type PaymentObserverRedeemer {
  // Observe a borrower's loan payment transaction.
  ObservePayment

  // Register the payment observer script as a staking script to allow it to be used
  // as a staking script.
  RegisterPaymentObserverScript
}

pub type RolloverObserverRedeemer {
  // Observe a borrower's loan rollover transaction.
  ObserveRollover

  // Register the rollover observer script as a staking script to allow it to be used
  // as a staking script.
  RegisterRolloverObserverScript
}
