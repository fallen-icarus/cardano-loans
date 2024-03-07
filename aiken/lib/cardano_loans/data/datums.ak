use aiken/hash.{Blake2b_224,Hash}
use aiken/transaction/credential.{Address,Script}

use cardano_loans/data/core.{
    ActiveID,
    NegotiationID,
    Asset,
    Rational,
    LenderID,
    BorrowerID,
    AssetBeacon,
    Duration,
    ExpirationTime,
    StartTime,
    LoanID
  }

// Note about Asset Beacons:
// While both the negotiation phase and the active phase both contain asset beacons,
// they are different asset beacons; each phase uses a different policy id for the
// asset beacons. When an offer is accepted, the negatiation asset beacons are all
// burned. and new, active asset beacons, are minted.

// The loan datum that can be in one of three forms.
pub type LoanDatum {
  AskDatum { 
    negotiation_beacon_id: NegotiationID, // The policy id for the negotiation beacon script.
    active_beacon_id: ActiveID, // The policy id for the active beacon script.
    borrower_id: BorrowerID, // The borrower's staking credential as a token name.
    loan_asset: Asset, // The asset to be loaned out.
    asset_beacon: AssetBeacon, // The asset name for the loan asset beacon.
    loan_principle: Int, // The size of the loan.
    loan_term: Duration, // How long the loan is active once accepted.
    collateral: List<Asset> // The assets that will be used as collateral.
  }

  OfferDatum { 
    negotiation_beacon_id: NegotiationID, // The policy id for the beacon script.
    active_beacon_id: ActiveID, // The policy id for the active beacon script.
    lender_id: LenderID, // The prefixed lender's staking credential as a token name.
    lender_address: Address, // The lender's address.
    loan_asset: Asset, // The asset to be loaned out.
    asset_beacon: AssetBeacon, // The asset name for the loan asset beacon.
    loan_principle: Int, // The size of the loan.
    rollover_frequency: Option<Duration>, // The frequency at which interest must be applied.
    loan_term: Duration, // How long the loan is active once accepted.
    loan_interest: Rational, // The interest that is applied with each rollover.
    min_payment: Int, // The minimum loan payment that can be made.
    collateralization: List<(Asset,Rational)>, // The relative values of the collateral assets.
    collateral_is_swappable: Bool, // Whether collateral can be swapped out during a loan payment.
    claim_period: Duration, // How long the lender will have to claim the defaulted UTxO.
    offer_deposit: Int, // How much ADA was used for the UTxO's minUTxOValue.
    offer_expiration: Option<ExpirationTime> // An optional offer expiration time.
  }

  ActiveDatum { 
    active_beacon_id: ActiveID, // The policy id for the beacon script.
    payment_observer: Hash<Blake2b_224,Script>, // The script has for the payment observer script.
    borrower_id: BorrowerID, // The borrower's staking credential as a token name.
    lender_address: Address, // The address where loan payments must be made.
    loan_asset: Asset, // The loan asset.
    asset_beacon: AssetBeacon, // The asset name for the loan asset beacon.
    loan_principle: Int, // The initial size of the loan.
    rollover_frequency: Option<Duration>, // The frequency at which interest must be applied.
    last_checkpoint: StartTime, // The last time interest was applied.
    loan_term: Duration, // The duration of the loan.
    loan_interest: Rational, // The interest that is applied with each rollover.
    min_payment: Int, // The minimum loan payment that can be made.
    collateralization: List<(Asset,Rational)>, // The relative values of the collateral assets.
    collateral_is_swappable: Bool, // Whether collateral can be swapped out during a loan payment.
    claim_expiration: ExpirationTime, // The absolute time at which collateral is considered "Lost".
    loan_expiration: ExpirationTime, // The absolute time at which the loan expires.
    loan_outstanding: Rational, // The loan's remaining unpaid balance.
    loan_id: LoanID // The unique identifier for this loan.
  }
}
