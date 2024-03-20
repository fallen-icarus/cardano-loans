use aiken/transaction/credential.{Address}
use aiken/time.{PosixTime}
use aiken/transaction/value.{PolicyId,AssetName}

use cardano_loans/data/core.{Asset,Fraction,ScriptHash}

// Note about Asset Beacons:
// While both the negotiation phase and the active phase both contain asset beacons,
// they are different asset beacons; each phase uses a different policy id for the
// asset beacons. When an offer is accepted, the negatiation asset beacons are all
// burned. and new, active asset beacons, are minted.

// The loan datum that can be in one of three forms.
pub type LoanDatum {
  AskDatum { 
    negotiation_beacon_id: PolicyId, // The policy id for the negotiation beacon script.
    active_beacon_id: PolicyId, // The policy id for the active beacon script.
    borrower_id: AssetName, // The borrower's staking credential as a token name.
    loan_asset: Asset, // The asset to be loaned out.
    asset_beacon: AssetName, // The asset name for the loan asset beacon.
    loan_principle: Int, // The size of the loan.
    loan_term: PosixTime, // How long the loan is active once accepted.
    collateral: List<Asset> // The assets that will be used as collateral.
  }

  OfferDatum { 
    negotiation_beacon_id: PolicyId, // The policy id for the beacon script.
    active_beacon_id: PolicyId, // The policy id for the active beacon script.
    lender_id: AssetName, // The prefixed lender's staking credential as a token name.
    lender_address: Address, // The lender's address.
    loan_asset: Asset, // The asset to be loaned out.
    asset_beacon: AssetName, // The asset name for the loan asset beacon.
    loan_principle: Int, // The size of the loan.
    rollover_frequency: Option<PosixTime>, // The frequency at which interest must be applied.
    loan_term: PosixTime, // How long the loan is active once accepted.
    loan_interest: Fraction, // The interest that is applied with each rollover.
    min_payment: Int, // The minimum loan payment that can be made.
    collateralization: List<(Asset,Fraction)>, // The relative values of the collateral assets.
    collateral_is_swappable: Bool, // Whether collateral can be swapped out during a loan payment.
    claim_period: PosixTime, // How long the lender will have to claim the defaulted UTxO.
    offer_deposit: Int, // How much ADA was used for the UTxO's minUTxOValue.
    offer_expiration: Option<PosixTime> // An optional offer expiration time.
  }

  ActiveDatum { 
    active_beacon_id: PolicyId, // The policy id for the beacon script.
    payment_observer_hash: ScriptHash, // The script hash for the payment observer script.
    rollover_observer_hash: ScriptHash, // The script hash for the rollover observer script.
    borrower_id: AssetName, // The borrower's staking credential as a token name.
    lender_address: Address, // The address where loan payments must be made.
    loan_asset: Asset, // The loan asset.
    asset_beacon: AssetName, // The asset name for the loan asset beacon.
    loan_principle: Int, // The initial size of the loan.
    rollover_frequency: Option<PosixTime>, // The frequency at which interest must be applied.
    last_checkpoint: PosixTime, // The last time interest was applied.
    loan_term: PosixTime, // The duration of the loan.
    loan_interest: Fraction, // The interest that is applied with each rollover.
    min_payment: Int, // The minimum loan payment that can be made.
    collateralization: List<(Asset,Fraction)>, // The relative values of the collateral assets.
    collateral_is_swappable: Bool, // Whether collateral can be swapped out during a loan payment.
    claim_expiration: PosixTime, // The absolute time at which collateral is considered "Lost".
    loan_expiration: PosixTime, // The absolute time at which the loan expires.
    loan_outstanding: Fraction, // The loan's remaining unpaid balance.
    loan_id: AssetName // The unique identifier for this loan.
  }
}
