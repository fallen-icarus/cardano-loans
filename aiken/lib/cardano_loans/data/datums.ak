use aiken/transaction/credential.{Address}
use aiken/time.{PosixTime}
use aiken/transaction/value.{PolicyId,AssetName}
use aiken/transaction.{OutputReference}

use cardano_loans/data/core.{Penalty,Asset,Fraction,ScriptHash}

// The loan datum that can be in one of three forms.
pub type LoanDatum {
  // The terms a borrower is asking for.
  AskDatum { 
    // The policy id for the negotiation beacon script.
    negotiation_beacon_id: PolicyId, 
    // The policy id for the active beacon script.
    active_beacon_id: PolicyId, 
    // The borrower's staking credential as a token name.
    borrower_id: AssetName, 
    // The asset to be loaned out.
    loan_asset: Asset, 
    // The asset name for the loan asset beacon. This is the sha2_256 hash of the loan asset's name,
    // prefixed with "Asset" (before hashing).
    asset_beacon: AssetName, 
    // The size of the loan. It must be > 0.
    loan_principal: Int, 
    // How long the loan is active once accepted. It must be > 0.
    loan_term: PosixTime, 
    // The assets that will be used as collateral. It must not be empty, it must be sorted
    // lexicographically, and there cannot be any duplicates in the list.
    collateral: List<Asset> 
  }

  // The terms a Lender is offering to a borrower.
  OfferDatum { 
    // The policy id for the beacon script.
    negotiation_beacon_id: PolicyId, 
    // The policy id for the active beacon script.
    active_beacon_id: PolicyId, 
    // The prefixed lender's staking credential as a token name. Pubkey credentials are prefixed
    // with "00" and script credentials are prefixed with "01".
    lender_id: AssetName, 
    // The lender's address. It must either use a payment pubkey credential, or the proxy script 
    // credential with a valid staking credential. No other payment script credentials are allowed.
    lender_address: Address, 
    // The asset to be loaned out.
    loan_asset: Asset, 
    // The asset name for the loan asset beacon. This is the sha2_256 hash of the loan asset's name,
    // prefixed with "Asset" (before hashing).
    asset_beacon: AssetName, 
    // The size of the loan. It must be > 0.
    loan_principal: Int, 
    // The length of a payment period. Each loan is divided into periods of length specified
    // by this parameter. If interest is compounding, it must be applied at this frequency.
    // Furthermore, if a penalty is set, the minimum payment must be satisfied for each epoch.
    // This parameter can be either None or Some(x), where x > 0.
    epoch_duration: Option<PosixTime>, 
    // How long the loan is active once accepted. It must be > 0.
    loan_term: PosixTime, 
    // The interest that must be applied either periodically or just once. The numerator must be >=
    // 0 and the denominator must be > 0. Interest free loans can be created by setting this field
    // to 0, and non-compounding loans can be created by specifying an interest rate > 0 while
    // setting compounding_interest to False. Setting an interest rate with compounding_interest
    // as True results in a compounding loan where the epoch_duration is the compounding period.
    loan_interest: Fraction, 
    // Whether the interest is compounding interest.
    compounding_interest: Bool,
    // The minimum loan payment that must be paid each loan epoch. It must be >= 0.
    min_payment: Int, 
    // The penalty that gets applied if the minimum payment has not been met this loan epach.
    // FixedFee and PercentFee must be > 0 if used.
    penalty: Penalty,
    // The maximum allowed missed consecutive epochs. If the borrower misses more epochs than this,
    // the lender can terminate the loan early. However, if the borrower satisfies the minimum
    // payment again before the lender has a chance to claim the defaulted collateral, the loan will
    // be in good standing again (i.e., no longer in default). This setting is optional.
    max_consecutive_misses: Option<Int>,
    // The relative values of the collateral assets. It must not be empty, it must be sorted
    // lexicographically, and there cannot be any duplicates in the list. All relative values must
    // have numerators >= 0 and denominators > 0. If a lender does not want a collateral asset used,
    // the relative value for that collateral can be set to 0.
    collateralization: List<(Asset,Fraction)>, 
    // Whether collateral can be swapped out during a loan payment. The lender must explicitly
    // enable this.
    collateral_is_swappable: Bool, 
    // How long the lender will have to claim the defaulted UTxO.
    claim_period: PosixTime, 
    // How much ADA was used for the Offer UTxO's minUTxOValue. This will be returned to the
    // lender when the borrower accepts the offer.
    offer_deposit: Int, 
    // An optional offer expiration time.
    offer_expiration: Option<PosixTime>,
    // The Ask this Offer corresponds to. It has no purpose internally to the protocol, but it is
    // useful for frontends to connect offers to asks.
    corresponding_ask: Option<OutputReference>
  }

  // Fields in common with OfferDatums are taken directly from the Offer UTxO's datum. The
  // information is included here to streamline looking up credit histories.
  ActiveDatum { 
    // The policy id for the beacon script.
    active_beacon_id: PolicyId, 
    // Script hash for the payment observer script.
    payment_observer_hash: ScriptHash, 
    // Script hash for the address update observer script.
    address_update_observer_hash: ScriptHash, 
    // The borrower's staking credential as a token name.
    borrower_id: AssetName, 
    // The address where loan payments must be made.
    lender_address: Address, 
    // The loan asset.
    loan_asset: Asset, 
    // The asset name for the loan asset beacon.
    asset_beacon: AssetName, 
    // The initial size of the loan.
    loan_principal: Int, 
    // The amount of time between interest compounding. If min_payment is > 0, then each epoch
    // must have total_epoch_payments >= min_payment in order to avoid penalties.
    epoch_duration: Option<PosixTime>, 
    // The last epoch boundary.
    last_epoch_boundary: PosixTime, 
    // The duration of the loan.
    loan_term: PosixTime, 
    // The interest that must be periodically applied or just once, depending on whether the
    // interest is compounding.
    loan_interest: Fraction, 
    // Whether the interest is compounding interest.
    compounding_interest: Bool,
    // The minimum loan payment that must be paid each loan epoch. It must be >= 0.
    min_payment: Int, 
    // The penalty that gets applied if the minimum payment has not been met this loan epach.
    // FixedFee and PercentFee must be > 0 if used.
    penalty: Penalty,
    // The maximum allowed missed consecutive epochs. If the borrower misses more epochs than this,
    // the lender can terminate the loan early. However, if the borrower satisfies the minimum
    // payment again before the lender has a chance to claim the defaulted collateral, the loan will
    // be in good standing again (i.e., no longer in default).
    max_consecutive_misses: Option<Int>,
    // The relative values of the collateral assets.
    collateralization: List<(Asset,Fraction)>, 
    // Whether collateral can be swapped out during a loan payment.
    collateral_is_swappable: Bool, 
    // The absolute time at which collateral is considered "Lost".
    claim_expiration: PosixTime, 
    // The absolute time at which the loan expires.
    loan_expiration: PosixTime, 
    // The loan's remaining unpaid balance. When an offer is first accepted, the interest is
    // immediately applied for the first time; this enables non-compounding loans.
    loan_outstanding: Fraction, 
    // The total amount paid this loan epoch.
    total_epoch_payments: Int,
    // The current consecutive loan epochs missed. If it is greater than the `max_consecutive_misses`,
    // the lender can terminate the loan early and confiscate the collateral. This gets reset to
    // zero whenever the borrower meets the min_payment requirement of the current loan epoch.
    current_consecutive_misses: Int,
    // The unique identifier for this loan. It is the sha2_256 hash of the corresponding 
    // Offer UTxO's OutputReference.
    loan_id: AssetName 
  }
}

// Datums attached to payments are always of the form: (active_beacon_id, loan_id). A dedicated 
// type is not necessary for this.
