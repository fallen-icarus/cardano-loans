# Cardano-Loans

## Beacons

### MintAskBeacon *PaymentPubKeyHash*
- [x] Only one ask token minted in the tx.
  - [x] Fail if ask token does not have the token name "Ask".
  - [x] Fail if additional kinds of tokens minted.
- [x] Must be minted to one of the dapp's addresses.
  - [x] Fail if minted to a pubkey address.
  - [x] Fail if minted to another script address.
- [x] Must be minted to an address using a staking pubkey.
  - [x] Fail if minted to an address using a staking script.
  - [x] Fail if minted to an address without a staking credential.
- [x] Must be minted to a dapp address using the supplied pubkey as the staking pubkey.
- [x] The ask token must be stored with the proper ask datum.
  - [x] Fail if loanBeaconSym /= beaconSym.
  - [x] Fail is borrowerId /= (beaconSym,supplied pubkey as token).
  - [x] Fail if loanPrinciple <= 0.
  - [x] Fail if loanTerm <= 0.
  - [x] Fail if collateral list is empty.
  - [x] Fail if datum not inline.
  - [x] Fail if not an AskDatum.
- [x] Fail if receiving address' staking credential does not sign.

### MintOfferBeacon *PaymentPubKeyHash*
- [x] Only one offer token minted in the tx.
- [x] The offer token must have the token name "Offer".
- [x] Only one lender token minted in the tx.
- [x] The lender token name must be the supplied pubkey.
- [x] No other tokens can be minted in the tx.
- [x] Must be minted to one of the dapp's addresses.
  - [x] Fail if minted to a pubkey address.
  - [x] Fail if minted to another script address.
- [x] Must be minted to an address using a staking pubkey.
  - [x] Fail if minted to an address using a staking script.
  - [x] Fail if minted to an address without a staking credential.
- [x] The offer token and the lender token must be stored in the same utxo at the dapp address.
  - [x] Fail if stored separately at address.
- [x] The offer datum must be valid.
  - [x] Fail if loanBeaconSym /= beaconSym.
  - [x] Fail if lenderId /= supplied pubkey as token.
  - [x] Fail if loanPrinciple <= 0.
  - [x] Fail if loanTerm <= 0.
  - [x] Fail if loanInterest <= 0.
  - [x] Fail if collateral list is empty.
  - [x] Fail if not all collateral rates > 0
  - [x] Fail if datum not OfferDatum.
  - [x] Fail if datum not inline.
- [x] Offer beacon must be stored with the asked loan amount.
  - [x] If lovelace is asked for, it must be stored with an additional 3 ADA for the beacons.
- [x] The lender must sign the tx.

### MintActiveToken *PaymentPubKeyHash* *PaymentPubKeyHash*
- [x] Both the ask token and the offer token must be burned in the tx.
- [x] Must mint exactly one active token.
  - [x] Fail if no active tokens minted.
  - [x] Fail if more than one minted.
  - [x] Fail if the active token does not have the token name 'Active'.
- [x] Must mint exactly one borrower ID.
  - [x] Fail if borrower ID not minted.
  - [x] Fail if more than one minted.
  - [x] Fail if the borrower ID token name is not the borrower's pubkey hash.
- [x] The tokens must be minted to the dapp address using the borrower's pubkey for the staking credential.
  - [x] Fail if minted to a pubkey address.
  - [x] Fail if minted to a dapp address using a different pubkey for the staking credential.
  - [x] Fail if minted to a dapp address without a staking credential.
- [x] The active token, borrower ID, and lender ID must be stored in the same UTxO at the dapp address.
  - [x] Fail if the active token stored separately.
  - [x] Fail if the borrower ID stored separately.
  - [x] Fail if the lender ID stored separately.

### BurnBeaconToken
- [x] Always allow burning.
  - [x] Allows burning many beacons.
  - [x] Allows burning a single beacon.
- [x] Must use burn redeemer.
  - [x] Fail if mint redeemer used to burn.
- [x] Fail if burn redeemer used to mint.

## Loan Validator

### AcceptOffer
- [x] The staking credential must signal approval.
  - [x] Pubkey must sign.
  - [x] Staking script must be executed in the tx.
- [x] There must/can only be two inputs from the loan address.
  - [x] Fail if only one input.
  - [x] Fail if more than two inputs.
- [x] One input must be an offer input and one input must be an ask input.
- [x] The offer input must have an offer beacon.
- [x] The ask input must have an ask beacon.
- [x] The input datums must agree.
  - [x] Fail if loanAssets are different.
  - [x] Fail if loanPrinciples are different.
  - [x] Fail if loanTerms are different.
  - [x] Fail if datums have different collaterals.
- [x] No other offer/ask beacons allowed in inputs.
  - [x] Fail if another offer beacon in inputs.
  - [x] Fail if another ask beacon in inputs.
- [x] There must/can only be one output to the address.
  - [x] Fail if no output to dapp address.
  - [x] Fail if more than one output to dapp address.
- [x] The output must contain the proper active datum.
  - [x] Fail if loanBeaconSym /= beaconSym.
  - [x] Fail if lenderId /= lenderId offerDatum.
  - [x] Fail if borrowerId /= borrowerId askDatum.
  - [x] Fail if loanAsset /= loanAsset askDatum.
  - [x] Fail if loanPrinciple /= loanPrinciple askDatum.
  - [x] Fail if loanTerm /= loanTerm askDatum.
  - [x] Fail if loanInterest /= loanInterest offerDatum.
  - [x] Fail if collateralization /= collateralization offerDatum.
  - [x] Fail if loanExpiration /= invalid-before + loanTerm offerDatum.
  - [x] Fail if loanOutstanding /= loanPrinciple askDatum * (1 + loanInterest offerDatum).
  - [x] Fail if datum is not ActiveDatum.
  - [x] Fail if datum is not inline.
- [x] The required amount of collateral must be posted to address.
  - [x] Fail if not enough collateral posted.
- [x] The active beacon must be minted and stored in the dapp address.
  - [x] Fail if the active beacon is stored in another address.
- [x] Fail if invalid-before not specified.

### CloseAsk
- [x] The input utxo must have an AskDatum.
  - [x] Fail if it has a different datum type.
- [x] The staking credential must approve.
- [x] All ask beacons among tx inputs must be burned.
  - [x] Fail if not all ask beacons burned.

### CloseOffer
- [x] The input utxo must have an OfferDatum.
  - [x] Fail if it has a different datum type.
- [x] All offer beacons among tx inputs must be burned.
- [x] All lender IDs in tx for the lender must be burned.
- [x] If offer beacon is present, the lender must sign.
- [x] If offer beacon NOT present, the staking credential must signal approval.

### RepayLoan
- [x] The input utxo must have an ActiveDatum.
  - [x] Fail if not an ActiveDatum.
- [x] Staking credential must approve.
- [x] If the active beacon is present
  - [x] No other phase beacons allowed in tx inputs.
  - [x] There can only be one utxo spent from the address.
    - [x] Fail if an invalid utxo spent from address.
  - [x] Loan must not be expired.
  - [x] There can/must be one output to the address.
    - [x] Fail if there are mulitple outputs to address.
    - [x] Fail if there are no outputs to address.
  - [x] The output must have the same datum accept for the updated loanOutstanding.
  - [x] Fail if output datum not inline.
  - [x] If newOutstanding <= 0
    - [x] Allows reclaiming remaining collateral.
    - [x] The borrower ID must be burned.
      - [x] Fail if the borrower ID not burned.
    - [x] No other tokens can be minted/burned in tx.
    - [x] The output must have the active beacon and the lender ID.
      - [x] Fail if output missing active beacon.
      - [x] Fail if output missing lender ID.
  - [x] Else
    - [x] collateralTaken / collateralization * (1 + interest) <= loanRepaid
      - [x] Fail if not enough loan repaid.
    - [x] The output must have the active beacon, borrower ID, and the lender ID.
      - [x] Fail if output missing active beacon.
      - [x] Fail if output missing lender ID.
      - [x] Fail if output missing borrower ID.
  - [x] Fail if invalid-hereafter not specified.
- [x] Allow reclaiming utxo when active beacon missing.

### Claim
- [x] The input utxo must have an ActiveDatum.
  - [x] Fail if not an ActiveDatum.
- [x] The active beacon must be present.
  - [x] Fail if redeemer used on a utxo without an active beacon.
- [x] Fail if any other phase beacons in tx inputs.
- [x] Fail if loan not expired.
- [x] The active beacon and the IDs must be burned.
  - [x] Fail if active beacon not burned.
  - [x] Fail if lender ID not burned.
  - [x] If borrower ID present, fail if borrower ID not burned.
- [x] Lender must sign tx.
- [x] No other tokens can be minted/burned.
- [x] Allow claiming fully repaid loan before expiration.