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
  - [x] Fail if askBeacon /= (beaconSym,"Ask").
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
  - [x] Fail if offerBeacon /= (beaconSym,"Offer").
  - [x] Fail if lenderId /= (beaconSym,supplied pubkey as token).
  - [x] Fail if loanPrinciple <= 0.
  - [x] Fail if loanTerm <= 0.
  - [x] Fail if loanInterest <= 0.
  - [x] Fail if collateral list is empty.
  - [x] Fail if not all collateral rates > 0
  - [x] Fail if loanDownPayment <= 0.
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
- [ ] Always allow burning.
  - [ ] Allows burning many beacons.
  - [ ] Allows burning a single beacon.
- [ ] Must use burn redeemer.
  - [ ] Fail if mint redeemer used to burn.
- [ ] Fail if burn redeemer used to mint.

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
  - [x] Fail if activeBeacon /= (beaconSym,"Active").
  - [x] Fail if lenderId /= lenderId offerDatum.
  - [x] Fail if borrowerId /= borrowerId askDatum.
  - [x] Fail if loanAsset /= loanAsset askDatum.
  - [x] Fail if loanPrinciple /= loanPrinciple askDatum.
  - [x] Fail if loanTerm /= loanTerm askDatum.
  - [x] Fail if loanInterest /= loanInterest offerDatum.
  - [x] Fail if loanDownPayment /= loanDownPayment offerDatum.
  - [x] Fail if collateralRates /= collateralRates offerDatum.
  - [x] Fail if loanExpiration /= TTL + loanTerm offerDatum.
  - [x] Fail if loanOutstanding /= loanPrinciple askDatum * (1 + loanInterest offerDatum).
  - [x] Fail if datum is not ActiveDatum.
  - [x] Fail if datum is not inline.
- [x] The required amount of collateral must be posted to address - based on loanDownPayment.
  - [x] Fail if not enough collateral posted.
  - [x] Allow undercollateralized loans.
  - [x] Allow overcollateralized loans.
- [x] The active beacon must be minted and stored in the dapp address.
  - [x] Fail if the active beacon is stored in another address.
- [x] Fail if TTL not specified.

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

### Claim