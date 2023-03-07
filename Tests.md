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
  - [x] Fail if loanQuantity <= 0.
  - [x] Fail if loanTerm <= 0.
  - [x] Fail if collateral list is empty.
  - [x] Fail if datum not inline.
- [x] Fail if receiving address' staking credential does not sign.

### MintOfferBeacon *PaymentPubKeyHash*
- [ ] Only one offer token minted in the tx.
- [ ] The offer token must have the token name "Offer".
- [ ] Only one lender token minted in the tx.
- [ ] The lender token name must be the supplied pubkey.
- [ ] Must be minted to one of the dapp's addresses.
  - [ ] Fail if minted to a pubkey address.
  - [ ] Fail if minted to another script address.
- [ ] Must be minted to an address using a staking pubkey.
  - [ ] Fail if minted to an address using a staking script.
  - [ ] Fail if minted to an address without a staking credential.
- [ ] The offer token and the lender token must be stored in the same utxo at the dapp address.
  - [ ] Fail if stored separately at address.
- [ ] The offer datum must be valid.
  - [ ] Fail if offerBeacon /= (beaconSym,"Offer").
  - [ ] Fail if lenderId /= (beaconSym,supplied pubkey as token).
  - [ ] Fail if loanQuantity <= 0.
  - [ ] Fail if loanTerm <= 0.
  - [ ] Fail if loanInterest <= 0.
  - [ ] Fail if collateral list is empty.
  - [ ] Fail if not all collateral rates > 0
  - [ ] Fail if datum not inline.
- [ ] The lender must sign the tx.
