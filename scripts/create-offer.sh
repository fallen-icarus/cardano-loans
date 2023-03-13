#!/bin/sh

# A helper script for showing how to create an Offer as a lender.

## Variables
dir="../assets/loan-files/"
tmpDir="../assets/tmp/"

beaconPolicyFile="${dir}beacons.plutus"

offerDatumFile="${dir}offer.json"

beaconRedeemerFile="${dir}mintOffer.json"

lenderPaymentPubKeyFile="../assets/wallets/02.vkey"
lenderPaymentPubKeyHashFile="../assets/wallets/02.pkh"

### Change these two variables to your target borrower.
loanAddr="addr_test1zqdjvw3d5m0utvdh79xdzmr55y7j808qpdduywfftq53pn3ualkqngnmdz2w9mv60zuucq0sswtn6lq2lwxwez76x0aqmtc6we"
borrowerPubKeyHash="3cefec09a27b6894e2ed9a78b9cc01f083973d7c0afb8cec8bda33fa"

### This is the hexidecimal encoding for 'Offer'.
offerTokenName="4f66666572"

## Generate the hash for the lender's payment pubkey.
cardano-cli address key-hash \
  --payment-verification-key-file $lenderPaymentPubKeyFile \
  --out-file $lenderPaymentPubKeyHashFile

## Export the beacon policy.
cabal run exe:cardano-loans -- export-script \
  --beacon-policy \
  --out-file $beaconPolicyFile

## Create the Offer datum.
cabal run exe:cardano-loans -- lender offer-datum \
  --lender-payment-pubkey-hash "$(cat $lenderPaymentPubKeyHashFile)" \
  --loan-asset-is-lovelace \
  --principle 100 \
  --loan-term 300 \
  --interest-rate 0.1 \
  --required-backing 100 \
  --collateral-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --collateral-asset-token-name 4f74686572546f6b656e0a \
  --collateral-rate 2 \
  --out-file $offerDatumFile

## Create the MintOfferToken beacon policy redeemer.
cabal run exe:cardano-loans -- lender offer-beacon \
  --lender-payment-pubkey-hash "$(cat $lenderPaymentPubKeyHashFile)" \
  --out-file $beaconRedeemerFile

## Get the beacon policy id.
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file $beaconPolicyFile)

## Helper Offer beacon variable
offerBeacon="${beaconPolicyId}.${offerTokenName}"

## Helper Lender ID beacon variable.
lenderPaymentPubKeyHash=$(cat $lenderPaymentPubKeyHashFile)
lenderBeacon="${beaconPolicyId}.${lenderPaymentPubKeyHash}"

# Create and submit the transaction.
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in 3b33133eb66a626547c5cfc07785e1d0c7abe8715944502221e991d2ebf32739#3 \
  --tx-out "${loanAddr} + 3000100 lovelace + 1 ${offerBeacon} + 1 ${lenderBeacon}" \
  --tx-out-inline-datum-file $offerDatumFile \
  --mint "1 ${offerBeacon} + 1 ${lenderBeacon}" \
  --mint-script-file $beaconPolicyFile \
  --mint-redeemer-file $beaconRedeemerFile \
  --required-signer-hash "$(cat $lenderPaymentPubKeyHashFile)" \
  --change-address "$(cat ../assets/wallets/02.addr)" \
  --tx-in-collateral 34e918108353b96e1d1ea290afd02c035131e4b514899b30129f657edbc2d13f#0 \
  --testnet-magic 1 \
  --protocol-params-file "${tmpDir}protocol.json" \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file ../assets/wallets/02.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"