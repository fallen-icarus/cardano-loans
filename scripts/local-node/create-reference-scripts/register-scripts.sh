#!/bin/sh

# A helper script for showing how to register the scripts for staking executions.

## Variables
mainDir="../../../ignored/"
walletDir="${mainDir}wallets/"
loanDir="${mainDir}loan-files/"
tmpDir="${mainDir}tmp/"

negotiationScript="${loanDir}negotiation_beacons.plutus"
paymentScript="${loanDir}payment_observer.plutus"
addressUpdateScript="${loanDir}address_update_observer.plutus"

negotiationRedeemer="${loanDir}negotiation_beacons_redeemer.plutus"
paymentRedeemer="${loanDir}payment_observer_redeemer.plutus"
addressUpdateRedeemer="${loanDir}address_update_observer_redeemer.plutus"

negotiationCert="${tmpDir}negotiation_beacons.cert"
paymentCert="${tmpDir}payment_observer.cert"
addressUpdateCert="${tmpDir}address_update_observer.cert"

## Export the scripts.
echo "Exporting the scripts..."
cardano-loans scripts \
  --negotiation-script \
  --out-file $negotiationScript

cardano-loans scripts \
  --payment-script \
  --out-file $paymentScript

cardano-loans scripts \
  --address-update-script \
  --out-file $addressUpdateScript

echo "Exporting the redeemers..."
cardano-loans redeemers negotiation-script register \
  --out-file $negotiationRedeemer

cardano-loans redeemers payment-script register \
  --out-file $paymentRedeemer

cardano-loans redeemers address-update-script register \
  --out-file $addressUpdateRedeemer

## Create the registration certificates.
echo "Creating the registration certificates..."
cardano-cli conway stake-address registration-certificate \
  --key-reg-deposit-amt 2000000 \
  --stake-script-file $negotiationScript \
  --out-file $negotiationCert

cardano-cli conway stake-address registration-certificate \
  --key-reg-deposit-amt 2000000 \
  --stake-script-file $paymentScript \
  --out-file $paymentCert

cardano-cli conway stake-address registration-certificate \
  --key-reg-deposit-amt 2000000 \
  --stake-script-file $addressUpdateScript \
  --out-file $addressUpdateCert

## Create and submit the transaction.
echo "Building the transaction..."
cardano-cli conway transaction build \
  --tx-in bb6f2dccf504ac11c6dc1150780fd660aa19ab9d7a048a3f2c045b5dd127c011#1 \
  --change-address "$(cat "${walletDir}01.addr")" \
  --certificate-file $negotiationCert \
  --certificate-tx-in-reference 6ccbd6cdff5c6305db5f249765941a79bd4fd12c1eaf451156795fbb194a5662#0 \
  --certificate-plutus-script-v2 \
  --certificate-reference-tx-in-redeemer-file $negotiationRedeemer \
  --certificate-file $paymentCert \
  --certificate-tx-in-reference 31555c4be564002ff811f9a15097991107e338a5cc9beaa10a0a037a84b36c21#0 \
  --certificate-plutus-script-v2 \
  --certificate-reference-tx-in-redeemer-file $paymentRedeemer \
  --certificate-file $addressUpdateCert \
  --certificate-tx-in-reference 6db101e5af2d2561add8aedbefefb5582b1830d824effa9555234b07bf50ddf9#0 \
  --certificate-plutus-script-v2 \
  --certificate-reference-tx-in-redeemer-file $addressUpdateRedeemer \
  --tx-in-collateral 4cc5755712fee56feabad637acf741bc8c36dda5f3d6695ac6487a77c4a92d76#0 \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.body"

cardano-cli conway transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file "${walletDir}/01.skey" \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli conway transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"
