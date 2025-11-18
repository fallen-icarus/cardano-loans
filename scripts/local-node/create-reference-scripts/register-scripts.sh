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
  --tx-in 6ad871a70308dd5d0e63ab6b1c7bf747c00a03c839ffdef7232b480097d6b3ac#1 \
  --change-address "$(cat "${walletDir}01.addr")" \
  --certificate-file $negotiationCert \
  --certificate-tx-in-reference 6014d14585f7d9f9c0c8328f63e632986549ca691b5c56a7ecf14773441d8e0e#0 \
  --certificate-plutus-script-v2 \
  --certificate-reference-tx-in-redeemer-file $negotiationRedeemer \
  --certificate-file $paymentCert \
  --certificate-tx-in-reference 6ad871a70308dd5d0e63ab6b1c7bf747c00a03c839ffdef7232b480097d6b3ac#0 \
  --certificate-plutus-script-v2 \
  --certificate-reference-tx-in-redeemer-file $paymentRedeemer \
  --certificate-file $addressUpdateCert \
  --certificate-tx-in-reference c07da9e3f71e7582f6d11efb2e92d3d2c3ca2f23017373b0a9cd63f03138eeb0#0 \
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
