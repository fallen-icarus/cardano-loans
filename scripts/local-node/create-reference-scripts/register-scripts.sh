#!/bin/sh

# A helper script for showing how to register the scripts for staking executions.

## Variables
mainDir="../../../ignored/"
walletDir="${mainDir}wallets/"
loanDir="${mainDir}loan-files/"
tmpDir="${mainDir}tmp/"

negotiationScript="${loanDir}negotiation_beacons.plutus"
paymentScript="${loanDir}payment_observer.plutus"
interestScript="${loanDir}interest_observer.plutus"
addressUpdateScript="${loanDir}address_update_observer.plutus"

negotiationCert="${tmpDir}negotiation_beacons.cert"
paymentCert="${tmpDir}payment_observer.cert"
interestCert="${tmpDir}interest_observer.cert"
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
  --interest-script \
  --out-file $interestScript

cardano-loans scripts \
  --address-update-script \
  --out-file $addressUpdateScript

## Create the registration certificates.
echo "Creating the registration certificates..."
cardano-cli stake-address registration-certificate \
  --stake-script-file $negotiationScript \
  --out-file $negotiationCert

cardano-cli stake-address registration-certificate \
  --stake-script-file $paymentScript \
  --out-file $paymentCert

cardano-cli stake-address registration-certificate \
  --stake-script-file $interestScript \
  --out-file $interestCert

cardano-cli stake-address registration-certificate \
  --stake-script-file $addressUpdateScript \
  --out-file $addressUpdateCert

## Create and submit the transaction.
cardano-cli transaction build \
  --tx-in a96248cd1788c4b435b8ff9268236f8fa5d62faaa6cb73d600de869e7361be40#1 \
  --change-address "$(cat "${walletDir}01.addr")" \
  --certificate-file $negotiationCert \
  --certificate-file $paymentCert \
  --certificate-file $interestCert \
  --certificate-file $addressUpdateCert \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file "${walletDir}/01.skey" \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"
