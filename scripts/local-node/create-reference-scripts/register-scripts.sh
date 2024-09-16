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

negotiationRedeemer="${loanDir}negotiation_beacons_redeemer.plutus"
paymentRedeemer="${loanDir}payment_observer_redeemer.plutus"
interestRedeemer="${loanDir}interest_observer_redeemer.plutus"
addressUpdateRedeemer="${loanDir}address_update_observer_redeemer.plutus"

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

echo "Exporting the redeemers..."
cardano-loans redeemers negotiation-script register \
  --out-file $negotiationRedeemer

cardano-loans redeemers payment-script register \
  --out-file $paymentRedeemer

cardano-loans redeemers interest-script register \
  --out-file $interestRedeemer

cardano-loans redeemers address-update-script register \
  --out-file $addressUpdateRedeemer

## Create the registration certificates.
echo "Creating the registration certificates..."
cardano-cli stake-address registration-certificate \
  --conway-era \
  --key-reg-deposit-amt 2000000 \
  --stake-script-file $negotiationScript \
  --out-file $negotiationCert

cardano-cli stake-address registration-certificate \
  --conway-era \
  --key-reg-deposit-amt 2000000 \
  --stake-script-file $paymentScript \
  --out-file $paymentCert

cardano-cli stake-address registration-certificate \
  --conway-era \
  --key-reg-deposit-amt 2000000 \
  --stake-script-file $interestScript \
  --out-file $interestCert

cardano-cli stake-address registration-certificate \
  --conway-era \
  --key-reg-deposit-amt 2000000 \
  --stake-script-file $addressUpdateScript \
  --out-file $addressUpdateCert

## Create and submit the transaction.
echo "Building the transaction..."
cardano-cli conway transaction build \
  --tx-in 670526b45c968321def17ca18cf0e507383e7fd596c45dd5a87b92d1f97943bc#1 \
  --change-address "$(cat "${walletDir}01.addr")" \
  --certificate-file $negotiationCert \
  --certificate-tx-in-reference a3ae17130ddbf4ce3117e218c920d219599ff935d024fac0d3ca4ef9ad6e4fde#0 \
  --certificate-plutus-script-v2 \
  --certificate-reference-tx-in-redeemer-file $negotiationRedeemer \
  --certificate-file $paymentCert \
  --certificate-tx-in-reference 670526b45c968321def17ca18cf0e507383e7fd596c45dd5a87b92d1f97943bc#0 \
  --certificate-plutus-script-v2 \
  --certificate-reference-tx-in-redeemer-file $paymentRedeemer \
  --certificate-file $interestCert \
  --certificate-tx-in-reference afb8696b7025e9cea04bd7a51834d6a18af2936ffd8c7947ebf673631a9dbb3d#0 \
  --certificate-plutus-script-v2 \
  --certificate-reference-tx-in-redeemer-file $interestRedeemer \
  --certificate-file $addressUpdateCert \
  --certificate-tx-in-reference fd9a97914e3553996c6cf631c53f9077bdd4289ca06e1eb34fcbb4d0e4c351ff#0 \
  --certificate-plutus-script-v2 \
  --certificate-reference-tx-in-redeemer-file $addressUpdateRedeemer \
  --tx-in-collateral 95de5a789ccedbf1444b8668e5724aa4ed95d799d616aae1e4d408d56c2c3485#0 \
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
