#!/bin/sh

# A helper script for showing how to store the scripts on chain as reference scripts.

## Variables
mainDir="../../../ignored/"
walletDir="${mainDir}wallets/"
loanDir="${mainDir}loan-files/"
tmpDir="${mainDir}tmp/"

scriptFile="${loanDir}negotiation_beacons.plutus"

## Export the script.
echo "Exporting the negotiation beacon script..."
cardano-loans scripts \
  --negotiation-script \
  --out-file $scriptFile

## Create and submit the transaction.
cardano-cli transaction build \
  --tx-in 146f833828ae3e29ae8460847eaf5ce32102ebb9a23614d9ced7e48000f29a1b#1 \
  --tx-out "$(cat "${walletDir}01.addr") + 37000000 lovelace" \
  --tx-out-reference-script-file $scriptFile \
  --change-address "$(cat "${walletDir}01.addr")" \
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
