#!/bin/sh

# A helper script for showing how to store the scripts on chain as reference scripts.

## Variables
mainDir="../../../ignored/"
walletDir="${mainDir}wallets/"
loanDir="${mainDir}loan-files/"
tmpDir="${mainDir}tmp/"

scriptFile="${loanDir}active_beacons.plutus"

## Export the script.
echo "Exporting the active beacon script..."
cardano-loans scripts \
  --active-script \
  --out-file $scriptFile

## Create and submit the transaction.
cardano-cli conway transaction build \
  --tx-in 106d811ab7f51d2c84a16ac00e8c091d80c92b893809ac8c898c0b277361ad6d#0 \
  --tx-in 106d811ab7f51d2c84a16ac00e8c091d80c92b893809ac8c898c0b277361ad6d#1 \
  --tx-out "$(cat "${walletDir}01.addr") + 51000000 lovelace" \
  --tx-out-reference-script-file $scriptFile \
  --change-address "$(cat "${walletDir}01.addr")" \
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
