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
  --tx-in b1c2aa05c55a83f92418caeecdb562c7466cff6643c697de16ebfbe16ff0b8ba#0 \
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
