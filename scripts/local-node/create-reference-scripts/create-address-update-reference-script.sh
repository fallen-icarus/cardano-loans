#!/bin/sh

# A helper script for showing how to store the scripts on chain as reference scripts.

## Variables
mainDir="../../../ignored/"
walletDir="${mainDir}wallets/"
loanDir="${mainDir}loan-files/"
tmpDir="${mainDir}tmp/"

scriptFile="${loanDir}address_update_observer.plutus"

## Export the script.
echo "Exporting the address update observer script..."
cardano-loans scripts \
  --address-update-script \
  --out-file $scriptFile

## Create and submit the transaction.
cardano-cli transaction build \
  --tx-in a7bd532b8c570667685fb1014b7ed47372ddab3e30bfb567a01c239833f966f0#1 \
  --tx-out "$(cat "${walletDir}01.addr") + 24000000 lovelace" \
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
