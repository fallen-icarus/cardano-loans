#!/bin/sh

# A helper script for showing how to store the scripts on chain as reference scripts.

## Variables
mainDir="../../../ignored/"
walletDir="${mainDir}wallets/"
loanDir="${mainDir}loan-files/"
tmpDir="${mainDir}tmp/"

scriptFile="${loanDir}payment_observer.plutus"

## Export the script.
echo "Exporting the payment observer script..."
cardano-loans scripts \
  --payment-script \
  --out-file $scriptFile

## Create and submit the transaction.
cardano-cli transaction build \
  --tx-in 292f25c6594169502c71ee82cd5285bba9a887a60a3b447bade71284acb172db#1 \
  --tx-out "$(cat "${walletDir}01.addr") + 39000000 lovelace" \
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
