#!/bin/sh

# A helper script for showing how to store the scripts on chain as reference scripts.

## Variables
mainDir="../../../ignored/"
walletDir="${mainDir}wallets/"
loanDir="${mainDir}loan-files/"
tmpDir="${mainDir}tmp/"

scriptFile="${loanDir}loan.plutus"

## Export the script.
echo "Exporting the loan script..."
cardano-loans scripts \
  --loan-script \
  --out-file $scriptFile

## Create and submit the transaction.
cardano-cli transaction build \
  --tx-in 9f4cad48231f7aa23008e244e92b96c75fdf56949d2244870cd46b73547b4b4a#1 \
  --tx-out "$(cat "${walletDir}01.addr") + 23000000 lovelace" \
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
