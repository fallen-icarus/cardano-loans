#!/bin/sh

# A helper script for showing how to store the scripts on chain as reference scripts.

## Variables
mainDir="../../../ignored/"
walletDir="${mainDir}wallets/"
loanDir="${mainDir}loan-files/"
tmpDir="${mainDir}tmp/"

scriptFile="${loanDir}interest_observer.plutus"

## Export the script.
echo "Exporting the interest observer script..."
cardano-loans scripts \
  --interest-script \
  --out-file $scriptFile

## Create and submit the transaction.
cardano-cli transaction build \
  --tx-in 6c87bb937fd938a4dd6dc65e0bc0740c55b1acad331740922b0f865a08cb54e8#1 \
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
