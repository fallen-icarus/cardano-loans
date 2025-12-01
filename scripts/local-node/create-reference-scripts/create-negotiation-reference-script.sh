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
cardano-cli conway transaction build \
  --tx-in 6ccbd6cdff5c6305db5f249765941a79bd4fd12c1eaf451156795fbb194a5662#0 \
  --tx-in 6ccbd6cdff5c6305db5f249765941a79bd4fd12c1eaf451156795fbb194a5662#1 \
  --tx-out "$(cat "${walletDir}01.addr") + 37000000 lovelace" \
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
