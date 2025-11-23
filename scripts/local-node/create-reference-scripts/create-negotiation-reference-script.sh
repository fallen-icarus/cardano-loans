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
  --tx-in f26ddef6d8c4ebc38bf5f62a4e4cbc00ea5a923d261365dc2c26f851444bc416#0 \
  --tx-in cd1f88ea5ee2bd98ca1ec0425df46bdbbaa91a6c4fea345d24963e3005a7e296#1 \
  --tx-out "$(cat "${walletDir}01.addr") + 36000000 lovelace" \
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
