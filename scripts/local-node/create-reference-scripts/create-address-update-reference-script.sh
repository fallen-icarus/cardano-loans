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
cardano-cli conway transaction build \
  --tx-in e8afb1899c37bb49bc0a26cceac447d3b6ac243532552f874258a9eccd06e257#0 \
  --tx-in e8afb1899c37bb49bc0a26cceac447d3b6ac243532552f874258a9eccd06e257#1 \
  --tx-out "$(cat "${walletDir}01.addr") + 24000000 lovelace" \
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
