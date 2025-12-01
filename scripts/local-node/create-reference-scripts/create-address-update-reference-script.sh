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
  --tx-in 6db101e5af2d2561add8aedbefefb5582b1830d824effa9555234b07bf50ddf9#0 \
  --tx-in 6db101e5af2d2561add8aedbefefb5582b1830d824effa9555234b07bf50ddf9#1 \
  --tx-out "$(cat "${walletDir}01.addr") + 25000000 lovelace" \
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
