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
cardano-cli conway transaction build \
  --tx-in 575c63eff95683a81f97e4f34fc7caedac5045bdb0c7ed79d93bb6905dcebc31#1 \
  --tx-in 575c63eff95683a81f97e4f34fc7caedac5045bdb0c7ed79d93bb6905dcebc31#0 \
  --tx-out "$(cat "${walletDir}01.addr") + 22000000 lovelace" \
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
