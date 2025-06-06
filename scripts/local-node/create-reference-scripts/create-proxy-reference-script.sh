#!/bin/sh

# A helper script for showing how to store the scripts on chain as reference scripts.

## Variables
mainDir="../../../ignored/"
walletDir="${mainDir}wallets/"
loanDir="${mainDir}loan-files/"
tmpDir="${mainDir}tmp/"

scriptFile="${loanDir}proxy.plutus"

## Export the script.
echo "Exporting the proxy script..."
cardano-loans scripts \
  --proxy-script \
  --out-file $scriptFile

## Create and submit the transaction.
cardano-cli conway transaction build \
  --tx-in f46301d27eb9a6e8b8f4134bb974bb60530c2eb679caf8b04652786d0d807769#0 \
  --tx-in ef395a55df1e1ab6a807e7e0471655e0d9b9510cab8edc933d59b0c31f26ebc8#0 \
  --tx-out "$(cat "${walletDir}01.addr") + 4000000 lovelace" \
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
