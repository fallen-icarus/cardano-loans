#!/bin/sh

# A helper script for showing how to store the loan validator on chain. The loan validator
# and beacon policy must be stored on chain in separate transactions due to their sizes.

## Variables
dir="../assets/loan-files/"
tmpDir="../assets/tmp/"

loanScriptFile="${dir}loan.plutus"

## Export the loan validator script.
echo "Exporting the loan validator script..."
cardano-loans export-script loan-script \
  --out-file $loanScriptFile

## Create and submit the transaction.
cardano-cli transaction build \
  --tx-in 0a61605c9ca946ed55842f7daf35efb91480872c8e8bc11ef6a4771438db4c41#3 \
  --tx-in 1a7b174caeeeddd9b47245e17e31d7593007a145fbcf863ba7167202fc883091#0 \
  --tx-out "$(cat ../assets/wallets/01.addr) + 57000000 lovelace " \
  --tx-out-reference-script-file $loanScriptFile \
  --change-address "$(cat ../assets/wallets/01.addr)" \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file ../assets/wallets/01.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"