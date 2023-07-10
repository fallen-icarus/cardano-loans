#!/bin/sh

# A helper script for showing how to store the beacon policy on chain. The loan validator
# and beacon policy must be stored on chain in separate transactions due to their sizes.

## Variables
dir="../assets/loan-files/"
tmpDir="../assets/tmp/"

beaconPolicyFile="${dir}beacons.plutus"

## Export the beacon policy.
echo "Exporting the beacon policy..."
cardano-loans export-script beacon-policy \
  --out-file $beaconPolicyFile

## Create and submit the transaction.
cardano-cli transaction build \
  --tx-in 37ad56611ccf1f9c0ebf56f296cb064f7bca7d0691ae3ba2f6317a5b0fc01320#1 \
  --tx-in 4012535189e894bc7a1e35d7f80c41b3474a289cd788ae73ab0929f603a4983a#0 \
  --tx-out "$(cat ../assets/wallets/01.addr) + 64000000 lovelace " \
  --tx-out-reference-script-file $beaconPolicyFile \
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