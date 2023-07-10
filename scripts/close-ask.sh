#!/bin/sh

# A helper script for showing how to close an Ask as a borrower.

## Variables
dir="../assets/loan-files/"
tmpDir="../assets/tmp/"

beaconPolicyFile="${dir}beacons.plutus" # This is used to get the beacon policy id.

borrowerPubKeyFile="../assets/wallets/01Stake.vkey"

beaconRedeemerFile="${dir}burnBeacons.json"
closeAskRedeemerFile="${dir}closeAsk.json"

askTokenName="41736b" # This is the hexidecimal encoding for 'Ask'.

## Generate the hash for the staking verification key.
echo "Calculating the hash for the borrower's staking pubkey..."
borrowerPubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file $borrowerPubKeyFile)

## Export the beacon policy.
echo "Exporting the beacon policy script..."
cardano-loans export-script beacon-policy \
  --out-file $beaconPolicyFile

## Get the beacon policy id.
echo "Calculating the beacon policy id..."
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file $beaconPolicyFile)

## Create the BurnBeacons beacon policy redeemer.
echo "Creating the burn redeemer..."
cardano-loans beacon-redeemer burn-beacons \
  --out-file $beaconRedeemerFile

## Create the CloseAsk redeemer for the loan validator.
echo "Creating the spending redeemer..."
cardano-loans loan-redeemer close-ask \
  --out-file $closeAskRedeemerFile

## Helper beacon variable
askBeacon="${beaconPolicyId}.${askTokenName}"

## Create and submit the transaction.
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in a1b40ef2375da448c604b30636140907e082ba2b23ea37096bb7c30e3fa46ae2#0 \
  --spending-tx-in-reference 1a7b174caeeeddd9b47245e17e31d7593007a145fbcf863ba7167202fc883091#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $closeAskRedeemerFile \
  --mint "-1 ${askBeacon}" \
  --mint-tx-in-reference 4012535189e894bc7a1e35d7f80c41b3474a289cd788ae73ab0929f603a4983a#0 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id $beaconPolicyId \
  --required-signer-hash $borrowerPubKeyHash \
  --change-address "$(cat ../assets/wallets/01.addr)" \
  --tx-in-collateral 80b6d884296198d7eaa37f97a13e2d8ac4b38990d8419c99d6820bed435bbe82#0 \
  --testnet-magic 1 \
  --protocol-params-file "${tmpDir}protocol.json" \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file ../assets/wallets/01.skey \
  --signing-key-file ../assets/wallets/01Stake.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"