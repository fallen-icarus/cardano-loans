#!/bin/sh

# A helper script for showing how to clean up a finished loan as the borrower. Doing this enables the
# borrower to reclaim the minUTxOValue that must be stored with the beacons for Active loans.

## Variables
dir="../assets/loan-files/"
tmpDir="../assets/tmp/"

beaconPolicyFile="${dir}beacons.plutus"

borrowerPubKeyFile="../assets/wallets/01Stake.vkey"

unlockRedeemerFile="${dir}unlock.json"
beaconRedeemerFile="${dir}burn.json"

currentTime=33319551

### This is the hexidecimal encoding for 'Active'.
activeTokenName="416374697665"

### This is the loan's ID.
loanId="8e691c6075dbc9c30536670a4e00023d33fe7041de690887504f79fc4b1949d1"

## Generate the hash for the staking verification key.
echo "Calculating the hash of the borrower's stake verification key..."
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

## Create the Unlock redeemer.
echo "Creating the spending redeemer..."
cardano-loans loan-redeemer unlock \
  --out-file $unlockRedeemerFile

## Helper beacon variables
activeBeacon="${beaconPolicyId}.${activeTokenName}"
loanIdBeacon="${beaconPolicyId}.${loanId}"

## Create and submit the transaction.
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in 6def0fb5c759ef91cf0120616a5012a33adfedaa82f67d6b83279ad1d0ebda56#0 \
  --spending-tx-in-reference 1a7b174caeeeddd9b47245e17e31d7593007a145fbcf863ba7167202fc883091#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $unlockRedeemerFile \
  --mint "-1 ${activeBeacon} + -1 ${loanIdBeacon}" \
  --mint-tx-in-reference 4012535189e894bc7a1e35d7f80c41b3474a289cd788ae73ab0929f603a4983a#0 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id $beaconPolicyId \
  --required-signer-hash $borrowerPubKeyHash \
  --change-address "$(cat ../assets/wallets/01.addr)" \
  --tx-in-collateral 80b6d884296198d7eaa37f97a13e2d8ac4b38990d8419c99d6820bed435bbe82#0 \
  --testnet-magic 1 \
  --protocol-params-file "${tmpDir}protocol.json" \
  --invalid-before $currentTime \
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