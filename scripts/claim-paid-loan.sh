#!/bin/sh

# A helper script for showing how to claim an expired loan as a lender.

## Variables
dir="../assets/loan-files/"
tmpDir="../assets/tmp/"

loanScriptFile="${dir}loan.plutus"
beaconPolicyFile="${dir}beacons.plutus"

lenderPaymentPubKeyFile="../assets/wallets/02.vkey"
lenderPaymentPubKeyHashFile="../assets/wallets/02.pkh"

claimRedeemerFile="${dir}claim.json"
beaconRedeemerFile="${dir}burn.json"

### The time used for claiming.
ttl=23212437

### This is the hexidecimal encoding for 'Active'.
activeTokenName="416374697665"

## Export the loan validator script.
cardano-loans export-script \
  --loan-script \
  --out-file $loanScriptFile

## Generate the hash for the lender's payment pubkey.
cardano-cli address key-hash \
  --payment-verification-key-file $lenderPaymentPubKeyFile \
  --out-file $lenderPaymentPubKeyHashFile

## Export the beacon policy.
cardano-loans export-script \
  --beacon-policy \
  --out-file $beaconPolicyFile

## Create the BurnBeaconToken beacon policy redeemer.
cardano-loans lender burn-beacons \
  --out-file $beaconRedeemerFile

## Create the ClaimLoan redeemer.
cardano-loans lender claim-loan \
  --out-file $claimRedeemerFile

## Get the beacon policy id.
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file $beaconPolicyFile)

## Helper beacon variables
activeBeacon="${beaconPolicyId}.${activeTokenName}"

## Helper Lender ID beacon variable.
lenderPaymentPubKeyHash=$(cat $lenderPaymentPubKeyHashFile)
lenderBeacon="${beaconPolicyId}.${lenderPaymentPubKeyHash}"

## Create and submit the transaction.
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in b156b6ee95f8992cbd31e4688e878a993d8df03c78aa72e9fc6fafc05bcf6326#1 \
  --tx-in 00c8967e60dea640df632d9a05c3afdc6493b0c04007f27ba8557e018343b5a0#0 \
  --tx-in-script-file $loanScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $claimRedeemerFile \
  --required-signer-hash "$(cat $lenderPaymentPubKeyHashFile)" \
  --mint "-1 ${activeBeacon} + -1 ${lenderBeacon}" \
  --mint-script-file $beaconPolicyFile \
  --mint-redeemer-file $beaconRedeemerFile \
  --change-address "$(cat ../assets/wallets/02.addr)" \
  --tx-in-collateral 71c87734cbab0e152b3619b6506ab6f6cebbabe76f56c6a0605f41f5f5c51d91#0 \
  --testnet-magic 1 \
  --protocol-params-file "${tmpDir}protocol.json" \
  --invalid-before $ttl \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file ../assets/wallets/02.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"