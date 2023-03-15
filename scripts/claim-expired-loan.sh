#!/bin/sh

# A helper script for showing how to claim an expired loan as a lender.

## Variables
dir="../assets/loan-files/"
tmpDir="../assets/tmp/"

loanScriptFile="${dir}loan.plutus"
beaconPolicyFile="${dir}beacons.plutus"

lenderPaymentPubKeyFile="../assets/wallets/02.vkey"
lenderPaymentPubKeyHashFile="../assets/wallets/02.pkh"

### Change this to your target borrower.
borrowerPubKeyHash="3cefec09a27b6894e2ed9a78b9cc01f083973d7c0afb8cec8bda33fa"

claimRedeemerFile="${dir}claim.json"
beaconRedeemerFile="${dir}burn.json"

### The time used for claiming.
ttl=23217543

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
borrowerBeacon="${beaconPolicyId}.${borrowerPubKeyHash}"

## Helper Lender ID beacon variable.
lenderPaymentPubKeyHash=$(cat $lenderPaymentPubKeyHashFile)
lenderBeacon="${beaconPolicyId}.${lenderPaymentPubKeyHash}"

## Create and submit the transaction.
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in 4a991394b4c5255b8e7432606078ddcfaf4e5ba0452d52e646461c61a50c31a6#0 \
  --tx-in 1027dabb7aedbc20217adac671beede6b7330494c90fb70bd3b52450a5dd0422#0 \
  --tx-in-script-file $loanScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $claimRedeemerFile \
  --tx-out "$(cat ../assets/wallets/02.addr) + 2000000 lovelace + 20 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --required-signer-hash "$(cat $lenderPaymentPubKeyHashFile)" \
  --mint "-1 ${activeBeacon} + -1 ${borrowerBeacon} + -1 ${lenderBeacon}" \
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