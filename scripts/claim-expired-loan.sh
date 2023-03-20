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
claimTime=23638305

### This is the hexidecimal encoding for 'Active'.
activeTokenName="416374697665"

## Export the loan validator script.
echo "Exporting the loan validator script..."
cardano-loans export-script \
  --loan-script \
  --out-file $loanScriptFile

## Generate the hash for the lender's payment pubkey.
echo "Calculating the hash of the lender's payment verification key..."
cardano-cli address key-hash \
  --payment-verification-key-file $lenderPaymentPubKeyFile \
  --out-file $lenderPaymentPubKeyHashFile

## Export the beacon policy.
echo "Exporting the beacon policy script..."
cardano-loans export-script \
  --beacon-policy \
  --out-file $beaconPolicyFile

## Create the BurnBeaconToken beacon policy redeemer.
echo "Creating the burn redeemer..."
cardano-loans lender burn-beacons \
  --out-file $beaconRedeemerFile

## Create the ClaimLoan redeemer.
echo "Creating the spending redeemer..."
cardano-loans lender claim-loan \
  --out-file $claimRedeemerFile

## Get the beacon policy id.
echo "Calculating the beacon policy id..."
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
  --tx-in 76a5888a1efe6199b73487824b1ecba06e8a5f0382ec32eb0f2a1052a145fc37#1 \
  --tx-in 48c65de6874878bf6025c83c74788373ac8e61c442eed74ab31b7238cd649fd9#0 \
  --tx-in-script-file $loanScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $claimRedeemerFile \
  --tx-out "$(cat ../assets/wallets/02.addr) + 2000000 lovelace + 40 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --required-signer-hash "$(cat $lenderPaymentPubKeyHashFile)" \
  --mint "-1 ${activeBeacon} + -1 ${borrowerBeacon} + -1 ${lenderBeacon}" \
  --mint-script-file $beaconPolicyFile \
  --mint-redeemer-file $beaconRedeemerFile \
  --change-address "$(cat ../assets/wallets/02.addr)" \
  --tx-in-collateral 71c87734cbab0e152b3619b6506ab6f6cebbabe76f56c6a0605f41f5f5c51d91#0 \
  --testnet-magic 1 \
  --protocol-params-file "${tmpDir}protocol.json" \
  --invalid-before $claimTime \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file ../assets/wallets/02.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"