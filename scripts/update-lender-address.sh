#!/bin/sh

# A helper script for showing how to update a lender's address for an active loan.

## Variables
dir="../assets/loan-files/"
tmpDir="../assets/tmp/"

beaconPolicyFile="${dir}beacons.plutus"

borrowerId="3cefec09a27b6894e2ed9a78b9cc01f083973d7c0afb8cec8bda33fa"

loanAddr="addr_test1zq7vqwep6eyw4jywc4cwrqmw0cwc5evx3266ceamqyeyaxfualkqngnmdz2w9mv60zuucq0sswtn6lq2lwxwez76x0aq9ggrvz"

updateDatumFile="${dir}updateDatum.json"

updateRedeemerFile="${dir}updateRedeemer.json"

### This is the loan's ID.
loanId="07b2177d87a3e1bcd41548bcf2dca24038dbf09865fae80bf0b511229bf7f2df"

activeTokenName="416374697665" # This is the hexidecimal encoding for 'Active'.

## Export the beacon policy.
echo "Exporting the beacon policy script..."
cardano-loans export-script beacon-policy \
  --out-file $beaconPolicyFile

## Get the beacon policy id.
echo "Calculating the beacon policy id..."
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file $beaconPolicyFile)

## Create the Update redeemer for the loan validator.
echo "Creating the spending redeemer..."
cardano-loans loan-redeemer update-address \
  --payment-pubkey-hash ae0d001455a855e6c00f98fa9061028f5c00d297926383bc501be2d2 \
  --staking-pubkey-hash 623a2b9a369454b382c131d7e3d12c4f93024022e5c5668cf0c5c25c \
  --out-file $updateRedeemerFile

## Create the new datum.
echo "Creating the new collateral datum..."
cardano-loans datum update-address-datum \
  --beacon-policy-id $beaconPolicyId \
  --borrower-staking-pubkey-hash $borrowerId \
  --payment-pubkey-hash ae0d001455a855e6c00f98fa9061028f5c00d297926383bc501be2d2 \
  --staking-pubkey-hash 623a2b9a369454b382c131d7e3d12c4f93024022e5c5668cf0c5c25c \
  --loan-asset-is-lovelace \
  --principle 10000000 \
  --past-checkpoint 33328517 \
  --loan-term 3600 \
  --interest-numerator 1 \
  --interest-denominator 10 \
  --collateral-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --collateral-asset-token-name 4f74686572546f6b656e0a \
  --rate-numerator 1 \
  --rate-denominator 500000 \
  --claim-expiration 33333917 \
  --loan-expiration 33330317 \
  --balance-numerator 11000000 \
  --balance-denominator 1 \
  --loan-id $loanId \
  --out-file $updateDatumFile

## Helper beacon variables
loanIdBeacon="${beaconPolicyId}.${loanId}"
activeBeacon="${beaconPolicyId}.${activeTokenName}"
borrowerBeacon="${beaconPolicyId}.${borrowerId}"

## Create and submit the transaction.
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in 2ea955c88872fef0fc15090e8eeacdf4534a0c33a4b373149b686724948c3eff#0 \
  --tx-in 41b4fb1b684bd47571ebfd41a53e26b1968d34e38ab92dbb4f0d0e1d29e1bb06#1 \
  --tx-in 884bc0345443ca14b4e71586abf6c4019ae8732f73e8bd36f15f1fd930529792#0 \
  --spending-tx-in-reference 28bd750b45a4459f6b2d184e6ed504a4ba54a8b8b21c9cf0eaa42ed12b5ab004#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $updateRedeemerFile \
  --tx-out "${loanAddr} + 3000000 lovelace + 1 ${activeBeacon} + 1 ${loanIdBeacon} + 1 ${borrowerBeacon} + 20 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file $updateDatumFile \
  --tx-out "$(cat ../assets/wallets/02.addr) + 2000000 lovelace + 1 0f775d969877cc0f876bed21b80443e35934c1952498a78bb637f006.07b2177d87a3e1bcd41548bcf2dca24038dbf09865fae80bf0b511229bf7f2df" \
  --change-address "$(cat ../assets/wallets/02.addr)" \
  --tx-in-collateral 11ed603b92e6164c6bb0c83e0f4d54a954976db7c39e2a82d3cbf70f098da1e0#0 \
  --testnet-magic 1 \
  --protocol-params-file "${tmpDir}protocol.json" \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file ../assets/wallets/02.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"