#!/bin/sh

# A helper script for showing how to claim an expired loan as a lender.

## Variables
dir="../assets/loan-files/"
tmpDir="../assets/tmp/"

beaconPolicyFile="${dir}beacons.plutus" # This is used to get the policy id.

lenderPaymentPubKeyFile="../assets/wallets/02.vkey"

### Change this to your target borrower.
borrowerId="3cefec09a27b6894e2ed9a78b9cc01f083973d7c0afb8cec8bda33fa"

### Change this to the target loan id.
loanId="d448fe3ca2c4bd02dc06f6e5e139a3c9237ee8d01d785812984c114df6e395e4"

claimRedeemerFile="${dir}claim.json"
beaconRedeemerFile="${dir}burn.json"

### Expiration slot for loan + 1.
### Make sure to add 1 to the expiration slot number.
expirationTime=$((33064150 + 1))

### This is the hexidecimal encoding for 'Active'.
activeTokenName="416374697665"

## Generate the hash for the lender's payment pubkey.
echo "Calculating the hash of the lender's payment verification key..."
lenderPaymentPubKeyHash=$(cardano-cli address key-hash \
  --payment-verification-key-file $lenderPaymentPubKeyFile)

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

## Create the Claim redeemer.
echo "Creating the spending redeemer..."
cardano-loans loan-redeemer claim-expired \
  --out-file $claimRedeemerFile

## Helper beacon variables
loanIdBeacon="${beaconPolicyId}.${loanId}"
activeBeacon="${beaconPolicyId}.${activeTokenName}"
borrowerBeacon="${beaconPolicyId}.${borrowerId}"

## Create and submit the transaction.
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in b1019c825c156faf0e98c928d7a7128d32fc941500307b13edbcde3a67d778d3#1 \
  --tx-in b1019c825c156faf0e98c928d7a7128d32fc941500307b13edbcde3a67d778d3#0 \
  --spending-tx-in-reference 45551cf4568e6357b627f22df499a3ed59fa7b72a9b2fe0160cbad87ce11c902#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $claimRedeemerFile \
  --tx-out "$(cat ../assets/wallets/02.addr) + 2000000 lovelace + 20 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --required-signer-hash $lenderPaymentPubKeyHash \
  --mint "-1 ${activeBeacon} + -1 ${borrowerBeacon} + -2 ${loanIdBeacon}" \
  --mint-tx-in-reference e8bff224cd50b66c39a98e3708f981fba1a27e9aa89ce5986ae36597b2646042#0 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id $beaconPolicyId \
  --change-address "$(cat ../assets/wallets/02.addr)" \
  --tx-in-collateral 11ed603b92e6164c6bb0c83e0f4d54a954976db7c39e2a82d3cbf70f098da1e0#0 \
  --invalid-before $expirationTime \
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