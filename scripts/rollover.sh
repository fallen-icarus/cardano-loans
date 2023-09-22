#!/bin/sh

# A helper script for showing how to rollover a loan once a checkpoint has passed.

## Variables
dir="../assets/loan-files/"
tmpDir="../assets/tmp/"

beaconPolicyFile="${dir}beacons.plutus"

borrowerPubKeyFile="../assets/wallets/01Stake.vkey"

loanAddrFile="${dir}loan.addr"

rolloverDatumFile="${dir}rolloverDatum.json"

rolloverRedeemerFile="${dir}rolloverRedeemer.json"

### This is the loan's ID.
loanId="07b2177d87a3e1bcd41548bcf2dca24038dbf09865fae80bf0b511229bf7f2df"

expirationTime=33330317 ### The slot where the loan will expire.

activeTokenName="416374697665" # This is the hexidecimal encoding for 'Active'.

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

## Create the Rollvoer redeemer for the loan validator.
echo "Creating the spending redeemer..."
cardano-loans loan-redeemer rollover \
  --out-file $rolloverRedeemerFile

## Create the new datum.
echo "Creating the new collateral active datum..."
cardano-loans datum rollover-datum \
  --beacon-policy-id $beaconPolicyId \
  --borrower-staking-pubkey-hash $borrowerPubKeyHash \
  --payment-pubkey-hash ae0d001455a855e6c00f98fa9061028f5c00d297926383bc501be2d2 \
  --loan-asset-is-lovelace \
  --principle 10000000 \
  --next-checkpoint 33328517 \
  --loan-term 3600 \
  --interest-numerator 1 \
  --interest-denominator 10 \
  --collateral-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --collateral-asset-token-name 4f74686572546f6b656e0a \
  --rate-numerator 1 \
  --rate-denominator 500000 \
  --claim-expiration 33333917 \
  --loan-expiration $expirationTime \
  --balance-numerator 10000000 \
  --balance-denominator 1 \
  --loan-id $loanId \
  --out-file $rolloverDatumFile

## Helper beacon variables
loanIdBeacon="${beaconPolicyId}.${loanId}"
activeBeacon="${beaconPolicyId}.${activeTokenName}"
borrowerBeacon="${beaconPolicyId}.${borrowerPubKeyHash}"

## Create and submit the transaction.
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in 4fd2ab62d72c8235b3c2dd725748b818fa9b3900c7bdc66ddafc0f827448d83c#4 \
  --tx-in 41b4fb1b684bd47571ebfd41a53e26b1968d34e38ab92dbb4f0d0e1d29e1bb06#0 \
  --spending-tx-in-reference 28bd750b45a4459f6b2d184e6ed504a4ba54a8b8b21c9cf0eaa42ed12b5ab004#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $rolloverRedeemerFile \
  --tx-out "$(cat ${loanAddrFile}) + 3000000 lovelace + 1 ${activeBeacon} + 1 ${loanIdBeacon} + 1 ${borrowerBeacon} + 20 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file $rolloverDatumFile \
  --required-signer-hash $borrowerPubKeyHash \
  --change-address "$(cat ../assets/wallets/01.addr)" \
  --tx-in-collateral 80b6d884296198d7eaa37f97a13e2d8ac4b38990d8419c99d6820bed435bbe82#0 \
  --invalid-hereafter $expirationTime \
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