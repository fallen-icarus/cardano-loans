#!/bin/sh

# A helper script for showing how to make a partial loan payment as a borrower.

## Variables
dir="../assets/loan-files/"
tmpDir="../assets/tmp/"

loanScriptFile="${dir}loan.plutus"
beaconPolicyFile="${dir}beacons.plutus"

borrowerPubKeyFile="../assets/wallets/01Stake.vkey"

loanAddrFile="${dir}loan.addr"

repayDatumFile="${dir}repayDatum.json"

repayRedeemerFile="${dir}repayRedeemer.json"

### This is the lender's ID.
lenderPubKeyHash="ae0d001455a855e6c00f98fa9061028f5c00d297926383bc501be2d2"

expirationTime=26668590 ### The slot where the loan will expire.

activeTokenName="416374697665" # This is the hexidecimal encoding for 'Active'.

## Generate the hash for the staking verification key.
echo "Calculating the hash of the borrower's stake verification key..."
borrowerPubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file $borrowerPubKeyFile)

# ## Export the loan validator script.
# echo "Exporting the loan validator script..."
# cardano-loans export-script \
#   --loan-script \
#   --out-file $loanScriptFile

# ## Export the beacon policy.
# echo "Exporting the beacon policy script..."
# cardano-loans export-script \
#   --beacon-policy \
#   --out-file $beaconPolicyFile

## Get the beacon policy id.
echo "Calculating the beacon policy id..."
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file $beaconPolicyFile)

## Create the RepayLoan redeemer for the loan validator.
echo "Creating the spending redeemer..."
cardano-loans loan-redeemer \
  --repay \
  --out-file $repayRedeemerFile

## Create the Active datum for a loan repayment.
echo "Creating the new active datum..."
cardano-loans loan-datum payment-datum \
  --beacon-policy-id $beaconPolicyId \
  --lender-payment-pubkey-hash $lenderPubKeyHash \
  --borrower-stake-pubkey-hash $borrowerPubKeyHash \
  --loan-asset-is-lovelace \
  --principle 10000000 \
  --loan-term 3600 \
  --interest-numerator 1 \
  --interest-denominator 10 \
  --collateral-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --collateral-asset-token-name 4f74686572546f6b656e0a \
  --rate-numerator 1 \
  --rate-denominator 500000 \
  --expiration $expirationTime \
  --balance-numerator 11000000 \
  --balance-denominator 1 \
  --payment-amount 5000000 \
  --out-file $repayDatumFile

## Helper beacon variables
lenderBeacon="${beaconPolicyId}.${lenderPubKeyHash}"
activeBeacon="${beaconPolicyId}.${activeTokenName}"
borrowerBeacon="${beaconPolicyId}.${borrowerPubKeyHash}"

## Create and submit the transaction.
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in aadb8f997d84aa4ddbac4e2d49f01626758ded73cf778a9d76eabb5c21fefd55#2 \
  --tx-in aadb8f997d84aa4ddbac4e2d49f01626758ded73cf778a9d76eabb5c21fefd55#0 \
  --tx-in-script-file $loanScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $repayRedeemerFile \
  --tx-out "$(cat ${loanAddrFile}) + 8000000 lovelace + 1 ${activeBeacon} + 1 ${lenderBeacon} + 1 ${borrowerBeacon} + 20 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file $repayDatumFile \
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