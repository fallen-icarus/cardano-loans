#!/bin/sh

# A helper script for showing how to create an Ask as a borrower.

## Variables
dir="../assets/loan-files/"
tmpDir="../assets/tmp/"

loanScriptFile="${dir}loan.plutus" # This is used to create the address.
beaconPolicyFile="${dir}beacons.plutus" # This is used to get the beacon policy id.

borrowerPubKeyFile="../assets/wallets/01Stake.vkey"

loanAddrFile="${dir}loan.addr"

askDatumFile="${dir}askDatum.json"

beaconRedeemerFile="${dir}mintAsk.json"

askTokenName="41736b" # This is the hexidecimal encoding for 'Ask'.

## Export the loan validator script.
echo "Exporting the loan validator script..."
cardano-loans export-script loan-script \
  --out-file $loanScriptFile

## Generate the hash for the staking verification key.
echo "Calculating the staking pubkey hash for the borrower..."
borrowerPubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file $borrowerPubKeyFile)

## Create the loan address.
echo "Creating the borrower's loan address..."
cardano-cli address build \
  --payment-script-file $loanScriptFile \
  --stake-verification-key-file $borrowerPubKeyFile \
  --testnet-magic 1 \
  --out-file $loanAddrFile

## Export the beacon policy.
echo "Exporting the beacon policy script..."
cardano-loans export-script beacon-policy \
  --out-file $beaconPolicyFile

## Get the beacon policy id.
echo "Calculating the beacon policy id..."
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file $beaconPolicyFile) 

## Create the Ask datum.
echo "Creating the ask datum..."
cardano-loans datum ask-datum \
  --beacon-policy-id $beaconPolicyId \
  --borrower-staking-pubkey-hash $borrowerPubKeyHash \
  --loan-asset-is-lovelace \
  --principle 10000000 \
  --loan-term 3600 \
  --collateral-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --collateral-asset-token-name 4f74686572546f6b656e0a \
  --out-file $askDatumFile

## Create the MintAsk beacon policy redeemer.
echo "Creating the minting redeemer..."
cardano-loans beacon-redeemer mint-ask \
  --borrower-staking-pubkey-hash $borrowerPubKeyHash \
  --out-file $beaconRedeemerFile

## Helper Ask beacon variable
askBeacon="${beaconPolicyId}.${askTokenName}"

## Create and submit the transaction.
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in 6def0fb5c759ef91cf0120616a5012a33adfedaa82f67d6b83279ad1d0ebda56#3 \
  --tx-out "$(cat ${loanAddrFile}) + 2000000 lovelace + 1 ${askBeacon}" \
  --tx-out-inline-datum-file $askDatumFile \
  --mint "1 ${askBeacon}" \
  --mint-tx-in-reference 0f94a13cf0207e9a322c15d52d372dc04bd292160277f94e4fc5fbad5598a209#0 \
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