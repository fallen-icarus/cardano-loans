#!/bin/sh

# A helper script for showing how to create an Ask as a borrower.

## Variables
dir="../assets/loan-files/"
tmpDir="../assets/tmp/"

loanScriptFile="${dir}loan.plutus"
beaconPolicyFile="${dir}beacons.plutus"

borrowerPubKeyFile="../assets/wallets/01Stake.vkey"

loanAddrFile="${dir}loan.addr"

askDatumFile="${dir}askDatum.json"

beaconRedeemerFile="${dir}mintAsk.json"

askTokenName="41736b" # This is the hexidecimal encoding for 'Ask'.

## Export the loan validator script.
echo "Exporting the loan validator script..."
cardano-loans export-script \
  --loan-script \
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
cardano-loans export-script \
  --beacon-policy \
  --out-file $beaconPolicyFile

## Get the beacon policy id.
echo "Calculating the beacon policy id..."
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file $beaconPolicyFile) 

## Create the Ask datum.
echo "Creating the ask datum..."
cardano-loans loan-datum ask-datum \
  --beacon-policy-id $beaconPolicyId \
  --borrower-stake-pubkey-hash $borrowerPubKeyHash \
  --loan-asset-is-lovelace \
  --principle 10000000 \
  --loan-term 3600 \
  --collateral-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --collateral-asset-token-name 4f74686572546f6b656e0a \
  --out-file $askDatumFile

## Create the MintAsk beacon policy redeemer.
echo "Creating the minting redeemer..."
cardano-loans beacon-redeemer mint-ask \
  --borrower-stake-pubkey-hash $borrowerPubKeyHash \
  --out-file $beaconRedeemerFile

## Helper Ask beacon variable
askBeacon="${beaconPolicyId}.${askTokenName}"

## Create and submit the transaction.
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in 115b6dc999bd75f78b3a82edd03d5f137b9523ee918137156492858afe2471e0#1 \
  --tx-out "$(cat ${loanAddrFile}) + 2000000 lovelace + 1 ${askBeacon}" \
  --tx-out-inline-datum-file $askDatumFile \
  --mint "1 ${askBeacon}" \
  --mint-script-file $beaconPolicyFile \
  --mint-redeemer-file $beaconRedeemerFile \
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