#!/bin/sh

# A helper script for showing how to create an Ask as a borrower.

## Variables
dir="../assets/loan-files/"
tmpDir="../assets/tmp/"

loanScriptFile="${dir}loan.plutus"
beaconPolicyFile="${dir}beacons.plutus"

borrowerPubKeyFile="../assets/wallets/01Stake.vkey"
borrowerPubKeyHashFile="../assets/wallets/01Stake.pkh"

loanAddrFile="${dir}loan.addr"

askDatumFile="${dir}ask.json"

beaconRedeemerFile="${dir}mintAsk.json"

askTokenName="41736b" # This is the hexidecimal encoding for 'Ask'.

## Export the loan validator script.
cardano-loans export-script \
  --loan-script \
  --out-file $loanScriptFile

## Generate the hash for the staking verification key.
cardano-cli stake-address key-hash \
  --stake-verification-key-file $borrowerPubKeyFile \
  --out-file $borrowerPubKeyHashFile

## Create the loan address.
cardano-cli address build \
  --payment-script-file $loanScriptFile \
  --stake-verification-key-file $borrowerPubKeyFile \
  --testnet-magic 1 \
  --out-file $loanAddrFile

## Export the beacon policy.
cardano-loans export-script \
  --beacon-policy \
  --out-file $beaconPolicyFile

## Create the Ask datum.
cardano-loans borrower ask-datum \
  --borrower-stake-pubkey-hash "$(cat $borrowerPubKeyHashFile)" \
  --loan-asset-is-lovelace \
  --principle 10000000 \
  --loan-term 3600 \
  --collateral-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --collateral-asset-token-name 4f74686572546f6b656e0a \
  --out-file $askDatumFile

## Create the MintAskToken beacon policy redeemer.
cardano-loans borrower ask-beacon \
  --borrower-stake-pubkey-hash "$(cat $borrowerPubKeyHashFile)" \
  --out-file $beaconRedeemerFile

## Get the beacon policy id.
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file $beaconPolicyFile) 

## Helper Ask beacon variable
askBeacon="${beaconPolicyId}.${askTokenName}"

## Create and submit the transaction.
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in fd1feefe02c91dfb7a10187964a8442646d62835ce8cdd18bda1cea02da176dc#1 \
  --tx-out "$(cat ${loanAddrFile}) + 2000000 lovelace + 1 ${askBeacon}" \
  --tx-out-inline-datum-file $askDatumFile \
  --mint "1 ${askBeacon}" \
  --mint-script-file $beaconPolicyFile \
  --mint-redeemer-file $beaconRedeemerFile \
  --required-signer-hash "$(cat $borrowerPubKeyHashFile)" \
  --change-address "$(cat ../assets/wallets/01.addr)" \
  --tx-in-collateral d5046a4d5a9c0a0ec6a9eabd0eb1524d54c3473459889b67ec17604f3c2e861b#0 \
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