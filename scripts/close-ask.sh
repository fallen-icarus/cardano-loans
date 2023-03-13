#!/bin/sh

# A helper script for showing how to close an Ask as a borrower.

## Variables
dir="../assets/loan-files/"
tmpDir="../assets/tmp/"

loanScriptFile="${dir}loan.plutus"
beaconPolicyFile="${dir}beacons.plutus"

borrowerPubKeyFile="../assets/wallets/01Stake.vkey"
borrowerPubKeyHashFile="../assets/wallets/01Stake.pkh"

loanAddrFile="${dir}loan.addr"

beaconRedeemerFile="${dir}burnBeacons.json"
closeAskRedeemerFile="${dir}closeAsk.json"

### This is the hexidecimal encoding for 'Ask'.
askTokenName="41736b"

## Export the loan validator script.
cabal run exe:cardano-loans -- export-script \
  --loan-script \
  --out-file $loanScriptFile

## Export the beacon policy.
cabal run exe:cardano-loans -- export-script \
  --beacon-policy \
  --out-file $beaconPolicyFile

## Create the BurnBeaconToken beacon policy redeemer.
cabal run exe:cardano-loans -- borrower burn-beacons \
  --out-file $beaconRedeemerFile

## Create the CloseAsk redeemer for the loan validator.
cabal run exe:cardano-loans -- borrower close-ask \
  --out-file $closeAskRedeemerFile

## Get the beacon policy id.
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file $beaconPolicyFile)

## Helper beacon variable
askBeacon="${beaconPolicyId}.${askTokenName}"

## Create and submit the transaction.
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in c9cf51fd9d947918cc79f05ec7728173a6141873ead6a27f4845792e0c573b16#1 \
  --tx-in c9cf51fd9d947918cc79f05ec7728173a6141873ead6a27f4845792e0c573b16#0 \
  --tx-in-script-file $loanScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $closeAskRedeemerFile \
  --mint "-1 ${askBeacon}" \
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