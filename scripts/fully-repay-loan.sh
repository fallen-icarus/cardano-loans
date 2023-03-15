#!/bin/sh

# A helper script for showing how to make a partial loan payment as a borrower.

## Variables
dir="../assets/loan-files/"
tmpDir="../assets/tmp/"

loanScriptFile="${dir}loan.plutus"
beaconPolicyFile="${dir}beacons.plutus"

borrowerPubKeyFile="../assets/wallets/01Stake.vkey"
borrowerPubKeyHashFile="../assets/wallets/01Stake.pkh"

### This is the lender's ID.
lenderPubKeyHash="ae0d001455a855e6c00f98fa9061028f5c00d297926383bc501be2d2"

loanAddrFile="${dir}loan.addr"

repayDatumFile="${dir}repayDatum.json"

repayRedeemerFile="${dir}repayRedeemer.json"

beaconRedeemerFile="${dir}burn.json"

### The time used for repayment.
tte=23212309

### This is the hexidecimal encoding for 'Active'.
activeTokenName="416374697665"

## Export the loan validator script.
cabal run exe:cardano-loans -- export-script \
  --loan-script \
  --out-file $loanScriptFile

## Generate the hash for the staking verification key.
cardano-cli stake-address key-hash \
  --stake-verification-key-file $borrowerPubKeyFile \
  --out-file $borrowerPubKeyHashFile

## Create the AcceptOffer redeemer for the loan validator.
cabal run exe:cardano-loans -- borrower repay \
  --out-file $repayRedeemerFile

## Create the Active datum for a loan repayment.
cabal run exe:cardano-loans -- borrower loan-payment-datum \
  --lender-payment-pubkey-hash $lenderPubKeyHash \
  --borrower-stake-pubkey-hash $(cat $borrowerPubKeyHashFile) \
  --loan-asset-is-lovelace \
  --principle 10000000 \
  --loan-term 3600 \
  --loan-interest-numerator 1 \
  --loan-interest-denominator 10 \
  --required-backing 10000000 \
  --collateral-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --collateral-asset-token-name 4f74686572546f6b656e0a \
  --collateral-rate-numerator 1 \
  --collateral-rate-denominator 500000 \
  --expiration-time 1678897943000 \
  --current-balance-numerator 6000000 \
  --current-balance-denominator 1 \
  --payment-amount 6000000 \
  --out-file $repayDatumFile

## Export the beacon policy.
cabal run exe:cardano-loans -- export-script \
  --beacon-policy \
  --out-file $beaconPolicyFile

## Create the BurnBeaconToken beacon policy redeemer.
cabal run exe:cardano-loans -- lender burn-beacons \
  --out-file $beaconRedeemerFile

## Get the beacon policy id.
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file $beaconPolicyFile)

## Helper beacon variables
lenderBeacon="${beaconPolicyId}.${lenderPubKeyHash}"
activeBeacon="${beaconPolicyId}.${activeTokenName}"

borrowerPubKeyHash=$(cat $borrowerPubKeyHashFile)
borrowerBeacon="${beaconPolicyId}.${borrowerPubKeyHash}"

## Create and submit the transaction.
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in 7bbdd65e6fae6c66d8c3b03973adbf9b1eb6c6a689ffe8ecbd4ef71dbf92a5c9#1 \
  --tx-in 593b6321410d537e7781b12fbaf44af29b313bbdda431a757084c8f5068d6513#1 \
  --tx-in 4763325930557dd5e4835bb0e6f1592b815673aa861d6ea0b602abe277f353e2#0 \
  --tx-in-script-file $loanScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $repayRedeemerFile \
  --tx-out "$(cat ${loanAddrFile}) + 14000000 lovelace + 1 ${activeBeacon} + 1 ${lenderBeacon}" \
  --tx-out-inline-datum-file $repayDatumFile \
  --tx-out "$(cat ../assets/wallets/01.addr) + 10000000 lovelace + 501 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --required-signer-hash "$(cat $borrowerPubKeyHashFile)" \
  --mint "-1 ${borrowerBeacon}" \
  --mint-script-file $beaconPolicyFile \
  --mint-redeemer-file $beaconRedeemerFile \
  --change-address "$(cat ../assets/wallets/01.addr)" \
  --tx-in-collateral d5046a4d5a9c0a0ec6a9eabd0eb1524d54c3473459889b67ec17604f3c2e861b#0 \
  --testnet-magic 1 \
  --protocol-params-file "${tmpDir}protocol.json" \
  --invalid-hereafter $tte \
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