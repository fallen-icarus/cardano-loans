#!/bin/sh

# A helper script for showing how to make a partial loan payment as a borrower.

## Variables
dir="../assets/loan-files/"
tmpDir="../assets/tmp/"

loanScriptFile="${dir}loan.plutus"
beaconPolicyFile="${dir}beacons.plutus"

borrowerPubKeyFile="../assets/wallets/01Stake.vkey"
borrowerPubKeyHashFile="../assets/wallets/01Stake.pkh"

loanAddrFile="${dir}loan.addr"

repayDatumFile="${dir}repayDatum.json"

repayRedeemerFile="${dir}repayRedeemer.json"

### This is the lender's ID.
lenderPubKeyHash="ae0d001455a855e6c00f98fa9061028f5c00d297926383bc501be2d2"

paymentTime=23636081 ### The time used for repayment; can be the expiration time.

activeTokenName="416374697665" # This is the hexidecimal encoding for 'Active'.

## Export the loan validator script.
echo "Exporting the loan validator script..."
cardano-loans export-script \
  --loan-script \
  --out-file $loanScriptFile

## Export the beacon policy.
echo "Exporting the beacon policy script..."
cardano-loans export-script \
  --beacon-policy \
  --out-file $beaconPolicyFile

## Get the beacon policy id.
echo "Calculating the beacon policy id..."
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file $beaconPolicyFile)

## Generate the hash for the staking verification key.
echo "Calculating the hash of the borrower's stake verification key..."
cardano-cli stake-address key-hash \
  --stake-verification-key-file $borrowerPubKeyFile \
  --out-file $borrowerPubKeyHashFile

## Create the AcceptOffer redeemer for the loan validator.
echo "Creating the spending redeemer..."
cardano-loans borrower repay \
  --out-file $repayRedeemerFile

## Create the Active datum for a loan repayment.
echo "Creating the new active datum..."
cardano-loans borrower loan-payment-datum \
  --lender-payment-pubkey-hash $lenderPubKeyHash \
  --borrower-stake-pubkey-hash "$(cat $borrowerPubKeyHashFile)" \
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
  --expiration-time 1679319281000 \
  --current-balance-numerator 11000000 \
  --current-balance-denominator 1 \
  --payment-amount 5000000 \
  --out-file $repayDatumFile

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
  --tx-in f91c10b841170583a47ac9641ed7eda80166e6199d435e61636ab2d9997aa92c#0 \
  --tx-in 3b24d952689c62b51b704142375d2a1cc4b23bbea0ad209f39b590950253d785#0 \
  --tx-in-script-file $loanScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $repayRedeemerFile \
  --tx-out "$(cat ${loanAddrFile}) + 8000000 lovelace + 1 ${activeBeacon} + 1 ${lenderBeacon} + 1 ${borrowerBeacon} + 20 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file $repayDatumFile \
  --required-signer-hash "$(cat $borrowerPubKeyHashFile)" \
  --change-address "$(cat ../assets/wallets/01.addr)" \
  --tx-in-collateral d5046a4d5a9c0a0ec6a9eabd0eb1524d54c3473459889b67ec17604f3c2e861b#0 \
  --testnet-magic 1 \
  --protocol-params-file "${tmpDir}protocol.json" \
  --invalid-hereafter $paymentTime \
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