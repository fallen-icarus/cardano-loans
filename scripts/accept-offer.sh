#!/bin/sh

# A helper script for showing how to accept an Offer as a borrower.

## Variables
dir="../assets/loan-files/"
tmpDir="../assets/tmp/"

loanScriptFile="${dir}loan.plutus"
beaconPolicyFile="${dir}beacons.plutus"

borrowerPubKeyFile="../assets/wallets/01Stake.vkey"
borrowerPubKeyHashFile="../assets/wallets/01Stake.pkh"

### This is the lender's ID. Change this for your target offer.
lenderPubKeyHash="ae0d001455a855e6c00f98fa9061028f5c00d297926383bc501be2d2"

loanAddrFile="${dir}loan.addr"

activeDatumFile="${dir}active.json"

beaconRedeemerFile="${dir}mintActive.json"
acceptOfferRedeemerFile="${dir}acceptOffer.json"

### This is the hexidecimal encoding for 'Active'.
activeTokenName="416374697665"

### This is the hexidecimal encoding for 'Ask'.
askTokenName="41736b"

### This is the hexidecimal encoding for 'Offer'.
offerTokenName="4f66666572"

start=23634704 # Slot number for start of loan.

## Export the loan validator script.
echo "Exporting the loan validator script..."
cardano-loans export-script \
  --loan-script \
  --out-file $loanScriptFile

## Generate the hash for the staking verification key.
echo "Calculating the borrower's stake pubkey hash..."
cardano-cli stake-address key-hash \
  --stake-verification-key-file $borrowerPubKeyFile \
  --out-file $borrowerPubKeyHashFile

## Export the beacon policy.
echo "Exporting the beacon policy script..."
cardano-loans export-script \
  --beacon-policy \
  --out-file $beaconPolicyFile

## Create the AcceptOffer redeemer for the loan validator.
echo "Creating the spending redeemer..."
cardano-loans borrower accept-offer \
  --out-file $acceptOfferRedeemerFile

## Create the Active datum for accepting an offer.
echo "Creating the active datum..."
cardano-loans borrower accept-offer-datum \
  --lender-payment-pubkey-hash $lenderPubKeyHash \
  --borrower-stake-pubkey-hash "$(cat $borrowerPubKeyHashFile)" \
  --loan-asset-is-lovelace \
  --principle 20000000 \
  --loan-term 3600 \
  --loan-interest-numerator 1 \
  --loan-interest-denominator 10 \
  --required-backing 10000000 \
  --collateral-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --collateral-asset-token-name 4f74686572546f6b656e0a \
  --collateral-rate-numerator 1 \
  --collateral-rate-denominator 500000 \
  --loan-start $start \
  --out-file $activeDatumFile

## Create the MintActiveToken beacon policy redeemer.
echo "Creating the mint redeemer..."
cardano-loans borrower active-beacon \
  --borrower-stake-pubkey-hash "$(cat $borrowerPubKeyHashFile)" \
  --lender-payment-pubkey-hash $lenderPubKeyHash \
  --out-file $beaconRedeemerFile

## Get the beacon policy id.
echo "Calculating the beacon policy id..."
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file $beaconPolicyFile)

## Helper beacon variables
askBeacon="${beaconPolicyId}.${askTokenName}"
offerBeacon="${beaconPolicyId}.${offerTokenName}"
lenderBeacon="${beaconPolicyId}.${lenderPubKeyHash}"
activeBeacon="${beaconPolicyId}.${activeTokenName}"

borrowerPubKeyHash=$(cat $borrowerPubKeyHashFile)
borrowerBeacon="${beaconPolicyId}.${borrowerPubKeyHash}"

## Create and submit the transaction.
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in 3b24d952689c62b51b704142375d2a1cc4b23bbea0ad209f39b590950253d785#1 \
  --tx-in 4b75518160c7e3df36b5754e8ed85f1988ea03f67bd7e92132b3c882a3338dc2#1 \
  --tx-in 06a486a9d7902bbf66786308faa29f0e0f99c7bdc8d9016b10cdeac29a988d8e#0 \
  --tx-in-script-file $loanScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $acceptOfferRedeemerFile \
  --tx-in 354de14e0933687d7030b6bd006c89de321bea8a451d4773c79ec6d3f15547f3#0 \
  --tx-in-script-file $loanScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $acceptOfferRedeemerFile \
  --tx-out "$(cat ${loanAddrFile}) + 3000000 lovelace + 1 ${activeBeacon} + 1 ${lenderBeacon} + 1 ${borrowerBeacon} + 40 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file $activeDatumFile \
  --tx-out "$(cat ../assets/wallets/01.addr) + 2000000 lovelace + 380 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --mint "1 ${activeBeacon} + 1 ${borrowerBeacon} + -1 ${askBeacon} + -1 ${offerBeacon}" \
  --mint-script-file $beaconPolicyFile \
  --mint-redeemer-file $beaconRedeemerFile \
  --required-signer-hash "$(cat $borrowerPubKeyHashFile)" \
  --change-address "$(cat ../assets/wallets/01.addr)" \
  --tx-in-collateral d5046a4d5a9c0a0ec6a9eabd0eb1524d54c3473459889b67ec17604f3c2e861b#0 \
  --testnet-magic 1 \
  --protocol-params-file "${tmpDir}protocol.json" \
  --invalid-before $start \
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