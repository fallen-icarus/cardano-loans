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

ttl=23211143

## Export the loan validator script.
cardano-loans export-script \
  --loan-script \
  --out-file $loanScriptFile

## Generate the hash for the staking verification key.
cardano-cli stake-address key-hash \
  --stake-verification-key-file $borrowerPubKeyFile \
  --out-file $borrowerPubKeyHashFile

## Export the beacon policy.
cardano-loans export-script \
  --beacon-policy \
  --out-file $beaconPolicyFile

## Create the AcceptOffer redeemer for the loan validator.
cardano-loans borrower accept-offer \
  --out-file $acceptOfferRedeemerFile

## Create the Active datum for accepting an offer.
cardano-loans borrower accept-offer-datum \
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
  --loan-ttl $ttl \
  --out-file $activeDatumFile

## Create the MintActiveToken beacon policy redeemer.
cardano-loans borrower active-beacon \
  --borrower-stake-pubkey-hash "$(cat $borrowerPubKeyHashFile)" \
  --lender-payment-pubkey-hash $lenderPubKeyHash \
  --out-file $beaconRedeemerFile

## Get the beacon policy id.
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
  --tx-in 1027dabb7aedbc20217adac671beede6b7330494c90fb70bd3b52450a5dd0422#1 \
  --tx-in 0ff5e4c90ba3ec2e128850bc4772fd1b6f57a17ae96e3b55b87ded8bcd7d4983#0 \
  --tx-in 593b6321410d537e7781b12fbaf44af29b313bbdda431a757084c8f5068d6513#0 \
  --tx-in-script-file $loanScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $acceptOfferRedeemerFile \
  --tx-in 77c734e1d554b951719352c60c5976827a948a0aac012ccd31dac95643ce6f9f#0 \
  --tx-in-script-file $loanScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $acceptOfferRedeemerFile \
  --tx-out "$(cat ${loanAddrFile}) + 3000000 lovelace + 1 ${activeBeacon} + 1 ${lenderBeacon} + 1 ${borrowerBeacon} + 20 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file $activeDatumFile \
  --tx-out "$(cat ../assets/wallets/01.addr) + 10000000 lovelace + 460 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --mint "1 ${activeBeacon} + 1 ${borrowerBeacon} + -1 ${askBeacon} + -1 ${offerBeacon}" \
  --mint-script-file $beaconPolicyFile \
  --mint-redeemer-file $beaconRedeemerFile \
  --required-signer-hash "$(cat $borrowerPubKeyHashFile)" \
  --change-address "$(cat ../assets/wallets/01.addr)" \
  --tx-in-collateral d5046a4d5a9c0a0ec6a9eabd0eb1524d54c3473459889b67ec17604f3c2e861b#0 \
  --testnet-magic 1 \
  --protocol-params-file "${tmpDir}protocol.json" \
  --invalid-before $ttl \
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