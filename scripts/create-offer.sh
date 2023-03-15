#!/bin/sh

# A helper script for showing how to create an Offer as a lender.

## Variables
dir="../assets/loan-files/"
tmpDir="../assets/tmp/"

beaconPolicyFile="${dir}beacons.plutus"

offerDatumFile="${dir}offer.json"

beaconRedeemerFile="${dir}mintOffer.json"

lenderPaymentPubKeyFile="../assets/wallets/02.vkey"
lenderPaymentPubKeyHashFile="../assets/wallets/02.pkh"

offerTokenName="4f66666572" # This is the hexidecimal encoding for 'Offer'.

### Change this to your target borrower.
loanAddr="addr_test1zrzk6pehy4n2wunznhgptaa3xftlrfkxenfv6craycf8ws3ualkqngnmdz2w9mv60zuucq0sswtn6lq2lwxwez76x0aqvtj3cx"

## Generate the hash for the lender's payment pubkey.
cardano-cli address key-hash \
  --payment-verification-key-file $lenderPaymentPubKeyFile \
  --out-file $lenderPaymentPubKeyHashFile

## Export the beacon policy.
cardano-loans export-script \
  --beacon-policy \
  --out-file $beaconPolicyFile

## Create the Offer datum.
cardano-loans lender offer-datum \
  --lender-payment-pubkey-hash "$(cat $lenderPaymentPubKeyHashFile)" \
  --loan-asset-is-lovelace \
  --principle 10000000 \
  --loan-term 3600 \
  --loan-interest-numerator 1 \
  --loan-interest-denominator 10 \
  --required-backing 10000000 \
  --collateral-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --collateral-asset-token-name 4f74686572546f6b656e0a \
  --collateral-rate-numerator 2 \
  --collateral-rate-denominator 1000000 \
  --out-file $offerDatumFile

## Create the MintOfferToken beacon policy redeemer.
cardano-loans lender offer-beacon \
  --lender-payment-pubkey-hash "$(cat $lenderPaymentPubKeyHashFile)" \
  --out-file $beaconRedeemerFile

## Get the beacon policy id.
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file $beaconPolicyFile)

## Helper Offer beacon variable
offerBeacon="${beaconPolicyId}.${offerTokenName}"

## Helper Lender ID beacon variable.
lenderPaymentPubKeyHash=$(cat $lenderPaymentPubKeyHashFile)
lenderBeacon="${beaconPolicyId}.${lenderPaymentPubKeyHash}"

## Create and submit the transaction.
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in 1a5f2de7ead5772dafee5325e98bb164261e1a226570396bdc7485d4278c032d#1 \
  --tx-out "${loanAddr} + 13000000 lovelace + 1 ${offerBeacon} + 1 ${lenderBeacon}" \
  --tx-out-inline-datum-file $offerDatumFile \
  --mint "1 ${offerBeacon} + 1 ${lenderBeacon}" \
  --mint-script-file $beaconPolicyFile \
  --mint-redeemer-file $beaconRedeemerFile \
  --required-signer-hash "$(cat $lenderPaymentPubKeyHashFile)" \
  --change-address "$(cat ../assets/wallets/02.addr)" \
  --tx-in-collateral 62d4e442d8f01e035003fc60d448289440ca9b390c71385f11a55ac07b695ee0#2 \
  --testnet-magic 1 \
  --protocol-params-file "${tmpDir}protocol.json" \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file ../assets/wallets/02.skey \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"