#!/bin/sh

# A helper script for showing how to create an Offer as a lender.

## Variables
dir="../assets/loan-files/"
tmpDir="../assets/tmp/"

beaconPolicyFile="${dir}beacons.plutus"

offerDatumFile="${dir}offerDatum.json"

beaconRedeemerFile="${dir}mintOffer.json"

lenderPaymentPubKeyFile="../assets/wallets/02.vkey"

offerTokenName="4f66666572" # This is the hexidecimal encoding for 'Offer'.

## Change this to your target borrower.
loanAddr="addr_test1zq749l3erdr67mmukh3mct038q5et2lkpgnqszgsx4n6n5eualkqngnmdz2w9mv60zuucq0sswtn6lq2lwxwez76x0aq0tg2ct"

## Generate the hash for the lender's payment pubkey.
echo "Calculating the lender's pubkey hash..."
lenderPaymentPubKeyHash=$(cardano-cli address key-hash \
  --payment-verification-key-file $lenderPaymentPubKeyFile)

# ## Export the beacon policy.
# echo "Exporting the beacon policy script..."
# cardano-loans export-script \
#   --beacon-policy \
#   --out-file $beaconPolicyFile

## Get the beacon policy id.
echo "Calculating the beacon policy id..."
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file $beaconPolicyFile)

## Create the Offer datum.
echo "Creating the offer datum..."
cardano-loans loan-datum offer-datum \
  --beacon-policy-id $beaconPolicyId \
  --lender-payment-pubkey-hash $lenderPaymentPubKeyHash \
  --loan-asset-is-lovelace \
  --principle 10000000 \
  --loan-term 3600 \
  --interest-numerator 1 \
  --interest-denominator 10 \
  --collateral-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --collateral-asset-token-name 4f74686572546f6b656e0a \
  --rate-numerator 1 \
  --rate-denominator 500000 \
  --out-file $offerDatumFile

## Create the MintOffer beacon policy redeemer.
echo "Creating the mint redeemer..."
cardano-loans beacon-redeemer mint-offer \
  --lender-payment-pubkey-hash $lenderPaymentPubKeyHash \
  --out-file $beaconRedeemerFile

## Helper beacon variables
offerBeacon="${beaconPolicyId}.${offerTokenName}"
lenderBeacon="${beaconPolicyId}.${lenderPaymentPubKeyHash}"

## Create and submit the transaction.
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in 38c7cd3ae8558fd41b85478402da02ee08789be43a1d5b51437e613c63bb01d7#1 \
  --tx-out "${loanAddr} + 13000000 lovelace + 1 ${offerBeacon} + 1 ${lenderBeacon}" \
  --tx-out-inline-datum-file $offerDatumFile \
  --mint "1 ${offerBeacon} + 1 ${lenderBeacon}" \
  --mint-script-file $beaconPolicyFile \
  --mint-redeemer-file $beaconRedeemerFile \
  --required-signer-hash $lenderPaymentPubKeyHash \
  --change-address "$(cat ../assets/wallets/02.addr)" \
  --tx-in-collateral 11ed603b92e6164c6bb0c83e0f4d54a954976db7c39e2a82d3cbf70f098da1e0#0 \
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