#!/bin/sh

# A helper script for showing how to close an Offer as a lender.

## Variables
dir="../assets/loan-files/"
tmpDir="../assets/tmp/"

beaconPolicyFile="${dir}beacons.plutus" # This is used to get the beacon policy id.

lenderPaymentPubKeyFile="../assets/wallets/02.vkey"

beaconRedeemerFile="${dir}burnBeacons.json"
closeOfferRedeemerFile="${dir}closeOffer.json"

offerTokenName="4f66666572" # This is the hexidecimal encoding for 'Offer'.

## Generate the hash for the lender's payment pubkey.
echo "Calculating the hash of the lender's payment pubkey..."
lenderPaymentPubKeyHash=$(cardano-cli address key-hash \
  --payment-verification-key-file $lenderPaymentPubKeyFile)

## Export the beacon policy.
echo "Exporting the beacon policy script..."
cardano-loans export-script beacon-policy \
  --out-file $beaconPolicyFile

## Get the beacon policy id.
echo "Calculating the beacon policy id..."
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file $beaconPolicyFile)

## Create the BurnBeacons beacon policy redeemer.
echo "Creating the burn redeemer..."
cardano-loans beacon-redeemer burn-beacons \
  --out-file $beaconRedeemerFile

## Create the CloseOffer redeemer for the loan validator.
echo "Creating the spending redeemer..."
cardano-loans loan-redeemer close-offer \
  --out-file $closeOfferRedeemerFile

## Helper beacon variables
offerBeacon="${beaconPolicyId}.${offerTokenName}"
lenderBeacon="${beaconPolicyId}.${lenderPaymentPubKeyHash}"

## Create and submit the transaction.
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in ce5fa492b6a38aef9f01bcc44b2376dd3b59f4cf6ff1e199d45ee33fc0c47242#0 \
  --spending-tx-in-reference 45551cf4568e6357b627f22df499a3ed59fa7b72a9b2fe0160cbad87ce11c902#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $closeOfferRedeemerFile \
  --mint "-1 ${offerBeacon} + -1 ${lenderBeacon}" \
  --mint-tx-in-reference e8bff224cd50b66c39a98e3708f981fba1a27e9aa89ce5986ae36597b2646042#0 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id $beaconPolicyId \
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