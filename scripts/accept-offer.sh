#!/bin/sh

# A helper script for showing how to accept an Offer as a borrower.

## Variables
dir="../assets/loan-files/"
tmpDir="../assets/tmp/"

loanScriptFile="${dir}loan.plutus"
beaconPolicyFile="${dir}beacons.plutus"

borrowerPubKeyFile="../assets/wallets/01Stake.vkey"

### This is the lender's ID. Change this for your target offer.
lenderPubKeyHash="ae0d001455a855e6c00f98fa9061028f5c00d297926383bc501be2d2"

loanAddrFile="${dir}loan.addr"

activeDatumFile="${dir}activeDatum.json"

beaconRedeemerFile="${dir}mintActive.json"
acceptOfferRedeemerFile="${dir}acceptOffer.json"

### This is the hexidecimal encoding for 'Active'.
activeTokenName="416374697665"

### This is the hexidecimal encoding for 'Ask'.
askTokenName="41736b"

### This is the hexidecimal encoding for 'Offer'.
offerTokenName="4f66666572"

start=26750275 # Slot number for start of loan.

## Generate the hash for the staking verification key.
echo "Calculating the borrower's stake pubkey hash..."
borrowerPubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file $borrowerPubKeyFile)

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

## Create the AcceptOffer redeemer for the loan validator.
echo "Creating the spending redeemer..."
cardano-loans loan-redeemer \
  --accept-offer \
  --out-file $acceptOfferRedeemerFile

## Create the MintActive beacon policy redeemer.
echo "Creating the mint redeemer..."
cardano-loans beacon-redeemer mint-active \
  --borrower-stake-pubkey-hash $borrowerPubKeyHash \
  --lender-payment-pubkey-hash $lenderPubKeyHash \
  --out-file $beaconRedeemerFile

## Create the Active datum for accepting an offer.
## Make sure to add the term length to the start variable.
echo "Creating the active datum..."
cardano-loans loan-datum accept-datum \
  --beacon-policy-id $beaconPolicyId \
  --lender-payment-pubkey-hash $lenderPubKeyHash \
  --borrower-stake-pubkey-hash $borrowerPubKeyHash \
  --loan-asset-is-lovelace \
  --principle 10000000 \
  --loan-term 3600 \
  --interest-numerator 1 \
  --interest-denominator 10 \
  --collateral-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --collateral-asset-token-name 4f74686572546f6b656e0a \
  --rate-numerator 1 \
  --rate-denominator 500000 \
  --expiration $((start + 3600)) \
  --out-file $activeDatumFile

## Helper beacon variables
askBeacon="${beaconPolicyId}.${askTokenName}"
offerBeacon="${beaconPolicyId}.${offerTokenName}"
lenderBeacon="${beaconPolicyId}.${lenderPubKeyHash}"
activeBeacon="${beaconPolicyId}.${activeTokenName}"
borrowerBeacon="${beaconPolicyId}.${borrowerPubKeyHash}"

## Create and submit the transaction.
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in ddcce58ba2e32d0b7d9385951f1b986d35de849cf62939c2e968d72186c6c34f#1 \
  --tx-in aadb8f997d84aa4ddbac4e2d49f01626758ded73cf778a9d76eabb5c21fefd55#1 \
  --tx-in 5691e25c50bebe82f8c92193d513bb9594787a52a8141d9f8e089d8fec3829e3#0 \
  --tx-in-script-file $loanScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $acceptOfferRedeemerFile \
  --tx-in ddcce58ba2e32d0b7d9385951f1b986d35de849cf62939c2e968d72186c6c34f#0 \
  --tx-in-script-file $loanScriptFile \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file $acceptOfferRedeemerFile \
  --tx-out "$(cat ${loanAddrFile}) + 3000000 lovelace + 1 ${activeBeacon} + 1 ${lenderBeacon} + 1 ${borrowerBeacon} + 20 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file $activeDatumFile \
  --tx-out "$(cat ../assets/wallets/01.addr) + 3000000 lovelace + 560 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --mint "1 ${activeBeacon} + 1 ${borrowerBeacon} + -1 ${askBeacon} + -1 ${offerBeacon}" \
  --mint-script-file $beaconPolicyFile \
  --mint-redeemer-file $beaconRedeemerFile \
  --required-signer-hash $borrowerPubKeyHash \
  --change-address "$(cat ../assets/wallets/01.addr)" \
  --tx-in-collateral 80b6d884296198d7eaa37f97a13e2d8ac4b38990d8419c99d6820bed435bbe82#0 \
  --invalid-before $start \
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