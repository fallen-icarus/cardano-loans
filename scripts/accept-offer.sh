#!/bin/sh

# A helper script for showing how to accept an Offer as a borrower.

## Variables
dir="../assets/loan-files/"
tmpDir="../assets/tmp/"

beaconPolicyFile="${dir}beacons.plutus" # This is used to get the policy id.

borrowerPubKeyFile="../assets/wallets/01Stake.vkey"

### This is the lender's ID. Change this for your target offer.
lenderId="ae0d001455a855e6c00f98fa9061028f5c00d297926383bc501be2d2"

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

start=33326717 # Slot number for start of loan.

## Generate the hash for the staking verification key.
echo "Calculating the borrower's stake pubkey hash..."
borrowerPubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file $borrowerPubKeyFile)

## Export the beacon policy.
echo "Exporting the beacon policy script..."
cardano-loans export-script beacon-policy \
  --out-file $beaconPolicyFile

## Get the beacon policy id.
echo "Calculating the beacon policy id..."
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file $beaconPolicyFile)

## Create the AcceptOffer redeemer for the loan validator.
echo "Creating the spending redeemer..."
cardano-loans loan-redeemer accept-offer \
  --out-file $acceptOfferRedeemerFile

## Create the MintActive beacon policy redeemer. The first output reference is for the Ask and
## the second one is for the corresponding Offer.
echo "Creating the mint redeemer..."
cardano-loans beacon-redeemer mint-active \
  --borrower-staking-pubkey-hash $borrowerPubKeyHash \
  --tx-hash 89b67297cff33ce4dfb5bbd1d2414b313cf8ee26ce0e7e970aa9c4a2682f38f5 \
  --output-index 0 \
  --tx-hash 07b2177d87a3e1bcd41548bcf2dca24038dbf09865fae80bf0b511229bf7f2df \
  --output-index 0 \
  --out-file $beaconRedeemerFile

## Create the Active datum for accepting an offer. The loan-id is the tx hash for the corresponding
## Offer.
echo "Creating the active datum..."
cardano-loans datum accept-datum \
  --beacon-policy-id $beaconPolicyId \
  --borrower-staking-pubkey-hash $borrowerPubKeyHash \
  --payment-pubkey-hash ae0d001455a855e6c00f98fa9061028f5c00d297926383bc501be2d2 \
  --loan-asset-is-lovelace \
  --principle 10000000 \
  --checkpoint 1800 \
  --loan-term 3600 \
  --interest-numerator 1 \
  --interest-denominator 10 \
  --collateral-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --collateral-asset-token-name 4f74686572546f6b656e0a \
  --rate-numerator 1 \
  --rate-denominator 500000 \
  --claim-period 3600 \
  --loan-id 07b2177d87a3e1bcd41548bcf2dca24038dbf09865fae80bf0b511229bf7f2df \
  --starting-slot $start \
  --out-file $activeDatumFile

## Create the lender's bech32 address.
echo "Creating the lender's bech32 address..."
lenderAddr=$(cardano-loans convert-address \
  --payment-pubkey-hash ae0d001455a855e6c00f98fa9061028f5c00d297926383bc501be2d2 \
  --stdout)

## Helper beacon variables
askBeacon="${beaconPolicyId}.${askTokenName}"
offerBeacon="${beaconPolicyId}.${offerTokenName}"
lenderBeacon="${beaconPolicyId}.${lenderId}"
activeBeacon="${beaconPolicyId}.${activeTokenName}"
borrowerBeacon="${beaconPolicyId}.${borrowerPubKeyHash}"
loanIdBeacon="${beaconPolicyId}.07b2177d87a3e1bcd41548bcf2dca24038dbf09865fae80bf0b511229bf7f2df"

## Create and submit the transaction.
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in 0f94a13cf0207e9a322c15d52d372dc04bd292160277f94e4fc5fbad5598a209#1 \
  --tx-in 7d9c0e57773e74316043dec295a53045f6cdaa1a6727238d55163bede34043bf#2 \
  --tx-in 89b67297cff33ce4dfb5bbd1d2414b313cf8ee26ce0e7e970aa9c4a2682f38f5#0 \
  --spending-tx-in-reference 28bd750b45a4459f6b2d184e6ed504a4ba54a8b8b21c9cf0eaa42ed12b5ab004#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $acceptOfferRedeemerFile \
  --tx-in 07b2177d87a3e1bcd41548bcf2dca24038dbf09865fae80bf0b511229bf7f2df#0 \
  --spending-tx-in-reference 28bd750b45a4459f6b2d184e6ed504a4ba54a8b8b21c9cf0eaa42ed12b5ab004#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $acceptOfferRedeemerFile \
  --tx-out "$(cat ${loanAddrFile}) + 3000000 lovelace + 1 ${loanIdBeacon} + 1 ${activeBeacon} + 1 ${borrowerBeacon} + 20 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file $activeDatumFile \
  --tx-out "${lenderAddr} + 5000000 lovelace + 1 ${loanIdBeacon}" \
  --tx-out "$(cat ../assets/wallets/01.addr) + 3000000 lovelace + 450 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --mint "1 ${activeBeacon} + 1 ${borrowerBeacon} + 2 ${loanIdBeacon} + -1 ${askBeacon} + -1 ${offerBeacon} + -1 ${lenderBeacon}" \
  --mint-tx-in-reference 0f94a13cf0207e9a322c15d52d372dc04bd292160277f94e4fc5fbad5598a209#0 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id $beaconPolicyId \
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