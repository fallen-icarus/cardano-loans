#!/bin/sh

# A helper script for showing how to make a partial loan payment as a borrower.
# You can take the collateral back proportionally to what you repay.

## Variables
dir="../assets/loan-files/"
tmpDir="../assets/tmp/"

beaconPolicyFile="${dir}beacons.plutus"

borrowerPubKeyFile="../assets/wallets/01Stake.vkey"

loanAddrFile="${dir}loan.addr"

repayDatumFile="${dir}repayDatum.json"

repayRedeemerFile="${dir}repayRedeemer.json"

lenderDatum="${dir}lenderDatum.json"

### This is the loan's ID.
loanId="8e691c6075dbc9c30536670a4e00023d33fe7041de690887504f79fc4b1949d1"

expirationTime=33319189 ### The slot where the loan will expire.

activeTokenName="416374697665" # This is the hexidecimal encoding for 'Active'.

## Generate the hash for the staking verification key.
echo "Calculating the hash of the borrower's stake verification key..."
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

## Create the RepayLoan redeemer for the loan validator.
echo "Creating the spending redeemer..."
cardano-loans loan-redeemer make-payment \
  --out-file $repayRedeemerFile

## Create the collateral's Active datum for a loan repayment.
echo "Creating the new collateral active datum..."
cardano-loans datum collateral-payment-datum \
  --beacon-policy-id $beaconPolicyId \
  --borrower-staking-pubkey-hash $borrowerPubKeyHash \
  --payment-pubkey-hash ae0d001455a855e6c00f98fa9061028f5c00d297926383bc501be2d2 \
  --loan-asset-is-lovelace \
  --principle 10000000 \
  --loan-term 3600 \
  --interest-numerator 1 \
  --interest-denominator 10 \
  --collateral-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --collateral-asset-token-name 4f74686572546f6b656e0a \
  --rate-numerator 1 \
  --rate-denominator 500000 \
  --claim-expiration 33322789 \
  --loan-expiration $expirationTime \
  --balance-numerator 10000000 \
  --balance-denominator 1 \
  --payment-amount 5000000 \
  --loan-id $loanId \
  --out-file $repayDatumFile

## Create the datum for the lender's payment.
echo "Creating the datum for the payment to the lender..."
cardano-loans datum lender-payment-datum \
  --loan-id $loanId \
  --out-file $lenderDatum

## Create the lender's bech32 address.
echo "Creating the lender's bech32 address..."
lenderAddr=$(cardano-loans convert-address \
  --payment-pubkey-hash ae0d001455a855e6c00f98fa9061028f5c00d297926383bc501be2d2 \
  --stdout)

## Helper beacon variables
loanIdBeacon="${beaconPolicyId}.${loanId}"
activeBeacon="${beaconPolicyId}.${activeTokenName}"
borrowerBeacon="${beaconPolicyId}.${borrowerPubKeyHash}"

## Create and submit the transaction.
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in 7d9c0e57773e74316043dec295a53045f6cdaa1a6727238d55163bede34043bf#3 \
  --tx-in 7d9c0e57773e74316043dec295a53045f6cdaa1a6727238d55163bede34043bf#0 \
  --spending-tx-in-reference 1a7b174caeeeddd9b47245e17e31d7593007a145fbcf863ba7167202fc883091#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $repayRedeemerFile \
  --tx-out "$(cat ${loanAddrFile}) + 3000000 lovelace + 1 ${activeBeacon} + 1 ${loanIdBeacon} + 1 ${borrowerBeacon} + 10 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file $repayDatumFile \
  --tx-out "${lenderAddr} + 5000000 lovelace" \
  --tx-out-inline-datum-file $lenderDatum \
  --tx-out "$(cat ../assets/wallets/01.addr) + 2000000 lovelace + 10 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --required-signer-hash $borrowerPubKeyHash \
  --change-address "$(cat ../assets/wallets/01.addr)" \
  --tx-in-collateral 80b6d884296198d7eaa37f97a13e2d8ac4b38990d8419c99d6820bed435bbe82#0 \
  --invalid-hereafter 33319188 \
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