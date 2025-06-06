#!/bin/sh

## Variables
mainDir="../../ignored/"
walletDir="${mainDir}wallets/"
loanDir="${mainDir}loan-files/"
tmpDir="${mainDir}tmp/"

loanScript="${loanDir}loan.plutus"

borrowerStakePubKeyFile="${walletDir}01Stake.vkey"

askDatumFile="${loanDir}askDatum.json"
beaconRedeemerFile="${loanDir}createCloseOrUpdateAsk.json"

loanAsset='lovelace'
collateral1='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a'
collateral2='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31'

## Export the scripts.
echo "Exporting the scripts..."
cardano-loans scripts \
  --loan-script \
  --out-file $loanScript

## Generate the hash for the staking verification key.
echo "Calculating the staking pubkey hash for the borrower..."
borrowerStakePubKeyHash=$(cardano-cli conway stake-address key-hash \
  --stake-verification-key-file $borrowerStakePubKeyFile)

## Create the loan address.
echo "Creating the borrower's loan address..."
borrowerLoanAddr=$(cardano-cli conway address build \
  --payment-script-file $loanScript \
  --stake-verification-key-file $borrowerStakePubKeyFile \
  --testnet-magic 1)

## Create the Ask datum. The collateral must be sorted lexicographically.
echo "Creating the ask datum..."
cardano-loans datums ask \
  --borrower-staking-pubkey-hash $borrowerStakePubKeyHash \
  --loan-asset $loanAsset \
  --principal 10000000 \
  --loan-term '10800 slots' \
  --collateral-asset $collateral1 \
  --collateral-asset $collateral2 \
  --out-file $askDatumFile

## Create the required redeemer.
echo "Creating the negotiation beacon redeemer..."
cardano-loans redeemers negotiation-script manage-asks \
  --borrower-staking-pubkey-hash $borrowerStakePubKeyHash \
  --out-file $beaconRedeemerFile

## Get the negotiation beacon policy id.
echo "Calculating the negotiation beacon policy id..."
beaconPolicyId=$(cardano-loans beacon-name policy-id \
  --negotiation-beacons \
  --stdout) 

## Get the required beacon names.
askTokenName=$(cardano-loans beacon-name asset-name \
  --ask-beacon \
  --stdout)
assetTokenName=$(cardano-loans beacon-name asset-name \
  --loan-asset $loanAsset \
  --stdout)

askBeacon="${beaconPolicyId}.${askTokenName}"
assetBeacon="${beaconPolicyId}.${assetTokenName}"

## Create and submit the transaction.
cardano-cli conway transaction build \
  --tx-in 7980edca1365c2cb6eb5cd97424660f6dd6e384a83df3455f2488082cf5536b0#3 \
  --tx-in 82ac4b618f9315cda59a6dbed59f9debad492e90335c7e17ebac10b2b9b00ed9#1 \
  --tx-out "${borrowerLoanAddr} + 3000000 lovelace + 1 ${askBeacon} + 1 ${assetBeacon} + 1 ${collateral1} + 1 ${collateral2}" \
  --tx-out-inline-datum-file $askDatumFile \
  --tx-out "$(cat ${walletDir}01.addr) + 3000000 lovelace + 9 ${collateral1}" \
  --tx-out "$(cat ${walletDir}01.addr) + 3000000 lovelace + 79 ${collateral2}" \
  --mint "1 ${askBeacon} + 1 ${assetBeacon}" \
  --mint-tx-in-reference a3ae17130ddbf4ce3117e218c920d219599ff935d024fac0d3ca4ef9ad6e4fde#0 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id $beaconPolicyId \
  --required-signer-hash $borrowerStakePubKeyHash \
  --change-address "$(cat ${walletDir}01.addr)" \
  --tx-in-collateral 4cc5755712fee56feabad637acf741bc8c36dda5f3d6695ac6487a77c4a92d76#0 \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.body"

cardano-cli conway transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file "${walletDir}/01.skey" \
  --signing-key-file "${walletDir}/01Stake.skey" \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli conway transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"
