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
borrowerStakePubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file $borrowerStakePubKeyFile)

## Create the loan address.
echo "Creating the borrower's loan address..."
borrowerLoanAddr=$(cardano-cli address build \
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
cardano-cli transaction build \
  --tx-in dd0e2977d8ea2af53c9d1cd5fea19e09f15eef356b91314835316f649375c1c8#1 \
  --tx-in 18d3124d14ae7e18610f3715c04809ac5807a12e01ad39b5bb02dbf1c1d2644e#2 \
  --tx-in 18d3124d14ae7e18610f3715c04809ac5807a12e01ad39b5bb02dbf1c1d2644e#3 \
  --tx-out "$(cat ${walletDir}01.addr) + 2000000 lovelace + 30 ${collateral1}" \
  --tx-out "$(cat ${walletDir}01.addr) + 2000000 lovelace + 954 ${collateral2}" \
  --tx-out "${borrowerLoanAddr} + 3000000 lovelace + 1 ${askBeacon} + 1 ${assetBeacon} + 1 ${collateral1} + 1 ${collateral2}" \
  --tx-out-inline-datum-file $askDatumFile \
  --mint "1 ${askBeacon} + 1 ${assetBeacon}" \
  --mint-tx-in-reference dd0e2977d8ea2af53c9d1cd5fea19e09f15eef356b91314835316f649375c1c8#0 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id $beaconPolicyId \
  --required-signer-hash $borrowerStakePubKeyHash \
  --change-address "$(cat ${walletDir}01.addr)" \
  --tx-in-collateral 4cc5755712fee56feabad637acf741bc8c36dda5f3d6695ac6487a77c4a92d76#0 \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file "${walletDir}/01.skey" \
  --signing-key-file "${walletDir}/01Stake.skey" \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"
