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
  --principle 10000000 \
  --loan-term '3600 slots' \
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
  --tx-in 25762e065cb4a6e07124e36fddc54b31b874565a445965042914265922cac677#1 \
  --tx-in 25762e065cb4a6e07124e36fddc54b31b874565a445965042914265922cac677#0 \
  --tx-in 1f4b5eaf864f57ad3ee4e58edd5bc955fa1315752b663f80719a67a5900f1b99#1 \
  --tx-out "$(cat ${walletDir}01.addr) + 2000000 lovelace + 19 ${collateral1}" \
  --tx-out "$(cat ${walletDir}01.addr) + 2000000 lovelace + 8 ${collateral2}" \
  --tx-out "${borrowerLoanAddr} + 3000000 lovelace + 1 ${askBeacon} + 1 ${assetBeacon} + 1 ${collateral1} + 1 ${collateral2}" \
  --tx-out-inline-datum-file $askDatumFile \
  --mint "1 ${askBeacon} + 1 ${assetBeacon}" \
  --mint-tx-in-reference 8fac9b184dc008243deccb3b812c1a13455ff7a34e22c2125ea3a303078d1c76#0 \
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
