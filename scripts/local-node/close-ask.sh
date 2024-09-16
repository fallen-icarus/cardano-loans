#!/bin/sh

## Variables
mainDir="../../ignored/"
walletDir="${mainDir}wallets/"
loanDir="${mainDir}loan-files/"
tmpDir="${mainDir}tmp/"

borrowerStakePubKeyFile="${walletDir}01Stake.vkey"

beaconRedeemerFile="${loanDir}createCloseOrUpdateAsk.json"
loanRedeemerFile="${loanDir}closeOrUpdateAsk.json"

loanAsset='lovelace'
collateral1='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31'

## Generate the hash for the staking verification key.
echo "Calculating the staking pubkey hash for the borrower..."
borrowerStakePubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file $borrowerStakePubKeyFile)

## Create the required redeemers.
echo "Creating the negotiation beacon redeemer..."
cardano-loans redeemers negotiation-script manage-asks \
  --borrower-staking-pubkey-hash $borrowerStakePubKeyHash \
  --out-file $beaconRedeemerFile

echo "Creating the loan spending redeemer..."
cardano-loans redeemers loan-script manage-ask \
  --out-file $loanRedeemerFile

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
  --tx-in efc6d88663c7a052dd09dc79e1e332f78e41d4be2cfde693715499ed7bb6d52e#1 \
  --spending-tx-in-reference 50f14254697370b7db435f93abff6e5952a6e0b7f267b033d96bac22d88c766b#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $loanRedeemerFile \
  --tx-in 9d7b8e4f3ab7fa082f020ae2cc6cf7577ebb2a7cb190b9517a3c8c1f94283a88#1 \
  --tx-out "$(cat ${walletDir}01.addr) + 3000000 lovelace + 9 ${collateral1}" \
  --mint "-1 ${askBeacon} + -1 ${assetBeacon}" \
  --mint-tx-in-reference a3ae17130ddbf4ce3117e218c920d219599ff935d024fac0d3ca4ef9ad6e4fde#0 \
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
