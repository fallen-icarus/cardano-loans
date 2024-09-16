#!/bin/sh

## Variables
mainDir="../../ignored/"
walletDir="${mainDir}wallets/"
loanDir="${mainDir}loan-files/"
tmpDir="${mainDir}tmp/"

lenderStakePubKeyFile="${walletDir}02Stake.vkey"

beaconRedeemerFile="${loanDir}createCloseOrUpdateOffer.json"
loanRedeemerFile="${loanDir}closeOrUpdateOffer.json"

loanAsset='lovelace'

## Generate the hash for the staking verification key.
echo "Calculating the staking pubkey hash for the lender..."
lenderStakePubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file $lenderStakePubKeyFile)

## Create the required redeemer.
echo "Creating the negotiation beacon redeemer..."
cardano-loans redeemers negotiation-script manage-offers \
  --lender-staking-pubkey-hash $lenderStakePubKeyHash \
  --out-file $beaconRedeemerFile

echo "Creating the loan spending redeemer..."
cardano-loans redeemers loan-script manage-offer \
  --out-file $loanRedeemerFile

## Get the negotiation beacon policy id.
echo "Calculating the negotiation beacon policy id..."
beaconPolicyId=$(cardano-loans beacon-name policy-id \
  --negotiation-beacons \
  --stdout) 

## Get the required beacon names.
offerTokenName=$(cardano-loans beacon-name asset-name \
  --offer-beacon \
  --stdout)
assetTokenName=$(cardano-loans beacon-name asset-name \
  --loan-asset $loanAsset \
  --stdout)
lenderIdTokenName=$(cardano-loans beacon-name asset-name \
  --lender-staking-pubkey-hash $lenderStakePubKeyHash \
  --stdout)

offerBeacon="${beaconPolicyId}.${offerTokenName}"
assetBeacon="${beaconPolicyId}.${assetTokenName}"
lenderId="${beaconPolicyId}.${lenderIdTokenName}"

## Create and submit the transaction.
cardano-cli conway transaction build \
  --tx-in 667c6a5bf9eb86f97ab5f2f2dbd66a484d32795cd244397dfec7953ee8cb2ff3#0 \
  --spending-tx-in-reference 50f14254697370b7db435f93abff6e5952a6e0b7f267b033d96bac22d88c766b#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $loanRedeemerFile \
  --tx-in 667c6a5bf9eb86f97ab5f2f2dbd66a484d32795cd244397dfec7953ee8cb2ff3#1 \
  --mint "-1 ${offerBeacon} + -1 ${assetBeacon} + -1 ${lenderId}" \
  --mint-tx-in-reference a3ae17130ddbf4ce3117e218c920d219599ff935d024fac0d3ca4ef9ad6e4fde#0 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id $beaconPolicyId \
  --required-signer-hash $lenderStakePubKeyHash \
  --change-address "$(cat ${walletDir}02.addr)" \
  --tx-in-collateral 11ed603b92e6164c6bb0c83e0f4d54a954976db7c39e2a82d3cbf70f098da1e0#0 \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file "${walletDir}/02.skey" \
  --signing-key-file "${walletDir}/02Stake.skey" \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"
