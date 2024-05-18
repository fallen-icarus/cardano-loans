#!/bin/sh

## Variables
mainDir="../../ignored/"
walletDir="${mainDir}wallets/"
loanDir="${mainDir}loan-files/"
tmpDir="${mainDir}tmp/"

activeRedeemerFile="${loanDir}burnAll.json"

loanIdTokenName='01ccf3dc6904b2cb3d6507f02e7cb52b575826f4962791e32bef0d60101fa86c'

## Create the required redeemers.
echo "Creating the active redeemer..."
cardano-loans redeemers active-script burn-all \
  --out-file $activeRedeemerFile

## Get the active beacon policy id.
echo "Calculating the active beacon policy id..."
activePolicyId=$(cardano-loans beacon-name policy-id \
  --active-beacons \
  --stdout) 

extraKey="${activePolicyId}.${loanIdTokenName}"

## Create and submit the transaction.
cardano-cli transaction build \
  --tx-in 18d3124d14ae7e18610f3715c04809ac5807a12e01ad39b5bb02dbf1c1d2644e#0 \
  --mint "-1 ${extraKey}" \
  --mint-tx-in-reference 9620379842501763c80c3737d219ee10b25f00a0449fd2a35457d1fb5dc08bb7#0 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file $activeRedeemerFile \
  --policy-id $activePolicyId \
  --change-address "$(cat ${walletDir}02.addr)" \
  --tx-in-collateral 11ed603b92e6164c6bb0c83e0f4d54a954976db7c39e2a82d3cbf70f098da1e0#0 \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file "${walletDir}/02.skey" \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"
