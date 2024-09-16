#!/bin/sh

## Variables
mainDir="../../ignored/"
walletDir="${mainDir}wallets/"
loanDir="${mainDir}loan-files/"
tmpDir="${mainDir}tmp/"

activeRedeemerFile="${loanDir}burnAll.json"

loanIdTokenName='9d364a309553f27e02028d0cef816bfcb3cfdd6ae60556c15e49eb557f52a368'

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
cardano-cli conway transaction build \
  --tx-in 292ccf2bc14d685258bf0d3f070d4319eca74889269c962451ccb1ef6462084d#1 \
  --mint "-1 ${extraKey}" \
  --mint-tx-in-reference 03d6221ffb7a85284a8871a18b6276788f99ec5caff69af098d7e9b4a6e14dec#0 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file $activeRedeemerFile \
  --policy-id $activePolicyId \
  --change-address "$(cat ${walletDir}03.addr)" \
  --tx-in-collateral 00267b3dafac0bbc886b44377a33bb2a2d131526668b1d1e31db6279094e1d7e#0 \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file "${walletDir}/03.skey" \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"
