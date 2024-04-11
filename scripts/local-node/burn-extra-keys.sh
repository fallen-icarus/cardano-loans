#!/bin/sh

## Variables
mainDir="../../ignored/"
walletDir="${mainDir}wallets/"
loanDir="${mainDir}loan-files/"
tmpDir="${mainDir}tmp/"

activeRedeemerFile="${loanDir}burnAll.json"

loanIdTokenName='4fe883a427bd96128e0fcfdbe94865c41ce92187d252f0ec45f261255f693c8f'

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
  --tx-in ee55fa696e5e7dd29dafef7c47e36e37207e88b0a120a1156a709c429fe08c8f#0 \
  --mint "-1 ${extraKey}" \
  --mint-tx-in-reference 5b8da34b6ed8b0bfbaa69fb7c6738f63e1011761f580287ee4792e231360d025#0 \
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
