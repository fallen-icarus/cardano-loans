#!/bin/sh

## Variables
mainDir="../../ignored/"
walletDir="${mainDir}wallets/"
loanDir="${mainDir}loan-files/"
tmpDir="${mainDir}tmp/"

activeRedeemerFile="${loanDir}BurnAndClaimExpired.json"
loanRedeemerFile="${loanDir}spendWithKeyNFT.json"

loanAsset='lovelace'
collateral1='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a'
collateral2='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31'

loanUTxO='168c1c4b71595c6e2c4191cea58dc58667657f04afb677bc70e0ac4b7da9f7bc#1'
claimTime=$((1715797442000+1000)) # The loan expiration + 1 slot.
loanIdTokenName='e82604a9900b9cb6ea388e586c81998070a6fad366991650f6b836f5b2eb204d'
borrowerIdTokenName='3cefec09a27b6894e2ed9a78b9cc01f083973d7c0afb8cec8bda33fa'

## Convert the posix time to a slot number for invalid-before.
echo "Calculating the required slot number..."
claimSlot=$(cardano-loans convert-time \
  --posix-time $claimTime \
  --testnet)

#### You can alternatively just use the chain tip.
# claimSlot=$(cardano-loans query current-slot --testnet)

## Create the required redeemers.
echo "Creating the active redeemer..."
cardano-loans redeemers active-script claim-expired \
  --out-file $activeRedeemerFile

echo "Creating the loan spending redeemer..."
cardano-loans redeemers loan-script claim-expired-collateral \
  --out-file $loanRedeemerFile

## Get the active beacon policy id.
echo "Calculating the active beacon policy id..."
activePolicyId=$(cardano-loans beacon-name policy-id \
  --active-beacons \
  --stdout) 

## Get the required beacon names.
activeTokenName=$(cardano-loans beacon-name asset-name \
  --active-beacon \
  --stdout)
assetTokenName=$(cardano-loans beacon-name asset-name \
  --loan-asset $loanAsset \
  --stdout)

activeBeacon="${activePolicyId}.${activeTokenName}"
activeAssetBeacon="${activePolicyId}.${assetTokenName}"
borrowerId="${activePolicyId}.${borrowerIdTokenName}"
loanId="${activePolicyId}.${loanIdTokenName}"

## Create and submit the transaction.
cardano-cli conway transaction build \
  --tx-in $loanUTxO \
  --spending-tx-in-reference 50f14254697370b7db435f93abff6e5952a6e0b7f267b033d96bac22d88c766b#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $loanRedeemerFile \
  --tx-in 677b9d48ad49cbbe6763bfea889ead8577219d6f3881eb4005372710c61f44d5#1 \
  --tx-in 168c1c4b71595c6e2c4191cea58dc58667657f04afb677bc70e0ac4b7da9f7bc#0 \
  --tx-out "$(cat ${walletDir}02.addr) + 3000000 lovelace + 8 ${collateral1}" \
  --tx-out "$(cat ${walletDir}02.addr) + 3000000 lovelace + 4 ${collateral2}" \
  --mint "-1 ${borrowerId} + -1 ${activeBeacon} + -1 ${activeAssetBeacon} + -2 ${loanId}" \
  --mint-tx-in-reference 03d6221ffb7a85284a8871a18b6276788f99ec5caff69af098d7e9b4a6e14dec#0 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file $activeRedeemerFile \
  --policy-id $activePolicyId \
  --change-address "$(cat ${walletDir}02.addr)" \
  --tx-in-collateral 11ed603b92e6164c6bb0c83e0f4d54a954976db7c39e2a82d3cbf70f098da1e0#0 \
  --testnet-magic 1 \
  --invalid-before $claimSlot \
  --out-file "${tmpDir}tx.body"

cardano-cli conway transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file "${walletDir}/02.skey" \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli conway transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"
