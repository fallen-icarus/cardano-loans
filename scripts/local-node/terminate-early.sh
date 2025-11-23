#!/bin/sh

## Variables
mainDir="../../ignored/"
walletDir="${mainDir}wallets/"
loanDir="${mainDir}loan-files/"
tmpDir="${mainDir}tmp/"

activeRedeemerFile="${loanDir}BurnAndClaimDefaulted.json"
loanRedeemerFile="${loanDir}spendWithKeyNFT.json"

loanAsset='lovelace'
collateral1='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a'
collateral2='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31'

loanUTxO='2bc79e72cd0cc06fa3d293acf42f322101b5f1796d8a8f5393bc845bdb7a0bdb#1'
loanIdTokenName='3d948df3aa567e075eddcb174c0876a0504cb321a6a43d84167277c15470fe5d'
borrowerIdTokenName='3cefec09a27b6894e2ed9a78b9cc01f083973d7c0afb8cec8bda33fa'

## Get the latest slot number.
### IMPORTANT: A local node may trail behind the actual blockchain by a few slots. So Koios may
### return a slot that your local node hasn't seen yet; this can cause transaction submission to
### fail. If this is an issue, you can get the current slot from you local node with: `cardano-cli
### query tip --testnet-magic 1`.
echo "Querying the latest slot..."
# currentSlot=$(cardano-loans query current-slot \
#   --testnet)
currentSlot=$(cardano-cli conway query tip --testnet-magic 1 | jq .slot)

#### You can alternatively just use the chain tip.
# currentSlot=$(cardano-loans query current-slot --testnet)

## Create the required redeemers.
echo "Creating the active redeemer..."
cardano-loans redeemers active-script claim-defaulted \
  --out-file $activeRedeemerFile

echo "Creating the loan spending redeemer..."
cardano-loans redeemers loan-script claim-defaulted-collateral \
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
  --spending-tx-in-reference d3e55cbd2cf714101dd052c65a7f7cbfda083650e2d6accc5d9197dc4a9b05ae#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $loanRedeemerFile \
  --tx-in 4b3eda964d1bec83ecd1d676feee2daa10e3c773383d2b694a11fff7ec186726#0 \
  --tx-in 2bc79e72cd0cc06fa3d293acf42f322101b5f1796d8a8f5393bc845bdb7a0bdb#0 \
  --mint "-1 ${borrowerId} + -1 ${activeBeacon} + -1 ${activeAssetBeacon} + -2 ${loanId}" \
  --mint-tx-in-reference 14949d195f80882b27ca0e68de5e3bfb9ba4f1ade3f707378efded60147b3218#0 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file $activeRedeemerFile \
  --policy-id $activePolicyId \
  --change-address "$(cat ${walletDir}02.addr)" \
  --tx-in-collateral 11ed603b92e6164c6bb0c83e0f4d54a954976db7c39e2a82d3cbf70f098da1e0#0 \
  --testnet-magic 1 \
  --invalid-before $currentSlot \
  --out-file "${tmpDir}tx.body"

cardano-cli conway transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file "${walletDir}/02.skey" \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli conway transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"
