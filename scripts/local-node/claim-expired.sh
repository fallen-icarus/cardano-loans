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

loanUTxO='fc908c46bcf79369a8c861cebbde867d87e7fb6a7da384e2165904cf95001725#1'
claimTime=$((1763394073000+1000)) # The loan expiration + 1 slot.
loanIdTokenName='11f1eb013953c6615db17dfaec6a6247357ca88858a1448d116fb35f736b567a'
borrowerIdTokenName='3cefec09a27b6894e2ed9a78b9cc01f083973d7c0afb8cec8bda33fa'

## Convert the posix time to a slot number for invalid-before.
echo "Calculating the required slot number..."
claimSlot=$(cardano-loans time convert-time \
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
  --spending-tx-in-reference 73b65770934204111b8916156c0275bfe5d52f0aa8f856d4d7359c10b7876a29#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $loanRedeemerFile \
  --tx-in 833c7fca3c73e692ef945f763aac27ce89158dcf2cc9f7e43373ffe07423c20b#0 \
  --tx-in acbe2aee3064a41571d438a2fe869dc06624892b8878a638afce9e374363f178#3 \
  --mint "-1 ${borrowerId} + -1 ${activeBeacon} + -1 ${activeAssetBeacon} + -2 ${loanId}" \
  --mint-tx-in-reference 47f7e129697675944421aea3c2faa4cf0ee9e90b9445fffe145567e5d6476bcc#0 \
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
