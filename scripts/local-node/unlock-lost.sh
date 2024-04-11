#!/bin/sh

## Variables
mainDir="../../ignored/"
walletDir="${mainDir}wallets/"
loanDir="${mainDir}loan-files/"
tmpDir="${mainDir}tmp/"

borrowerStakePubKeyFile="${walletDir}01Stake.vkey"

activeRedeemerFile="${loanDir}BurnRemainderAndUnlock.json"
loanRedeemerFile="${loanDir}unlock.json"

loanAsset='lovelace'
collateral1='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a'
collateral2='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31'

loanUTxO='097505df366d4695ee8a445b9337a2cdb2b45f22f55d43d075ad56a76d9581de#0'
unlockTime=$((1712771754000+1000)) # The claim expiration + 1 slot.
loanIdTokenName='4fe883a427bd96128e0fcfdbe94865c41ce92187d252f0ec45f261255f693c8f'

## Generate the hash for the staking verification key.
echo "Calculating the staking pubkey hash for the borrower..."
borrowerStakePubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file $borrowerStakePubKeyFile)

## Convert the posix time to a slot number for invalid-before.
echo "Calculating the required slot number..."
unlockSlot=$(cardano-loans convert-time \
  --posix-time $unlockTime \
  --testnet)

## Create the required redeemers.
echo "Creating the active redeemer..."
cardano-loans redeemers active-script unlock \
  --out-file $activeRedeemerFile

echo "Creating the loan spending redeemer..."
cardano-loans redeemers loan-script unlock-active \
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
borrowerId="${activePolicyId}.${borrowerStakePubKeyHash}"
loanId="${activePolicyId}.${loanIdTokenName}"

## Create and submit the transaction.
cardano-cli transaction build \
  --tx-in $loanUTxO \
  --spending-tx-in-reference 09166e4f77c701c0607c4edaad2abf7b24a7a46d9f7ca38beead51ac8845a729#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $loanRedeemerFile \
  --tx-in ee55fa696e5e7dd29dafef7c47e36e37207e88b0a120a1156a709c429fe08c8f#4 \
  --tx-out "$(cat ${walletDir}01.addr) + 3000000 lovelace + 11 ${collateral1}" \
  --tx-out "$(cat ${walletDir}01.addr) + 3000000 lovelace + 9 ${collateral2}" \
  --mint "-1 ${borrowerId} + -1 ${activeBeacon} + -1 ${activeAssetBeacon} + -1 ${loanId}" \
  --mint-tx-in-reference 5b8da34b6ed8b0bfbaa69fb7c6738f63e1011761f580287ee4792e231360d025#0 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file $activeRedeemerFile \
  --policy-id $activePolicyId \
  --change-address "$(cat ${walletDir}01.addr)" \
  --required-signer-hash $borrowerStakePubKeyHash \
  --tx-in-collateral 4cc5755712fee56feabad637acf741bc8c36dda5f3d6695ac6487a77c4a92d76#0 \
  --testnet-magic 1 \
  --invalid-before $unlockSlot \
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
