#!/bin/sh

## Variables
mainDir="../../ignored/"
walletDir="${mainDir}wallets/"
loanDir="${mainDir}loan-files/"
tmpDir="${mainDir}tmp/"

addressUpdateObserverScript="${loanDir}address_update_observer.plutus"

borrowerLoanAddr="addr_test1zr265ke6yq0krxr5xuansyeggscxdem2w5rtk6s99deap6fualkqngnmdz2w9mv60zuucq0sswtn6lq2lwxwez76x0aqlgq56x"

activeDatumFile="${loanDir}activeDatum.json"
paymentDatumFile="${loanDir}paymentDatum.json"
observerRedeemerFile="${loanDir}observeAddressUpdate.json"
loanRedeemerFile="${loanDir}updateAddress.json"

loanAsset='lovelace'
collateral1='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a'
collateral2='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31'

loanUTxO='097505df366d4695ee8a445b9337a2cdb2b45f22f55d43d075ad56a76d9581de#0'
updateTime=$((1712768154000)) # The loan expiration.
loanIdTokenName='4fe883a427bd96128e0fcfdbe94865c41ce92187d252f0ec45f261255f693c8f'
borrowerIdTokenName='3cefec09a27b6894e2ed9a78b9cc01f083973d7c0afb8cec8bda33fa'
newAddress=$(cat "${walletDir}03.addr")

## Convert the posix time to a slot number for invalid-hereafter.
echo "Calculating the required slot number..."
updateSlot=$(cardano-loans convert-time \
  --posix-time $updateTime \
  --testnet)

## Export the address update observer script so you can generate its required stake address.
echo "Exporting the address update observer script..."
cardano-loans scripts \
  --address-update-script \
  --out-file $addressUpdateObserverScript

## Build the observer script's stake address.
echo "Building the observer script's stake address..."
observerAddress=$(cardano-cli stake-address build \
  --testnet-magic 1 \
  --stake-script-file $addressUpdateObserverScript)

## Create the Active datum.
echo "Creating the active datum..."

cardano-loans datums active post-address-update auto \
  --testnet \
  --loan-ref $loanUTxO \
  --payment-address $newAddress \
  --out-file $activeDatumFile

# cardano-loans datums active post-address-update manual \
#   --payment-address $newAddress \
#   --loan-asset $loanAsset \
#   --principal 10000000 \
#   --loan-term '3600 slots' \
#   --interest '3602879701896397 / 36028797018963968' \
#   --compound-frequency '1200 slots' \
#   --minimum-payment 2000000 \
#   --fixed-penalty 500000 \
#   --collateral-asset $collateral1 \
#   --relative-rate '1 / 1000000' \
#   --collateral-asset $collateral2 \
#   --relative-rate '1 / 500000' \
#   --claim-expiration '1712756592000' \
#   --loan-expiration '1712752992000' \
#   --last-compounding '1712749392000' \
#   --total-epoch-payments 0 \
#   --outstanding-balance '3096224743817216015625 / 281474976710656' \
#   --borrower-staking-pubkey-hash $borrowerIdTokenName \
#   --loan-id $loanIdTokenName \
#   --out-file $activeDatumFile

## Create the required redeemers.
echo "Creating the observer redeemer..."
cardano-loans redeemers address-update-script observe-address-update \
  --out-file $observerRedeemerFile

echo "Creating the loan spending redeemer..."
cardano-loans redeemers loan-script update-lender-address \
  --deposit-increase 0 \
  --payment-address $newAddress \
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

## Create the payment datum.
echo "Creating the payment datum..."
cardano-loans datums payment \
  --loan-id $loanIdTokenName \
  --out-file $paymentDatumFile

# Create and submit the transaction.
cardano-cli transaction build \
  --tx-in $loanUTxO \
  --spending-tx-in-reference 09166e4f77c701c0607c4edaad2abf7b24a7a46d9f7ca38beead51ac8845a729#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $loanRedeemerFile \
  --tx-in ee55fa696e5e7dd29dafef7c47e36e37207e88b0a120a1156a709c429fe08c8f#0 \
  --tx-in fde2b2d830ce58e43eb2e32f7b0e111df707632757e7230b89ff79f88b3d0984#3 \
  --tx-out "${borrowerLoanAddr} + 4000000 lovelace + 1 ${loanId} + 1 ${borrowerId} + 1 ${activeBeacon} + 1 ${activeAssetBeacon} + 8 ${collateral1} + 4 ${collateral2}" \
  --tx-out-inline-datum-file $activeDatumFile \
  --tx-out "${newAddress} + 2000000 lovelace + 1 ${loanId}" \
  --tx-out-inline-datum-file $paymentDatumFile \
  --withdrawal "${observerAddress}+0" \
  --withdrawal-tx-in-reference 583e99294417bd271444d89021523c8f9762f96009113e8a02f69914f9e2ee73#0 \
  --withdrawal-plutus-script-v2 \
  --withdrawal-reference-tx-in-redeemer-file $observerRedeemerFile \
  --change-address "$(cat ${walletDir}02.addr)" \
  --tx-in-collateral 11ed603b92e6164c6bb0c83e0f4d54a954976db7c39e2a82d3cbf70f098da1e0#0 \
  --testnet-magic 1 \
  --invalid-hereafter $updateSlot \
  --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file "${walletDir}/02.skey" \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"
