#!/bin/sh

## Variables
mainDir="../../ignored/"
walletDir="${mainDir}wallets/"
loanDir="${mainDir}loan-files/"
tmpDir="${mainDir}tmp/"

addressUpdateObserverScript="${loanDir}address_update_observer.plutus"

borrowerLoanAddr="addr_test1zzc8hkkr9ygdfs02gf0d4hu8awlp8yeefm3kvglf0cw3fnpualkqngnmdz2w9mv60zuucq0sswtn6lq2lwxwez76x0aqwr65gm"

activeDatumFile="${loanDir}activeDatum.json"
paymentDatumFile="${loanDir}paymentDatum.json"
observerRedeemerFile="${loanDir}observeAddressUpdate.json"
loanRedeemerFile="${loanDir}updateAddress.json"

loanAsset='lovelace'
collateral1='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a'
collateral2='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31'

loanUTxO='03b077252ffa635ff08f89b844dc3480b184fa8344089868eddaf22a12f9a2c6#0'
updateTime=$((1726417379000)) # The loan expiration.
loanIdTokenName='9d364a309553f27e02028d0cef816bfcb3cfdd6ae60556c15e49eb557f52a368'
borrowerIdTokenName='3cefec09a27b6894e2ed9a78b9cc01f083973d7c0afb8cec8bda33fa'
newAddress=$(cat "${walletDir}03.addr")

## Convert the posix time to a slot number for invalid-hereafter.
### IMPORTANT: The invalid-hereafter can be set to a maximum of 1.5 days (129600 slots) passed the 
### current slot. If the loan expiration is beyond this "horizon", the node will reject the
### transaction. This is because hardforks can change slot lengths and, therefore, the node doesn't
### want to make guarantees about time too far into the future. If the loan's expiration is more
### than 1.5 days away, set the invalid-hereafter to be: `current_slot + 129600`.
echo "Calculating the required slot number..."
updateSlot=$(cardano-loans time convert-time \
  --posix-time $updateTime \
  --testnet)

## Export the address update observer script so you can generate its required stake address.
echo "Exporting the address update observer script..."
cardano-loans scripts \
  --address-update-script \
  --out-file $addressUpdateObserverScript

## Build the observer script's stake address.
echo "Building the observer script's stake address..."
observerAddress=$(cardano-cli conway stake-address build \
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
#   --loan-term '10800 slots' \
#   --interest '3602879701896397 / 36028797018963968' \
#   --compounding-interest \
#   --epoch-duration '1200 slots' \
#   --minimum-payment 2000000 \
#   --fixed-penalty 500000 \
#   --collateral-asset $collateral1 \
#   --relative-rate '1 / 1000000' \
#   --collateral-asset $collateral2 \
#   --relative-rate '1 / 500000' \
#   --claim-expiration '1726420979000' \
#   --loan-expiration '1726417379000' \
#   --last-epoch-boundary '1726407779000' \
#   --total-epoch-payments 0 \
#   --outstanding-balance '128286240743096816698103759582003203125 / 10141204801825835211973625643008' \
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
### IMPORTANT: Make sure to send the key NFT to the new address being used to recieve payments.
### You can find the key NFT within your address using:
### `cardano-loans query personal-address --address <old_address> --keys --testnet --stdout --pretty`
cardano-cli conway transaction build \
  --tx-in $loanUTxO \
  --spending-tx-in-reference 50f14254697370b7db435f93abff6e5952a6e0b7f267b033d96bac22d88c766b#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $loanRedeemerFile \
  --tx-in c543a5bd46d6e0edfc145f97641100b36d40be15d19c669f07f2ccb2bd76a0fd#0 \
  --tx-in ed97e918a37391e2c1722d2f1f757f3ddd0601d47e531908fd82fd1f56384cb4#0 \
  --tx-out "${borrowerLoanAddr} + 4000000 lovelace + 1 ${loanId} + 1 ${borrowerId} + 1 ${activeBeacon} + 1 ${activeAssetBeacon} + 8 ${collateral1} + 4 ${collateral2}" \
  --tx-out-inline-datum-file $activeDatumFile \
  --tx-out "${newAddress} + 2000000 lovelace + 1 ${loanId}" \
  --tx-out-inline-datum-file $paymentDatumFile \
  --withdrawal "${observerAddress}+0" \
  --withdrawal-tx-in-reference fd9a97914e3553996c6cf631c53f9077bdd4289ca06e1eb34fcbb4d0e4c351ff#0 \
  --withdrawal-plutus-script-v2 \
  --withdrawal-reference-tx-in-redeemer-file $observerRedeemerFile \
  --change-address "$(cat ${walletDir}02.addr)" \
  --tx-in-collateral 11ed603b92e6164c6bb0c83e0f4d54a954976db7c39e2a82d3cbf70f098da1e0#0 \
  --testnet-magic 1 \
  --invalid-hereafter $updateSlot \
  --out-file "${tmpDir}tx.body"

cardano-cli conway transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file "${walletDir}/02.skey" \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli conway transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"
