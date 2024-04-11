#!/bin/sh

## Variables
mainDir="../../ignored/"
walletDir="${mainDir}wallets/"
loanDir="${mainDir}loan-files/"
tmpDir="${mainDir}tmp/"

paymentObserverScript="${loanDir}payment_observer.plutus"

lenderAddress="addr_test1vzhq6qq52k59tekqp7v04yrpq284cqxjj7fx8qau2qd795s7wfhhm"

borrowerStakePubKeyFile="${walletDir}01Stake.vkey"
borrowerLoanAddr="addr_test1zr265ke6yq0krxr5xuansyeggscxdem2w5rtk6s99deap6fualkqngnmdz2w9mv60zuucq0sswtn6lq2lwxwez76x0aqlgq56x"

activeDatumFile="${loanDir}activeDatum.json"
paymentDatumFile="${loanDir}paymentDatum.json"
observerRedeemerFile="${loanDir}observePayment.json"
loanRedeemerFile="${loanDir}makePayment.json"

loanAsset='lovelace'
collateral1='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a'
collateral2='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31'

paymentAmount=77000000
loanUTxO='234f86d19e550469b654fb9ee9e1cc94c19a481a16192df015a362125697e812#0'
expirationTime=$((1712852202000+1200000)) # Either the next compounding time or the loan expiration.
loanIdTokenName='0f7deb6eca31425e357b1a7a9284f0e60782f5b2a36c80c5ef4b89bcbc4b5ced'

## Convert the posix time to a slot number for invalid-hereafter.
echo "Calculating the required slot number..."
expirationSlot=$(cardano-loans convert-time \
  --posix-time $expirationTime \
  --testnet)

## Generate the hash for the staking verification key.
echo "Calculating the staking pubkey hash for the borrower..."
borrowerStakePubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file $borrowerStakePubKeyFile)

## Export the payment observer script so you can generate its required stake address.
echo "Exporting the payment observer script..."
cardano-loans scripts \
  --payment-script \
  --out-file $paymentObserverScript

## Build the observer script's stake address.
echo "Building the observer script's stake address..."
observerAddress=$(cardano-cli stake-address build \
  --testnet-magic 1 \
  --stake-script-file $paymentObserverScript)

## Create the Active datum.
echo "Creating the active datum..."

cardano-loans datums active post-payment auto \
  --testnet \
  --loan-ref $loanUTxO \
  --payment-amount $paymentAmount \
  --out-file $activeDatumFile

# cardano-loans datums active post-payment manual \
#   --payment-address $lenderAddress \
#   --loan-asset $loanAsset \
#   --principle 10000000 \
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
#   --borrower-staking-pubkey-hash $borrowerStakePubKeyHash \
#   --loan-id $loanIdTokenName \
#   --payment-amount $paymentAmount \
#   --out-file $activeDatumFile

## Create the required redeemers.
echo "Creating the observer redeemer..."
cardano-loans redeemers payment-script observe-payment \
  --out-file $observerRedeemerFile

echo "Creating the loan spending redeemer..."
cardano-loans redeemers loan-script make-payment \
  --payment-amount $paymentAmount \
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

## Create the payment datum.
echo "Creating the payment datum..."
cardano-loans datums payment \
  --loan-id $loanIdTokenName \
  --out-file $paymentDatumFile

# If you are making a full payment, you will also need these.
activeRedeemerFile="${loanDir}burnActive.json"

echo "Creating the active beacon redeemer..."
cardano-loans redeemers active-script burn-all \
  --out-file $activeRedeemerFile


## Create and submit the transaction.

# Full payment transaction.
cardano-cli transaction build \
  --tx-in $loanUTxO \
  --spending-tx-in-reference 09166e4f77c701c0607c4edaad2abf7b24a7a46d9f7ca38beead51ac8845a729#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $loanRedeemerFile \
  --tx-in 234f86d19e550469b654fb9ee9e1cc94c19a481a16192df015a362125697e812#1 \
  --tx-out "${lenderAddress} + ${paymentAmount} ${loanAsset}" \
  --tx-out-inline-datum-file $paymentDatumFile \
  --tx-out "${borrowerLoanAddr} + 4000000 lovelace + 1 ${loanId} + 1 ${activeBeacon} + 1 ${activeAssetBeacon}" \
  --tx-out-inline-datum-file $activeDatumFile \
  --tx-out "$(cat ${walletDir}01.addr) + 3000000 lovelace + 5 ${collateral1}" \
  --tx-out "$(cat ${walletDir}01.addr) + 3000000 lovelace + 3 ${collateral2}" \
  --mint "-1 ${borrowerId}" \
  --mint-tx-in-reference 5b8da34b6ed8b0bfbaa69fb7c6738f63e1011761f580287ee4792e231360d025#0 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file $activeRedeemerFile \
  --policy-id $activePolicyId \
  --withdrawal "${observerAddress}+0" \
  --withdrawal-tx-in-reference c0b3e96be19325a3277b6531d5d8a64925db0ef92989a29fb96be7b65b02fa0b#0 \
  --withdrawal-plutus-script-v2 \
  --withdrawal-reference-tx-in-redeemer-file $observerRedeemerFile \
  --required-signer-hash $borrowerStakePubKeyHash \
  --change-address "$(cat ${walletDir}01.addr)" \
  --tx-in-collateral 4cc5755712fee56feabad637acf741bc8c36dda5f3d6695ac6487a77c4a92d76#0 \
  --testnet-magic 1 \
  --invalid-hereafter $expirationSlot \
  --out-file "${tmpDir}tx.body"

# # Parial payment transaction.
# cardano-cli transaction build \
#   --tx-in $loanUTxO \
#   --spending-tx-in-reference 09166e4f77c701c0607c4edaad2abf7b24a7a46d9f7ca38beead51ac8845a729#0 \
#   --spending-plutus-script-v2 \
#   --spending-reference-tx-in-inline-datum-present \
#   --spending-reference-tx-in-redeemer-file $loanRedeemerFile \
#   --tx-in a4afa3648bf273c563c6b66c51110f7bfdfbe761d53a7a8e1961d79e3d53b0cd#4 \
#   --tx-out "${lenderAddress} + ${paymentAmount} ${loanAsset}" \
#   --tx-out-inline-datum-file $paymentDatumFile \
#   --tx-out "${borrowerLoanAddr} + 4000000 lovelace + 1 ${loanId} + 1 ${borrowerId} + 1 ${activeBeacon} + 1 ${activeAssetBeacon} + 5 ${collateral1} + 3 ${collateral2}" \
#   --tx-out-inline-datum-file $activeDatumFile \
#   --tx-out "$(cat ${walletDir}01.addr) + 3000000 lovelace + 4 ${collateral1}" \
#   --tx-out "$(cat ${walletDir}01.addr) + 3000000 lovelace + 2 ${collateral2}" \
#   --withdrawal "${observerAddress}+0" \
#   --withdrawal-tx-in-reference c0b3e96be19325a3277b6531d5d8a64925db0ef92989a29fb96be7b65b02fa0b#0 \
#   --withdrawal-plutus-script-v2 \
#   --withdrawal-reference-tx-in-redeemer-file $observerRedeemerFile \
#   --required-signer-hash $borrowerStakePubKeyHash \
#   --change-address "$(cat ${walletDir}01.addr)" \
#   --tx-in-collateral 4cc5755712fee56feabad637acf741bc8c36dda5f3d6695ac6487a77c4a92d76#0 \
#   --testnet-magic 1 \
#   --invalid-hereafter $expirationSlot \
#   --out-file "${tmpDir}tx.body"

cardano-cli transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file "${walletDir}/01.skey" \
  --signing-key-file "${walletDir}/01Stake.skey" \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"
