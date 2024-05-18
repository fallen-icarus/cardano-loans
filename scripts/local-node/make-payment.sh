#!/bin/sh

## Variables
mainDir="../../ignored/"
walletDir="${mainDir}wallets/"
loanDir="${mainDir}loan-files/"
tmpDir="${mainDir}tmp/"

paymentObserverScript="${loanDir}payment_observer.plutus"

lenderAddress="addr_test1vzhq6qq52k59tekqp7v04yrpq284cqxjj7fx8qau2qd795s7wfhhm"

borrowerStakePubKeyFile="${walletDir}01Stake.vkey"
borrowerLoanAddr="addr_test1zrv3ff2vrjj3rujdnggeap27r69w763dkauumks70jngey3ualkqngnmdz2w9mv60zuucq0sswtn6lq2lwxwez76x0aqe70yty"

activeDatumFile="${loanDir}activeDatum.json"
paymentDatumFile="${loanDir}paymentDatum.json"
observerRedeemerFile="${loanDir}observePayment.json"
loanRedeemerFile="${loanDir}makePayment.json"

loanAsset='lovelace'
collateral1='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a'
collateral2='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31'

paymentAmount=9900001
loanUTxO='57101ea3e7b2bc8daadca48cc680db81d45361ce9674f1db3ff3a2b9100de855#0'
expirationTime=$((1715793320000+1200000)) # Either the next compounding time or the loan expiration.
loanIdTokenName='01ccf3dc6904b2cb3d6507f02e7cb52b575826f4962791e32bef0d60101fa86c'

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
  --spending-tx-in-reference 292f25c6594169502c71ee82cd5285bba9a887a60a3b447bade71284acb172db#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $loanRedeemerFile \
  --tx-in 1b13cb14dd34452e0c0f771290f98c53250cb0e41cb02c57843fb9ae109049d5#0 \
  --tx-out "${lenderAddress} + ${paymentAmount} ${loanAsset}" \
  --tx-out-inline-datum-file $paymentDatumFile \
  --tx-out "$(cat ${walletDir}01.addr) + 3000000 lovelace + 8 ${collateral1}" \
  --tx-out "$(cat ${walletDir}01.addr) + 3000000 lovelace + 3 ${collateral2}" \
  --mint "-1 ${borrowerId} + -1 ${activeBeacon} + -1 ${activeAssetBeacon} + -1 ${loanId}" \
  --mint-tx-in-reference 9620379842501763c80c3737d219ee10b25f00a0449fd2a35457d1fb5dc08bb7#0 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file $activeRedeemerFile \
  --policy-id $activePolicyId \
  --withdrawal "${observerAddress}+0" \
  --withdrawal-tx-in-reference a96248cd1788c4b435b8ff9268236f8fa5d62faaa6cb73d600de869e7361be40#0 \
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
#   --spending-tx-in-reference 292f25c6594169502c71ee82cd5285bba9a887a60a3b447bade71284acb172db#0 \
#   --spending-plutus-script-v2 \
#   --spending-reference-tx-in-inline-datum-present \
#   --spending-reference-tx-in-redeemer-file $loanRedeemerFile \
#   --tx-in 9620379842501763c80c3737d219ee10b25f00a0449fd2a35457d1fb5dc08bb7#1 \
#   --tx-out "${lenderAddress} + ${paymentAmount} ${loanAsset}" \
#   --tx-out-inline-datum-file $paymentDatumFile \
#   --tx-out "${borrowerLoanAddr} + 4000000 lovelace + 1 ${loanId} + 1 ${borrowerId} + 1 ${activeBeacon} + 1 ${activeAssetBeacon} + 8 ${collateral1} + 3 ${collateral2}" \
#   --tx-out-inline-datum-file $activeDatumFile \
#   --tx-out "$(cat ${walletDir}01.addr) + 3000000 lovelace + 1 ${collateral2}" \
#   --withdrawal "${observerAddress}+0" \
#   --withdrawal-tx-in-reference a96248cd1788c4b435b8ff9268236f8fa5d62faaa6cb73d600de869e7361be40#0 \
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
