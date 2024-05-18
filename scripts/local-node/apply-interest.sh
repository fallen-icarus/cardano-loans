#!/bin/sh

## Variables
mainDir="../../ignored/"
walletDir="${mainDir}wallets/"
loanDir="${mainDir}loan-files/"
tmpDir="${mainDir}tmp/"

interestObserverScript="${loanDir}interest_observer.plutus"

lenderAddress="addr_test1vzhq6qq52k59tekqp7v04yrpq284cqxjj7fx8qau2qd795s7wfhhm"

borrowerStakePubKeyFile="${walletDir}01Stake.vkey"
borrowerLoanAddr="addr_test1zrv3ff2vrjj3rujdnggeap27r69w763dkauumks70jngey3ualkqngnmdz2w9mv60zuucq0sswtn6lq2lwxwez76x0aqe70yty"

activeDatumFile="${loanDir}activeDatum.json"
observerRedeemerFile="${loanDir}observeInterest.json"
loanRedeemerFile="${loanDir}applyInterest.json"

loanAsset='lovelace'
collateral1='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a'
collateral2='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31'

numberOfApplications=1
loanUTxO='af3e17004877cb9ec8025c0c2e3540ce4dce0c2774ba66f3962b5e358c88f41e#1'
expirationTime=$((1715804120000)) # The loan expiration.
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

## Export the interest observer script so you can generate its required stake address.
echo "Exporting the interest observer script..."
cardano-loans scripts \
  --interest-script \
  --out-file $interestObserverScript

## Build the observer script's stake address.
echo "Building the observer script's stake address..."
observerAddress=$(cardano-cli stake-address build \
  --testnet-magic 1 \
  --stake-script-file $interestObserverScript)

## Create the Active datum.
echo "Creating the active datum..."

cardano-loans datums active post-interest auto \
  --testnet \
  --loan-ref $loanUTxO \
  --times-applied $numberOfApplications \
  --out-file $activeDatumFile

# cardano-loans datums active post-interest manual \
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
#   --times-applied $numberOfApplications \
#   --out-file $activeDatumFile

## Create the required redeemers.
echo "Creating the observer redeemer..."
cardano-loans redeemers interest-script observe-interest \
  --out-file $observerRedeemerFile

echo "Creating the loan spending redeemer..."
cardano-loans redeemers loan-script apply-interest \
  --deposit-increase 0 \
  --times-applied $numberOfApplications \
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

# Create and submit the transaction.
cardano-cli transaction build \
  --tx-in $loanUTxO \
  --spending-tx-in-reference 292f25c6594169502c71ee82cd5285bba9a887a60a3b447bade71284acb172db#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $loanRedeemerFile \
  --tx-in 0094644ca620e01f8d9594f6b2ce6a4186ffcc09ff6f4d6fe72fbe99b850ae41#1 \
  --tx-out "${borrowerLoanAddr} + 4000000 lovelace + 1 ${loanId} + 1 ${borrowerId} + 1 ${activeBeacon} + 1 ${activeAssetBeacon} + 8 ${collateral1} + 3 ${collateral2}" \
  --tx-out-inline-datum-file $activeDatumFile \
  --withdrawal "${observerAddress}+0" \
  --withdrawal-tx-in-reference 146f833828ae3e29ae8460847eaf5ce32102ebb9a23614d9ced7e48000f29a1b#0 \
  --withdrawal-plutus-script-v2 \
  --withdrawal-reference-tx-in-redeemer-file $observerRedeemerFile \
  --required-signer-hash $borrowerStakePubKeyHash \
  --change-address "$(cat ${walletDir}01.addr)" \
  --tx-in-collateral 4cc5755712fee56feabad637acf741bc8c36dda5f3d6695ac6487a77c4a92d76#0 \
  --testnet-magic 1 \
  --invalid-hereafter $expirationSlot \
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
