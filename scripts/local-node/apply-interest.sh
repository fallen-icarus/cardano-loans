#!/bin/sh

## Variables
mainDir="../../ignored/"
walletDir="${mainDir}wallets/"
loanDir="${mainDir}loan-files/"
tmpDir="${mainDir}tmp/"

interestObserverScript="${loanDir}interest_observer.plutus"

lenderAddress="addr_test1vzhq6qq52k59tekqp7v04yrpq284cqxjj7fx8qau2qd795s7wfhhm"

borrowerStakePubKeyFile="${walletDir}01Stake.vkey"
borrowerLoanAddr="addr_test1zr265ke6yq0krxr5xuansyeggscxdem2w5rtk6s99deap6fualkqngnmdz2w9mv60zuucq0sswtn6lq2lwxwez76x0aqlgq56x"

activeDatumFile="${loanDir}activeDatum.json"
observerRedeemerFile="${loanDir}observeInterest.json"
loanRedeemerFile="${loanDir}applyInterest.json"

loanAsset='lovelace'
collateral1='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a'
collateral2='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31'

numberOfApplications=1
loanUTxO='ee55fa696e5e7dd29dafef7c47e36e37207e88b0a120a1156a709c429fe08c8f#1'
expirationTime=$((1712768154000)) # The loan expiration.
loanIdTokenName='4fe883a427bd96128e0fcfdbe94865c41ce92187d252f0ec45f261255f693c8f'

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
  --spending-tx-in-reference 09166e4f77c701c0607c4edaad2abf7b24a7a46d9f7ca38beead51ac8845a729#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $loanRedeemerFile \
  --tx-in 933a3df06ce0d7863185b798e9c982355102d19be016b849994b6e66c13487fd#1 \
  --tx-out "${borrowerLoanAddr} + 4000000 lovelace + 1 ${loanId} + 1 ${borrowerId} + 1 ${activeBeacon} + 1 ${activeAssetBeacon} + 8 ${collateral1} + 4 ${collateral2}" \
  --tx-out-inline-datum-file $activeDatumFile \
  --withdrawal "${observerAddress}+0" \
  --withdrawal-tx-in-reference 97a8f2492c058e3a95191628ba1584227d751c84db6b30ea056edf7142724ba8#0 \
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
