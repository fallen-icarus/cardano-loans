#!/bin/sh

## Variables
mainDir="../../ignored/"
walletDir="${mainDir}wallets/"
loanDir="${mainDir}loan-files/"
tmpDir="${mainDir}tmp/"

interestObserverScript="${loanDir}interest_observer.plutus"

lenderAddress="addr_test1vzhq6qq52k59tekqp7v04yrpq284cqxjj7fx8qau2qd795s7wfhhm"

borrowerStakePubKeyFile="${walletDir}01Stake.vkey"
borrowerLoanAddr="addr_test1zzc8hkkr9ygdfs02gf0d4hu8awlp8yeefm3kvglf0cw3fnpualkqngnmdz2w9mv60zuucq0sswtn6lq2lwxwez76x0aqwr65gm"

activeDatumFile="${loanDir}activeDatum.json"
observerRedeemerFile="${loanDir}observeInterest.json"
loanRedeemerFile="${loanDir}applyInterest.json"

loanAsset='lovelace'
collateral1='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a'
collateral2='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31'

numberOfApplications=1
loanUTxO='292ccf2bc14d685258bf0d3f070d4319eca74889269c962451ccb1ef6462084d#0'
expirationTime=$((1726417379000)) # The loan expiration.
loanIdTokenName='9d364a309553f27e02028d0cef816bfcb3cfdd6ae60556c15e49eb557f52a368'

## Convert the posix time to a slot number for invalid-hereafter.
### IMPORTANT: The invalid-hereafter can be set to a maximum of 1.5 days (129600 slots) passed the 
### current slot. If the loan expiration is beyond this "horizon", the node will reject the
### transaction. This is because hardforks can change slot lengths and, therefore, the node doesn't
### want to make guarantees about time too far into the future. If the loan's expiration is more
### than 1.5 days away, set the invalid-hereafter to be: `current_slot + 129600`.
echo "Calculating the required slot number..."
expirationSlot=$(cardano-loans convert-time \
  --posix-time $expirationTime \
  --testnet)

## Generate the hash for the staking verification key.
echo "Calculating the staking pubkey hash for the borrower..."
borrowerStakePubKeyHash=$(cardano-cli conway stake-address key-hash \
  --stake-verification-key-file $borrowerStakePubKeyFile)

## Export the interest observer script so you can generate its required stake address.
echo "Exporting the interest observer script..."
cardano-loans scripts \
  --interest-script \
  --out-file $interestObserverScript

## Build the observer script's stake address.
echo "Building the observer script's stake address..."
observerAddress=$(cardano-cli conway stake-address build \
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
#   --last-epoch-boundary '1726406579000' \
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
  --deposit-increase 51400 \
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
cardano-cli conway transaction build \
  --tx-in $loanUTxO \
  --spending-tx-in-reference 50f14254697370b7db435f93abff6e5952a6e0b7f267b033d96bac22d88c766b#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $loanRedeemerFile \
  --tx-in 95de5a789ccedbf1444b8668e5724aa4ed95d799d616aae1e4d408d56c2c3485#0 \
  --tx-out "${borrowerLoanAddr} + 4051400 lovelace + 1 ${loanId} + 1 ${borrowerId} + 1 ${activeBeacon} + 1 ${activeAssetBeacon} + 8 ${collateral1} + 4 ${collateral2}" \
  --tx-out-inline-datum-file $activeDatumFile \
  --withdrawal "${observerAddress}+0" \
  --withdrawal-tx-in-reference afb8696b7025e9cea04bd7a51834d6a18af2936ffd8c7947ebf673631a9dbb3d#0 \
  --withdrawal-plutus-script-v2 \
  --withdrawal-reference-tx-in-redeemer-file $observerRedeemerFile \
  --required-signer-hash $borrowerStakePubKeyHash \
  --change-address "$(cat ${walletDir}01.addr)" \
  --tx-in-collateral 4cc5755712fee56feabad637acf741bc8c36dda5f3d6695ac6487a77c4a92d76#0 \
  --testnet-magic 1 \
  --invalid-hereafter $expirationSlot \
  --out-file "${tmpDir}tx.body"

cardano-cli conway transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file "${walletDir}/01.skey" \
  --signing-key-file "${walletDir}/01Stake.skey" \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli conway transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"
