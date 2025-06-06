#!/bin/sh

## Variables
mainDir="../../ignored/"
walletDir="${mainDir}wallets/"
loanDir="${mainDir}loan-files/"
tmpDir="${mainDir}tmp/"

paymentObserverScript="${loanDir}payment_observer.plutus"

lenderAddress="addr_test1vpz6g5ecxv6mc036lckg6w06wmj7vr073j73llzpsn5t0pguw7m5u"

borrowerStakePubKeyFile="${walletDir}01Stake.vkey"
borrowerLoanAddr="addr_test1zzc8hkkr9ygdfs02gf0d4hu8awlp8yeefm3kvglf0cw3fnpualkqngnmdz2w9mv60zuucq0sswtn6lq2lwxwez76x0aqwr65gm"

activeDatumFile="${loanDir}activeDatum.json"
paymentDatumFile="${loanDir}paymentDatum.json"
observerRedeemerFile="${loanDir}observePayment.json"
loanRedeemerFile="${loanDir}makePayment.json"

loanAsset='lovelace'
collateral1='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a'
collateral2='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31'

paymentAmount=14500000
loanUTxO='5c43cd80fb435163364854733c7db24af17984ab031e9a6a9aed8f0ff7b5ed28#0'
loanIdTokenName='9d364a309553f27e02028d0cef816bfcb3cfdd6ae60556c15e49eb557f52a368'

# Either the next epoch boundary or the loan expiration; whichever is first. This is used for 
# invalid-hereafter.
### IMPORTANT: The invalid-hereafter can be set to a maximum of 1.5 days (129600 slots) passed the 
### current slot. If the loan expiration is beyond this "horizon", the node will reject the
### transaction. This is because hardforks can change slot lengths and, therefore, the node doesn't
### want to make guarantees about time too far into the future. If the loan's expiration is more
### than 1.5 days away, set the invalid-hereafter to be: `current_slot + 129600`.
expirationTime=$((1726408979000+1200000))

## Convert the posix time to a slot number for invalid-hereafter.
echo "Calculating the required slot number..."
expirationSlot=$(cardano-loans convert-time \
  --posix-time $expirationTime \
  --testnet)

## Generate the hash for the staking verification key.
echo "Calculating the staking pubkey hash for the borrower..."
borrowerStakePubKeyHash=$(cardano-cli conway stake-address key-hash \
  --stake-verification-key-file $borrowerStakePubKeyFile)

## Export the payment observer script so you can generate its required stake address.
echo "Exporting the payment observer script..."
cardano-loans scripts \
  --payment-script \
  --out-file $paymentObserverScript

## Build the observer script's stake address.
echo "Building the observer script's stake address..."
observerAddress=$(cardano-cli conway stake-address build \
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
cardano-cli conway transaction build \
  --tx-in $loanUTxO \
  --spending-tx-in-reference 50f14254697370b7db435f93abff6e5952a6e0b7f267b033d96bac22d88c766b#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $loanRedeemerFile \
  --tx-in c835095f3339fd84276e87539a329615e8a11f286e3f76dbf9daeef93b0a0ea8#1 \
  --tx-in 5c43cd80fb435163364854733c7db24af17984ab031e9a6a9aed8f0ff7b5ed28#1 \
  --tx-in 85783aed629ea0fdee95e63d234f10c71775794c365299ca408e1bd6b8218911#1 \
  --tx-out "${lenderAddress} + ${paymentAmount} ${loanAsset}" \
  --tx-out-inline-datum-file $paymentDatumFile \
  --tx-out "$(cat ${walletDir}01.addr) + 3000000 lovelace + 8 ${collateral1}" \
  --tx-out "$(cat ${walletDir}01.addr) + 3000000 lovelace + 4 ${collateral2}" \
  --mint "-1 ${borrowerId} + -1 ${activeBeacon} + -1 ${activeAssetBeacon} + -1 ${loanId}" \
  --mint-tx-in-reference 03d6221ffb7a85284a8871a18b6276788f99ec5caff69af098d7e9b4a6e14dec#0 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file $activeRedeemerFile \
  --policy-id $activePolicyId \
  --withdrawal "${observerAddress}+0" \
  --withdrawal-tx-in-reference 670526b45c968321def17ca18cf0e507383e7fd596c45dd5a87b92d1f97943bc#0 \
  --withdrawal-plutus-script-v2 \
  --withdrawal-reference-tx-in-redeemer-file $observerRedeemerFile \
  --required-signer-hash $borrowerStakePubKeyHash \
  --change-address "$(cat ${walletDir}01.addr)" \
  --tx-in-collateral 4cc5755712fee56feabad637acf741bc8c36dda5f3d6695ac6487a77c4a92d76#0 \
  --testnet-magic 1 \
  --invalid-hereafter $expirationSlot \
  --out-file "${tmpDir}tx.body"

# # Parial payment transaction.
# cardano-cli conway transaction build \
#   --tx-in $loanUTxO \
#   --spending-tx-in-reference 50f14254697370b7db435f93abff6e5952a6e0b7f267b033d96bac22d88c766b#0 \
#   --spending-plutus-script-v2 \
#   --spending-reference-tx-in-inline-datum-present \
#   --spending-reference-tx-in-redeemer-file $loanRedeemerFile \
#   --tx-in c835095f3339fd84276e87539a329615e8a11f286e3f76dbf9daeef93b0a0ea8#1 \
#   --tx-in 95de5a789ccedbf1444b8668e5724aa4ed95d799d616aae1e4d408d56c2c3485#0 \
#   --tx-in 85783aed629ea0fdee95e63d234f10c71775794c365299ca408e1bd6b8218911#1 \
#   --tx-out "${lenderAddress} + ${paymentAmount} ${loanAsset}" \
#   --tx-out-inline-datum-file $paymentDatumFile \
#   --tx-out "${borrowerLoanAddr} + 4000000 lovelace + 1 ${loanId} + 1 ${borrowerId} + 1 ${activeBeacon} + 1 ${activeAssetBeacon} + 2 ${collateral2}" \
#   --tx-out-inline-datum-file $activeDatumFile \
#   --tx-out "$(cat ${walletDir}01.addr) + 3000000 lovelace + 8 ${collateral1}" \
#   --tx-out "$(cat ${walletDir}01.addr) + 3000000 lovelace + 2 ${collateral2}" \
#   --withdrawal "${observerAddress}+0" \
#   --withdrawal-tx-in-reference 670526b45c968321def17ca18cf0e507383e7fd596c45dd5a87b92d1f97943bc#0 \
#   --withdrawal-plutus-script-v2 \
#   --withdrawal-reference-tx-in-redeemer-file $observerRedeemerFile \
#   --required-signer-hash $borrowerStakePubKeyHash \
#   --change-address "$(cat ${walletDir}01.addr)" \
#   --tx-in-collateral 4cc5755712fee56feabad637acf741bc8c36dda5f3d6695ac6487a77c4a92d76#0 \
#   --testnet-magic 1 \
#   --invalid-hereafter $expirationSlot \
#   --out-file "${tmpDir}tx.body"

cardano-cli conway transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file "${walletDir}/01.skey" \
  --signing-key-file "${walletDir}/01Stake.skey" \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli conway transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"
