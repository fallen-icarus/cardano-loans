#!/bin/sh

## Variables
mainDir="../../ignored/"
walletDir="${mainDir}wallets/"
loanDir="${mainDir}loan-files/"
tmpDir="${mainDir}tmp/"

paymentObserverScript="${loanDir}payment_observer.plutus"

lenderAddress="addr_test1vzhq6qq52k59tekqp7v04yrpq284cqxjj7fx8qau2qd795s7wfhhm"

borrowerStakePubKeyFile="${walletDir}01Stake.vkey"
borrowerLoanAddr="addr_test1zz8g9s4nkamzh6jcgsfnjcgafrwh46sxpd7l5ajf8rzln0eualkqngnmdz2w9mv60zuucq0sswtn6lq2lwxwez76x0aqy79dk4"

activeDatumFile="${loanDir}activeDatum.json"
paymentDatumFile="${loanDir}paymentDatum.json"
observerRedeemerFile="${loanDir}observePayment.json"
loanRedeemerFile="${loanDir}makePayment.json"

loanAsset='lovelace'
collateral1='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a'
collateral2='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31'

paymentAmount=11000001
loanUTxO='8e781e2edcff81a31bd0c82359890036db85b7f9b7006231f96c86a5d325ab3a#1'
loanIdTokenName='d58bc66b3f3778e572797ee4d96d0f81adda164f7865c452e682cf28bcec6362'

# Either the next epoch boundary or the loan expiration; whichever is first. This is used for 
# invalid-hereafter.
### IMPORTANT: The invalid-hereafter can be set to a maximum of 1.5 days (129600 slots) passed the 
### current slot. If the loan expiration is beyond this "horizon", the node will reject the
### transaction. This is because hardforks can change slot lengths and, therefore, the node doesn't
### want to make guarantees about time too far into the future. If the loan's expiration is more
### than 1.5 days away, set the invalid-hereafter to be: `current_slot + 129600`.
lastEpochBoundary=1763920160000
epochDuration=3600000
currentSlot=$(cardano-loans query current-slot --testnet)
currentTime=$(cardano-loans time convert-time --slot $currentSlot --testnet)
boundaryTime=$(cardano-loans time calc-next-boundary \
  --last-epoch-boundary $lastEpochBoundary \
  --epoch-duration $epochDuration \
  --current-time $currentTime)

## Convert the posix time to a slot number for invalid-hereafter.
echo "Calculating the required slot number..."
boundarySlot=$(cardano-loans time convert-time \
  --posix-time $boundaryTime \
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
  --next-epoch-boundary $boundaryTime \
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
#   --next-epoch-boundary $boundaryTime \
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
  --spending-tx-in-reference 575c63eff95683a81f97e4f34fc7caedac5045bdb0c7ed79d93bb6905dcebc31#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $loanRedeemerFile \
  --tx-in 0afac3458b96ea93232777a701f27051f2fff0a61bf0ae88481e3be83d649cfa#3 \
  --tx-out "${lenderAddress} + ${paymentAmount} ${loanAsset}" \
  --tx-out-inline-datum-file $paymentDatumFile \
  --mint "-1 ${borrowerId} + -1 ${activeBeacon} + -1 ${activeAssetBeacon} + -1 ${loanId}" \
  --mint-tx-in-reference 106d811ab7f51d2c84a16ac00e8c091d80c92b893809ac8c898c0b277361ad6d#0 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file $activeRedeemerFile \
  --policy-id $activePolicyId \
  --withdrawal "${observerAddress}+0" \
  --withdrawal-tx-in-reference 31555c4be564002ff811f9a15097991107e338a5cc9beaa10a0a037a84b36c21#0 \
  --withdrawal-plutus-script-v2 \
  --withdrawal-reference-tx-in-redeemer-file $observerRedeemerFile \
  --required-signer-hash $borrowerStakePubKeyHash \
  --change-address "$(cat ${walletDir}01.addr)" \
  --tx-in-collateral 4cc5755712fee56feabad637acf741bc8c36dda5f3d6695ac6487a77c4a92d76#0 \
  --testnet-magic 1 \
  --invalid-hereafter $boundarySlot \
  --out-file "${tmpDir}tx.body"

# # Parial payment transaction.
# cardano-cli conway transaction build \
#   --tx-in $loanUTxO \
#   --spending-tx-in-reference 73b65770934204111b8916156c0275bfe5d52f0aa8f856d4d7359c10b7876a29#0 \
#   --spending-plutus-script-v2 \
#   --spending-reference-tx-in-inline-datum-present \
#   --spending-reference-tx-in-redeemer-file $loanRedeemerFile \
#   --tx-in e653cd28e2e08011362481967f8494a65c651b42c3d53e021aae2c775a97be59#0 \
#   --tx-out "${lenderAddress} + ${paymentAmount} ${loanAsset}" \
#   --tx-out-inline-datum-file $paymentDatumFile \
#   --tx-out "${borrowerLoanAddr} + 5000000 lovelace + 1 ${loanId} + 1 ${borrowerId} + 1 ${activeBeacon} + 1 ${activeAssetBeacon} + 8 ${collateral1} + 3 ${collateral2}" \
#   --tx-out-inline-datum-file $activeDatumFile \
#   --withdrawal "${observerAddress}+0" \
#   --withdrawal-tx-in-reference 6ad871a70308dd5d0e63ab6b1c7bf747c00a03c839ffdef7232b480097d6b3ac#0 \
#   --withdrawal-plutus-script-v2 \
#   --withdrawal-reference-tx-in-redeemer-file $observerRedeemerFile \
#   --required-signer-hash $borrowerStakePubKeyHash \
#   --change-address "$(cat ${walletDir}01.addr)" \
#   --tx-in-collateral 4cc5755712fee56feabad637acf741bc8c36dda5f3d6695ac6487a77c4a92d76#0 \
#   --testnet-magic 1 \
#   --invalid-hereafter $boundarySlot \
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
