#!/bin/sh

## Variables
mainDir="../../ignored/"
walletDir="${mainDir}wallets/"
loanDir="${mainDir}loan-files/"
tmpDir="${mainDir}tmp/"

lenderAddress="addr_test1vzhq6qq52k59tekqp7v04yrpq284cqxjj7fx8qau2qd795s7wfhhm"
lenderId="00623a2b9a369454b382c131d7e3d12c4f93024022e5c5668cf0c5c25c"

borrowerStakePubKeyFile="${walletDir}01Stake.vkey"
borrowerLoanAddr="addr_test1zzz0x4advysvysx9wvxzhxlk26fc7cyh2swn9jx8a2z8mv3ualkqngnmdz2w9mv60zuucq0sswtn6lq2lwxwez76x0aqu6j55s"

activeDatumFile="${loanDir}activeDatum.json"
paymentDatumFile="${loanDir}paymentDatum.json"
negotiationRedeemerFile="${loanDir}burnNegotiationBeacons.json"
activeRedeemerFile="${loanDir}createActive.json"
loanRedeemerFile="${loanDir}acceptOffer.json"

loanAsset='lovelace'
collateral1='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a'
collateral2='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31'

offerDeposit=4000000
offerUTxO='8c4184114086e79620b1d8d1b720dcb68f7cb89fe565c11f04572c0d4dda785e#0'

askUTxO='8865c607f541efc7c374b0c486ba76beae5cba358f4de6759ec797b54684a076#0'

## Get the latest slot number.
### IMPORTANT: A local node may trail behind the actual blockchain by a few slots. So Koios may
### return a slot that your local node hasn't seen yet; this can cause transaction submission to
### fail. If this is an issue, you can get the current slot from you local node with: `cardano-cli
### query tip --testnet-magic 1`.
echo "Querying the latest slot..."
# startSlot=$(cardano-loans query current-slot \
#   --testnet)
startSlot=$(cardano-cli conway query tip --testnet-magic 1 | jq .slot)

## Convert the slot number to the required posix time.
echo "Calculating the slot's posix time..."
startTime=$(cardano-loans time convert-time \
  --slot $startSlot \
  --testnet)

## Generate the hash for the staking verification key.
echo "Calculating the staking pubkey hash for the borrower..."
borrowerStakePubKeyHash=$(cardano-cli conway stake-address key-hash \
  --stake-verification-key-file $borrowerStakePubKeyFile)

## Create the Active datum.
echo "Creating the active datum..."

cardano-loans datums active new auto \
  --testnet \
  --borrower-staking-pubkey-hash $borrowerStakePubKeyHash \
  --offer-id $offerUTxO \
  --start-time $startTime \
  --out-file $activeDatumFile

# cardano-loans datums active new manual \
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
#   --claim-period '3600 slots' \
#   --borrower-staking-pubkey-hash $borrowerStakePubKeyHash \
#   --offer-id $offerUTxO \
#   --start-time $startTime \
#   --out-file $activeDatumFile

## Create the required redeemers.
echo "Creating the negotiation beacon redeemer..."
cardano-loans redeemers negotiation-script burn-all \
  --out-file $negotiationRedeemerFile

echo "Creating the active beacon redeemer..."
cardano-loans redeemers active-script accept-offers \
  --out-file $activeRedeemerFile

echo "Creating the loan spending redeemer..."
cardano-loans redeemers loan-script accept-offer \
  --out-file $loanRedeemerFile

## Get the negotiation beacon policy id.
echo "Calculating the negotiation beacon policy id..."
negotiationPolicyId=$(cardano-loans beacon-name policy-id \
  --negotiation-beacons \
  --stdout) 

## Get the active beacon policy id.
echo "Calculating the active beacon policy id..."
activePolicyId=$(cardano-loans beacon-name policy-id \
  --active-beacons \
  --stdout) 

## Get the required beacon names.
askTokenName=$(cardano-loans beacon-name asset-name \
  --ask-beacon \
  --stdout)
offerTokenName=$(cardano-loans beacon-name asset-name \
  --offer-beacon \
  --stdout)
activeTokenName=$(cardano-loans beacon-name asset-name \
  --active-beacon \
  --stdout)
assetTokenName=$(cardano-loans beacon-name asset-name \
  --loan-asset $loanAsset \
  --stdout)
loanIdTokenName=$(cardano-loans beacon-name asset-name \
  --offer-id $offerUTxO \
  --stdout)

askBeacon="${negotiationPolicyId}.${askTokenName}"
offerBeacon="${negotiationPolicyId}.${offerTokenName}"
negotiationAssetBeacon="${negotiationPolicyId}.${assetTokenName}"
lenderIdBeacon="${negotiationPolicyId}.${lenderId}"

activeBeacon="${activePolicyId}.${activeTokenName}"
activeAssetBeacon="${activePolicyId}.${assetTokenName}"
borrowerId="${activePolicyId}.${borrowerStakePubKeyHash}"
loanId="${activePolicyId}.${loanIdTokenName}"

## Create the payment datum.
echo "Creating the payment datum..."
cardano-loans datums payment \
  --loan-id $loanIdTokenName \
  --out-file $paymentDatumFile

## Create and submit the transaction.
cardano-cli conway transaction build \
  --tx-in $offerUTxO \
  --spending-tx-in-reference 73b65770934204111b8916156c0275bfe5d52f0aa8f856d4d7359c10b7876a29#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $loanRedeemerFile \
  --tx-in $askUTxO \
  --spending-tx-in-reference 73b65770934204111b8916156c0275bfe5d52f0aa8f856d4d7359c10b7876a29#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $loanRedeemerFile \
  --tx-in 833c7fca3c73e692ef945f763aac27ce89158dcf2cc9f7e43373ffe07423c20b#2 \
  --tx-in aae9b47200c5eb99d8cdc07297d50ad47a939e90bec6123fe275d8d7676385d0#1 \
  --tx-out "${lenderAddress} + ${offerDeposit} lovelace + 1 ${loanId}" \
  --tx-out-inline-datum-file $paymentDatumFile \
  --tx-out "${borrowerLoanAddr} + 4000000 lovelace + 1 ${loanId} + 1 ${borrowerId} + 1 ${activeBeacon} + 1 ${activeAssetBeacon} + 8 ${collateral1} + 4 ${collateral2}" \
  --tx-out-inline-datum-file $activeDatumFile \
  --mint "-1 ${askBeacon} + -2 ${negotiationAssetBeacon} + -1 ${offerBeacon} + -1 ${lenderIdBeacon} + 1 ${activeBeacon} + 1 ${activeAssetBeacon} + 1 ${borrowerId} + 2 ${loanId}" \
  --mint-tx-in-reference 6014d14585f7d9f9c0c8328f63e632986549ca691b5c56a7ecf14773441d8e0e#0 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file $negotiationRedeemerFile \
  --policy-id $negotiationPolicyId \
  --mint-tx-in-reference 47f7e129697675944421aea3c2faa4cf0ee9e90b9445fffe145567e5d6476bcc#0 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file $activeRedeemerFile \
  --policy-id $activePolicyId \
  --required-signer-hash $borrowerStakePubKeyHash \
  --change-address "$(cat ${walletDir}01.addr)" \
  --tx-in-collateral 4cc5755712fee56feabad637acf741bc8c36dda5f3d6695ac6487a77c4a92d76#0 \
  --testnet-magic 1 \
  --invalid-before $startSlot \
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
