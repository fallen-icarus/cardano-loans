#!/bin/sh

## Variables
mainDir="../../ignored/"
walletDir="${mainDir}wallets/"
loanDir="${mainDir}loan-files/"
tmpDir="${mainDir}tmp/"

lenderAddress="addr_test1vzhq6qq52k59tekqp7v04yrpq284cqxjj7fx8qau2qd795s7wfhhm"
lenderId="00623a2b9a369454b382c131d7e3d12c4f93024022e5c5668cf0c5c25c"

borrowerStakePubKeyFile="${walletDir}01Stake.vkey"
borrowerLoanAddr="addr_test1zrv3ff2vrjj3rujdnggeap27r69w763dkauumks70jngey3ualkqngnmdz2w9mv60zuucq0sswtn6lq2lwxwez76x0aqe70yty"

activeDatumFile="${loanDir}activeDatum.json"
paymentDatumFile="${loanDir}paymentDatum.json"
negotiationRedeemerFile="${loanDir}burnNegotiationBeacons.json"
activeRedeemerFile="${loanDir}createActive.json"
loanRedeemerFile="${loanDir}acceptOffer.json"

loanAsset='lovelace'
collateral1='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a'
collateral2='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31'

offerDeposit=4000000
offerUTxO='677b9d48ad49cbbe6763bfea889ead8577219d6f3881eb4005372710c61f44d5#0'

askUTxO='6fc9e482959befc6928d2691caa62c8a52a5abce167bd5fce8874cc315bb346d#2'

## Get the latest slot number.
echo "Querying the latest slot..."
startSlot=$(cardano-loans query current-slot \
  --testnet)

## Convert the slot number to the required posix time.
echo "Calculating the slot's posix time..."
startTime=$(cardano-loans convert-time \
  --slot $startSlot \
  --testnet)

## Generate the hash for the staking verification key.
echo "Calculating the staking pubkey hash for the borrower..."
borrowerStakePubKeyHash=$(cardano-cli stake-address key-hash \
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
#   --compound-frequency '1200 slots' \
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
cardano-cli transaction build \
  --tx-in $offerUTxO \
  --spending-tx-in-reference 292f25c6594169502c71ee82cd5285bba9a887a60a3b447bade71284acb172db#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $loanRedeemerFile \
  --tx-in $askUTxO \
  --spending-tx-in-reference 292f25c6594169502c71ee82cd5285bba9a887a60a3b447bade71284acb172db#0 \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file $loanRedeemerFile \
  --tx-in 6fc9e482959befc6928d2691caa62c8a52a5abce167bd5fce8874cc315bb346d#3 \
  --tx-in 6fc9e482959befc6928d2691caa62c8a52a5abce167bd5fce8874cc315bb346d#0 \
  --tx-in 6fc9e482959befc6928d2691caa62c8a52a5abce167bd5fce8874cc315bb346d#1 \
  --tx-out "${lenderAddress} + ${offerDeposit} lovelace + 1 ${loanId}" \
  --tx-out-inline-datum-file $paymentDatumFile \
  --tx-out "${borrowerLoanAddr} + 4000000 lovelace + 1 ${loanId} + 1 ${borrowerId} + 1 ${activeBeacon} + 1 ${activeAssetBeacon} + 8 ${collateral1} + 4 ${collateral2}" \
  --tx-out-inline-datum-file $activeDatumFile \
  --tx-out "$(cat ${walletDir}01.addr) + 3000000 lovelace + 22 ${collateral1}" \
  --tx-out "$(cat ${walletDir}01.addr) + 3000000 lovelace + 950 ${collateral2}" \
  --mint "-1 ${askBeacon} + -2 ${negotiationAssetBeacon} + -1 ${offerBeacon} + -1 ${lenderIdBeacon} + 1 ${activeBeacon} + 1 ${activeAssetBeacon} + 1 ${borrowerId} + 2 ${loanId}" \
  --mint-tx-in-reference dd0e2977d8ea2af53c9d1cd5fea19e09f15eef356b91314835316f649375c1c8#0 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file $negotiationRedeemerFile \
  --policy-id $negotiationPolicyId \
  --mint-tx-in-reference 9620379842501763c80c3737d219ee10b25f00a0449fd2a35457d1fb5dc08bb7#0 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file $activeRedeemerFile \
  --policy-id $activePolicyId \
  --required-signer-hash $borrowerStakePubKeyHash \
  --change-address "$(cat ${walletDir}01.addr)" \
  --tx-in-collateral 4cc5755712fee56feabad637acf741bc8c36dda5f3d6695ac6487a77c4a92d76#0 \
  --testnet-magic 1 \
  --invalid-before $startSlot \
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
