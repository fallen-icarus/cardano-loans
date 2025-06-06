#!/bin/sh

## Variables
mainDir="../../ignored/"
walletDir="${mainDir}wallets/"
loanDir="${mainDir}loan-files/"
tmpDir="${mainDir}tmp/"

borrowerAddress="addr_test1zzc8hkkr9ygdfs02gf0d4hu8awlp8yeefm3kvglf0cw3fnpualkqngnmdz2w9mv60zuucq0sswtn6lq2lwxwez76x0aqwr65gm"

paymentAddress=$(cat "${walletDir}02.addr")
lenderStakePubKeyFile="${walletDir}02Stake.vkey"

offerDatumFile="${loanDir}offerDatum.json"
beaconRedeemerFile="${loanDir}createCloseOrUpdateOffer.json"

loanAsset='lovelace'
collateral1='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a'
collateral2='c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31'

offerDeposit=4000000
loanPrincipal=10000000

## Generate the hash for the staking verification key.
echo "Calculating the staking pubkey hash for the lender..."
lenderStakePubKeyHash=$(cardano-cli conway stake-address key-hash \
  --stake-verification-key-file $lenderStakePubKeyFile)

## Create the Offer datum. The collateral must be sorted lexicographically.
echo "Creating the offer datum..."
cardano-loans datums offer \
  --payment-address $paymentAddress \
  --lender-staking-pubkey-hash $lenderStakePubKeyHash \
  --loan-asset $loanAsset \
  --principal $loanPrincipal \
  --loan-term '10800 slots' \
  --interest '0.1' \
  --compounding-interest \
  --epoch-duration '1200 slots' \
  --minimum-payment 2000000 \
  --fixed-penalty 500000 \
  --collateral-asset $collateral1 \
  --relative-rate '1 / 1000000' \
  --collateral-asset $collateral2 \
  --relative-rate '2 / 1000000' \
  --claim-period '3600 slots' \
  --offer-deposit $offerDeposit \
  --out-file $offerDatumFile

## Create the required redeemer.
echo "Creating the negotiation beacon redeemer..."
cardano-loans redeemers negotiation-script manage-offers \
  --lender-staking-pubkey-hash $lenderStakePubKeyHash \
  --out-file $beaconRedeemerFile

## Get the negotiation beacon policy id.
echo "Calculating the negotiation beacon policy id..."
beaconPolicyId=$(cardano-loans beacon-name policy-id \
  --negotiation-beacons \
  --stdout) 

## Get the required beacon names.
offerTokenName=$(cardano-loans beacon-name asset-name \
  --offer-beacon \
  --stdout)
assetTokenName=$(cardano-loans beacon-name asset-name \
  --loan-asset $loanAsset \
  --stdout)
lenderIdTokenName=$(cardano-loans beacon-name asset-name \
  --lender-staking-pubkey-hash $lenderStakePubKeyHash \
  --stdout)

offerBeacon="${beaconPolicyId}.${offerTokenName}"
assetBeacon="${beaconPolicyId}.${assetTokenName}"
lenderId="${beaconPolicyId}.${lenderIdTokenName}"

## Create and submit the transaction.
cardano-cli conway transaction build \
  --tx-in a8d0f74ffd7bdb60e8a286e2a994af22b67c617a09e10849aeb7d06e37c52fc0#0 \
  --tx-in 8498f8e46ed2d1edef10ed90be4adf2a3d1620e6164ce6a5ca513d983c430c16#1 \
  --tx-out "${borrowerAddress} + ${offerDeposit} lovelace + ${loanPrincipal} ${loanAsset} + 1 ${offerBeacon} + 1 ${assetBeacon} + 1 ${lenderId}" \
  --tx-out-inline-datum-file $offerDatumFile \
  --mint "1 ${offerBeacon} + 1 ${assetBeacon} + 1 ${lenderId}" \
  --mint-tx-in-reference a3ae17130ddbf4ce3117e218c920d219599ff935d024fac0d3ca4ef9ad6e4fde#0 \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file $beaconRedeemerFile \
  --policy-id $beaconPolicyId \
  --required-signer-hash $lenderStakePubKeyHash \
  --change-address "$(cat ${walletDir}02.addr)" \
  --tx-in-collateral 11ed603b92e6164c6bb0c83e0f4d54a954976db7c39e2a82d3cbf70f098da1e0#0 \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.body"

cardano-cli conway transaction sign \
  --tx-body-file "${tmpDir}tx.body" \
  --signing-key-file "${walletDir}/02.skey" \
  --signing-key-file "${walletDir}/02Stake.skey" \
  --testnet-magic 1 \
  --out-file "${tmpDir}tx.signed"

cardano-cli conway transaction submit \
  --testnet-magic 1 \
  --tx-file "${tmpDir}tx.signed"
