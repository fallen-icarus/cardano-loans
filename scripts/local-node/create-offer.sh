#!/bin/sh

## Variables
mainDir="../../ignored/"
walletDir="${mainDir}wallets/"
loanDir="${mainDir}loan-files/"
tmpDir="${mainDir}tmp/"

borrowerAddress="addr_test1zz8g9s4nkamzh6jcgsfnjcgafrwh46sxpd7l5ajf8rzln0eualkqngnmdz2w9mv60zuucq0sswtn6lq2lwxwez76x0aqy79dk4"

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
  --epoch-duration '3600 slots' \
  --minimum-payment 1000000 \
  --fixed-penalty 500000 \
  --collateral-asset $collateral1 \
  --relative-rate '1 / 1000000' \
  --collateral-asset $collateral2 \
  --relative-rate '2 / 1000000' \
  --claim-period '3600 slots' \
  --offer-deposit $offerDeposit \
  --max-missed-payments 3 \
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
  --tx-in 4743707d8e94b35520670acd31c6e5404c9bba583a5de20bbd38b1de0d78e1a2#1 \
  --tx-out "${borrowerAddress} + ${offerDeposit} lovelace + ${loanPrincipal} ${loanAsset} + 1 ${offerBeacon} + 1 ${assetBeacon} + 1 ${lenderId}" \
  --tx-out-inline-datum-file $offerDatumFile \
  --mint "1 ${offerBeacon} + 1 ${assetBeacon} + 1 ${lenderId}" \
  --mint-tx-in-reference 6ccbd6cdff5c6305db5f249765941a79bd4fd12c1eaf451156795fbb194a5662#0 \
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
