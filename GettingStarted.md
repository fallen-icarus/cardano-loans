# Getting Started

:warning: Assumes a local PreProduction Testnet node running locally and `cardano-cli` installed since it is used to actually build and sign transactions.

Template bash scripts that follow these steps are available [here](scripts/).

When integration testing, it is highly recommended that you change the string passed to the mkBeaconPolicy function [here](src/CardanoLoans.hs#L168). When developers make mistakes (myself included), it can create bad/locked utxos that will appear when you query the beacons. This can complicate your own testing. To avoid this, this extra parameter was added. Change the string to something unique to you. You should remember to change it to the desired string for mainnet.

**Make this change before building the executable in the next section.**

The `cardano-loans` CLI uses the Blockfrost API endpoints for the Preproduction Testnet (Koios does not have endpoints for the Preproduction Testnet). You will need an api key to query the beacon tokens. You can go [here](https://blockfrost.io/#pricing) to get one for free; only an email address is required.

If a specific beacon token has never been minted before, querying the Blockfrost endpoints will return "The requested component has not been found." This is due to the beacon name being part of the Blockfrost api url like:

``` Url
https://cardano-preprod.blockfrost.io/api/v0/assets/{beacon_name}/addresses
```

If the beacon has not been minted before, this URL does not exist yet. Once the beacon is minted, the URL is generated. If the beacons have been minted before but there are currently no beacons in circulation, then the API will return an empty list. This is relevant when querying a borrower's credit history. If the borrower has never taken out a loan before, then you will get "The requested component hash not been found." A future version can make this error more user friendly.

---
## Table of Contents
- [Installing](#installing)
- [Minting Test Tokens](#minting-test-tokens)
- [Borrower Actions](#borrower-actions)
  - [Asking for a loan](#asking-for-a-loan)
  - [Checking all own asks](#checking-all-own-asks)
  - [Checking all offers](#checking-all-offers)
  - [Accepting a loan offer](#accepting-a-loan-offer)
  - [Checking all current loans](#checking-all-current-loans)
  - [Making a loan payment](#making-a-loan-payment)
  - [Closing an ask](#closing-an-ask)
- [Lender Actions](#lender-actions)
  - [Checking all current asks](#checking-all-current-asks)
  - [Checking a borrower's credit history](#checking-a-borrowers-credit-history)
  - [Creating an offer](#creating-an-offer)
  - [Checking all current offers](#checking-all-current-offers)
  - [Checking all current loans](#checking-all-current-loans-1)
  - [Claiming an expired/paid loan](#claiming-an-expiredpaid-loan)
  - [Closing an offer](#closing-an-offer)
- [Convert POSIX time <--> Slot](#convert-posix-time----slot)

---
## Installing
Instructions are adapted from the [plutus-pioneers-program](https://github.com/input-output-hk/plutus-pioneer-program/tree/third-iteration) iteration 3, week 1 exercise.

1. Install NixOS cross-referencing the following resources.
     - https://nixos.org/download.html
     - https://docs.plutus-community.com
     - A few resources to understand the what and why regarding NixOS
       - https://nixos.org/manual/nix/stable
       - https://serokell.io/blog/what-is-nix
2. Set-up IOHK binary caches [How to set up the IOHK binary caches](https://github.com/input-output-hk/plutus-apps#iohk-binary-cache). "If you do not do this, you will end up building GHC, which takes several hours. If you find yourself building GHC, *stop* and fix the cache."

3. After adding the cache, you will need to restart the nix service. This can be done by executing `sudo systemctl restart nix` or by restarting your machine. If the cache was configured properly, you should see a lot of `copying path ... from 'https://cache.iog.io'` when you execute `nix-shell` in the next step.

4. Execute the following:
```
git clone https://github.com/fallen-icarus/cardano-loans
git clone https://github.com/input-output-hk/plutus-apps
cd plutus-apps
git checkout v1.0.0
nix-shell           # this may take a while the first time

# Your terminal should now have a nix-shell prompt

cd ../cardano-loans
cabal clean
cabal update
cabal build all
```
The `cardano-loans` CLI program should now be at `dist-newstyle/build/x86_64-linux/ghc-8.10.7/cardano-loans-0.1.0.0/x/cardano-loans/build/cardano-loans/cardano-loans`. Move the program to somewhere in your $PATH.

You can now exit the nix-shell with `exit`.

All `cardano-loans` subcommands have an associated `--help` option. The functionality is meant to feel like `cardano-cli`.

--- 
## Minting test tokens
An always succeeding minting policy as well as the required redeemer are included [here](scripts/mint-test-tokens/). In that directory is also the template bash script that uses them. These can be used to create as many native tokens as needed to test this lending dApp.

---
## Borrower Actions

### Asking for a loan
1. Export the loan validator script.
``` Bash
cardano-loans export-script \
  --loan-script \
  --out-file loan.plutus
```

2. Calculate the hash for the borrower's staking verification key.
``` Bash
borrowerPubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file borrowerStake.vkey)
```

Borrowers must use their staking pubkey for signing everything.

3. Create the borrower's loan address.
``` Bash
cardano-cli address build \
  --payment-script-file loan.plutus \
  --stake-verification-key-file borrowerStake.vkey \
  --testnet-magic 1 \
  --out-file loan.addr
```

4. Export the beacon policy script.
``` Bash
cardano-loans export-script \
  --beacon-policy \
  --out-file beacons.plutus
```

5. Get the beacon policy id.
``` Bash
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file beacons.plutus) 
```

6. Create the AskDatum.
``` Bash
cardano-loans loan-datum ask-datum \
  --beacon-policy-id $beaconPolicyId \
  --borrower-stake-pubkey-hash $borrowerPubKeyHash \
  --loan-asset-is-lovelace \
  --principle 10000000 \
  --loan-term 3600 \
  --collateral-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --collateral-asset-token-name 4f74686572546f6b656e0a \
  --out-file askDatum.json
```

The `principle` is in units of the loan asset. **When the loan asset is lovelace, make sure the `principle` is also in lovelace.**

The `loan-term` option is the number of slots the loan will be valid for once started. The CLI program will convert this to POSIX time (1 slot = 1000 POSIXTime).

The `collateral-asset` fields are where you state what assets you are willing to put up as collateral for the loan. You can suggest more than one asset by repeating the two fields like:

``` Bash
  --collateral-asset-policy-id ... \
  --collateral-asset-token-name ... \
  --collateral-asset-policy-id ... \
  --collateral-asset-token-name ... \
```

The lender will tell you what collateral rates they want for each asset.

7. Create the `MintAskToken` redeemer.
``` Bash
cardano-loans beacon-redeemer mint-ask \
  --borrower-stake-pubkey-hash $borrowerPubKeyHash \
  --out-file mintAsk.json
```

8. Create a helper beacon variable.
``` Bash
askTokenName="41736b" # This is the hexidecimal encoding for 'Ask'.
askBeacon="${beaconPolicyId}.${askTokenName}"
```

9. Create and submit the transaction.
``` Bash
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file protocol.json

cardano-cli transaction build \
  --tx-in <borrower_utxo> \
  --tx-out "$(cat loan.addr) + 2000000 lovelace + 1 ${askBeacon}" \
  --tx-out-inline-datum-file askDatum.json \
  --mint "1 ${askBeacon}" \
  --mint-script-file beacons.plutus \
  --mint-redeemer-file mintAsk.json \
  --required-signer-hash $borrowerPubKeyHash \
  --change-address <borrower_personal_addr> \
  --tx-in-collateral <borrower_collateral_utxo> \
  --testnet-magic 1 \
  --protocol-params-file protocol.json \
  --out-file tx.body

cardano-cli transaction sign \
  --tx-body-file tx.body \
  --signing-key-file borrowerPayment.skey \
  --signing-key-file borrowerStake.skey \
  --testnet-magic 1 \
  --out-file tx.signed

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file tx.signed
```

The borrower must sign with both their payment and stake keys since the beacon policy checks for the staking key signature while spending the UTxOs requires the payment key signature.

### Checking all own asks
This option makes it easy for a borrower to see his/her own asks inside their loan address.

``` Bash
cardano-loans query own-asks \
  --preprod-testnet $(cat api.txt) \
  --beacon-policy-id <beacon_policy_id> \
  --loan-address $(cat loan.addr) \
  --stdout
```

Here is an example response when piped to `jq`:

``` JSON
[
  {
    "address": "addr_test1zq749l3erdr67mmukh3mct038q5et2lkpgnqszgsx4n6n5eualkqngnmdz2w9mv60zuucq0sswtn6lq2lwxwez76x0aq0tg2ct",
    "loan_info": {
      "borrower_id": "3cefec09a27b6894e2ed9a78b9cc01f083973d7c0afb8cec8bda33fa",
      "collateral": [
        "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a"
      ],
      "loan_asset": "lovelace",
      "loan_beacon": "f5ba317f03ff0868a6067f3b3a3f98199b037184ad4eaecafdf1d79e",
      "principle": 10000000,
      "term": 3600
    },
    "output_index": "0",
    "tx_hash": "3aa90ec234b31c11f1346c3fdc04f2ab6b5279b893519f0545cbb013b53af7bd",
    "type": "Ask",
    "utxo_assets": [
      {
        "asset": "lovelace",
        "quantity": 2000000
      },
      {
        "asset": "f5ba317f03ff0868a6067f3b3a3f98199b037184ad4eaecafdf1d79e.41736b",
        "quantity": 1
      }
    ]
  }
]
```
This borrower only has one open ask. This returns all of the information necessary for the borrower to act on. If the borrower would like to change something, this Ask must be closed and a new one must be opened.

### Checking all offers
``` Bash
cardano-loans query all-offers \
  --preprod-testnet $(cat api.txt) \
  --beacon-policy-id <beacon_policy_id> \
  --loan-address $(cat loan.addr) \
  --stdout
```

Here is an example response when piped to `jq`:
``` JSON
[
  {
    "address": "addr_test1zq749l3erdr67mmukh3mct038q5et2lkpgnqszgsx4n6n5eualkqngnmdz2w9mv60zuucq0sswtn6lq2lwxwez76x0aq0tg2ct",
    "loan_info": {
      "collateralization": [
        [
          "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a",
          {
            "denominator": 500000,
            "numerator": 1
          }
        ]
      ],
      "interest": {
        "denominator": 10,
        "numerator": 1
      },
      "lender_id": "ae0d001455a855e6c00f98fa9061028f5c00d297926383bc501be2d2",
      "loan_asset": "lovelace",
      "loan_beacon": "f5ba317f03ff0868a6067f3b3a3f98199b037184ad4eaecafdf1d79e",
      "principle": 10000000,
      "term": 3600
    },
    "output_index": "0",
    "tx_hash": "7b025d0b57dccf535f9eff36c377b83bbcbb2753314604d17136bea84d2183ab",
    "type": "Offer",
    "utxo_assets": [
      {
        "asset": "lovelace",
        "quantity": 13000000
      },
      {
        "asset": "f5ba317f03ff0868a6067f3b3a3f98199b037184ad4eaecafdf1d79e.4f66666572",
        "quantity": 1
      },
      {
        "asset": "f5ba317f03ff0868a6067f3b3a3f98199b037184ad4eaecafdf1d79e.ae0d001455a855e6c00f98fa9061028f5c00d297926383bc501be2d2",
        "quantity": 1
      }
    ]
  }
]
```

Only one offer was made. Lender `ae0d001455a855e6c00f98fa9061028f5c00d297926383bc501be2d2` has made this offer. This returns all of the information necessary for the borrower to decide on and accept the offer. The loan amount of 10 ADA is present in this Offer UTxO.

The `collateralization` field shows a list of the ratios the lender would like for the collateral you mentioned in your Ask. This lender would like 2 of your collateral asset for every 1 ADA you borrow.

The non-compounding interest being offered is 10%.

The other fields should match what you asked for. In order to accept the loan, all the other fields **must** match what you asked for. Also, the collateralization list must appear in the same order (based on the asset name) as your collateral list in order for acceptance to succeed.

### Accepting a loan offer
1. Export the loan validator script.
``` Bash
cardano-loans export-script \
  --loan-script \
  --out-file loan.plutus
```

2. Calculate the hash for the borrower's staking verification key.
``` Bash
borrowerPubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file borrowerStake.vkey)
```

3. Export the beacon policy script.
``` Bash
cardano-loans export-script \
  --beacon-policy \
  --out-file beacons.plutus
```

4. Get the beacon policy id.
``` Bash
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file beacons.plutus)
```

5. Create the `AcceptOffer` spending redeemer.
``` Bash
cardano-loans loan-redeemer \
  --accept-offer \
  --out-file acceptOffer.json
```

6. Create the active datum.
``` Bash
cardano-loans loan-datum accept-datum \
  --beacon-policy-id $beaconPolicyId \
  --lender-payment-pubkey-hash <lender_id> \
  --borrower-stake-pubkey-hash $borrowerPubKeyHash \
  --loan-asset-is-lovelace \
  --principle 10000000 \
  --loan-term 3600 \
  --interest-numerator 1 \
  --interest-denominator 10 \
  --collateral-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --collateral-asset-token-name 4f74686572546f6b656e0a \
  --rate-numerator 1 \
  --rate-denominator 500000 \
  --expiration $((<invalid_before_slot_number> + 3600)) \
  --out-file activeDatum.json
```

You must make sure to calculate the proper expiration slot based off the `loan-term` and the invalid-before slot number used for the transaction. The fields in the datum should match what is in the offer and ask datums.

7. Create the `MintActiveToken` redeemer.
``` Bash
cardano-loans beacon-redeemer mint-active \
  --borrower-stake-pubkey-hash $borrowerPubKeyHash \
  --lender-payment-pubkey-hash <lender_id> \
  --out-file mintActive.json
```

8. Create helper beacon variables.
``` Bash
activeTokenName="416374697665" # This is the hexidecimal encoding for 'Active'.
askTokenName="41736b" # This is the hexidecimal encoding for 'Ask'.
offerTokenName="4f66666572" # This is the hexidecimal encoding for 'Offer'.

askBeacon="${beaconPolicyId}.${askTokenName}"
offerBeacon="${beaconPolicyId}.${offerTokenName}"
lenderBeacon="${beaconPolicyId}.<lender_id>"
activeBeacon="${beaconPolicyId}.${activeTokenName}"
borrowerBeacon="${beaconPolicyId}.${borrowerPubKeyHash}"
```

9. Create and submit the transaction.
``` Bash
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file protocol.json

cardano-cli transaction build \
  --tx-in <borrower_utxo_with_loan_collateral_and_fee> \
  --tx-in <ask_utxo> \
  --tx-in-script-file loan.plutus \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file acceptOffer.json \
  --tx-in <offer_utxo> \
  --tx-in-script-file loan.plutus \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file acceptOffer.json \
  --tx-out "$(cat loan.addr) + 3000000 lovelace + 1 ${activeBeacon} + 1 ${lenderBeacon} + 1 ${borrowerBeacon} + 20 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file activeDatum.json \
  --mint "1 ${activeBeacon} + 1 ${borrowerBeacon} + -1 ${askBeacon} + -1 ${offerBeacon}" \
  --mint-script-file beacons.plutus \
  --mint-redeemer-file mintActive.json \
  --required-signer-hash $borrowerPubKeyHash \
  --change-address <borrowers_personal_addr> \
  --tx-in-collateral <borrower_collateral_utxo> \
  --testnet-magic 1 \
  --protocol-params-file protocol.json \
  --invalid-before <slot_where_loan_starts> \
  --out-file tx.body

cardano-cli transaction sign \
  --tx-body-file tx.body \
  --signing-key-file borrowerPayment.skey \
  --signing-key-file borrowerStake.skey \
  --testnet-magic 1 \
  --out-file tx.signed

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file tx.signed
```

Since 10 ADA was borrowed, 20 of the collateral asset needed to be deposited.

### Checking all current loans
This option is for the convenience of checking the loan information in the borrower's address.

``` Bash
cardano-loans query borrower-loans \
  --borrower-stake-pubkey-hash <borrower_stake_pubkey_hash> \
  --loan-address $(cat loan.addr) \
  --preprod-testnet $(cat api.txt) \
  --stdout
```

Here is an example response when piped to `jq`:
``` JSON
[
  {
    "address": "addr_test1zq749l3erdr67mmukh3mct038q5et2lkpgnqszgsx4n6n5eualkqngnmdz2w9mv60zuucq0sswtn6lq2lwxwez76x0aq0tg2ct",
    "loan_info": {
      "balance_owed": {
        "denominator": 1,
        "numerator": 11000000
      },
      "borrower_id": "3cefec09a27b6894e2ed9a78b9cc01f083973d7c0afb8cec8bda33fa",
      "collateralization": [
        [
          "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a",
          {
            "denominator": 500000,
            "numerator": 1
          }
        ]
      ],
      "expiration_slot": 26655777,
      "interest": {
        "denominator": 10,
        "numerator": 1
      },
      "lender_id": "ae0d001455a855e6c00f98fa9061028f5c00d297926383bc501be2d2",
      "loan_asset": "lovelace",
      "loan_beacon": "f5ba317f03ff0868a6067f3b3a3f98199b037184ad4eaecafdf1d79e",
      "principle": 10000000,
      "term": 600
    },
    "output_index": "0",
    "tx_hash": "9f7143d32545ac4c2c1ce0833b3f75a0a2969279cfc855f44ee5442417c99b18",
    "type": "Loan",
    "utxo_assets": [
      {
        "asset": "lovelace",
        "quantity": 3000000
      },
      {
        "asset": "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a",
        "quantity": 20
      },
      {
        "asset": "f5ba317f03ff0868a6067f3b3a3f98199b037184ad4eaecafdf1d79e.3cefec09a27b6894e2ed9a78b9cc01f083973d7c0afb8cec8bda33fa",
        "quantity": 1
      },
      {
        "asset": "f5ba317f03ff0868a6067f3b3a3f98199b037184ad4eaecafdf1d79e.416374697665",
        "quantity": 1
      },
      {
        "asset": "f5ba317f03ff0868a6067f3b3a3f98199b037184ad4eaecafdf1d79e.ae0d001455a855e6c00f98fa9061028f5c00d297926383bc501be2d2",
        "quantity": 1
      }
    ]
  }
]
```

Only one loan was found. Some of the information is there for the credit history. The `balance_owed` field and the `expiration_slot` field are the most important fields for an active loan. The starting `balance_owed` for a loan is always:
```
principle * (1 + interest)
```

### Making a loan payment
1. Export the loan validator script.
``` Bash
cardano-loans export-script \
  --loan-script \
  --out-file loan.plutus
```

2. Export the beacon policy script.
``` Bash
cardano-loans export-script \
  --beacon-policy \
  --out-file beacons.plutus
```

3. Calculate the beacon policy id.
``` Bash
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file beacons.plutus)
```

4. Calculate the hash of the borrower's staking verification key.
``` Bash
borrowerPubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file borrowerStake.vkey)
```

5. Create the active datum with the updated outstanding balance.
``` Bash
cardano-loans loan-datum payment-datum \
  --beacon-policy-id $beaconPolicyId \
  --lender-payment-pubkey-hash <lender_id> \
  --borrower-stake-pubkey-hash $borrowerPubKeyHash \
  --loan-asset-is-lovelace \
  --principle 10000000 \
  --loan-term 3600 \
  --interest-numerator 1 \
  --interest-denominator 10 \
  --collateral-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --collateral-asset-token-name 4f74686572546f6b656e0a \
  --rate-numerator 1 \
  --rate-denominator 500000 \
  --expiration <expiration_slot_number> \
  --balance-numerator 11000000 \
  --balance-denominator 1 \
  --payment-amount 5000000 \
  --out-file repayDatum.json
```

Everything should be the same as the current active datum. The new `payment-amount` flag is the amount of the loan asset that will be repaid this transaction. **This field must be exact.**

6. Create helper beacon variables.
``` Bash
activeTokenName="416374697665" # This is the hexidecimal encoding for 'Active'.
activeBeacon="${beaconPolicyId}.${activeTokenName}"
lenderBeacon="${beaconPolicyId}.<lender_id>"
borrowerBeacon="${beaconPolicyId}.${borrowerPubKeyHash}"
```

7. Create the `RepayLoan` spending redeemer.
``` Bash
cardano-loans loan-redeemer \
  --repay \
  --out-file $repayRedeemerFile
```

8. If the loan is being fully paid off, you will also need the `BurnBeaconToken` redeemer.
``` Bash
cardano-loans beacon-redeemer burn-beacons \
  --out-file burnBeacons.json
```

9. Create and submit the transaction.
``` Bash
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file protocol.json

cardano-cli transaction build \
  --tx-in <borrower_utxo_with_loan_asset_to_repay_and_fee> \
  --tx-in <loan_utxo> \
  --tx-in-script-file loan.addr \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file repayLoan.json \
  --tx-out "<loan_address> + 8000000 lovelace + 1 ${activeBeacon} + 1 ${lenderBeacon} + 1 ${borrowerBeacon} + 20 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file repaymentActiveDatum.json \
  --required-signer-hash $borrowerPubKeyHash \
  --change-address <borrower_personal_addr> \
  --tx-in-collateral <borrower_colalteral_utxo> \
  --testnet-magic 1 \
  --protocol-params-file protocol.json \
  --invalid-hereafter <any_slot_between_now_and_expiration> \
  --out-file tx.body

cardano-cli transaction sign \
  --tx-body-file tx.body \
  --signing-key-file borrowerPayment.skey \
  --signing-key-file borrowerStake.skey \
  --testnet-magic 1 \
  --out-file tx.signed

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file tx.signed
```

The amount actually repaid must exactly match that specified in the `payment-amount` field when creating the new active datum.

If the borrower is fully paying off the loan, then he/she must also burn the `borrowerBeacon` and withdraw the collateral in this transaction. **There are no checks to make sure the collateral is taken. If the borrower misses this opportunity to take his/her collateral, custody of the collateral will be transferred to the lender.**

For convenience, the `invalid-hereafter` option can always be set to the slot where the loan expires. This is only used to tell the script that the slot has not passed yet.

### Closing an ask
1. Export the loan validator script.
``` Bash
cardano-loans export-script \
  --loan-script \
  --out-file loan.plutus
```

2. Export the beacon policy script.
``` Bash
cardano-loans export-script \
  --beacon-policy \
  --out-file beacons.plutus
```

3. Calculate the hash for the borrower's staking pubkey.
``` Bash
borrowerPubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file borrowerStake.vkey)
```

4. Create the `BurnBeaconToken` redeemer.
``` Bash
cardano-loans beacon-redeemer burn-beacons \
  --out-file burnBeacons.json
```

5. Create the `CloseAsk` spending redeemer.
``` Bash
cardano-loans loan-redeemer \
  --close-ask \
  --out-file closeAsk.json
```

6. Calculate the beacon policy id.
``` Bash
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file beacons.plutus)
```

7. Create a helper beacon variable.
``` Bash
askTokenName="41736b" # This is the hexidecimal encoding for 'Ask'.
askBeacon="${beaconPolicyId}.${askTokenName}"
```

8. Create and submit the transaction.
``` Bash
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file protocol.json

cardano-cli transaction build \
  --tx-in <ask_utxo> \
  --tx-in-script-file loan.plutus \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file closeAsk.json \
  --mint "-1 ${askBeacon}" \
  --mint-script-file beacons.plutus \
  --mint-redeemer-file burnBeacons.json \
  --required-signer-hash $borrowerPubKeyHash \
  --change-address <borrower_personal_addr> \
  --tx-in-collateral <borrower_collateral_utxo> \
  --testnet-magic 1 \
  --protocol-params-file protocol.json \
  --out-file tx.body

cardano-cli transaction sign \
  --tx-body-file tx.body \
  --signing-key-file borrowerPayment.skey \
  --signing-key-file borrowerStake.skey \
  --testnet-magic 1 \
  --out-file tx.signed

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file tx.signed
```

---
## Lender Actions

### Checking all current asks
``` Bash
cardano-loans query all-asks \
  --preprod-testnet $(cat api.txt) \
  --beacon-policy-id <beacon_policy_id> \
  --stdout
```

Here is an example response when piped to `jq`:

``` JSON
[
  {
    "address": "addr_test1zq749l3erdr67mmukh3mct038q5et2lkpgnqszgsx4n6n5eualkqngnmdz2w9mv60zuucq0sswtn6lq2lwxwez76x0aq0tg2ct",
    "loan_info": {
      "borrower_id": "3cefec09a27b6894e2ed9a78b9cc01f083973d7c0afb8cec8bda33fa",
      "collateral": [
        "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a"
      ],
      "loan_asset": "lovelace",
      "loan_beacon": "f5ba317f03ff0868a6067f3b3a3f98199b037184ad4eaecafdf1d79e",
      "principle": 10000000,
      "term": 3600
    },
    "output_index": "0",
    "tx_hash": "3aa90ec234b31c11f1346c3fdc04f2ab6b5279b893519f0545cbb013b53af7bd",
    "type": "Ask",
    "utxo_assets": [
      {
        "asset": "lovelace",
        "quantity": 2000000
      },
      {
        "asset": "f5ba317f03ff0868a6067f3b3a3f98199b037184ad4eaecafdf1d79e.41736b",
        "quantity": 1
      }
    ]
  }
]
```

Only one ask was found. Borrower `3cefec09a27b6894e2ed9a78b9cc01f083973d7c0afb8cec8bda33fa` is asking to borrower 10 ADA for 3600 slots. They are willing to use the following asset as collateral:
`c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a`

### Checking a borrower's credit history
``` Bash
cardano-loans query borrower-history \
  --preprod-testnet $(cat api.txt) \
  --beacon-policy-id <beacon_policy_id> \
  --borrower-stake-pubkey-hash <borrower_id> \
  --stdout
```

Here is an example response when piped to `jq`:
``` JSON
[
  {
    "default": true,
    "loan_info": {
      "balance_owed": {
        "denominator": 1,
        "numerator": 11000000
      },
      "borrower_id": "3cefec09a27b6894e2ed9a78b9cc01f083973d7c0afb8cec8bda33fa",
      "collateralization": [
        [
          "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a",
          {
            "denominator": 500000,
            "numerator": 1
          }
        ]
      ],
      "expiration_slot": 26655777,
      "interest": {
        "denominator": 10,
        "numerator": 1
      },
      "lender_id": "ae0d001455a855e6c00f98fa9061028f5c00d297926383bc501be2d2",
      "loan_asset": "lovelace",
      "loan_beacon": "f5ba317f03ff0868a6067f3b3a3f98199b037184ad4eaecafdf1d79e",
      "principle": 10000000,
      "term": 600
    }
  },
  {
    "default": false,
    "loan_info": {
      "balance_owed": {
        "denominator": 1,
        "numerator": 6000000
      },
      "borrower_id": "3cefec09a27b6894e2ed9a78b9cc01f083973d7c0afb8cec8bda33fa",
      "collateralization": [
        [
          "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a",
          {
            "denominator": 500000,
            "numerator": 1
          }
        ]
      ],
      "expiration_slot": 26668590,
      "interest": {
        "denominator": 10,
        "numerator": 1
      },
      "lender_id": "ae0d001455a855e6c00f98fa9061028f5c00d297926383bc501be2d2",
      "loan_asset": "lovelace",
      "loan_beacon": "f5ba317f03ff0868a6067f3b3a3f98199b037184ad4eaecafdf1d79e",
      "principle": 10000000,
      "term": 3600
    }
  }
]
```

Two previous loans were found. This borrower successfully paid back the second loan but defaulted on the first. This query also returns the loan terms for each loan as well as the UTxOs at the time of the default / final payment. The terms come from the input in the transaction where the `BorrowerID` beacon was burned.

The lender is also able to see the borrower's current loans using the following command:
``` Bash
cardano-loans query borrower-loans \
  --borrower-stake-pubkey-hash <borrower_stake_pubkey_hash> \
  --loan-address <borrower_loan_addr> \
  --preprod-testnet $(cat api.txt) \
  --stdout
```

While this is technically a command for the borrower, it is designed in such a way that the lender can also use it. The `loan-address` would be the address returned in the `all-asks` query.

### Creating an offer
1. Calculate the lender's pubkey hash.
``` Bash
lenderPaymentPubKeyHash=$(cardano-cli address key-hash \
  --payment-verification-key-file lenderPayment.vkey)
```

2. Export the beacon policy script.
``` Bash
cardano-loans export-script \
  --beacon-policy \
  --out-file beacons.plutus
```

3. Get the beacon policy id.
``` Bash
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file beacons.plutus)
```

4. Create the offer datum.
``` Bash
cardano-loans loan-datum offer-datum \
  --beacon-policy-id $beaconPolicyId \
  --lender-payment-pubkey-hash $lenderPaymentPubKeyHash \
  --loan-asset-is-lovelace \
  --principle 10000000 \
  --loan-term 3600 \
  --interest-numerator 1 \
  --interest-denominator 10 \
  --collateral-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --collateral-asset-token-name 4f74686572546f6b656e0a \
  --rate-numerator 1 \
  --rate-denominator 500000 \
  --out-file offerDatum.json
```

The following part should be repeated for every asset being used as collateral:
``` Bash
  --collateral-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --collateral-asset-token-name 4f74686572546f6b656e0a \
  --rate-numerator 1 \
  --rate-denominator 500000 \
```

Make sure the collateral order matches the order in the borrower's ask datum.

The `rate-numerator` and the `rate-denominator` fields set the relative price between this collateral asset and the loan asset. So the above example is saying the lender thinks 2 units of this collateral asset is equivalent to 1 ADA. 

If you do not want a given asset to be used for collateral, set the relative price to something unreasonable so that the borrower will be incentivized not to use it. A future version of the dApp can allow for the lender to disqualify certain assets.

By setting the relative prices above/below market value, the lender can offer an over-collateralized loan or under-collateralized loan, respectively.

5. Create the `MintOfferToken` redeemer.
``` Bash
cardano-loans beacon-redeemer mint-offer \
  --lender-payment-pubkey-hash $lenderPaymentPubKeyHash \
  --out-file mintOffer.json
```

6. Create helper beacon variables.
``` Bash
offerTokenName="4f66666572" # This is the hexidecimal encoding for 'Offer'.
offerBeacon="${beaconPolicyId}.${offerTokenName}"
lenderBeacon="${beaconPolicyId}.${lenderPaymentPubKeyHash}"
```

7. Create and submit the transaction.
``` Bash
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file protocol.json

cardano-cli transaction build \
  --tx-in <lender_utxo> \
  --tx-out "<borrower_loan_addr> + 13000000 lovelace + 1 ${offerBeacon} + 1 ${lenderBeacon}" \
  --tx-out-inline-datum-file offerDatum.json \
  --mint "1 ${offerBeacon} + 1 ${lenderBeacon}" \
  --mint-script-file beacons.plutus \
  --mint-redeemer-file mintOffer.json \
  --required-signer-hash $lenderPaymentPubKeyHash \
  --change-address <lender_personal_addr> \
  --tx-in-collateral <lender_collateral_utxo> \
  --testnet-magic 1 \
  --protocol-params-file protocol.json \
  --out-file tx.body

cardano-cli transaction sign \
  --tx-body-file tx.body \
  --signing-key-file lenderPayment.skey \
  --testnet-magic 1 \
  --out-file tx.signed

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file tx.signed
```

Remember that the lender must store the offer information with the offered loan amount. If lovelace is the offered asset, then an additional 3 ADA is required (due to the minimum UTxO limits after the borrower accepts the loan).

### Checking all current offers
This returns all open offers that belong to the lender. This allows the lender to keep track of all their offers despite the UTxOs being located in other addresses.

``` Bash
cardano-loans query own-offers \
  --lender-payment-pubkey-hash <lender_payment_pubkey_hash> \
  --beacon-policy-id <beacon_policy_id> \
  --preprod-testnet $(cat api.txt) \
  --stdout
```

Here is an example response when piped to `jq`:

``` JSON
[
  {
    "address": "addr_test1zq749l3erdr67mmukh3mct038q5et2lkpgnqszgsx4n6n5eualkqngnmdz2w9mv60zuucq0sswtn6lq2lwxwez76x0aq0tg2ct",
    "loan_info": {
      "collateralization": [
        [
          "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a",
          {
            "denominator": 500000,
            "numerator": 1
          }
        ]
      ],
      "interest": {
        "denominator": 10,
        "numerator": 1
      },
      "lender_id": "ae0d001455a855e6c00f98fa9061028f5c00d297926383bc501be2d2",
      "loan_asset": "lovelace",
      "loan_beacon": "f5ba317f03ff0868a6067f3b3a3f98199b037184ad4eaecafdf1d79e",
      "principle": 10000000,
      "term": 3600
    },
    "output_index": "0",
    "tx_hash": "7b025d0b57dccf535f9eff36c377b83bbcbb2753314604d17136bea84d2183ab",
    "type": "Offer",
    "utxo_assets": [
      {
        "asset": "lovelace",
        "quantity": 13000000
      },
      {
        "asset": "f5ba317f03ff0868a6067f3b3a3f98199b037184ad4eaecafdf1d79e.4f66666572",
        "quantity": 1
      },
      {
        "asset": "f5ba317f03ff0868a6067f3b3a3f98199b037184ad4eaecafdf1d79e.ae0d001455a855e6c00f98fa9061028f5c00d297926383bc501be2d2",
        "quantity": 1
      }
    ]
  }
]
```

This lender only has one open offer at this time. There is enough information here for the lender to close the offer if desired.

The lender can also check if the ask is still present by taking the `offer_address` from this response and using it in the following command:

``` Bash
cardano-loans query own-asks \
  --preprod-testnet $(cat api.txt) \
  --beacon-policy-id <beacon_policy_id> \
  --loan-address <offer_address> \
  --stdout
```

If the borrower still has the ask open, it will be returned by this command.

### Checking all current loans
This command allows the lender to easily keep track of all their opens loans.

``` Bash
cardano-loans query lender-loans \
  --lender-payment-pubkey-hash <lender_payment_pubkey_hash> \
  --beacon-policy-id <beacon_policy_id> \
  --preprod-testnet $(cat api.txt) \
  --stdout
```

Here is an example response when piped to `jq`:
``` JSON
[
  {
    "address": "addr_test1zq749l3erdr67mmukh3mct038q5et2lkpgnqszgsx4n6n5eualkqngnmdz2w9mv60zuucq0sswtn6lq2lwxwez76x0aq0tg2ct",
    "loan_info": {
      "balance_owed": {
        "denominator": 1,
        "numerator": 11000000
      },
      "borrower_id": "3cefec09a27b6894e2ed9a78b9cc01f083973d7c0afb8cec8bda33fa",
      "collateralization": [
        [
          "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a",
          {
            "denominator": 500000,
            "numerator": 1
          }
        ]
      ],
      "expiration_slot": 26655777,
      "interest": {
        "denominator": 10,
        "numerator": 1
      },
      "lender_id": "ae0d001455a855e6c00f98fa9061028f5c00d297926383bc501be2d2",
      "loan_asset": "lovelace",
      "loan_beacon": "f5ba317f03ff0868a6067f3b3a3f98199b037184ad4eaecafdf1d79e",
      "principle": 10000000,
      "term": 600
    },
    "output_index": "0",
    "tx_hash": "9f7143d32545ac4c2c1ce0833b3f75a0a2969279cfc855f44ee5442417c99b18",
    "type": "Loan",
    "utxo_assets": [
      {
        "asset": "lovelace",
        "quantity": 3000000
      },
      {
        "asset": "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a",
        "quantity": 20
      },
      {
        "asset": "f5ba317f03ff0868a6067f3b3a3f98199b037184ad4eaecafdf1d79e.3cefec09a27b6894e2ed9a78b9cc01f083973d7c0afb8cec8bda33fa",
        "quantity": 1
      },
      {
        "asset": "f5ba317f03ff0868a6067f3b3a3f98199b037184ad4eaecafdf1d79e.416374697665",
        "quantity": 1
      },
      {
        "asset": "f5ba317f03ff0868a6067f3b3a3f98199b037184ad4eaecafdf1d79e.ae0d001455a855e6c00f98fa9061028f5c00d297926383bc501be2d2",
        "quantity": 1
      }
    ]
  }
]
```

Only one loan was found for this lender. In order for the loan to be claimable by the lender either the `expiration_slot` must have passed or the loan must have been fully repaid, signified by the absence of the borrower's BorrowerID token. In this case, the BorrowerID token (`f5ba317f03ff0868a6067f3b3a3f98199b037184ad4eaecafdf1d79e.3cefec09a27b6894e2ed9a78b9cc01f083973d7c0afb8cec8bda33fa`) is still present so it can only be reclaimed if the expiration has passed.

### Claiming an expired/paid loan
1. Export the loan validator script.
``` Bash
cardano-loans export-script \
  --loan-script \
  --out-file loan.plutus
```

2. Calculate the hash of the lender's payment verification key.
``` Bash
lenderPaymentPubKeyHash=$(cardano-cli address key-hash \
  --payment-verification-key-file lenderPayment.vkey)
```

3. Export the beacon policy script.
``` Bash
cardano-loans export-script \
  --beacon-policy \
  --out-file beacons.plutus
```

4. Get the beacon policy id.
``` Bash
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file beacons.plutus)
```

5. Create the `BurnBeaconToken` redeemer.
``` Bash
cardano-loans beacon-redeemer burn-beacons \
  --out-file burnBeacons.json
```

6. Create the `Claim` spending redeemer.
``` Bash
cacardano-loans loan-redeemer \
  --claim \
  --out-file claimLoan.json
```

7. Create helper beacon variables.
``` Bash
activeTokenName="416374697665" # This is the hexidecimal encoding for 'Active'.
activeBeacon="${beaconPolicyId}.${activeTokenName}"
borrowerBeacon="${beaconPolicyId}.<borrower_id>"
lenderBeacon="${beaconPolicyId}.${lenderPaymentPubKeyHash}"
```

8. Create and submit the transaction.
``` Bash
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file protocol.json

cardano-cli transaction build \
  --tx-in <lender_utxo_for_fee> \
  --tx-in <loan_utxo> \
  --tx-in-script-file loan.plutus \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file claimLoan.json \
  --required-signer-hash $lenderPaymentPubKeyHash \
  --mint "-1 ${activeBeacon} + -1 ${borrowerBeacon} + -1 ${lenderBeacon}" \
  --mint-script-file beacons.plutus \
  --mint-redeemer-file burnBeacons.json \
  --change-address <lender_personal_addr> \
  --tx-in-collateral <lender_collateral_utxo> \
  --testnet-magic 1 \
  --protocol-params-file protocol.json \
  --invalid-before <expiration_slot_plus_one> \
  --out-file tx.body

cardano-cli transaction sign \
  --tx-body-file tx.body \
  --signing-key-file lenderPayment.skey \
  --testnet-magic 1 \
  --out-file tx.signed
```

If the loan was fully paid off, the `borrowerBeacon` was already burned and doesn't need to be burned here. 

When claiming an expired loan, make sure to add one to the expiration slot (the script uses `>=` to determine if the loan is not expired). If the loan is fully paid, the `invalid-before` flag can be set to the current slot number.

### Closing an offer
1. Export the loan validator script.
``` Bash
cardano-loans export-script \
  --loan-script \
  --out-file loan.plutus
```

2. Export the beacon policy script.
``` Bash
cardano-loans export-script \
  --beacon-policy \
  --out-file beacons.plutus
```

3. Get the beacon policy id.
``` Bash
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file beacons.plutus)
```

4. Create the `BurnBeaconToken` redeemer.
``` Bash
cardano-loans beacon-redeemer burn-beacons \
  --out-file burnBeacons.json
```

5. Calculate the hash of the lender's payment pubkey.
``` Bash
lenderPaymentPubKeyHash=$(cardano-cli address key-hash \
  --payment-verification-key-file lenderPayment.vkey)
```

6. Create the `CloseOffer` redeemer.
``` Bash
cardano-loans loan-redeemer \
  --close-offer \
  --out-file closeOffer.json
```

7. Create helper beacon variables.
``` Bash
offerTokenName="4f66666572" # This is the hexidecimal encoding for 'Offer'.
offerBeacon="${beaconPolicyId}.${offerTokenName}"
lenderBeacon="${beaconPolicyId}.${lenderPaymentPubKeyHash}"
```

8. Create and submit the transaction.
``` Bash
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file protocol.json

cardano-cli transaction build \
  --tx-in <offer_utxo> \
  --tx-in-script-file loan.plutus \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file closeOffer.json \
  --mint "-1 ${offerBeacon} + -1 ${lenderBeacon}" \
  --mint-script-file beacons.plutus \
  --mint-redeemer-file burnBeacons.json \
  --required-signer-hash $lenderPaymentPubKeyHash \
  --change-address <lender_personal_addr> \
  --tx-in-collateral <lender_collateral_utxo> \
  --testnet-magic 1 \
  --protocol-params-file protocol.json \
  --out-file tx.body

cardano-cli transaction sign \
  --tx-body-file tx.body \
  --signing-key-file lenderPayment.skey \
  --testnet-magic 1 \
  --out-file tx.signed

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file tx.signed
```

---
## Convert POSIX time <--> Slot
``` Bash
cardano-loans convert --slot 26668590

cardano-loans convert --posix-time 1682351790000
```
