# Getting Started

:warning: Assumes a local PreProduction Testnet node running locally and `cardano-cli` installed since it is used to actually build and sign transactions.

Template bash scripts that follow these steps are available [here](scripts/).

When integration testing, it is highly recommended that you change the string passed to the mkBeaconPolicy function [here](src/CardanoLoans.hs#L199). When developers make mistakes (myself included), it can create bad/locked utxos that will appear when you query the beacons. This can complicate your own testing. To avoid this, this extra parameter was added. Change the string to something unique to you. You should remember to change it to the desired string for mainnet.

If you change any on-chain code (including the string passed to the mkBeaconPolicy), then you should also change the string passed [here](app/CLI/Types.hs#L15) if you still intend to use the accompanying `cardano-loans` CLI. This variable is meant to be set to the beacon policy id. Using this variable whenever possible circumvents the need to create the beacon policy every time the CLI program runs. There is a `beaconPolicySymbol` variable that can be executed from within `cabal repl` that will display the beacon policy symbol of the current on-chain code.

**Make these changes before building the executable in the next section.**

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

---
## Installing
Instructions are adapted from the [plutus-pioneers-program](https://github.com/input-output-hk/plutus-pioneer-program) week 1 exercise.

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
cardano-cli stake-address key-hash \
  --stake-verification-key-file borrowerStake.vkey \
  --out-file borrowerStake.pkh
```

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
  --out-file beacon.plutus
```

5. Create the AskDatum.
``` Bash
cardano-loans borrower ask-datum \
  --borrower-stake-pubkey-hash "$(cat borrowerStake.pkh)" \
  --loan-asset-is-lovelace \
  --principle 10000000 \
  --loan-term 3600 \
  --collateral-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --collateral-asset-token-name 4f74686572546f6b656e0a \
  --out-file askDatum.json
```

The `principle` is in units of the loan asset. **When the loan asset is lovelace, make sure the `principle` is also in lovelace.**

The `loan-term` option is the number of slots the loan will be valid for once started. The CLI program will convert this to POSIX time (1 slot = 1000 POSIXTime).

6. Create the `MintAskToken` redeemer.
``` Bash
cardano-loans borrower ask-beacon \
  --borrower-stake-pubkey-hash "$(cat borrowerStake.pkh)" \
  --out-file mintAsk.json
```

7. Get the beacon policy id.
``` Bash
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file beacon.plutus) 
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
  --mint-script-file beacon.plutus \
  --mint-redeemer-file mintAsk.json \
  --required-signer-hash "$(cat borrowerStake.pkh)" \
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

The borrower must sign with both his/her payment and stake keys.

### Checking all own asks
This option is to make it easy for a borrower to see his/her own asks inside their loan address.

``` Bash
cardano-loans borrower query-asks \
  --borrower-address $(cat loan.addr) \
  --preprod-testnet $(cat api.txt) \
  --stdout
```

Here is an example response when piped to `jq`:

``` JSON
[
  {
    "ask_address": "addr_test1zphtaf8kvhsukaq9fmwgr0xqcm9769r5p2uasqtl497thtfualkqngnmdz2w9mv60zuucq0sswtn6lq2lwxwez76x0aq9smjcg",
    "ask_tx_ix": "9c3c08e405586b27225765c6f834dd38ede556e876a09685e1ca432400d24d31#0",
    "assets_available_for_collateral": [
      "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a"
    ],
    "borrower_id": "3cefec09a27b6894e2ed9a78b9cc01f083973d7c0afb8cec8bda33fa",
    "length_of_loan_in_slots": 3600,
    "loan_asset": "lovelace",
    "loan_principle": 10000000,
    "utxo_assets": [
      {
        "asset": "lovelace",
        "quantity": 2000000
      },
      {
        "asset": "53267afd1e0b2db650c3c08f034ef95e686b8ff74f039caf89274c64.41736b",
        "quantity": 1
      }
    ]
  }
]
```
This borrower only has one open ask. This returns all of the information necessary for the borrower to act on.

### Checking all offers
``` Bash
cardano-loans borrower query-offers \
  --borrower-address $(cat loan.addr) \
  --preprod-testnet $(cat api.txt) \
  --stdout
```

Here is an example response when piped to `jq`:
``` JSON
[
  {
    "collateral_rates": [
      {
        "collateral_asset_name": "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a",
        "collateral_rate_denominator": 500000,
        "collateral_rate_numerator": 1
      }
    ],
    "interest_denominator": 10,
    "interest_numerator": 1,
    "lender_id": "ae0d001455a855e6c00f98fa9061028f5c00d297926383bc501be2d2",
    "length_of_loan_in_slots": 3600,
    "loan_asset": "lovelace",
    "loan_principle": 10000000,
    "offerAddress": "addr_test1zpjwer4uw4wj64a7dp6zkn97sc02mqsxrpmgdngaqdsek3fualkqngnmdz2w9mv60zuucq0sswtn6lq2lwxwez76x0aqumhh9t",
    "offer_tx_ix": "76a5888a1efe6199b73487824b1ecba06e8a5f0382ec32eb0f2a1052a145fc37#0",
    "required_backing": 10000000,
    "utxo_assets": [
      {
        "asset": "lovelace",
        "quantity": 13000000
      },
      {
        "asset": "53267afd1e0b2db650c3c08f034ef95e686b8ff74f039caf89274c64.4f66666572",
        "quantity": 1
      },
      {
        "asset": "53267afd1e0b2db650c3c08f034ef95e686b8ff74f039caf89274c64.ae0d001455a855e6c00f98fa9061028f5c00d297926383bc501be2d2",
        "quantity": 1
      }
    ]
  }
]
```

Only one offer was made. This returns all of the information necessary for the borrower to decide and accept the offer.

### Accepting a loan offer
1. Export the loan validator script.
``` Bash
cardano-loans export-script \
  --loan-script \
  --out-file loan.plutus
```

2. Calculate the hash for the borrower's staking verification key.
``` Bash
cardano-cli stake-address key-hash \
  --stake-verification-key-file borrowerStake.vkey \
  --out-file borrowerStake.pkh
```

3. Export the beacon policy script.
``` Bash
cardano-loans export-script \
  --beacon-policy \
  --out-file beacon.plutus
```

4. Create the `AcceptOffer` spending redeemer.
``` Bash
cardano-loans borrower accept-offer \
  --out-file acceptOffer.json
```

5. Create the active datum.
``` Bash
cardano-loans borrower accept-offer-datum \
  --lender-payment-pubkey-hash <lender_id> \
  --borrower-stake-pubkey-hash "$(cat borrowerStake.pkh)" \
  --loan-asset-is-lovelace \
  --principle 10000000 \
  --loan-term 3600 \
  --loan-interest-numerator 1 \
  --loan-interest-denominator 10 \
  --required-backing 10000000 \
  --collateral-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --collateral-asset-token-name 4f74686572546f6b656e0a \
  --collateral-rate-numerator 1 \
  --collateral-rate-denominator 500000 \
  --loan-start <slot_of_invalid_before> \
  --out-file activeDatum.json
```

6. Create the `MintActiveToken` redeemer.
``` Bash
cardano-loans borrower active-beacon \
  --borrower-stake-pubkey-hash "$(cat borrowerStake.pkh)" \
  --lender-payment-pubkey-hash <lender_id> \
  --out-file mintActive.json
```

7. Calculate the beacon policy id.
``` Bash
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file beacon.plutus)
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

borrowerPubKeyHash=$(cat borrowerStake.pkh)
borrowerBeacon="${beaconPolicyId}.${borrowerPubKeyHash}"
```

9. Create and submit the transaction.
``` Bash
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file protocol.json

cardano-cli transaction build \
  --tx-in <borrower_utxo_with_collateral> \
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
  --mint-script-file beacon.plutus \
  --mint-redeemer-file mintActive.json \
  --required-signer-hash "$(cat borrowerStake.pkh)" \
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

### Checking all current loans
This option is for the convenience of checking the loan information in the borrower's address.

``` Bash
cardano-loans borrower query-loans \
  --borrower-stake-pubkey-hash $(cat borrowerStake.pkh) \
  --preprod-testnet $(cat api.txt) \
  --stdout
```

Here is an example response when piped to `jq`:
``` JSON
[
  {
    "activeAddress": "addr_test1zphtaf8kvhsukaq9fmwgr0xqcm9769r5p2uasqtl497thtfualkqngnmdz2w9mv60zuucq0sswtn6lq2lwxwez76x0aq9smjcg",
    "active_tx_ix": "3b24d952689c62b51b704142375d2a1cc4b23bbea0ad209f39b590950253d785#0",
    "borrower_id": "3cefec09a27b6894e2ed9a78b9cc01f083973d7c0afb8cec8bda33fa",
    "collateral_rates": [
      {
        "collateral_asset_name": "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a",
        "collateral_rate_denominator": 500000,
        "collateral_rate_numerator": 1
      }
    ],
    "interest_denominator": 10,
    "interest_numerator": 1,
    "lender_id": "ae0d001455a855e6c00f98fa9061028f5c00d297926383bc501be2d2",
    "length_of_loan_in_slots": 3600,
    "loan_asset": "lovelace",
    "loan_expiration": 1679319281000,
    "loan_outstanding_denominator": 1,
    "loan_outstanding_numerator": 11000000,
    "loan_principle": 10000000,
    "required_backing": 10000000,
    "utxo_assets": [
      {
        "asset": "lovelace",
        "quantity": 3000000
      },
      {
        "asset": "53267afd1e0b2db650c3c08f034ef95e686b8ff74f039caf89274c64.3cefec09a27b6894e2ed9a78b9cc01f083973d7c0afb8cec8bda33fa",
        "quantity": 1
      },
      {
        "asset": "53267afd1e0b2db650c3c08f034ef95e686b8ff74f039caf89274c64.416374697665",
        "quantity": 1
      },
      {
        "asset": "53267afd1e0b2db650c3c08f034ef95e686b8ff74f039caf89274c64.ae0d001455a855e6c00f98fa9061028f5c00d297926383bc501be2d2",
        "quantity": 1
      },
      {
        "asset": "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a",
        "quantity": 20
      }
    ]
  }
]
```

Only one loan was found.

The `loan_outstanding_numerator` and `loan_outstanding_denominator` fields say how much of the loan is left to pay off. This loan still has 11 ADA due.

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
  --out-file beacon.plutus
```

3. Calculate the beacon policy id.
``` Bash
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file beacon.plutus)
```

4. Calculate the hash of the borrower's staking verification key.
``` Bash
cardano-cli stake-address key-hash \
  --stake-verification-key-file borrowerStake.vkey \
  --out-file borrowerStake.pkh
```

5. Create the active datum with the updated outstanding balance.
``` Bash
cardano-loans borrower loan-payment-datum \
  --lender-payment-pubkey-hash <lender_id> \
  --borrower-stake-pubkey-hash "$(cat borrowerStake.pkh)" \
  --loan-asset-is-lovelace \
  --principle 10000000 \
  --loan-term 3600 \
  --loan-interest-numerator 1 \
  --loan-interest-denominator 10 \
  --required-backing 10000000 \
  --collateral-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --collateral-asset-token-name 4f74686572546f6b656e0a \
  --collateral-rate-numerator 1 \
  --collateral-rate-denominator 500000 \
  --expiration-time 1679061927000 \
  --current-balance-numerator 11000000 \
  --current-balance-denominator 1 \
  --payment-amount 5000000 \
  --out-file repaymentActiveDatum.json
```

6. Create helper beacon variables.
``` Bash
activeTokenName="416374697665" # This is the hexidecimal encoding for 'Active'.
activeBeacon="${beaconPolicyId}.${activeTokenName}"
lenderBeacon="${beaconPolicyId}.<lender_id>"

borrowerPubKeyHash=$(cat borrowerStake.pkh)
borrowerBeacon="${beaconPolicyId}.${borrowerPubKeyHash}"
```

7. Create the `RepayLoan` spending redeemer.
``` Bash
cardano-loans borrower repay \
  --out-file repayLoan.json
```

8. If the loan is being fully paid off, you will also need the `BurnBeaconToken` redeemer.
``` Bash
cardano-loans lender burn-beacons \
  --out-file burnBeacons.json
```

9. Create and submit the transaction.
``` Bash
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file "${tmpDir}protocol.json"

cardano-cli transaction build \
  --tx-in <borrower_utxo_with_loan_asset_to_repay> \
  --tx-in <loan_utxo> \
  --tx-in-script-file loan.addr \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file repayLoan.json \
  --tx-out "<loan_address> + 8000000 lovelace + 1 ${activeBeacon} + 1 ${lenderBeacon} + 1 ${borrowerBeacon} + 20 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file repaymentActiveDatum.json \
  --required-signer-hash "$(cat borrowerStake.pkh)" \
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

If the borrower is fully paying off the loan, then he/she must also burn the `borrowerBeacon` and withdraw the collateral in this transaction. **There are no checks to make sure the collateral is taken. If the borrower misses this opportunity to take his/her collateral, custody of the collateral will be transferred to the lender.**

For convenience, the `invalid-hereafter` option can always be set to the slot where the loan expires. This is only used to tell the script that the slot has not passed yet. You can use the following command to convert POSIX time to the corresponding slot number:

``` Bash
cardano-loans borrower convert-posix \
  --posix-time 1679061927000
```

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
  --out-file beacon.plutus
```

3. Calculate the hash for the borrower's staking pubkey.
``` Bash
cardano-cli stake-address key-hash \
  --stake-verification-key-file borrowerStake.vkey \
  --out-file borrowerStake.pkh
```

4. Create the `BurnBeaconToken` redeemer.
``` Bash
cardano-loans borrower burn-beacons \
  --out-file burnBeacons.json
```

5. Create the `CloseAsk` spending redeemer.
``` Bash
cardano-loans borrower close-ask \
  --out-file closeAsk.json
```

6. Calculate the beacon policy id.
``` Bash
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file beacon.plutus)
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
  --mint-script-file beacon.plutus \
  --mint-redeemer-file burnBeacons.json \
  --required-signer-hash "$(cat borrowerStake.pkh)" \
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
cardano-loans lender query-asks \
  --preprod-testnet $(cat api.txt) \
  --stdout
```

Here is an example response when piped to `jq`:

``` JSON
[
  {
    "ask_address": "addr_test1zphtaf8kvhsukaq9fmwgr0xqcm9769r5p2uasqtl497thtfualkqngnmdz2w9mv60zuucq0sswtn6lq2lwxwez76x0aq9smjcg",
    "ask_tx_ix": "9c3c08e405586b27225765c6f834dd38ede556e876a09685e1ca432400d24d31#0",
    "assets_available_for_collateral": [
      "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a"
    ],
    "borrower_id": "3cefec09a27b6894e2ed9a78b9cc01f083973d7c0afb8cec8bda33fa",
    "length_of_loan_in_slots": 3600,
    "loan_asset": "lovelace",
    "loan_principle": 10000000,
    "utxo_assets": [
      {
        "asset": "lovelace",
        "quantity": 2000000
      },
      {
        "asset": "53267afd1e0b2db650c3c08f034ef95e686b8ff74f039caf89274c64.41736b",
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
cardano-loans lender query-credit-history \
  --borrower-stake-pubkey-hash <borrower_id> \
  --preprod-testnet $(cat api.txt) \
  --stdout
```

Here is an example response when piped to `jq`:
``` JSON
[
  {
    "default": false,
    "loan_info": {
      "activeAddress": "addr_test1zphtaf8kvhsukaq9fmwgr0xqcm9769r5p2uasqtl497thtfualkqngnmdz2w9mv60zuucq0sswtn6lq2lwxwez76x0aq9smjcg",
      "active_tx_ix": "e5b4c3b3d8b408e923644e73ef010ae4180cd60acf3441e72ad581901e9e5579#0",
      "borrower_id": "3cefec09a27b6894e2ed9a78b9cc01f083973d7c0afb8cec8bda33fa",
      "collateral_rates": [
        {
          "collateral_asset_name": "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a",
          "collateral_rate_denominator": 500000,
          "collateral_rate_numerator": 1
        }
      ],
      "interest_denominator": 10,
      "interest_numerator": 1,
      "lender_id": "ae0d001455a855e6c00f98fa9061028f5c00d297926383bc501be2d2",
      "length_of_loan_in_slots": 3600,
      "loan_asset": "lovelace",
      "loan_expiration": 1679319281000,
      "loan_outstanding_denominator": 1,
      "loan_outstanding_numerator": 6000000,
      "loan_principle": 10000000,
      "required_backing": 10000000,
      "utxo_assets": [
        {
          "asset": "lovelace",
          "quantity": 8000000
        },
        {
          "asset": "53267afd1e0b2db650c3c08f034ef95e686b8ff74f039caf89274c64.3cefec09a27b6894e2ed9a78b9cc01f083973d7c0afb8cec8bda33fa",
          "quantity": 1
        },
        {
          "asset": "53267afd1e0b2db650c3c08f034ef95e686b8ff74f039caf89274c64.416374697665",
          "quantity": 1
        },
        {
          "asset": "53267afd1e0b2db650c3c08f034ef95e686b8ff74f039caf89274c64.ae0d001455a855e6c00f98fa9061028f5c00d297926383bc501be2d2",
          "quantity": 1
        },
        {
          "asset": "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a",
          "quantity": 20
        }
      ]
    }
  },
  {
    "default": true,
    "loan_info": {
      "activeAddress": "addr_test1zphtaf8kvhsukaq9fmwgr0xqcm9769r5p2uasqtl497thtfualkqngnmdz2w9mv60zuucq0sswtn6lq2lwxwez76x0aq9smjcg",
      "active_tx_ix": "48c65de6874878bf6025c83c74788373ac8e61c442eed74ab31b7238cd649fd9#0",
      "borrower_id": "3cefec09a27b6894e2ed9a78b9cc01f083973d7c0afb8cec8bda33fa",
      "collateral_rates": [
        {
          "collateral_asset_name": "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a",
          "collateral_rate_denominator": 500000,
          "collateral_rate_numerator": 1
        }
      ],
      "interest_denominator": 10,
      "interest_numerator": 1,
      "lender_id": "ae0d001455a855e6c00f98fa9061028f5c00d297926383bc501be2d2",
      "length_of_loan_in_slots": 3600,
      "loan_asset": "lovelace",
      "loan_expiration": 1679321504000,
      "loan_outstanding_denominator": 1,
      "loan_outstanding_numerator": 22000000,
      "loan_principle": 20000000,
      "required_backing": 10000000,
      "utxo_assets": [
        {
          "asset": "lovelace",
          "quantity": 3000000
        },
        {
          "asset": "53267afd1e0b2db650c3c08f034ef95e686b8ff74f039caf89274c64.3cefec09a27b6894e2ed9a78b9cc01f083973d7c0afb8cec8bda33fa",
          "quantity": 1
        },
        {
          "asset": "53267afd1e0b2db650c3c08f034ef95e686b8ff74f039caf89274c64.416374697665",
          "quantity": 1
        },
        {
          "asset": "53267afd1e0b2db650c3c08f034ef95e686b8ff74f039caf89274c64.ae0d001455a855e6c00f98fa9061028f5c00d297926383bc501be2d2",
          "quantity": 1
        },
        {
          "asset": "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a",
          "quantity": 40
        }
      ]
    }
  }
]
```

Two previous loans were found. This borrower successfully paid back the first loan but defaulted on the second. This query also returns the loan terms for each loan. The terms come from the input in the transaction where the `BorrowerID` beacon was burned.

The lender is also able to see the borrower's current loans using the following command:
``` Bash
cardano-loans borrower query-loans \
  --borrower-stake-pubkey-hash <borrower_id> \
  --preprod-testnet $(cat api.txt) \
  --stdout
```

While this is technically a command for the borrower, it is designed in such a way that the lender can also use it.

### Creating an offer
1. Calculate the lender's pubkey hash.
``` Bash
cardano-cli address key-hash \
  --payment-verification-key-file lenderPayment.vkey \
  --out-file lenderPayment.pkh
```

2. Export the beacon policy script.
``` Bash
cardano-loans export-script \
  --beacon-policy \
  --out-file beacon.plutus
```

3. Create the offer datum.
``` Bash
cardano-loans lender offer-datum \
  --lender-payment-pubkey-hash "$(cat lenderPayment.pkh)" \
  --loan-asset-is-lovelace \
  --principle 10000000 \
  --loan-term 3600 \
  --loan-interest-numerator 1 \
  --loan-interest-denominator 10 \
  --required-backing 10000000 \
  --collateral-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --collateral-asset-token-name 4f74686572546f6b656e0a \
  --collateral-rate-numerator 2 \
  --collateral-rate-denominator 1000000 \
  --out-file offerDatum.json
```

The following part should be repeated for every asset being used as collateral:
``` Bash
  --collateral-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --collateral-asset-token-name 4f74686572546f6b656e0a \
  --collateral-rate-numerator 2 \
  --collateral-rate-denominator 1000000 \
```

Make sure the order matches the order in the borrower's ask datum.

The `collateral-rate-numerator` and the `collateral-rate-denominator` fields set the relative price between this collateral asset and the loan asset. So the above example is saying the lender thinks 2 units of this collateral asset is equivalent to 1 ADA.

The `loan-backing` field specifies how many units of the loan asset must be backed by collateral of equal value. By setting this field to less than or greater than the `principle` field, you can offer an under-collateralized or over-collateralized loan, respectively.

4. Create the `MintOfferToken` redeemer.
``` Bash
cardano-loans lender offer-beacon \
  --lender-payment-pubkey-hash "$(cat lenderPayment.pkh)" \
  --out-file mintOffer.json
```

5. Calculate the beacon policy id.
``` Bash
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file beacon.plutus)
```

6. Create helper beacon variables.
``` Bash
offerTokenName="4f66666572" # This is the hexidecimal encoding for 'Offer'.
offerBeacon="${beaconPolicyId}.${offerTokenName}"

lenderPaymentPubKeyHash=$(cat lenderPayment.pkh)
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
  --mint-script-file beacon.plutus \
  --mint-redeemer-file mintOffer.json \
  --required-signer-hash "$(cat lenderPayment.pkh)" \
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

Remember that the lender must store the offer information with the asked loan amount.

### Checking all current offers
This returns all open offers that belong to the lender.

``` Bash
cardano-loans lender query-open-offers \
  --lender-payment-pubkey-hash $(cat lenderPayment.pkh) \
  --preprod-testnet $(cat api.txt) \
  --stdout
```

Here is an example response when piped to `jq`:

``` JSON
[
  {
    "collateral_rates": [
      {
        "collateral_asset_name": "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a",
        "collateral_rate_denominator": 500000,
        "collateral_rate_numerator": 1
      }
    ],
    "interest_denominator": 10,
    "interest_numerator": 1,
    "lender_id": "ae0d001455a855e6c00f98fa9061028f5c00d297926383bc501be2d2",
    "length_of_loan_in_slots": 3600,
    "loan_asset": "lovelace",
    "loan_principle": 10000000,
    "offerAddress": "addr_test1zpjwer4uw4wj64a7dp6zkn97sc02mqsxrpmgdngaqdsek3fualkqngnmdz2w9mv60zuucq0sswtn6lq2lwxwez76x0aqumhh9t",
    "offer_tx_ix": "76a5888a1efe6199b73487824b1ecba06e8a5f0382ec32eb0f2a1052a145fc37#0",
    "required_backing": 10000000,
    "utxo_assets": [
      {
        "asset": "lovelace",
        "quantity": 13000000
      },
      {
        "asset": "53267afd1e0b2db650c3c08f034ef95e686b8ff74f039caf89274c64.4f66666572",
        "quantity": 1
      },
      {
        "asset": "53267afd1e0b2db650c3c08f034ef95e686b8ff74f039caf89274c64.ae0d001455a855e6c00f98fa9061028f5c00d297926383bc501be2d2",
        "quantity": 1
      }
    ]
  }
]
```

This lender only has one open offer at this time. There is enough information here for the lender to close the offer if desired.

### Checking all current loans
``` Bash
cardano-loans lender query-loans \
  --lender-payment-pubkey-hash $(cat lenderPayment.pkh) \
  --preprod-testnet $(cat api.txt) \
  --stdout
```

Here is an example response when piped to `jq`:
``` JSON
[
  {
    "activeAddress": "addr_test1zphtaf8kvhsukaq9fmwgr0xqcm9769r5p2uasqtl497thtfualkqngnmdz2w9mv60zuucq0sswtn6lq2lwxwez76x0aq9smjcg",
    "active_tx_ix": "3b24d952689c62b51b704142375d2a1cc4b23bbea0ad209f39b590950253d785#0",
    "borrower_id": "3cefec09a27b6894e2ed9a78b9cc01f083973d7c0afb8cec8bda33fa",
    "collateral_rates": [
      {
        "collateral_asset_name": "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a",
        "collateral_rate_denominator": 500000,
        "collateral_rate_numerator": 1
      }
    ],
    "interest_denominator": 10,
    "interest_numerator": 1,
    "lender_id": "ae0d001455a855e6c00f98fa9061028f5c00d297926383bc501be2d2",
    "length_of_loan_in_slots": 3600,
    "loan_asset": "lovelace",
    "loan_expiration": 1679319281000,
    "loan_outstanding_denominator": 1,
    "loan_outstanding_numerator": 11000000,
    "loan_principle": 10000000,
    "required_backing": 10000000,
    "utxo_assets": [
      {
        "asset": "lovelace",
        "quantity": 3000000
      },
      {
        "asset": "53267afd1e0b2db650c3c08f034ef95e686b8ff74f039caf89274c64.3cefec09a27b6894e2ed9a78b9cc01f083973d7c0afb8cec8bda33fa",
        "quantity": 1
      },
      {
        "asset": "53267afd1e0b2db650c3c08f034ef95e686b8ff74f039caf89274c64.416374697665",
        "quantity": 1
      },
      {
        "asset": "53267afd1e0b2db650c3c08f034ef95e686b8ff74f039caf89274c64.ae0d001455a855e6c00f98fa9061028f5c00d297926383bc501be2d2",
        "quantity": 1
      },
      {
        "asset": "c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a",
        "quantity": 20
      }
    ]
  }
]
```

### Claiming an expired/paid loan
1. Export the loan validator script.
``` Bash
cardano-loans export-script \
  --loan-script \
  --out-file loan.plutus
```

2. Calculate the hash of the lender's payment verification key.
``` Bash
cardano-cli address key-hash \
  --payment-verification-key-file lenderPayment.vkey \
  --out-file lenderPayment.pkh
```

3. Export the beacon policy script.
``` Bash
cardano-loans export-script \
  --beacon-policy \
  --out-file beacon.plutus
```

4. Create the `BurnBeaconToken` redeemer.
``` Bash
cardano-loans lender burn-beacons \
  --out-file burnBeacons.json
```

5. Create the `Claim` spending redeemer.
``` Bash
cardano-loans lender claim-loan \
  --out-file claimLoan.json
```

6. Calculate the beacon policy id.
``` Bash
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file beacon.plutus)
```

7. Create helper beacon variables.
``` Bash
activeTokenName="416374697665" # This is the hexidecimal encoding for 'Active'.
activeBeacon="${beaconPolicyId}.${activeTokenName}"
borrowerBeacon="${beaconPolicyId}.<borrower_id>"

lenderPaymentPubKeyHash=$(cat lenderPayment.pkh)
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
  --required-signer-hash "$(cat lenderPayment.pkh)" \
  --mint "-1 ${activeBeacon} + -1 ${borrowerBeacon} + -1 ${lenderBeacon}" \
  --mint-script-file beacon.plutus \
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

If the loan was fully paid off the `borrowerBeacon` was already burned and doesn't need to be burned here.

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
  --out-file beacon.plutus
```

3. Create the `BurnBeaconToken` redeemer.
``` Bash
cardano-loans lender burn-beacons \
  --out-file burnBeacons.json
```

4. Calculate the hash of the lender's payment pubkey.
``` Bash
cardano-cli address key-hash \
  --payment-verification-key-file lenderPayment.vkey \
  --out-file lenderPayment.pkh
```

5. Create the `CloseOffer` redeemer.
``` Bash
cardano-loans lender close-offer \
  --out-file closeOffer.json
```

6. Calculate the beacon policy id.
``` Bash
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file beacon.plutus)
```

7. Create helper beacon variables.
``` Bash
offerTokenName="4f66666572" # This is the hexidecimal encoding for 'Offer'.
offerBeacon="${beaconPolicyId}.${offerTokenName}"

lenderPaymentPubKeyHash=$(cat $lenderPaymentPubKeyHashFile)
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
  --mint-script-file beacon.plutus \
  --mint-redeemer-file burnBeacons.json \
  --required-signer-hash "$(cat lenderPayment.pkh)" \
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