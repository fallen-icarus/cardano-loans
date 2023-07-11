# Getting Started

:warning: Assumes a local PreProduction Testnet node running locally and `cardano-cli` installed since it is used to actually build and sign transactions.

Template bash scripts that follow these steps are available [here](scripts/).

For now, the cardano-loans CLI only supports the preproduction testnet and the Blockfrost Api.

---
## Table of Contents
- [Installing](#installing)
- [Minting Test Tokens](#minting-test-tokens)
- [Asking for a loan](#asking-for-a-loan)
- [Closing an Ask](#closing-an-ask)
- [Offering a Loan](#offering-a-loan)
- [Closing an Offer](#closing-an-offer)
- [Accepting an Offer](#accepting-an-offer)
- [Making a loan payment](#making-a-loan-payment)
- [Rollover a loan once a checkpoint is reached](#rolling-over-a-loan)
- [Claim an expired loan](#claim-an-expired-loan)
- [Updating a lender address](#update-lender-address)
- [Clean up finished loan UTxOs or claim lost collateral](#clean-up-finished-loan-utxos-or-claim-lost-collateral)
- [Convert POSIX time <--> Slot](#convert-posix-time----slot)
- [Address Conversions](#address-conversions)
- [Query Beacons](#query-beacons)

---
## Installing
### Using Cabal - RECOMMENDED

#### Install the necessary packages - similar to cardano-node
```
sudo apt update
sudo apt upgrade
sudo apt-get install autoconf automake build-essential curl g++ git jq libffi-dev libgmp-dev libncursesw5 libssl-dev libsystemd-dev libtinfo-dev libtool make pkg-config wget zlib1g-dev liblzma-dev libpq-dev
```

#### Install libsodium and scep256k1
```
git clone https://github.com/input-output-hk/libsodium
cd libsodium
git checkout dbb48cc
./autogen.sh
./configure
make
sudo make install

cd ../
git clone https://github.com/bitcoin-core/secp256k1
cd secp256k1
git checkout ac83be33
./autogen.sh
./configure --enable-module-schnorrsig --enable-experimental
make
make check
sudo make install
sudo ldconfig
```

Add the following lines to your `$HOME/.bashrc` file:
```
export LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH"
export PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH"
```

#### Install GHC 8.10.7 and cabal
```
cd
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```
Make sure to install the required packages it mentions before hitting ENTER.

Prepend or append the required PATH variable.

You do not need to install the haskell-langauge-server.

You do not need to install stack.

Press ENTER to proceed.
```
source .bashrc
ghcup install ghc 8.10.7
ghcup set ghc 8.10.7
```

#### Build the executable
```
git clone https://github.com/fallen-icarus/cardano-loans
cd cardano-loans
cabal clean
cabal update
cabal build all
```

The `cardano-loans` CLI program should now be at `dist-newstyle/build/x86_64-linux/ghc-8.10.7/cardano-loans-0.2.0.0/x/cardano-loans/build/cardano-loans/cardano-loans`. Move the program to somewhere in your `$PATH`.

All `cardano-loans` subcommands have an associated `--help` option. The functionality is meant to feel like `cardano-cli`.

### Using Nix
The [Nix Package Manager](https://nixos.org/) can be installed on most Linux distributions by downloading and running the installation script
```
curl -L https://nixos.org/nix/install > install-nix.sh
chmod +x install-nix.sh
./install-nix.sh
```
and following the directions.

#### Configuring the Binary Caches
While this step is optional, it can save several hours of time since nix will need a copy of every necessary package. Therefore, it is highly recommended that you do this.
```
sudo mkdir -p /etc/nix
cat <<EOF | sudo tee -a /etc/nix/nix.conf
experimental-features = nix-command flakes
allow-import-from-derivation = true
substituters = https://cache.nixos.org https://cache.iog.io https://cache.zw3rk.com
trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk=
EOF
```
The caches used here come from the plutus-apps contributing [doc](https://github.com/input-output-hk/plutus-apps/blob/713955dea45739de6df3c388717123cfec648914/CONTRIBUTING.adoc#how-to-get-a-shell-environment-with-tools).

You will need to restart the nix service in order to make sure that it uses the newly configured caches. A sure fire way to do this is restart your machine.

#### Building the Executable
```
git clone https://github.com/fallen-icarus/cardano-loans
git clone https://github.com/input-output-hk/plutus-apps
cd plutus-apps
git checkout 68c3721
nix develop # This step can take an hour even with the caches configured
# Set accept-flake-config to true and permanently mark the value as trusted
```
The last command should drop you into a nix terminal once it is finished running. Execute the following within the nix terminal.
```
cd ../cardano-loans
cabal clean
cabal update
cabal build all
```

If all goes well, the `cardano-loans` CLI program should now be at `dist-newstyle/build/x86_64-linux/ghc-8.10.7/cardano-loans-0.2.0.0/x/cardano-loans/build/cardano-loans/cardano-loans`. Move the program to somewhere in your $PATH.

You can now exit the nix terminal with `exit`.

All `cardano-loans` subcommands have an associated `--help` option. The functionality is meant to feel like `cardano-cli`.

#### Troubleshooting Nix
If you encounter a libsodium error, you may need to first install libsodium separately. While not inside the nix terminal (you can leave with `exit`), execute the following:
```
cd # return to your home directory
git clone https://github.com/input-output-hk/libsodium
cd libsodium
git checkout dbb48cc
./autogen.sh
./configure
make
sudo make install
```
Once installed, you can retry the build after exporting the following variables while inside the nix terminal:
```
cd ../plutus-apps
nix develop # This should only take a minute this time
export LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH"
export PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH"
cabal build all
```

### Aiken For Developers
The aiken scripts come precompiled but if you would like to make changes or wish to confirm the compiled scripts yourself, you will also need to install `aiken`. You can install `aiken` using cargo like this:

``` Bash
cargo install aiken --version 1.0.11-alpha
```

Make sure you instal verison 1.0.11-alpha. Newer versions can change things that can break the compilation. As aiken stabilizes, the code will be updated to the latest version.

When building the dApp's blueprints, make sure to use
``` Bash
aiken build --keep-traces
```
or else the user friendly error messages will be stripped from the scripts and the resulting beacons will be different.

For integration testing, you can create your own custom beacons without changing the dApp's logic by changing the string passed [here](aiken/validators/cardano_loans.ak#L23). Currently, it is set to "testing". You can change this to any string personal to you so that you can get custom beacons to play with for testing.

--- 
## Minting test tokens
An always succeeding minting policy as well as the required redeemer are included [here](scripts/mint-test-tokens/). In that directory is also the template bash script that uses them. These can be used to create as many native tokens as needed to test this DEX.

---
## Asking for a loan
#### Export the loan validator script.
``` Bash
cardano-loans export-script loan-script \
  --out-file loan.plutus
```

#### Generate the hash for the staking verification key.
```Bash
borrowerPubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file borrowerStake.vkey)
```

#### Create the loan address.
```Bash
cardano-cli address build \
  --payment-script-file loan.plutus \
  --stake-verification-key-file borrowerStake.vkey \
  --testnet-magic 1 \
  --out-file loan.addr
```

#### Export the beacon policy.
``` Bash
cardano-loans export-script beacon-policy \
  --out-file beacons.plutus
```

#### Get the beacon policy id.
```Bash
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file beacons.plutus) 
```

#### Create the Ask datum.
```Bash
cardano-loans datum ask-datum \
  --beacon-policy-id $beaconPolicyId \
  --borrower-staking-pubkey-hash $borrowerPubKeyHash \
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

#### Create the MintAsk beacon policy redeemer.
```Bash
cardano-loans beacon-redeemer mint-ask \
  --borrower-staking-pubkey-hash $borrowerPubKeyHash \
  --out-file mintAsk.json
```

#### Helper Ask beacon variable
```Bash
askBeacon="${beaconPolicyId}.41736b"
```

#### Create and submit the transaction.
```Bash
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file protocol.json

cardano-cli transaction build \
  --tx-in <borrower_personal_utxo> \
  --tx-out "$(cat loan.addr) + 2000000 lovelace + 1 ${askBeacon}" \
  --tx-out-inline-datum-file askDatum.json \
  --mint "1 ${askBeacon}" \
  --mint-tx-in-reference <beacon_reference_script_utxo> \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file mintAsk.json \
  --policy-id $beaconPolicyId \
  --required-signer-hash $borrowerPubKeyHash \
  --change-address <borrower_personal_addr> \
  --tx-in-collateral <borrower_collateral_utxo> \
  --testnet-magic 1 \
  --protocol-params-file protocol.json \
  --out-file tx.body

cardano-cli transaction sign \
  --tx-body-file tx.body \
  --signing-key-file borrowerPersonalPayment.skey \
  --signing-key-file borrowerStake.skey \
  --testnet-magic 1 \
  --out-file tx.signed

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file tx.signed
```

---
## Closing an Ask
#### Generate the hash for the staking verification key.
```Bash
borrowerPubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file borrowerStake.vkey)
```

#### Export the beacon policy.
```Bash
cardano-loans export-script beacon-policy \
  --out-file beacons.plutus
```

#### Get the beacon policy id.
```Bash
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file beacons.plutus)
```

#### Create the BurnBeacons beacon policy redeemer.
```Bash
cardano-loans beacon-redeemer burn-beacons \
  --out-file burnBeacons.jsons
```

#### Create the CloseAsk redeemer for the loan validator.
```Bash
cardano-loans loan-redeemer close-ask \
  --out-file closeAsk.json
```

#### Helper beacon variable
```Bash
askBeacon="${beaconPolicyId}.41736b"
```

#### Create and submit the transaction.
```Bash
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file protocol.json

cardano-cli transaction build \
  --tx-in <ask_utxo_to_close> \
  --spending-tx-in-reference <loan_reference_script_utxo> \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file closeAsk.json \
  --mint "-1 ${askBeacon}" \
  --mint-tx-in-reference <beacons_reference_script_utxo> \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file burnBeacons.json \
  --policy-id $beaconPolicyId \
  --required-signer-hash $borrowerPubKeyHash \
  --change-address <borrower_personal_addr> \
  --tx-in-collateral <borrower_collateral_utxo> \
  --testnet-magic 1 \
  --protocol-params-file protocol.json \
  --out-file tx.body

cardano-cli transaction sign \
  --tx-body-file tx.body \
  --signing-key-file borrowerPersonalPayment.skey \
  --signing-key-file borrowerStake.skey \
  --testnet-magic 1 \
  --out-file tx.signed

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file tx.signed
```

---
## Offering a Loan
Due to the way the LoanIDs work, only one Offer can be created in a single transaction.

#### Calculate the hash for the lender's pubkey hash.
```Bash
lenderPaymentPubKeyHash=$(cardano-cli address key-hash \
  --payment-verification-key-file lenderPayment.vkey)
```

#### Export the beacon policy.
``` Bash
cardano-loans export-script beacon-policy \
  --out-file beacons.plutus
```

#### Get the beacon policy id.
```Bash
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file beacons.plutus) 
```

#### Create the Offer datum.
```Bash
cardano-loans datum offer-datum \
  --beacon-policy-id $beaconPolicyId \
  --lender-pubkey-hash $lenderPaymentPubKeyHash \
  --payment-pubkey-hash $lenderPaymentPubKeyHash \
  --staking-pubkey-hash <lenders_staking_pubkey_hash> \
  --loan-asset-is-lovelace \
  --principle 10000000 \
  --checkpoint 600 \
  --checkpoint 1200 \
  --checkpoint 1800 \
  --loan-term 3600 \
  --interest-numerator 1 \
  --interest-denominator 10 \
  --collateral-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --collateral-asset-token-name 4f74686572546f6b656e0a \
  --rate-numerator 1 \
  --rate-denominator 500000 \
  --claim-period 3600 \
  --out-file offerDatum.json
```

The `--lender-pubkey-hash` flag is used to declare what the LenderID will be. This can be either a payment pubkey, a staking pubkey, or a staking script. Payment scripts are not supported for the LenderID.

The `--payment-pubkey-hash` flag and `--staking-pubkey-hash` flag are for the lender's address where payments should go. Staking scripts are supported but payment scripts are not.

This loan offer has 3 checkpoints: the first is after 600 slots, the second is after 1200 slots, and the last is after 1800 slots. These are the points at which the compound interest must be applied to the remaining balance at the time of the checkpoint. These checkpoints must be in ascending order and the last one must be less than the slot number specified with the `loan-term` flag (this is the expiration for the loan). 

The following part should be repeated for every asset being used as collateral:
``` Bash
  --collateral-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --collateral-asset-token-name 4f74686572546f6b656e0a \
  --rate-numerator 1 \
  --rate-denominator 500000 \
```

Make sure the collateral order matches the order in the borrower's ask datum.

The `rate-numerator` and the `rate-denominator` fields set the relative price between this collateral asset and the loan asset. So the above example is saying the lender thinks 2 units of this collateral asset is equivalent to 1 ADA. If you do not want a given asset to be used for collateral, set the relative price for that asset to zero. By setting the relative prices above/below market value, the lender can offer an over-collateralized loan or under-collateralized loan, respectively.

The `--claim-period` flag is how long the lender wants to be able to claim the collateral of defaulted loans. Once the claim period has passed, the borrower can reclaim the collateral. This feature is necessary in case the Key NFT is lost. The above example is asking for 3600 slots to claim the loan once the expiration has passed.

#### Create the MintOffer beacon policy redeemer.
```Bash
cardano-loans beacon-redeemer mint-offer \
  --lender-pubkey-hash $lenderPaymentPubKeyHash \
  --out-file mintOffer.json
```

The lender address credential used here must be the same as the one used for the LenderID in the OfferDatum.

#### Create helper variables.
```Bash
offerBeacon="${beaconPolicyId}.4f66666572"
lenderBeacon="${beaconPolicyId}.${lenderPaymentPubKeyHash}"
loanAddr=<target_loan_address>
```

#### Create and submit the transaction.
```Bash
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file protocol.json

cardano-cli transaction build \
  --tx-in <utxo_with_loan_asset_and_fee> \
  --tx-out "${loanAddr} + 15000000 lovelace + 1 ${offerBeacon} + 1 ${lenderBeacon}" \
  --tx-out-inline-datum-file offerDatum.json \
  --mint "1 ${offerBeacon} + 1 ${lenderBeacon}" \
  --mint-tx-in-reference <beacon_policy_reference_script_utxo> \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file mintOffer.json \
  --policy-id $beaconPolicyId \
  --required-signer-hash $lenderPaymentPubKeyHash \
  --change-address <lender_personal_addr> \
  --tx-in-collateral <collateral_utxo> \
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
## Closing an Offer

#### Calculate the hash for the lender's pubkey hash.
```Bash
lenderPaymentPubKeyHash=$(cardano-cli address key-hash \
  --payment-verification-key-file lenderPayment.vkey)
```

#### Export the beacon policy.
``` Bash
cardano-loans export-script beacon-policy \
  --out-file beacons.plutus
```

#### Get the beacon policy id.
```Bash
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file beacons.plutus) 
```

#### Create the BurnBeacons beacon policy redeemer.
```Bash
cardano-loans beacon-redeemer burn-beacons \
  --out-file burnBeacons.json
```

#### Create the CloseOffer redeemer for the loan validator.
``` Bash
cardano-loans loan-redeemer close-offer \
  --out-file closeOffer.json
```

#### Create helper beacon variables.
```Bash
offerBeacon="${beaconPolicyId}.4f66666572"
lenderBeacon="${beaconPolicyId}.${lenderPaymentPubKeyHash}"
```

#### Create and submit the transaction.
```Bash
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file protocol.json

cardano-cli transaction build \
  --tx-in <offer_to_close> \
  --spending-tx-in-reference <spending_reference_script_utxo> \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file closeOffer.json \
  --mint "-1 ${offerBeacon} + -1 ${lenderBeacon}" \
  --mint-tx-in-reference <beacon_reference_script_utxo> \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file burnBeacons.json \
  --policy-id $beaconPolicyId \
  --required-signer-hash $lenderPaymentPubKeyHash \
  --change-address <lender_personal_address> \
  --tx-in-collateral <collateral_utxo> \
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

The lender must signal approve with the same credential used as the LenderID.

---
## Accepting an Offer

#### Generate the hash for the staking verification key.
```Bash
borrowerPubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file borrowerStake.vkey)
```

#### Export the beacon policy.
``` Bash
cardano-loans export-script beacon-policy \
  --out-file beacons.plutus
```

#### Get the beacon policy id.
```Bash
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file beacons.plutus) 
```

#### Create the AcceptOffer redeemer for the loan validator.
```Bash
cardano-loans loan-redeemer accept-offer \
  --out-file acceptOffer.json
```

#### Create the MintActive beacon policy redeemer.
```Bash
cardano-loans beacon-redeemer mint-active \
  --borrower-staking-pubkey-hash $borrowerPubKeyHash \
  --tx-hash <ask_input_tx_hash> \
  --output-index <ask_input_output_index> \
  --tx-hash <offer_input_tx_hash> \
  --output-index <offer_input_output_index> \
  --out-file mintActive.json
```

The Ask and Offer inputs must be paired together like the above example. The Ask input must always come first. If multiple Offers are being accepted, the above can be expanded like this:
```Bash
cardano-loans beacon-redeemer mint-active \
  --borrower-staking-pubkey-hash $borrowerPubKeyHash \
  --tx-hash <ask_input1_tx_hash> \
  --output-index <ask_input1_output_index> \
  --tx-hash <offer_input1_tx_hash> \
  --output-index <offer_input1_output_index> \
  --tx-hash <ask_input2_tx_hash> \
  --output-index <ask_input2_output_index> \
  --tx-hash <offer_input2_tx_hash> \
  --output-index <offer_input2_output_index> \
  --out-file mintActive.json
```

Again, make sure the Ask input is first for each pairing.

#### Create the Active datum for accepting an offer. 
The loan-id is the tx hash for the corresponding Offer.
```Bash
cardano-loans datum accept-datum \
  --beacon-policy-id $beaconPolicyId \
  --borrower-staking-pubkey-hash $borrowerPubKeyHash \
  --payment-pubkey-hash <lender_address_payment_pubkey> \
  --staking-pubkey-hash <lender_address_staking_pubkey> \
  --loan-asset-is-lovelace \
  --principle 10000000 \
  --checkpoint 600 \
  --checkpoint 1200 \
  --checkpoint 1800 \
  --loan-term 3600 \
  --interest-numerator 1 \
  --interest-denominator 10 \
  --collateral-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --collateral-asset-token-name 4f74686572546f6b656e0a \
  --rate-numerator 1 \
  --rate-denominator 500000 \
  --claim-period 3600 \
  --loan-id <offer_input_tx_hash> \
  --starting-slot <slot_used_for_invalid_before_in_acceptance_tx> \
  --out-file activeDatum.json
```

#### Create the lender's bech32 address.
```Bash
lenderAddr=$(cardano-loans convert-address \
  --payment-pubkey-hash <lender_address_payment_pubkey> \
  --staking-pubkey-hash <lender_address_staking_pubkey> \
  --stdout)
```

The address credentials here come directly from the Offer datum.

#### Helper beacon variables
```Bash
askBeacon="${beaconPolicyId}.41736b"
offerBeacon="${beaconPolicyId}.4f66666572"
lenderBeacon="${beaconPolicyId}.<lender_id_from_offer_datum>"
activeBeacon="${beaconPolicyId}.416374697665"
borrowerBeacon="${beaconPolicyId}.${borrowerPubKeyHash}"
loanIdBeacon="${beaconPolicyId}.<offer_tx_hash>"
```

#### Create and submit the transaction.
```Bash
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file protocol.json

cardano-cli transaction build \
  --tx-in <borrower_input_for_fee_and_collateral> \
  --tx-in <ask_input> \
  --spending-tx-in-reference <spending_reference_script_utxo> \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file acceptOffer.json \
  --tx-in <offer_input> \
  --spending-tx-in-reference <spending_reference_script_utxo> \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file acceptOffer.json \
  --tx-out "<loan_addr> + 3000000 lovelace + 1 ${loanIdBeacon} + 1 ${activeBeacon} + 1 ${borrowerBeacon} + 20 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file activeDatum.json \
  --tx-out "${lenderAddr} + 5000000 lovelace + 1 ${loanIdBeacon}" \
  --mint "1 ${activeBeacon} + 1 ${borrowerBeacon} + 2 ${loanIdBeacon} + -1 ${askBeacon} + -1 ${offerBeacon} + -1 ${lenderBeacon}" \
  --mint-tx-in-reference <beacons_reference_script_utxo> \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file mintOffer.json \
  --policy-id $beaconPolicyId \
  --required-signer-hash $borrowerPubKeyHash \
  --change-address <borrower_personal_addr> \
  --tx-in-collateral <collateral_utxo> \
  --invalid-before <slot_for_start_of_loan> \
  --testnet-magic 1 \
  --protocol-params-file protocol.json \
  --out-file tx.body

cardano-cli transaction sign \
  --tx-body-file tx.body \
  --signing-key-file borrowerPersonal.skey \
  --signing-key-file borrowerStake.skey \
  --testnet-magic 1 \
  --out-file tx.signed

cardano-cli transaction submit \
  --testnet-magic 1 \
  --tx-file tx.signed
```

The borrower must signal approval with whatever credential the loan address uses for the staking credential.

---
## Making a loan payment
A single transaction can mix partial and full loan payments.

#### Generate the hash for the staking verification key.
```Bash
borrowerPubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file borrowerStake.vkey)
```

#### Export the beacon policy.
``` Bash
cardano-loans export-script beacon-policy \
  --out-file beacons.plutus
```

#### Get the beacon policy id.
```Bash
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file beacons.plutus) 
```

#### Create the spending redeemer.
```Bash
cardano-loans loan-redeemer make-payment \
  --out-file makePayment.json
```

#### Create the new collateral datum.
```Bash
cardano-loans datum collateral-payment-datum \
  --beacon-policy-id $beaconPolicyId \
  --borrower-staking-pubkey-hash $borrowerPubKeyHash \
  --payment-pubkey-hash <lender_payment_pubkey_hash> \
  --staking-pubkey-hash <lender_staking_pubkey_hash> \
  --loan-asset-is-lovelace \
  --principle 10000000 \
  --checkpoint 600 \
  --checkpoint 1200 \
  --checkpoint 1800 \
  --loan-term 3600 \
  --interest-numerator 1 \
  --interest-denominator 10 \
  --collateral-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --collateral-asset-token-name 4f74686572546f6b656e0a \
  --rate-numerator 1 \
  --rate-denominator 500000 \
  --claim-expiration <slot_where_claim_period_ends> \
  --loan-expiration <slot_where_loan_expires> \
  --balance-numerator 10000000 \
  --balance-denominator 1 \
  --payment-amount 10000000 \
  --loan-id <loan_id_for_target_loan> \
  --out-file collateralPaymentDatum.json
```

The `--balance-numerator` and `--balance-denominator` flag are used to specify the current balance of the loan (prior to payment). The `--payment-amount` flag is used to say how much the borrower intends to pay. The `cardano-loans` CLI is able to generate the proper datum from this information.

#### Create the datum for the lender's payment.
```Bash
cardano-loans datum lender-payment-datum \
  --loan-id <loan_id_for_target_loan> \
  --out-file lenderPaymentDatum.json
```

#### Create the lender's bech32 address.
```Bash
lenderAddr=$(cardano-loans convert-address \
  --payment-pubkey-hash <lender_address_payment_pubkey> \
  --staking-pubkey-hash <lender_address_staking_pubkey> \
  --stdout)
```

#### Create the burn redeemer.
```Bash
cardano-loans beacon-redeemer burn-beacons \
  --out-file burnBeacons.json
```

#### Helper beacon variables.
```Bash
loanIdBeacon="${beaconPolicyId}.<loan_id_for_target_loan>"
activeBeacon="${beaconPolicyId}.416374697665"
borrowerBeacon="${beaconPolicyId}.${borrowerPubKeyHash}"
```

#### Create and submit the transaction.
```Bash
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file protocol.json

cardano-cli transaction build \
  --tx-in <utxo_with_payment> \
  --tx-in <loan_utxo> \
  --spending-tx-in-reference <spending_reference_script_utxo> \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file makePayment.json \
  --tx-out "<loan_addr> + 3000000 lovelace + 1 ${activeBeacon} + 1 ${loanIdBeacon}" \
  --tx-out-inline-datum-file collateralPaymentDatum.json \
  --tx-out "${lenderAddr} + 10000000 lovelace" \
  --tx-out-inline-datum-file lenderPaymentDatum.json \
  --mint "-1 ${borrowerBeacon}" \
  --mint-tx-in-reference <beacons_reference_script_utxo> \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file burnBeacons.json \
  --policy-id $beaconPolicyId \
  --required-signer-hash $borrowerPubKeyHash \
  --change-address <borrower_personal_addr> \
  --tx-in-collateral <collateral_utxo> \
  --invalid-hereafter <loan_expiration_slot> \
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

The amount paid to the lender's address must exactly match the amount specified in the datum with the `--payment-amount` flag. 

If the full amount is not being paid off, then the BorrowerID must still be stored with the loan UTxO.

The `--invalid-hereafter` flag can always be set to the loan expiration slot.

---
## Rolling over a loan

#### Generate the hash for the staking verification key.
```Bash
borrowerPubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file borrowerStake.vkey)
```

#### Export the beacon policy.
``` Bash
cardano-loans export-script beacon-policy \
  --out-file beacons.plutus
```

#### Get the beacon policy id.
```Bash
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file beacons.plutus) 
```

#### Create the Rollvoer redeemer for the loan validator.
```Bash
cardano-loans loan-redeemer rollover \
  --out-file rollover.json
```

#### Create the new datum.
```Bash
cardano-loans datum rollover-datum \
  --beacon-policy-id $beaconPolicyId \
  --borrower-staking-pubkey-hash $borrowerPubKeyHash \
  --payment-pubkey-hash <lender_payment_pubkey_hash> \
  --staking-pubkey-hash <lender_staking_pubkey_hash> \
  --loan-asset-is-lovelace \
  --principle 10000000 \
  --next-checkpoint <slot_of_next_checkpoint> \
  --next-checkpoint <slot_of_checkpoint_after_that> \
  --past-checkpoint <most_recent_passed_checkpoint> \
  --past-checkpoint <passed_checkpoint_before_that> \
  --loan-term 3600 \
  --interest-numerator 1 \
  --interest-denominator 10 \
  --collateral-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --collateral-asset-token-name 4f74686572546f6b656e0a \
  --rate-numerator 1 \
  --rate-denominator 500000 \
  --claim-expiration <slot_where_claim_period_ends> \
  --loan-expiration <slot_where_loan_expires> \
  --balance-numerator 10000000 \
  --balance-denominator 1 \
  --loan-id <target_loan_id> \
  --out-file rolloverDatum.json
```

All of the above fields should be what the datum currently has. The `cardano-loans` CLI will be able to generate the proper datum from that.

The `--next-checkpoint` flags should be in order from closest to farthest away in time (ascending). The `--past-checkpoint` flags should be in order from most recent to least recent (descending). If the checkpoint list is empty, you can ommit those flags.

#### Helper beacon variables.
```Bash
loanIdBeacon="${beaconPolicyId}.<target_loan_id>"
activeBeacon="${beaconPolicyId}.416374697665"
borrowerBeacon="${beaconPolicyId}.${borrowerPubKeyHash}"
```

#### Create and submit the transaction.
```Bash
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file protocol.json

cardano-cli transaction build \
  --tx-in <utxo_for_fee> \
  --tx-in <loan_utxo> \
  --spending-tx-in-reference <spending_reference_script_utxo> \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file rollover.json \
  --tx-out "<loan_addr> + 3000000 lovelace + 1 ${activeBeacon} + 1 ${loanIdBeacon} + 1 ${borrowerBeacon} + 20 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file rolloverDatum.json \
  --required-signer-hash $borrowerPubKeyHash \
  --change-address <borrower_personal_addr> \
  --tx-in-collateral <collateral_utxo> \
  --invalid-hereafter <slot_where_loan_expires> \
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
## Claim an expired loan

#### Calculate the hash for the lender's pubkey hash.
```Bash
lenderPaymentPubKeyHash=$(cardano-cli address key-hash \
  --payment-verification-key-file lenderPayment.vkey)
```

#### Export the beacon policy.
``` Bash
cardano-loans export-script beacon-policy \
  --out-file beacons.plutus
```

#### Get the beacon policy id.
```Bash
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file beacons.plutus) 
```

#### Create the BurnBeacons beacon policy redeemer.
```Bash
cardano-loans beacon-redeemer burn-beacons \
  --out-file burnBeacons.json
```

#### Create the ClaimExpired redeemer for the loan validator.
``` Bash
cardano-loans loan-redeemer claim-expired \
  --out-file claimExpired.json
```

#### Helper beacon variables.
```Bash
loanIdBeacon="${beaconPolicyId}.<target_loan_id>"
activeBeacon="${beaconPolicyId}.416374697665"
borrowerBeacon="${beaconPolicyId}.<borrower_id>"
```

#### Create and submit the transaction.
```Bash
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file protocol.json

cardano-cli transaction build \
  --tx-in <utxo_with_key_nft_and_fee> \
  --tx-in <loan_utxo> \
  --spending-tx-in-reference <spending_reference_script_utxo> \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file claimExpired.json \
  --required-signer-hash $lenderPaymentPubKeyHash \
  --mint "-1 ${activeBeacon} + -1 ${borrowerBeacon} + -2 ${loanIdBeacon}" \
  --mint-tx-in-reference <beacons_reference_script_utxo> \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file burnBeacons.json \
  --policy-id $beaconPolicyId \
  --change-address <lender_personal_addr> \
  --tx-in-collateral <collateral_utxo> \
  --invalid-before <loan_expiration_plus_one_slot> \
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

Whoever owns the Key NFT for the target loan is able to claim the collateral for that expired loan.

---
## Update Lender Address
The lender address in the ActiveDatum can be updated by anyone who controls the Key NFT. It can even be updated in the same transaction where the Key NFT is purchased. All loan payments go to the address specified in the datum so it is important for it to remain up-to-date.

#### Export the beacon policy.
``` Bash
cardano-loans export-script beacon-policy \
  --out-file beacons.plutus
```

#### Get the beacon policy id.
```Bash
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file beacons.plutus) 
```

#### Create the spending redeemer.
```Bash
cardano-loans loan-redeemer update-address \
  --payment-pubkey-hash <new_lender_payment_pubkey_hash> \
  --staking-pubkey-hash <new_lender_staking_pubkey_hash> \
  --out-file updateAddress.json
```

The address that gets passed with this redeemer must match the address specified in the new datum. No payment scripts are allowed for the lender's address.

#### Create the new datum.
```Bash
cardano-loans datum update-address-datum \
  --beacon-policy-id $beaconPolicyId \
  --borrower-id <borrower_id> \
  --payment-pubkey-hash <new_lender_payment_pubkey_hash> \
  --staking-pubkey-hash <new_lender_staking_pubkey_hash> \
  --loan-asset-is-lovelace \
  --principle 10000000 \
  --past-checkpoint 33328517 \
  --loan-term 3600 \
  --interest-numerator 1 \
  --interest-denominator 10 \
  --collateral-asset-policy-id c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d \
  --collateral-asset-token-name 4f74686572546f6b656e0a \
  --rate-numerator 1 \
  --rate-denominator 500000 \
  --claim-expiration 33333917 \
  --loan-expiration 33330317 \
  --balance-numerator 11000000 \
  --balance-denominator 1 \
  --loan-id <target_loan_id> \
  --out-file updateDatum.json
```

#### Helper beacon variables.
``` Bash
loanIdBeacon="${beaconPolicyId}.<target_loan_id>"
activeBeacon="${beaconPolicyId}.416374697665"
borrowerBeacon="${beaconPolicyId}.<borrower_id>"
```

#### Create and submit the transaction.
```Bash
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file protocol.json

cardano-cli transaction build \
  --tx-in <utxo_with_fee_and_key_nft> \
  --tx-in <loan_utxo> \
  --spending-tx-in-reference <spending_reference_script_utxo> \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file updateAddress.json \
  --tx-out "<loan_addr> + 3000000 lovelace + 1 ${activeBeacon} + 1 ${loanIdBeacon} + 1 ${borrowerBeacon} + 20 c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a" \
  --tx-out-inline-datum-file updateDatum.json \
  --change-address <lender_personal_address> \
  --tx-in-collateral <collateral_utxo> \
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
## Clean up finished loan utxos or claim lost collateral
#### Generate the hash for the staking verification key.
```Bash
borrowerPubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file borrowerStake.vkey)
```

#### Export the beacon policy.
```Bash
cardano-loans export-script beacon-policy \
  --out-file beacons.plutus
```

#### Get the beacon policy id.
```Bash
beaconPolicyId=$(cardano-cli transaction policyid \
  --script-file beacons.plutus)
```

#### Create the BurnBeacons beacon policy redeemer.
```Bash
cardano-loans beacon-redeemer burn-beacons \
  --out-file burnBeacons.jsons
```

#### Create the spending redeemer.
```Bash
cardano-loans loan-redeemer unlock \
  --out-file unlock.json
```

#### Helper beacon variables.
```Bash
activeBeacon="${beaconPolicyId}.416374697665"
loanIdBeacon="${beaconPolicyId}.<target_loan_id>"
```

#### Create and submit the transaction.
```Bash
cardano-cli query protocol-parameters \
  --testnet-magic 1 \
  --out-file protocol.json

cardano-cli transaction build \
  --tx-in <loan_utxo> \
  --spending-tx-in-reference <spending_reference_script_utxo> \
  --spending-plutus-script-v2 \
  --spending-reference-tx-in-inline-datum-present \
  --spending-reference-tx-in-redeemer-file unlock.json \
  --mint "-1 ${activeBeacon} + -1 ${loanIdBeacon}" \
  --mint-tx-in-reference <beacons_reference_script_utxo> \
  --mint-plutus-script-v2 \
  --mint-reference-tx-in-redeemer-file burnBeacons.json \
  --policy-id $beaconPolicyId \
  --required-signer-hash $borrowerPubKeyHash \
  --change-address <borrower_personal_addr> \
  --tx-in-collateral <collateral_utxo> \
  --testnet-magic 1 \
  --protocol-params-file protocol.json \
  --invalid-before <claim_expiration_slot_plus_one> \
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

If the BorrowerID is still present (in the case of a lost collateral UTxO), it must also be burned.

---
## Convert POSIX time <--> Slot
``` Bash
cardano-loans convert-time --slot 26668590

cardano-loans convert-time --posix-time 1682351790000
```

---
## Address Conversions
Since plutus smart contracts do not use bech32 encoded addresses while cardano-cli does, addresses will need to be converted as necessary. To make this as painless as possible, `cardano-loans` is capable of doing these conversions for you. It uses [`cardano-addresses`](https://github.com/input-output-hk/cardano-addresses) under the hood.

### Plutus Hashes to Bech32
``` Bash
cardano-loans convert-address \
  --payment-pubkey-hash ae0d001455a855e6c00f98fa9061028f5c00d297926383bc501be2d2 \
  --staking-pubkey-hash 623a2b9a369454b382c131d7e3d12c4f93024022e5c5668cf0c5c25c \
  --stdout
```

All bech32 addresses generated with this command will be for the preproduction testnet. When the protocol is ready for mainnet, support will be added for mainnet addresses.

### Bech32 to Plutus Hashes
``` Bash
cardano-loans convert-address \
  --address addr_test1vrlfp27zjnjlsak5f7dnjkpl9ekeq5ezc3e4uw769y5rgtc4qvv2f \
  --stdout
```

This will result in the following output when piped to `jq`:
``` JSON
{
  "network_tag": 0,
  "payment_pubkey_hash": "fe90abc294e5f876d44f9b39583f2e6d905322c4735e3bda2928342f",
  "payment_script_hash": null,
  "staking_pubkey_hash": null,
  "staking_script_hash": null
}
```

The `network_tag` of 0 corresponds to the Preproduction testnet (1 would be Mainnet). This address uses a spending pubkey and has no staking credential.

---
## Query Beacons

The `cardano-loans query --help` command lists all queries currently supported by the `cardano-loans` CLI.