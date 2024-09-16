# Getting Started

The `cardano-loans` CLI assumes that all transactions are built and signed using `cardano-cli`.
**Access to a local node is not necessary**, although it does simplify things. Koios can be used for
all steps that require access to a node.

Template bash scripts that follow these steps are available [here](scripts/). There are only
examples using a local node. Using a remote node requires extra steps since the transaction must be
manually balanced. If you would like to use a remote node, the `cardano-loans` CLI supports
everything you need. You can cross-reference these local node template scripts with the
[cardano-swaps](https://github.com/fallen-icarus/cardano-swaps/tree/main/scripts) remote node
template scripts to come up with your own remote node template scripts for cardano-loans.

## Table of Contents
- [Installing](#installing)
- [Aiken For Developers](#aiken-for-developers)
- [Overspent Budget](#overspent-budget)
- [Using Remote Nodes](#using-remote-nodes)
- [Invalid-Hereafter](#invalid-hereafter)
- [Minting Test Tokens](#minting-test-tokens)
- [Registering Scripts - DEVELOPERS ONLY](#registering-the-scripts-for-staking-execution---developers-only)
- [Creating Reference Scripts](#creating-reference-scripts)
- [Creating an Ask UTxO](#creating-an-ask-utxo)
- [Closing an Ask UTxO](#closing-an-ask-utxo)
- [Creating an Offer UTxO](#creating-an-offer-utxo)
- [Closing an Offer UTxO](#closing-an-offer-utxo)
- [Accepting an Offer](#accepting-an-offer)
- [Making a Loan Payment](#making-a-loan-payment)
- [Applying Interest](#applying-interest)
- [Updating a Lender Address](#updating-a-lender-address)
- [Claiming Expired Collateral](#claiming-expired-collateral)
- [Unlocking Lost Collateral](#unlocking-lost-collateral)
- [Time Conversions](#time-conversions)
- [Queries](#queries)
  - [Querying Personal Addresses](#querying-personal-addresses)
  - [Querying Ask UTxOs](#querying-ask-utxos)
  - [Querying Offer UTxOs](#querying-offer-utxos)
  - [Querying Active UTxOs](#querying-active-utxos)
  - [Querying the Current Time](#querying-the-current-time)
  - [Querying a Borrower's Credit History](#querying-a-borrowers-credit-history)
  - [Querying a Loan's History](#querying-a-loans-history)

## Installing

Make sure `cardano-cli` is also installed. You can get the most up-to-date copy from IOG's
cardano-node repo [here](https://github.com/IntersectMBO/cardano-node/releases). It will be in the
cardano-node tarball under the latest release.

### Install the necessary packages - similar to cardano-node
```bash
sudo apt update
sudo apt upgrade
sudo apt-get install autoconf automake build-essential curl g++ git jq libffi-dev libgmp-dev libncursesw5 libssl-dev libsystemd-dev libtinfo-dev libtool make pkg-config wget zlib1g-dev liblzma-dev libpq-dev
```

### Install GHC and Cabal
```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

- Prepend or append the required PATH variable.
- You do not need to install the haskell-langauge-server.
- You do not need to install stack.
- Install the required packages. You can keep this terminal window open and install from another
window.
- Press ENTER to proceed.

```bash
source $HOME/.bashrc
ghcup install ghc 9.6.5
```

### Install libsodium, scep256k1, and blst
```bash
git clone https://github.com/intersectmbo/libsodium
cd libsodium
git checkout dbb48cc
./autogen.sh
./configure
make
sudo make install

cd ../ # Leave the libsodium directory.
git clone https://github.com/bitcoin-core/secp256k1
cd secp256k1
git checkout ac83be33
./autogen.sh
./configure --enable-module-schnorrsig --enable-experimental
make
make check
sudo make install
sudo ldconfig

cd ../ # Leave the secp256k1 directory.
git clone https://github.com/supranational/blst
cd blst
git checkout v0.3.10
./build.sh
cat > libblst.pc << EOF # This command extends until the next EOF
prefix=/usr/local
exec_prefix=\${prefix}
libdir=\${exec_prefix}/lib
includedir=\${prefix}/include

Name: libblst
Description: Multilingual BLS12-381 signature library
URL: https://github.com/supranational/blst
Version: 0.3.10
Cflags: -I\${includedir}
Libs: -L\${libdir} -lblst
EOF
sudo cp libblst.pc /usr/local/lib/pkgconfig/
sudo cp bindings/blst_aux.h bindings/blst.h bindings/blst.hpp  /usr/local/include/
sudo cp libblst.a /usr/local/lib
sudo chmod u=rw,go=r /usr/local/{lib/{libblst.a,pkgconfig/libblst.pc},include/{blst.{h,hpp},blst_aux.h}}
```

You need to execute the following to make the new packages usable:
```bash
echo '' >> $HOME/.bashrc # Add a newline to your .bashrc file.
echo 'export LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH"' >> $HOME/.bashrc
echo 'export PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH"' >> $HOME/.bashrc
source $HOME/.bashrc
```

### Build the executable - this may take about 30 minutes
```bash
cd ../ # Leave the blst directory.
git clone https://github.com/fallen-icarus/cardano-loans
cd cardano-loans
cabal clean
cabal update
cabal build exe:cardano-loans
```

The `cardano-loans` CLI program should now be at
`dist-newstyle/build/x86_64-linux/ghc-9.6.5/cardano-loans-1.0.0.0/x/cardano-loans/build/cardano-loans/cardano-loans`.
Move the program to somewhere in your `$PATH`.

All `cardano-loans` subcommands have an associated `--help` option. The functionality is meant to
feel like `cardano-cli`.

The smart contracts are compiled *into* the created `cardano-loans` CLI. The executable has
everything you need for using the protocol. It is a batteries included CLI.

## Aiken For Developers

The aiken smart contracts come precompiled but if you would like to make changes or wish to confirm
the compiled scripts yourself, you will also need to install `aiken`. You can install `aiken` using
cargo like this:

```bash
cargo install aiken --version 1.0.26-alpha
```

Make sure you instal verison 1.0.26-alpha. Newer versions may change some things and so the source
code may not compile or may result in a different script. As aiken stabilizes, the code will be
updated to the latest version.

> [!TIP] 
> If the above command doesn't work, you can build aiken from source:
> ```bash
> git clone https://github.com/aiken-lang/aiken
> cd aiken
> git checkout v1.0.26-alpha
> cargo build
> ```
> The executable should now be located at `target/debug/aiken`.

When building the protocol's blueprints, make sure to use

```bash
aiken build -f user-defined -t verbose
```

or else the user friendly error messages will be stripped from the smart contracts and the resulting
beacons will be different.

If you would like to use the `cardano-loans` CLI after making your changes, you will need to
rebuild it with `cabal bulid exe:cardano-loans`. As long as you did not make any breaking changes,
the CLI should still work for you.

If you would like to test your changes, you can run the tests using `cabal run tests`. As long
as you did not make any breaking changes, the tests should quickly give you feedback. There are
four kinds of tests:

1) Regression tests - tests for features that should work.
2) Failure tests - tests for scenarios that are supposed to fail.
3) Bench tests - tests to check for degraded performance in specific scenarios.
4) Performance Increase tests - tests to check for improved performance in specific scenarios; these
tests will fail if performance increases to alert you of the change.

To see the documentation for the tests, you can build the haddocks for the tests using `cabal
haddock tests`. The documentation may be easier to read than the source code. You can view the
documentation in any browser.

## Overspent Budget

While `cardano-cli` is able to auto-balance transactions, the auto-balancer does not always work
when scripts are executed in a transaction where native tokens must go to the change address. It
does not properly add the change *before* estimating the execution budgets for the transaction which
always results in it under-estimating the required execution units needed by the scripts. There are
open issues about this [here](https://github.com/input-output-hk/cardano-node/issues/5386) and
[here](https://github.com/input-output-hk/cardano-api/issues/302). If you ever see a very long and
confusing error message while using `cardano-cli conway transaction build`, this is probably the issue.

As a work around, whenever you build a transaction using `cardano-cli conway transaction build` where
scripts are being executed, you must manually create an output that has all of the native tokens
that would normally go into the change output. You can let the auto-balancer balance the ada.

## Using Remote Nodes

`cardano-cli conway transaction build` requires a local node for the auto-balancer which means it cannot be
used to build a transaction for a remote node. Instead, the `cardano-cli conway transaction build-raw` 
command is required. This command requires the following steps:
1. Build a temporary transaction that is missing the execution units and transaciton fee but is
   properly balanced. You can assume a fee of zero for this transaction.
2. Submit the temporary transaction for execution budget estimations.
3. Rebuild the transaction with the proper execution budgets. The fee is still set to zero.
4. Calculate the required fee for this new temporary transaction.
5. Create the final transaction with the required fee and properly balanced outputs (subtract off
   the fee from the change).
6. Sign the transaction and submit to a remote node.

The `cardano-loans` CLI uses [Koios](https://koios.rest/) in all scenarios where a node is required.

#### Exporting protocol parameters

Some of the above steps will require the current protocol parameters. The `cardano-loans` CLI had
the preproduction testnet and mainnet protocol parameters compiled into the executable when it was
built with `cabal build exe:cardano-loans`. The parameters are already formatted in the way
`cardano-cli` requires. To export the parameters, you can use:
```bash
cardano-loans protocol-params \
  --testnet \
  --out-file protocolParams.json
```

#### Estimating execution budgets

Submitting a transaction for execution budget estimations can be done with this command:
```bash
cardano-loans evaluate-tx \
  --testnet \
  --tx-file tx.body
```

This action uses Koios. The returned budgets will be indexed by the input order, policy id order,
and withdrawal credential order. **This may not be the same order you specified when building the
temporary transaction.** The node will reorder them based on lexicographical ordering. If you are
not sure of the proper ordering, you can view the transaction file that is created with
`cardano-cli` using `cardano-cli transaction view`; the inputs, policy ids, and withdrawal
credentials will be properly ordered.

#### Submitting the final transaction

Submitting the final transaction for addition to the blockchain can be done with this command:
```bash
cardano-loans submit \
  --testnet \
  --tx-file tx.signed
```

The transaction will be submitted through Koios.

## Invalid-Hereafter

The invalid-hereafter validity interval bound is used to prove that a loan's deadline has not
passed. However, the Cardano blockchain does not allow setting invalid-hereafter to be more than 36
hours (129600 slots) passed the current slot. The reason for this is that hardforks can change slot
lengths and therefore change the conversion from slot number to POSIX time. The 36 hours is a window
of time where a hardfork is not possible and therefore, the node can guarantee the time conversion
is correct.

This restriction is only an issue when building transactions. Loans can still have expirations way
passed the 36 hour horizon. The only thing to be aware of is that the invalid-hereafter must be set
to `129600 + current_slot` unless the loan's next deadline is within 36 hours of the current time.

> [!IMPORTANT] 
> The 36 hours (129600 slots) is a network parameter that can be changed.

## Minting Test Tokens

An always succeeding minting policy as well as the required redeemer are included with template bash
scripts for a local node. These can be used to create as many native tokens as needed to test this
protocol.

To see how to mint test tokens using a local node, refer
[here](scripts/local-node/mint-test-tokens/)

## Registering the scripts for staking execution - DEVELOPERS ONLY

**This action only needs to be done once per network for the entire protocol. It does not need to be
done by any users.** These instructions are for completeness as they may be needed by developers.

The plutus scripts cannot be executed as staking scripts until after they are registered. Once the
scripts are registered, they can be used as staking scripts immediately by all users. Registering
the scripts does not require executing the scripts (this may change in the future). Once they are
registered, *they cannot be deregistered*.

**Registration has already been done for all scripts for the preproduction testnet.**

The following protocol scripts requires staking executions, and therefore, require registration:
- Negotiation Beacon Script
- Payment Observer Script
- Interest Observer Script
- Address Update Observer Script

Registering the scripts involve:
1. Exporting each script from the `cardano-loans` CLI.
2. Creating a registration certificate for each script.
3. Submitting a transaction that also pays the registration deposit (2 ADA per registration).

All scripts can be registered in a single transaction (this may change if registration eventually
requires executing each script).

#### Exporting the scripts
```bash
cardano-loans scripts \
  --negotiation-script \
  --out-file negotiation_beacons.plutus

cardano-loans scripts \
  --payment-script \
  --out-file payment_observer.plutus

cardano-loans scripts \
  --interest-script \
  --out-file interest_observer.plutus

cardano-loans scripts \
  --address-update-script \
  --out-file address_update_observer.plutus
```

#### Create the registration certificates
```bash
cardano-cli stake-address registration-certificate \
  --stake-script-file negotiation_beacons.plutus \
  --out-file negotiation_beacons.cert

cardano-cli stake-address registration-certificate \
  --stake-script-file payment_observer.plutus \
  --out-file payment_observer.cert

cardano-cli stake-address registration-certificate \
  --stake-script-file interest_observer.plutus \
  --out-file interest_observer.cert

cardano-cli stake-address registration-certificate \
  --stake-script-file address_update_observer.plutus \
  --out-file address_update_observer.cert
```

#### Building the transaction
To see how to build the transaction using a local node, refer 
[here](scripts/local-node/create-reference-scripts/register-scripts.sh)

## Creating Reference Scripts

**Do not skip this step!** While beacon tokens can be used to trustlessly share reference scripts,
this has not been set up for the beta testing. For now, you will need your own reference scripts.

Creating reference scripts involves the following steps:
1. Export the scripts from the `cardano-loans` CLI.
2. Submit a transaction with the reference script stored in the outputs.

Due to the sizes of the scripts, each script will require its own transaction.

You can see examples [here](scripts/local-node/create-reference-scripts/).

## Creating an Ask UTxO

Creating an Ask UTxO involves the following steps:
1. Create your borrower address.
2. Calculate the hash of the staking credential used in your borrower address.
2. Calculate the required beacon names to mint.
3. Create the required beacon script redeemer.
4. Create the required AskDatum for each Ask UTxO you wish to create.
5. Submit a transaction that creates the Ask UTxOs.

#### Creating your borrower address
```bash
# Export the spending script.
cardano-loans scripts loan-script \
  --out-file loan.plutus

# Create the borrower address.
cardano-cli address build \
  --payment-script-file loan.plutus \
  --stake-verification-key-file borrower_stake.vkey \
  --testnet-magic 1 \
  --out-file borrower_loan.addr
```

Cardano-Loans also supports using staking scripts for the address. To use a staking script, use
the `--stake-script-file` flag instead of the `--stake-verification-key-file` flag.

For a mainnet address, just use the `--mainnet` flag instead of `--testnet-magic 1` when creating
the address.

#### Calculate the hash of the staking credential used in the borrower address
```bash
borrowerStakePubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file borrower_stake.vkey)
```

If you are using a staking script credential, you can create the credential hash with this command:
```bash
borrowerStakeScriptHash=$(cardano-cli transaciton policyid \
  --script-file stake_script.plutus)
```

While the above command is technically meant for minting policies, it works for generating the hash
of any script.

#### Calculate the required beacon names to mint
Ask UTxOs require two negotiation beacons: the "Ask" beacon and the loan asset beacon.

```bash
# Get the policy id for the negotiation beacons.
beaconPolicyId=$(cardano-loans beacon-name policy-id \
  --negotiation-beacons \
  --stdout) 

# Get the required "Ask" beacon name. It is just the hexidecimal encoding of "Ask".
askTokenName=$(cardano-loans beacon-name asset-name \
  --ask-beacon \
  --stdout)

# Get the required loan asset beacon name.
assetTokenName=$(cardano-loans beacon-name asset-name \
  --loan-asset 'lovelace' \
  --stdout)

# Create the required full beacon names.
askBeacon="${beaconPolicyId}.${askTokenName}"
assetBeacon="${beaconPolicyId}.${assetTokenName}"
```

The above beacons are for an Ask UTxO where ada would be the loan asset. If you would like to ask
for a native asset instead, replace `'lovelace'` with `'<policy_id>.<asset_name>'`. For example:

```bash
assetTokenName=$(cardano-loans beacon-name asset-name \
  --loan-asset 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a' \
  --stdout)
```

#### Create the required negotiation beacon script redeemer
```bash
cardano-loans redeemers negotiation-script manage-asks \
  --borrower-staking-pubkey-hash $borrowerStakePubKeyHash \
  --out-file negotiation_redeemer.json
```

If you are using a staking script, use `borrower-staking-script-hash` instead.

#### Creating the required AskDatum
```bash
cardano-loans datums ask \
  --borrower-staking-pubkey-hash $borrowerStakePubKeyHash \
  --loan-asset 'lovelace' \
  --principal 10000000 \
  --loan-term '3600 slots' \
  --collateral-asset 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a' \
  --collateral-asset 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31' \
  --out-file ask_datum.json
```

`loan-term` can either be in terms of slots (like above) or in terms of days. If you would like to
specify days, you can use `--loan-term '2 days'`.

The `collateral-asset` fields *must* be in lexicographical order. If you reverse the order of the two
collateral assets above, the negotiation beacon script will crash with an error saying your
collateral assets are wrong.

#### Building the transaction
To see how to build the transaction using a local node, refer
[here](scripts/local-node/create-ask.sh).

## Closing an Ask UTxO

Closing an Ask UTxO requires the following steps:
1. Calculate the hash of the borrower's staking credential.
2. Create the required spending script redeemer.
3. Calculate the required negotiation beacon names to burn.
4. Create the required negotiation beacon script redeemer.
5. Submit the transaction.

#### Calculate the hash of the staking credential used in the borrower address
```bash
borrowerStakePubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file borrower_stake.vkey)
```

#### Calculate the required beacon names to burn
Ask UTxOs have two negotiation beacons: the "Ask" beacon and the loan asset beacon.

```bash
# Get the policy id for the negotiation beacons.
beaconPolicyId=$(cardano-loans beacon-name policy-id \
  --negotiation-beacons \
  --stdout) 

# Get the required "Ask" beacon name. It is just the hexidecimal encoding of "Ask".
askTokenName=$(cardano-loans beacon-name asset-name \
  --ask-beacon \
  --stdout)

# Get the required loan asset beacon name.
assetTokenName=$(cardano-loans beacon-name asset-name \
  --loan-asset 'lovelace' \
  --stdout)

# Create the required full beacon names.
askBeacon="${beaconPolicyId}.${askTokenName}"
assetBeacon="${beaconPolicyId}.${assetTokenName}"
```

#### Create the required negotiation beacon script redeemer
```bash
cardano-loans redeemers negotiation-script manage-asks \
  --borrower-staking-pubkey-hash $borrowerStakePubKeyHash \
  --out-file negotiation_redeemer.json
```

If you are using a staking script for the borrower's address, use `borrower-staking-script-hash`
instead.

#### Create the required spending script redeemer
```bash
cardano-loans redeemers loan-script manage-ask \
  --out-file spending_redeemer.json
```

#### Building the transaction
To see how to build the transaction using a local node, refer
[here](scripts/local-node/close-ask.sh).

## Creating an Offer UTxO

Creating an Offer UTxO involves the following steps:
1. Calculate the hash of the staking credential used for your Lender ID.
2. Calculate the required beacon names to mint.
3. Create the required beacon script redeemer.
4. Create the required OfferDatum for each Offer UTxO you wish to create.
5. Submit a transaction that creates the Offer UTxOs.

*Offer UTxOs do not need to agree with the Ask UTxO you are responding to!* You can make a
counter-offer that can be immediately accepted by the borrower.

#### Calculate the hash of the staking credential used in the Lender ID
```bash
lenderCredentialHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file lender_stake.vkey)
```

If you are using a staking script credential, you can create the credential hash with this command:
```bash
lenderCredentialHash=$(cardano-cli transaciton policyid \
  --script-file stake_script.plutus)
```

While the above command is technically meant for minting policies, it works for generating the hash
of any script.

#### Calculate the required beacon names to mint
Offer UTxOs require three negotiation beacons: the "Offer" beacon, the Lender ID, and the loan 
asset beacon.

```bash
# Get the policy id for the negotiation beacons.
beaconPolicyId=$(cardano-loans beacon-name policy-id \
  --negotiation-beacons \
  --stdout) 

# Get the required "Offer" beacon name. It is just the hexidecimal encoding of "Offer".
offerTokenName=$(cardano-loans beacon-name asset-name \
  --offer-beacon \
  --stdout)

# Get the required loan asset beacon name.
assetTokenName=$(cardano-loans beacon-name asset-name \
  --loan-asset 'lovelace' \
  --stdout)

# Create the required full beacon names.
offerBeacon="${beaconPolicyId}.${offerTokenName}"
assetBeacon="${beaconPolicyId}.${assetTokenName}"
lenderIdBeacon="${beaconPolicyId}.${lenderCredentialHash}"
```

The above beacons are for an Offer UTxO where ada would be the loan asset. If you would like to
offer a native asset instead, replace `'lovelace'` with `'<policy_id>.<asset_name>'`. For
example:

```bash
assetTokenName=$(cardano-loans beacon-name asset-name \
  --loan-asset 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a' \
  --stdout)
```

#### Create the required negotiation beacon script redeemer
```bash
cardano-loans redeemers negotiation-script manage-offers \
  --lender-staking-pubkey-hash $lenderCredentialHash \
  --out-file negotiation_redeemer.json
```

If you are using a staking script for the Lender ID, use `lender-staking-script-hash`
instead.

#### Creating the required OfferDatum
```bash
cardano-loans datums offer \
  --lender-staking-pubkey-hash $lenderCredentialHash \
  --loan-asset 'lovelace' \
  --principal 100000000 \
  --loan-term '3600 slots' \
  --interest '0.1' \
  --compounding-interest \
  --epoch-duration '1200 slots' \
  --minimum-payment 2000000 \
  --fixed-penalty 500000 \
  --collateral-asset 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a' \
  --relative-rate '1 / 1000000' \
  --collateral-asset 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31' \
  --relative-rate '2 / 1000000' \
  --claim-period '3600 slots' \
  --offer-deposit 4000000 \
  --offer-expiration 1712752992000 \
  --payment-address lender_personal.addr \
  --out-file offer_datum.json
```

The `lender-staking-pubkey-hash` is the hash of your staking pubkey you wish to use as your
Lender ID. If you wish to use a staking script instead, you can use `lender-staking-script-hash`.
The negotiation script will check that you correctly identify whether your credential hash is for
a pubkey or a script.

The `loan-term` can either be in terms of slots (like above) or in terms of days. If you would like to
specify days, you can use `--loan-term '2 days'`.

The `interest` can be expressed as either a decimal (like above) or a fraction: `--interest '1 /
10'`. If you use decimals, the ultimate fraction will look off since floating point numbers do not
have arbitrary precision. The approximation is good enough, though.

The `compounding-interest` flag is optional. If used, the interest will be compounding. If not used,
the interest will be non-compounding. If the interest is 0, this flag is disregarded since it is an
interest-free loan.

The `epoch-duration` is optional, but if you want to incentivize periodic payments, this field must
be set. This field splits the loan into epochs of the specified length. The duration can be
specified in either slots or days. The interest/penalties must be applied at the end of each loan
epoch. The borrower has this time period to satisfy the minimum payment in order to avoid the
penalty.

The `minimum-payment` field specifies how much a borrower needs to pay back in a given loan epoch.
*This minimum does not need to be met in a single transaction.* As long as the total amount paid
back in the loan epoch is greater than the `minimum-payment`, the penalty can be avoided. This field
can only be a flat amount (no percents).

The penalty field can either be: `--no-penalty`, `--fixed-penalty INT`, `--percent-penalty PERCENT`.
The `PERCENT` can be specified as either a decimal or a fraction. This penalty will be applied
whenever the `minimum-payment` is missed in a given loan epoch. *It is possible to enforce a
penalty on interest-free and non-compounding interest loans.*

The `collateral-asset` fields *must* be in lexicographical order. If you reverse the order of the two
collateral assets above, the negotiation beacon script will crash with an error saying your
collateral assets are wrong. The `relative-rate` fields must be paired with the corresponding 
`collateral-asset` field. The `relative-rate` can be specified as either a decimal or a fraction,
just like with the `interest` field. If you do not want a specific collateral to be used, you can
either omit that collateral from the OfferDatum or you can set its `relative-rate` to zero.

The `claim-period` is how long you will have to claim expired collateral after the borrower defaults
on the loan. Once this time period passes, you will still be able to claim the collateral, but so
will the borrower. It will be a race to get the collateral. This is required to prevent permanent
locking of assets in the event the Key NFT is lost.

The `offer-deposit` is how much ada you used to satisfy the minUTxOValue of this Offer UTxO. It will
be returned to you when the borrower accepts the offer.

The `offer-expiration` is optional. If you set it, the borrower will only have until the specified
POSIX time (in milliseconds) to accept your offer. You can use the `cardano-loans convert-time`
command to convert between slot numbers and POSIX time.

The `payment-address` must either be a payment pubkey address or a proxy script address with a valid
staking credential. When the offer is accepted, the `offer-deposit` and the new Key NFT will be
deposited to this address.

#### Building the transaction
To see how to build the transaction using a local node, refer
[here](scripts/local-node/create-offer.sh).

## Closing an Offer UTxO

Closing an Offer UTxO requires the following steps:
1. Calculate the hash of the lender's staking credential.
2. Create the required spending script redeemer.
3. Calculate the required negotiation beacon names to burn.
4. Create the required negotiation beacon script redeemer.
5. Submit the transaction.

#### Calculate the hash of the staking credential used in the Lender ID
```bash
lenderCredentialHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file lender_stake.vkey)
```

If you are using a staking script credential, you can create the credential hash with this command:
```bash
lenderCredentialHash=$(cardano-cli transaciton policyid \
  --script-file stake_script.plutus)
```

While the above command is technically meant for minting policies, it works for generating the hash
of any script.

#### Calculate the required beacon names to burn
Offer UTxOs have three negotiation beacons: the "Offer" beacon, the Lender ID, and the loan 
asset beacon.

```bash
# Get the policy id for the negotiation beacons.
beaconPolicyId=$(cardano-loans beacon-name policy-id \
  --negotiation-beacons \
  --stdout) 

# Get the required "Offer" beacon name. It is just the hexidecimal encoding of "Offer".
offerTokenName=$(cardano-loans beacon-name asset-name \
  --offer-beacon \
  --stdout)

# Get the required loan asset beacon name.
assetTokenName=$(cardano-loans beacon-name asset-name \
  --loan-asset 'lovelace' \
  --stdout)

# Create the required full beacon names.
offerBeacon="${beaconPolicyId}.${offerTokenName}"
assetBeacon="${beaconPolicyId}.${assetTokenName}"
lenderIdBeacon="${beaconPolicyId}.${lenderCredentialHash}"
```

#### Create the required negotiation beacon script redeemer
```bash
cardano-loans redeemers negotiation-script manage-offers \
  --lender-staking-pubkey-hash $lenderCredentialHash \
  --out-file negotiation_redeemer.json
```

If you are using a staking script for the Lender ID, use `lender-staking-script-hash`
instead.

#### Create the required spending script redeemer
```bash
cardano-loans redeemers loan-script manage-offer \
  --out-file spending_redeemer.json
```

#### Building the transaction
To see how to build the transaction using a local node, refer
[here](scripts/local-node/close-offer.sh).

## Accepting an Offer

*You will also need to close an Ask UTxO using the `AcceptOffer` redeemer.* You need to close one
Ask UTxO for every Offer UTxO accepted. The Ask UTxO and Offer UTxO do not need to have the same
terms; they do not even need to use the same loan assets. If they use different loan assets, make
sure you burn the loan asset beacon from both the Ask UTxO and the Offer UTxO.

Accepting an Offer requires the following steps:
1. Get the current slot time and convert it to POSIX time.
2. Calculate the hash of the borrower's staking credential.
3. Create the required spending script redeemer.
4. Create the new ActiveDatum for each offer accepted.
5. Calculate the required negotiation beacon names to burn.
6. Calculate the required active beacon names to mint.
7. Create the required negotiation beacon script redeemer.
8. Create the required active beacon script redeemer.
9. Create the required datum for the output to the lender.
10. Submit the transaction.

#### Get the latest slot number.
```bash
startSlot=$(cardano-loans query current-slot \
  --testnet)
```

#### Convert the slot number to the required posix time.
```bash
startTime=$(cardano-loans convert-time \
  --slot $startSlot \
  --testnet)
```

#### Calculate the hash of the staking credential used in the borrower address
```bash
borrowerStakePubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file borrower_stake.vkey)
```

If you are using a staking script credential, you can create the credential hash with this command:
```bash
borrowerStakeScriptHash=$(cardano-cli transaciton policyid \
  --script-file stake_script.plutus)
```

While the above command is technically meant for minting policies, it works for generating the hash
of any script.

#### Calculate the required beacon names to burn
You must burn all negotiation beacons attached to the Ask and Offer UTxOs. If the UTxOs are using
different loan assets, make sure to burn both loan asset beacons.

```bash
# Get the policy id for the negotiation beacons.
negotiationBeaconPolicyId=$(cardano-loans beacon-name policy-id \
  --negotiation-beacons \
  --stdout) 

# Get the required "Ask" beacon name. It is just the hexidecimal encoding of "Ask".
askTokenName=$(cardano-loans beacon-name asset-name \
  --ask-beacon \
  --stdout)

# Get the required "Offer" beacon name. It is just the hexidecimal encoding of "Offer".
offerTokenName=$(cardano-loans beacon-name asset-name \
  --offer-beacon \
  --stdout)

# Get the required loan asset beacon name.
assetTokenName=$(cardano-loans beacon-name asset-name \
  --loan-asset 'lovelace' \
  --stdout)

# Create the required full beacon names.
askBeacon="${negotiationBeaconPolicyId}.${askTokenName}"
negotiationAssetBeacon="${negotiationBeaconPolicyId}.${assetTokenName}"
offerBeacon="${negotiationBegotiationPolicyId}.${offerTokenName}"
```

#### Calculate the required beacon names to mint
Active UTxOs require four beacons: an "Active" beacon, a loan asset beacon, a Borrower ID, and a
Loan ID. The loan asset beacon will have the same token name as the loan asset beacon attached to
the Offer being accepted; it will just have a different policy id.

```bash
# Get the policy id for the active beacons.
activeBeaconPolicyId=$(cardano-loans beacon-name policy-id \
  --active-beacons \
  --stdout) 

# Get the required "Active" beacon name. It is just the hexidecimal encoding of "Active".
activeTokenName=$(cardano-loans beacon-name asset-name \
  --active-beacon \
  --stdout)

# Get the Loan ID name from the target Offer UTxO's output reference.
loanIdTokenName=$(cardano-loans beacon-name asset-name \
  --offer-id '667c6a5bf9eb86f97ab5f2f2dbd66a484d32795cd244397dfec7953ee8cb2ff3#0' \
  --stdout)

# Create the required full beacon names.
activeBeacon="${activeBeaconPolicyId}.${activeTokenName}"
activeAssetBeacon="${activeBeaconPolicyId}.${assetTokenName}"
borrowerId="${activeBeaconPolicyId}.${borrowerStakePubKeyHash}"
loanId="${activeBeaconPolicyId}.${loanIdTokenName}"
```

#### Creating the required ActiveDatum
There are two ways to create the required ActiveDatum: manually or automatically.

You can create the ActiveDatum automatically with this command:
```bash
cardano-loans datums active new auto \
  --testnet \
  --borrower-staking-pubkey-hash $borrowerStakePubKeyHash \
  --offer-id '667c6a5bf9eb86f97ab5f2f2dbd66a484d32795cd244397dfec7953ee8cb2ff3#0' \
  --start-time $startTime \
  --out-file active_datum.json
```

This command will query Koios to find that Offer UTxO. It will create the new ActiveDatum based on
the Offer UTxO's OfferDatum.

If you would like to create the ActiveDatum manually (without requiring internet), you can do so 
with this command:
```bash
cardano-loans datums active new manual \
  --payment-address addr_test1vzhq6qq52k59tekqp7v04yrpq284cqxjj7fx8qau2qd795s7wfhhm \
  --loan-asset 'lovelace' \
  --principal 10000000 \
  --loan-term '3600 slots' \
  --interest '3602879701896397 / 36028797018963968' \
  --compounding-interest \
  --epoch-duration '1200 slots' \
  --minimum-payment 2000000 \
  --fixed-penalty 500000 \
  --collateral-asset 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a' \
  --relative-rate '1 / 1000000' \
  --collateral-asset 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31' \
  --relative-rate '1 / 500000' \
  --claim-period '3600 slots' \
  --borrower-staking-pubkey-hash $borrowerStakePubKeyHash \
  --offer-id '667c6a5bf9eb86f97ab5f2f2dbd66a484d32795cd244397dfec7953ee8cb2ff3#0' \
  --start-time $startTime \
  --out-file active_datum.json
```

*All fields must match the OfferDatum exactly*. While the fields that accept fractions can also
accept decimals, it is more accurate to use fractions directly. Converting decimals to fractions is
subject to the imprecision of floating-point numbers.

#### Create the required redeemers.
```bash
cardano-loans redeemers negotiation-script burn-all \
  --out-file negotiation_redeemer.json

cardano-loans redeemers active-script accept-offers \
  --out-file active_redeemer.json

cardano-loans redeemers loan-script accept-offer \
  --out-file accept_offer.json
```

#### Create the payment datum.
```bash
cardano-loans datums payment \
  --loan-id $loanIdTokenName \
  --out-file lender_datum.json
```

#### Building the transaction
You will need to set invalid-before of this transaction to the `$startSlot` variable.

When accepting mulitple offers, make sure the required outputs are in the same order as the offer
inputs!

To see how to build the transaction using a local node, refer
[here](scripts/local-node/accept-offer.sh).

## Making a Loan Payment

Making a loan payment requires:
1. Calculate the invalid-hereafter slot number based on the next deadline (either epoch boundary or
   expiration, whichever is sooner).
1. Calculate the hash of the borrower's staking credential.
2. Create the required spending script redeemer.
3. Create the post-payment ActiveDatum for each payment made.
4. If making a full payment, calculate the required active beacon names to burn.
5. Create the required active beacon script redeemer.
3. Create the required observer script redeemer.
6. Create the required datum for the output to the lender.
7. Create the required staking address for the payment observer script.
8. Submit the transaction.

#### Convert the next deadline (in POSIX time) to a slot number
```bash
deadlineSlot=$(cardano-loans convert-time \
  --posix-time 1712768154000 \
  --testnet)
```

You need to prove to the script that the next deadline has not passed yet so you need to set the
invalid-hereafter of the transaction to the next deadline. The next deadline is either the next
epoch boundary or the loan's expiration, whichever is first. When making payments on multiple
loans, the invalid-hereafter should be set to the earliest time.

#### Calculate the hash of the staking credential used in the borrower address
```bash
borrowerStakePubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file borrower_stake.vkey)
```

If you are using a staking script credential, you can create the credential hash with this command:
```bash
borrowerStakeScriptHash=$(cardano-cli transaciton policyid \
  --script-file stake_script.plutus)
```

While the above command is technically meant for minting policies, it works for generating the hash
of any script.

#### If making a full payment, get the required beacon names to burn
All active beacons attached to the input must be burned.

```bash
# Get the policy id for the active beacons.
activeBeaconPolicyId=$(cardano-loans beacon-name policy-id \
  --active-beacons \
  --stdout) 

# Get the required "Active" beacon name. It is just the hexidecimal encoding of "Active".
activeTokenName=$(cardano-loans beacon-name asset-name \
  --active-beacon \
  --stdout)

# Get the Loan ID name from the input's ActiveDatum.
loanIdTokenName='0f7deb6eca31425e357b1a7a9284f0e60782f5b2a36c80c5ef4b89bcbc4b5ced'

# Create the required full beacon names.
activeBeacon="${activeBeaconPolicyId}.${activeTokenName}"
activeAssetBeacon="${activeBeaconPolicyId}.${assetTokenName}"
borrowerId="${activeBeaconPolicyId}.${borrowerStakePubKeyHash}"
loanId="${activeBeaconPolicyId}.${loanIdTokenName}"
```

#### Create the payment observer's staking address
This address is needed to execute the script as a staking script.

```bash
cardano-loans scripts \
  --payment-script \
  --out-file payment_observer.plutus

observerAddress=$(cardano-cli stake-address build \
  --testnet-magic 1 \
  --stake-script-file payment_observer.plutus)
```

#### Creating the required ActiveDatum
There are two ways to create the required ActiveDatum: manually or automatically.

You can create the ActiveDatum automatically with this command:
```bash
cardano-loans datums active post-payment auto \
  --testnet \
  --loan-ref '234f86d19e550469b654fb9ee9e1cc94c19a481a16192df015a362125697e812#0' \
  --payment-amount 2000000 \
  --out-file post_payment_active_datum.json
```

This command will query Koios to find that Active UTxO. It will create the new ActiveDatum based on
the current Active UTxO's ActiveDatum.

If you would like to create the ActiveDatum manually (without requiring internet), you can do so 
with this command:
```bash
cardano-loans datums active post-payment manual \
  --payment-address addr_test1vzhq6qq52k59tekqp7v04yrpq284cqxjj7fx8qau2qd795s7wfhhm \
  --loan-asset 'lovelace' \
  --principal 10000000 \
  --loan-term '3600 slots' \
  --interest '3602879701896397 / 36028797018963968' \
  --compounding-interest \
  --epoch-duration '1200 slots' \
  --minimum-payment 2000000 \
  --fixed-penalty 500000 \
  --collateral-asset 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a' \
  --relative-rate '1 / 1000000' \
  --collateral-asset 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31' \
  --relative-rate '1 / 500000' \
  --claim-expiration '1712756592000' \
  --loan-expiration '1712752992000' \
  --last-epoch-boundary '1712749392000' \
  --total-epoch-payments 0 \
  --outstanding-balance '3096224743817216015625 / 281474976710656' \
  --borrower-staking-pubkey-hash $borrowerStakePubKeyHash \
  --loan-id '0f7deb6eca31425e357b1a7a9284f0e60782f5b2a36c80c5ef4b89bcbc4b5ced' \
  --payment-amount 2000000 \
  --out-file post_payment_active_datum.json
```

*All fields must match the current ActiveDatum exactly*. While the fields that accept fractions can
also accept decimals, it is more accurate to use fractions directly. Converting decimals to
fractions is subject to the imprecision of floating-point numbers.

#### Create the required redeemers.
```bash
cardano-loans redeemers payment-script observe-payment \
  --out-file observer_payment.json

# If you are making a full payment, you will also need this redeemer to burn the Borrower IDs.
cardano-loans redeemers active-script accept-offers \
  --out-file active_redeemer.json

cardano-loans redeemers loan-script make-payment \
  --payment-amount 2000000 \
  --out-file make_payment.json
```

The `payment-amount` field must exactly match the corresponding field in the datums command *and*
the amount actually paid to the lender.

#### Create the payment datum.
```bash
cardano-loans datums payment \
  --loan-id '0f7deb6eca31425e357b1a7a9284f0e60782f5b2a36c80c5ef4b89bcbc4b5ced' \
  --out-file lender_datum.json
```

#### Building the transaction
You need to set the invalid-hereafter of this transaction to the `$deadlineSlot` variable.

When making payments on multiple loans, make sure the required outputs are in the same order as the
payment inputs!

To see how to build the transaction using a local node, refer
[here](scripts/local-node/make-payment.sh). There is an example `cardano-cli transaction build` for
both full payments and partial payments.

## Applying Interest and/or Penalties

Applying interest to a loan requires:
1. Calculate the invalid-hereafter slot number based on the loan's expiration.
1. Calculate the hash of the borrower's staking credential.
2. Create the required spending script redeemer.
3. Create the post-interest ActiveDatum for each payment made.
3. Create the required observer script redeemer.
7. Create the required staking address for the interest observer script.
8. Submit the transaction.

> [!IMPORTANT] 
> This step is required even if a loan is interest-free or non-compounding. These periodic rollovers
> are required in order for the protocol to check if any penalties are required, and apply them when
> necessary. The only time this step is *not* required is when the loan is **both** interest-free
> and has no penalty.

#### Convert the next deadline (in POSIX time) to a slot number
```bash
deadlineSlot=$(cardano-loans convert-time \
  --posix-time 1712768154000 \
  --testnet)
```

You need to prove to the script that the loan's expiration has not passed yet so you need to set the
invalid-hereafter of the transaction to the loan expiration time. When applying interest to multiple
loans, the invalid-hereafter should be set to the earliest time.

#### Calculate the hash of the staking credential used in the borrower address
```bash
borrowerStakePubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file borrower_stake.vkey)
```

If you are using a staking script credential, you can create the credential hash with this command:
```bash
borrowerStakeScriptHash=$(cardano-cli transaciton policyid \
  --script-file stake_script.plutus)
```

While the above command is technically meant for minting policies, it works for generating the hash
of any script.

#### Create the interest observer's staking address
This address is needed to execute the script as a staking script.

```bash
cardano-loans scripts \
  --interest-script \
  --out-file payment_observer.plutus

observerAddress=$(cardano-cli stake-address build \
  --testnet-magic 1 \
  --stake-script-file interest_observer.plutus)
```

#### Creating the required ActiveDatum
There are two ways to create the required ActiveDatum: manually or automatically.

You can create the ActiveDatum automatically with this command:
```bash
cardano-loans datums active post-interest auto \
  --testnet \
  --loan-ref '234f86d19e550469b654fb9ee9e1cc94c19a481a16192df015a362125697e812#0' \
  --times-applied 1 \
  --out-file $activeDatumFile
  --out-file post_interest_active_datum.json
```

This command will query Koios to find that Active UTxO. It will create the new ActiveDatum off of
the current Active UTxO's ActiveDatum.

If you would like to create the ActiveDatum manually (without requiring internet), you can do so 
with this command:
```bash
cardano-loans datums active post-interest manual \
  --payment-address addr_test1vzhq6qq52k59tekqp7v04yrpq284cqxjj7fx8qau2qd795s7wfhhm \
  --loan-asset 'lovelace' \
  --principal 10000000 \
  --loan-term '3600 slots' \
  --interest '3602879701896397 / 36028797018963968' \
  --compounding-interest \
  --epoch-duration '1200 slots' \
  --minimum-payment 2000000 \
  --fixed-penalty 500000 \
  --collateral-asset 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a' \
  --relative-rate '1 / 1000000' \
  --collateral-asset 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31' \
  --relative-rate '1 / 500000' \
  --claim-expiration '1712756592000' \
  --loan-expiration '1712752992000' \
  --last-epoch-boundary '1712749392000' \
  --total-epoch-payments 0 \
  --outstanding-balance '3096224743817216015625 / 281474976710656' \
  --borrower-staking-pubkey-hash $borrowerStakePubKeyHash \
  --loan-id '0f7deb6eca31425e357b1a7a9284f0e60782f5b2a36c80c5ef4b89bcbc4b5ced' \
  --times-applied 1 \
  --out-file post_interest_active_datum.json
```

*All fields must match the current ActiveDatum exactly*. While the fields that accept fractions can
also accept decimals, it is more accurate to use fractions directly. Converting decimals to
fractions is subject to the imprecision of floating-point numbers.

#### Create the required redeemers.
```bash
cardano-loans redeemers interest-script observe-interest \
  --out-file observer_interest.json

cardano-loans redeemers loan-script apply-interest \
  --deposit-increase 0 \
  --times-applied 1 \
  --out-file make_payment.json
```

If you need to increase the amount of ada stored in the Active UTxO, you must specify the increase
with the `deposit-increase` field.

The `times-aplied` field tells the script how many times to apply the interest. If you need to apply
the interest several times to catch up to the current loan epoch, you can use this field to
do them all in one transaction.

#### Building the transaction
You need to set the invalid-hereafter of this transaction to the `$deadlineSlot` variable.

When applying interest to multiple loans, make sure the required outputs are in the same order as
the inputs!

To see how to build the transaction using a local node, refer
[here](scripts/local-node/apply-interest.sh). 

## Updating a Lender Address

Updating the lender address requires the following steps:
1. Calculate the invalid-hereafter slot number based on the loan's expiration.
1. The Key NFT must be sent to the new address in this transaction. If updating multiple UTxOs, each
   Key NFT must get their own output.
3. Create the required spending script redeemer.
3. Create the required observer script redeemer.
3. Create the post-payment ActiveDatum for each update made.
6. Create the required datum for the output to the lender.
7. Create the required staking address for the address update observer script.
8. Submit the transaction.

#### Convert the loan's expiration (in POSIX time) to a slot number
```bash
deadlineSlot=$(cardano-loans convert-time \
  --posix-time 1712768154000 \
  --testnet)
```

You need to prove to the script that the loan has not expired so you need to set the
invalid-hereafter of the transaction to the expiration time. When updating multiple
loans, the invalid-hereafter should be set to the earliest time.

#### Create the address update observer's staking address
This address is needed to execute the script as a staking script.

```bash
cardano-loans scripts \
  --address-update-script \
  --out-file address_update_observer.plutus

observerAddress=$(cardano-cli stake-address build \
  --testnet-magic 1 \
  --stake-script-file address_update_observer.plutus)
```

#### Creating the required ActiveDatum
There are two ways to create the required ActiveDatum: manually or automatically.

You can create the ActiveDatum automatically with this command:
```bash
cardano-loans datums active post-address-update auto \
  --testnet \
  --loan-ref '234f86d19e550469b654fb9ee9e1cc94c19a481a16192df015a362125697e812#0' \
  --payment-address addr_test1vpz6g5ecxv6mc036lckg6w06wmj7vr073j73llzpsn5t0pguw7m5u \
  --out-file post_update_active_datum.json
```

This command will query Koios to find that Active UTxO. It will create the new ActiveDatum off of
the current Active UTxO's ActiveDatum.

If you would like to create the ActiveDatum manually (without requiring internet), you can do so 
with this command:
```bash
cardano-loans datums active post-payment manual \
  --payment-address addr_test1vpz6g5ecxv6mc036lckg6w06wmj7vr073j73llzpsn5t0pguw7m5u \
  --loan-asset 'lovelace' \
  --principal 10000000 \
  --loan-term '3600 slots' \
  --interest '3602879701896397 / 36028797018963968' \
  --compounding-interest \
  --epoch-duration '1200 slots' \
  --minimum-payment 2000000 \
  --fixed-penalty 500000 \
  --collateral-asset 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a' \
  --relative-rate '1 / 1000000' \
  --collateral-asset 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31' \
  --relative-rate '1 / 500000' \
  --claim-expiration '1712756592000' \
  --loan-expiration '1712752992000' \
  --last-epoch-boundary '1712749392000' \
  --total-epoch-payments 0 \
  --outstanding-balance '3096224743817216015625 / 281474976710656' \
  --borrower-staking-pubkey-hash '3cefec09a27b6894e2ed9a78b9cc01f083973d7c0afb8cec8bda33fa' \
  --loan-id '0f7deb6eca31425e357b1a7a9284f0e60782f5b2a36c80c5ef4b89bcbc4b5ced' \
  --out-file post_payment_active_datum.json
```

*All fields must match the current ActiveDatum exactly*. While the fields that accept fractions can
also accept decimals, it is more accurate to use fractions directly. Converting decimals to
fractions is subject to the imprecision of floating-point numbers.

#### Create the required redeemers.
```bash
cardano-loans redeemers address-update-script observe-address-update \
  --out-file observer_payment.json

cardano-loans redeemers loan-script update-lender-address \
  --deposit-increase 0 \
  --payment-address addr_test1vpz6g5ecxv6mc036lckg6w06wmj7vr073j73llzpsn5t0pguw7m5u \
  --out-file make_payment.json
```

If you need to increase the amount of ada stored in the Active UTxO, you must specify the increase
with the `deposit-increase` field.

The `payment-adress` field must exactly match the corresponding field in the datums command *and*
the destination of the corresponding Key NFT.

#### Create the payment datum.
```bash
cardano-loans datums payment \
  --loan-id '0f7deb6eca31425e357b1a7a9284f0e60782f5b2a36c80c5ef4b89bcbc4b5ced' \
  --out-file key_datum.json
```

The Key NFT must be stored with this datum.

#### Building the transaction
You need to set the invalid-hereafter of this transaction to the `$deadlineSlot` variable.

When updating the outputs for mulitple loans, make sure the required outputs are in the same order
as the associated inputs!

To see how to build the transaction using a local node, refer
[here](scripts/local-node/update-lender-address.sh). 

## Claiming Expired Collateral

Claiming expired collateral requires the following steps:
1. Calculate the invalid-before slot number based on the current chain tip.
3. Create the required spending script redeemer.
3. Create the required active beacon script redeemer.
3. Create the required beacon names to burn.
8. Submit the transaction.

#### Get the current chain tip.
```bash
tipSlot=$(cardano-loans query current-slot --testnet)
```

The invalid-before of the transaction must be at least the loan expiration time + one slot.

#### Create the required redeemers.
```bash
cardano-loans redeemers active-script claim-expired \
  --out-file burn_and_claim_expired.json

cardano-loans redeemers loan-script claim-expired-collateral \
  --out-file spend_with_key.json
```

#### Get the required beacon names.
```bash
activePolicyId=$(cardano-loans beacon-name policy-id \
  --active-beacons \
  --stdout) 

activeTokenName=$(cardano-loans beacon-name asset-name \
  --active-beacon \
  --stdout)
assetTokenName=$(cardano-loans beacon-name asset-name \
  --loan-asset 'lovelace' \
  --stdout)

# These names come from the current ActiveDatum.
loanIdTokenName='a07d02e5a58f6714075dad0476d7a627e12d93ce54e05f0c5ed26099a239e532'
borrowerIdTokenName='3cefec09a27b6894e2ed9a78b9cc01f083973d7c0afb8cec8bda33fa'

activeBeacon="${activePolicyId}.${activeTokenName}"
activeAssetBeacon="${activePolicyId}.${assetTokenName}"
borrowerId="${activePolicyId}.${borrowerIdTokenName}"
loanId="${activePolicyId}.${loanIdTokenName}"
```

#### Building the transaction
You need to set the invalid-before flag to the `$tipSlot` variable.

To see how to build the transaction using a local node, refer
[here](scripts/local-node/claim-expired.sh). 

## Unlocking Lost Collateral

Unlocking lost collateral requires the following steps:
1. Calculate the invalid-before slot number based on the current chain tip.
1. Calculate the borrower's staking credential hash.
3. Create the required spending script redeemer.
3. Create the required active beacon script redeemer.
3. Create the required beacon names to burn.
8. Submit the transaction.

#### Get the current chain tip.
```bash
tipSlot=$(cardano-loans query current-slot --testnet)
```

The invalid-before of the transaction must be at least the claim expiration time + one slot.

#### Calculate the hash of the staking credential used in the borrower address
```bash
borrowerStakePubKeyHash=$(cardano-cli stake-address key-hash \
  --stake-verification-key-file borrower_stake.vkey)
```

If you are using a staking script credential, you can create the credential hash with this command:
```bash
borrowerStakeScriptHash=$(cardano-cli transaciton policyid \
  --script-file stake_script.plutus)
```

While the above command is technically meant for minting policies, it works for generating the hash
of any script.

#### Create the required redeemers.
```bash
cardano-loans redeemers active-script unlock \
  --out-file burn_remainder_and_unlock.json

cardano-loans redeemers loan-script unlock-active \
  --out-file unlock.json
```

#### Get the required beacon names.
```bash
activePolicyId=$(cardano-loans beacon-name policy-id \
  --active-beacons \
  --stdout) 

activeTokenName=$(cardano-loans beacon-name asset-name \
  --active-beacon \
  --stdout)
assetTokenName=$(cardano-loans beacon-name asset-name \
  --loan-asset 'lovelace' \
  --stdout)

# This name comes from the curennt ActiveDatum.
loanIdTokenName='a07d02e5a58f6714075dad0476d7a627e12d93ce54e05f0c5ed26099a239e532'

activeBeacon="${activePolicyId}.${activeTokenName}"
activeAssetBeacon="${activePolicyId}.${assetTokenName}"
borrowerId="${activePolicyId}.${borrowerStakePubKeyHash}"
loanId="${activePolicyId}.${loanIdTokenName}"
```

#### Building the transaction
You need to set the invalid-before flag to the `$tipSlot` variable.

To see how to build the transaction using a local node, refer
[here](scripts/local-node/unlock-lost.sh). 

## Time Conversions
Since plutus scripts use POSIX time (in milliseconds) while cardano-cli uses slot numbers for the
transaction validity intervals, you need a way to convert between the two units.

``` Bash
cardano-loans convert-time --testnet --slot 26668590

cardano-loans convert-time --testnet --posix-time 1682351790000
```

## Queries

- All queries use Koios.
- All query commands are capable of saving results to a file or printing to stdout. 
- Results can be formatted as JSON, pretty, or plain. 

The pretty and plain formats are meant for printing to the stdout, but both can also be saved to a
file. The only difference between the pretty format and the plain format is the pretty format uses
ansii escape sequences to highlight certain items with color. The plain format is there as a
fallback in case the ansii escape sequences are causing issues for a user.

The JSON response to stdout can be directly piped into `jq` for a more human-friendly format.

> Note: Currently, the `cardano-loans` CLI will only get the first 1000 UTxOs that satisfy a query.
> This could be 1000 personal UTxOs or 1000 loan UTxOs, depending on the query. For the beta
> release, 1000 should be plenty. The CLI will be expanded in the future to remove this cap.

### Querying Personal Addresses

In order to facilitate the use of remote nodes, `cardano-loans` is capable of querying personal
addresses.

The command is simply:
```bash
cardano-loans query personal-address \
  --testnet \
  --address $(cat personal.addr) \
  --pretty \
  --stdout
```

The UTxOs will be returned in lexicographical order based on the transaction hash.

For the pretty and plain formats, UTxOs that contain a reference script and/or a datum will show the
script hash or datum hash, respectively. For the pretty format, each type of hash will have a color
associated with it: Blue for script hashes and Green for datum hashes. UTxO assets are always shown.

There is an optional `--keys` flag. If used, it will only return UTxOs that contain Key NFTs for
Cardano-Loans. This could be helpful for managing these Key NFTs.

This query can also work on plutus script addresses.

### Querying Ask UTxOs

All possible queries for Ask UTxOs are organized under the `cardano-loans query asks` command.

The `loan-asset` field is optional. If you use it, the query will only return Ask UTxOs using that
asset as the loan asset.

The `collateral-asset` field is optional. If you use it, the query will only return Ask UTxOs using that
asset as one of the collateral assets. You can specify many collateral assets to only return Ask
UTxOs using all of the specified collateral assets.

The `borrower-staking-pubkey-hash` and `borrower-staking-script-hash` fields are optional. If you
use it, *only one needs to be specified*. When used, the query will only return Ask UTxOs for the
borrower address using that credential.

The three above fields can be combined to create more complicated queries. For example, this query
will only return Ask UTxOs located at the borrower address using the staking pubkey credential
`623a2b9a369454b382c131d7e3d12c4f93024022e5c5668cf0c5c25c`, that are asking for ada, and are
offering *both* `c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a`
`c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31` as collateral:

```bash
cardano-loans query asks \
  --testnet \
  --stdout \
  --pretty \
  --loan-asset 'lovelace' \
  --borrower-staking-pubkey-hash 623a2b9a369454b382c131d7e3d12c4f93024022e5c5668cf0c5c25c \
  --collateral-asset 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a' \
  --collateral-asset 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31'
```

Using `cardano-loans query asks` without any of the optional fields will return *all* Ask UTxOs.

### Querying Offer UTxOs

All possible queries for Offer UTxOs are organized under the `cardano-loans query offers` command.

The `loan-asset` field is optional. If you use it, the query will only return Offer UTxOs using that
asset as the loan asset.

The `collateral-asset` field is optional. If you use it, the query will only return Offer UTxOs
using that asset as one of the collateral assets. You can specify many collateral assets to only
return Offer UTxOs using all of the specified collateral assets.

The `borrower-staking-pubkey-hash` and `borrower-staking-script-hash` fields are optional. If you
use it, *only one needs to be specified*. When used, the query will only return Offer UTxOs made to
the borrower address using that credential.

The `lender-staking-pubkey-hash` and `lender-staking-script-hash` fields are optional. If you
use it, *only one needs to be specified*. When used, the query will only return Offer UTxOs that
belong to the specified lender staking credential.

The four above fields can be combined to create more complicated queries. For example, this query
will only return Offer UTxOs located at the borrower address using the staking pubkey credential
`623a2b9a369454b382c131d7e3d12c4f93024022e5c5668cf0c5c25c`, that are offering ada, are allowing
*both* `c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a`
`c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31` as collateral, and
belong to the lender using staking pubkey `3cefec09a27b6894e2ed9a78b9cc01f083973d7c0afb8cec8bda33fa`
as their Lender ID:

```bash
cardano-loans query offers \
  --testnet \
  --stdout \
  --pretty \
  --loan-asset 'lovelace' \
  --borrower-staking-pubkey-hash 623a2b9a369454b382c131d7e3d12c4f93024022e5c5668cf0c5c25c \
  --lender-staking-pubkey-hash 3cefec09a27b6894e2ed9a78b9cc01f083973d7c0afb8cec8bda33fa \
  --collateral-asset 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.4f74686572546f6b656e0a' \
  --collateral-asset 'c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d.54657374546f6b656e31'
```

Using `cardano-loans query offers` without any of the optional fields will return *all* Offer UTxOs.

### Querying Active UTxOs

All possible queries for Active UTxOs are organized under the `cardano-loans query actives` command.

The `loan-asset` field is optional. If you use it, the query will only return Offer UTxOs using that
asset as the loan asset.

The `borrower-staking-pubkey-hash` and `borrower-staking-script-hash` fields are optional. If you
use it, *only one needs to be specified*. When used, the query will only return Active UTxOs located
at the borrower address using that credential. This can be useful for checking if a borrower
currently has any open loans.

The `loan-id` field is optional. If you use it, the query will only return the Active UTxO with that
Loan ID. When this field is used, there can never be more than one result. It can return nothing if
there is no Active UTxO with that Loan ID.

The above fields can be combined to create more complicated queries. For example, this query
will only return Active UTxOs located at the borrower address using the staking pubkey credential
`623a2b9a369454b382c131d7e3d12c4f93024022e5c5668cf0c5c25c` that are for ada loans:

```bash
cardano-loans query actives \
  --testnet \
  --stdout \
  --pretty \
  --loan-asset 'lovelace' \
  --borrower-staking-pubkey-hash 623a2b9a369454b382c131d7e3d12c4f93024022e5c5668cf0c5c25c \
```

Using `cardano-loans query actives` without any of the optional fields will return *all* Active UTxOs.

### Querying the Current Time

Certain actions require knowing the current slot number. You can query the most recent slot number
using the `cardano-loans query current-slot` command.

### Querying a Borrower's Credit History

The borrower's credit history can be queried using the `cardano-loans query borrower-history`
command. The `borrower-id` field is the target borrower address' staking credential hash (whether
the hash is for a pubkey or a script is irrelevent).

The response will have all loans the borrower ever took out as well as whether they defaulted or
successfully repaid it. The loan information returned is either the state of the loan at the time of
the default or the borrower's final payment.

### Querying a Loan's History

The loan's event history can be queried using the `cardano-loans query loan-history`
command. The `loan-id` field is the target loan's Loan ID.

The response will have every action taken against that loan's Active UTxO. For example, you can see:
- What payments were made, when, and for how much?
- When was interest applied, and how many times?
- When was the lender's address updated?

The loan information attached to each event is the state *at the time of the event*; the information
does not show the results of the event.

This query can complement the borrower's credit history query.
