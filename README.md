# Cardano-Loans

:warning: Knowledge of basic Haskell syntax and cardano-cli usage is assumed.

---
## Table of Contents
- [Abstract](#abstract)
- [Motivation](#motivation)
- [Preliminary Discussion](#preliminary-discussion)
- [Specification](#specification)
- [Features Discussion](#features-discussion)
- [Future Considerations](#future-directions-and-considerations)
- [Conclusion](#conclusion)

## Abstract

Cardano-Loans is a proof-of-concept implementation of a *fully p2p* lending/borrowing protocol for
the Cardano Settlement Layer (CSL). It empowers users to create and operate a CSL-native credit/debt
market via trustlessly negotiable & repayable p2p loans, and on-chain credit history. This
circumvents the need for oracles and/or concentrated lending pools in favor of *endogenous* price &
interest-rate discovery. Users deploy (and interact with each others') script addresses, so full
spending *and* delegation control of assets is maintained, and contract upgrades occur
democratically.

## Motivation

A healthy credit-debt market is a vital (and often the largest) component of a thriving economy. A
healthy market is one in which prices reflect underlying reality as quickly and fluidly (with as
little friction) as possible. For the sake of avoiding a whole treatise on economics, suffice to say
that *the best way to achieve fast and frictionless price discovery is through the aggregation of
maximally expressive individual sentiments.* This is especially true for credit-debt markets, where
money itself is the asset, priced via interest rates. 

Many lending/borrowing dApps on Cardano are implemented in controlled manners that limit users'
flexibility in negotiating loan terms. This results in a sub-optimal expression of market sentiment.
Furthermore, existing protocols rely on oracles for price feeds, which complicates the trust model,
increases the dApp's attack surface, and subjects prices to a broader economy in which interest
rates are not set by the market, but by central actors. 

Protocols that offer an alternative to this status quo will likely be perceived as a threat by those
in positions of high power, so censorship resistance is an essential feature. Fully p2p dApps, such
as Cardano-Loans, offer the highest level of censorship-resistance of any dApp architecture, because
users not only have full custody of their assets, but can also fractionalize and recompose their
interactions across an already decentralized ledger, instead of pooling their assets into one or a
few contracts.

Since users do not have to pool assets together, both delegation control and voting control is
maintained by all users. This is in stark contrast to any dApp that is using liquidity pools - users
that want to participate must either give up control entirely or severly limit their choices. Since
both Cardano's consensus and governance models depend on users being able to fully express their
sentiments, a DeFi ecosystem built on liquidity pools gets less secure as adoption grows. 

Finally, p2p lending dApps may be the fastest way for the most financially underserved peoples to
begin building credit scores. Those who *need* atypical financing options are in the best position
to begin bootstrapping the system, as they would be willing to pay higher interest rates than any
other group. This in turn incentivizes yield-seeking lenders to develop new strategies for such
unprecedented conditions. In time, borrowers and lenders can build p2p relationships, and begin
dis-intermediating the banking system.


## Preliminary Discussion

To appreciate the necessity for new lending/borrowing protocols on Cardano, it is first important to
understand the deficiencies of the status quo:

### Current Lending/Borrowing dApp Deficiencies

1. **Oracles** - dApps that rely on off-chain information feeds are subject to the integrity of the
underlying oracle network. Oracles are a nuanced topic that is beyond the scope of this document,
but in short, oracles increase the attack surface of dApps, and have a long way to go before they
can be safely relied upon by ledger-wide distributed dApps.

2. **Non-negotiable Loan Terms** - lending/borrowing dApps that are mediated by a central
contract/entity lack negotiability of all loan terms, resulting in inefficient markets. This is
especially troubling for a credit-debt market, whose efficiency is a vital component to a healthy
economy.
   
3. **Concentrated Lending Pools** - concentrated lending pools (or any concentrated dApp design) are
a higher security risk compared to distributed dApps. Aside from catastrophic draining attacks,
centralized dApps often have a complex security model that relies on additional entities like DAOs
(and associated "dApp Tokens") for upgradeability. This not only increases the technical attack
surface, but the social attack surface as well. Furthermore, concentrated dApps do not scale
naturally compared to distributed dApp architectures, especially in the context of state channels,
like Hydra. Furthermore, since Cardano's security is based on users maintaining full delegation
and voting control of their assets, concentrated lending pools directly undermine the security
of Cardano.
   
4. **Inefficient Markets** - concentrated lending pools set interest rates formulaically, rather
than emergently. Although the formula may be transparent, a healthy economy *continuously
discovers* prices and rates via real-time aggregation of individual interactions, not by formulas.
There is no single formula that can optimally capture market sentiment. Even if there was, agreeing
on it would be a great challenge in and of itself. 

### The Cardano-Loans Protocol

Cardano-Loans gives users the great power (and responsibility) to create a fully on-chain debt
market. It is entirely divorced from "global" off-chain markets, in favor of a bottom-up, or
*endogenous*, approach to price & interest rate discovery.

##### The protocol is broadly comprised of three distinct phases:

1. **Ask Phase** - Alice initiates this phase by "broadcasting" her (easily queryable) loan request
for a *specific quantity of a certain token, collateralized by a certain token (or tokens), to be
repaid over a specific timeframe.*

2. **Offer Phase** - Bob determines that Alice is creditworthy (via her on-chain credit history) and
initiates an offer to fulfill the loan *in the amount and over a timeframe that Alice* has
specified, *with a collateral ratio, interest rate, required minimum payments, etc, that Bob*
specifies. 
   
3. **Active Phase** - Alice accepts the loan terms Bob has offered by spending his Offer UTxO,
thereby gaining full control over the requested assets, and locking her collateral into a
time-locked Active UTxO. Alice may incrementally pay off the loan, and can reclaim her collateral
*in proportion to the amount she just paid off.* If the loan expires, Alice can no longer repay the
outstanding amount, and Bob can claim the remaining collateral. Note that Alice maintains staking
(and voting) rights over her locked collateral through the length of the loan.

All three of these phases are expanded upon in the [Specification Section](#specification) below.

##### Distinguishing features specific to Cardano-Loans:

1. **On-Chain Credit History** - the status/conditions of current & past loans associated with a
borrower's stake credential are easily queryable by prospective lenders and third-party data
miners.
2. **Trustless Negotiations** - all loan conditions are negotiated in a fully p2p fashion - interest
rates, collateral (token type(s) and relative prices), length of the loan, required minimum
payments, etc - all are negotiable parameters. Multiple tokens can be used as collateral, even NFTs.
Over/under collateralization is implied by the relative prices in the lender's offer, which is
impacted by the borrower's credit history. Interest-free, non-compounding, and compounding interest
are all supported.
3. **Partial Repayments** - borrowers can repay loans incrementally and withdraw collateral in
proportion to the repayment. If an outstanding balance remains at expiry, the lender may withdraw
the remaining collateral. 
4. **No Auto-liquidation** - All loan terms (including relative token prices) are agreed upon
explicitly by both parties, so "margin" is constant throughout the length of the loan, and does
*not* change with global price movements. 
5. **Endogenous Price & Interest Rate Discovery** - instead of relying on oracles or trusted actors,
relative token prices and interest rates are explicitly agreed upon during loan negotiations,
resulting in true market-driven discovery. In due time and with enough users, sufficiently
distributed dApps may *serve* as oracles, instead of having to consume them.


##### Features that Cardano-Loans shares with other *distributed dApp* protocols:

- **Full Custody** - users always maintain full spending *and* delegation control *and* voting
control over their assets.
- **Natural Concurrency** - throughput scales *with* the number of users. No specialized
batchers/indexers are necessary, though they *may* be used in proprietary backends of large lending
providers.
- **Frontend/Backend Agnosticism & Interoperability** - relatively straightforward integration with
existing frontends (i.e. wallets). Third-party platforms can build business models atop
open-contract p2p protocols. 
- **Censorship Resistance** - due to the aforementioned agnosticism, distributed p2p dApps offer the
highest level of censorship-resistance of any dApp architecture.
- **No Superfluous "dApp" Tokens** - ADA is all you need to pay for script/TX fees. 
- **Democratic Upgradability** - users choose if/when to use new contracts.


## Specification

The dApp logic is composed of one minting policy and one validator script that work together. All
loans, no matter how different the terms, use the same minting policy and validator script. 

### Loan Components:

This section outlines the different components of Cardano-Loans. 

#### The Borrower's Address

Borrowers create a unique "loan" address - this is where negotiations take place *and* where
collateral is kept until the loan is either repaid or expired. As is common in *distributed dApps*,
all such "loan" addresses use the same validator script for the payment credential, and a unique,
user-defined staking key or script for the staking credential. Owner-related actions are "overloaded" to the
staking key *by* the validator script, so the user maintains full control of the address. The
borrower's credit history is cryptographically tied to the staking credential used in the address.

Since negotiations occur in the borrower's address, the borrower maintains staking rights over all
assets throughout the life cycle of the loan. This includes collateral, which makes sense, since
collateral does belong to the borrower until they default. It also includes any Offer UTxOs, which
incentivizes lenders to be proactive and not leave Offers open for too long. 

:warning: If, at any point, a misconfigured UTxO is sent to a borrower's address, the borrower will
get custody. Such UTxOs can only come about by misusing the dApp. As long as beacon tokens are
minted in the Tx, the resultant UTxO will be locked appropriately, as per the contract.

:important: Staking scripts can be executed without actually withdrawing an rewards. In other words,
staking scripts can be executed by withrawing 0 ADA from the rewards address. The only requirement
for this trick is that the staking credential is registered/delegated. Since the protocol requires
the staking script to signal approval in the transaction, this technique can be used by users to do
just that.

#### Telling Time

Cardano-Loans enforces time-sensitive logic by marrying user incentives with transaction validity
intervals.

Consider the following transaction scenarios separately:

1. **Borrower accepts loan** - The borrower cannot access the funds until the loan starts, so they
want to set the time as early as possible. However, the loan expiration is calculated by adding
the loan term length to the start time. If the borrower sets the time to be earlier than it actually
is, the loan expiration would also be earlier than it otherwise would be. This means less time for
the loan than the borrower wants. If the borrower sets the time to be later than it actually is so
that the loan expiration would be later than it otherwise would be, the transaction would not be
valid until the start time *actually* occurs. This means the borrower would only be delaying when
they can access the loan asset. Therefore, **the borrower is incentivized to set `invalid-before` to
be as close to the current time as possible.**
   
2. **Lender claims an expired loan** - The lender cannot claim a loan unless it is expired. If the
lender set the time to be earlier than it actually is, the protocol would conclude the loan is
would still active and the lender cannot claim it. If the lender set the time to be later than it
actually is the transaction would not (yet) be valid, so the lender would have to wait longer to
claim what they are owed. These two together result in **the lender being incentivized to set
`invalid-before` to be as close to the loan's true expiration as possible.**

3. **Borrower makes a payment** - The borrower cannot make payments once a loan has expired. This is
enforced by the borrower having to use `invalid-hereafter` to specify the current time. Since the
borrower wants as much time as possible to pay off the loan, the borrower wants to set
`invalid-hereafter` to be as late as possible. But if it is set to *after* the loan's expiration,
the protocol will conclude that the loan is already expired and deny the borrower's payment.
Therefore, **borrowers are incentivized to set `invalid-hereafter` as close to the expiration slot
as possible.**

#### Interest

Unlike other lending/borrowering protocols, Cardano-Loans does not use an algorithm to determine the
interest rates. Instead, rates are one of the explicitly negotiated terms between borrowers and
lenders.

**Cardano-Loans v2 allows for interest-free, non-compounding, and compounding interest rates.** By
requiring the loan to be rolled over at certain checkpoints, the interest can be applied to the
outstanding balance at each checkpoint. The checkpoints themselves are negotiable.

#### Beacon Tokens
Cardano-Loans v2 uses 7 types of tokens:

1. **Ask Token** - demarcates UTxOs in the "Ask Phase"
2. **Offer Token** - demarcates UTxOs in the "Offer Phase"
3. **Active Token** - demarcates UTxOs in the "Active Phase"
4. **LenderID Token** - demarcates the lender's pubkey or staking script and mediates their ability
to interact with their Offer UTxOs.
5. **BorrowerID Token** - demarcates the borrower's staking credential and cryptographically links
the borrower to their credit history. 
6. **LoanID Token** - demarcates a specific loan and provides a lock/key pair to allow the lender to
freely trade the Key NFT on secondary markets.
7. **Asset Token** - demarcates what asset is being lent out for the loan.

The presence of these tokens help mediate dApp logic by acting as "stamps of approvals" for datums
(ie, a datum with a token present will always be valid). In addition to this, all these tokens
double as Beacons that users can query (via existing APIs) to interact with each other, without
relying on a specialized aggregator/batcher.

#### Proxy Script

Since Cardano-Loans has borrowers make payments directly to lender's, care must be taken when the
lender's address uses a payment plutus script. This is because the protocol enforces a specific
datum with loan payments in order to guarantee uniqueness of outputs - this is currently the
cheapest option. If the loan payments with this enforced datum are sent to a lender address that
requires a different datum, the loan payment can be locked forever.

To address this issue, Cardano-Loans uses a pre-approved proxy script to trustlessly enable
arbitrary logic for lender addresses without compromising security and composition. The proxy script
can accept any datum, use any redeemer, and simply delegates spending authorization to the address'
staking credential, which can be anything. You can read more about this in the
[ADR](architectural-decisions/002-proxy-script.md).

#### Loan Datums

Three different inline datums (one for each of the phases: Ask, Offer, Active) are used for loan
negotiations/agreements. For reference, here is the (Haskell) code:

``` Haskell
data LoanDatum 
  = AskDatum
      { beaconSym :: CurrencySymbol
      , borrowerId :: TokenName
      , loanAsset :: (CurrencySymbol,TokenName)
      , loanPrinciple :: Integer
      , loanTerm :: POSIXTime
      , collateral :: [(CurrencySymbol,TokenName)]
      }
  | OfferDatum
      { beaconSym :: CurrencySymbol
      , lenderId :: TokenName
      , lenderAddress :: Address
      , loanAsset :: (CurrencySymbol,TokenName)
      , loanPrinciple :: Integer
      , rolloverFrequency :: Maybe POSIXTime
      , loanTerm :: POSIXTime
      , loanInterest :: Plutus.Rational
      , minPayment :: Integer
      , collateralization :: [((CurrencySymbol,TokenName),Plutus.Rational)]
      , collateralIsSwappable :: Bool
      , claimPeriod :: POSIXTime
      , offerDeposit :: Integer
      }
  | ActiveDatum
      { beaconSym :: CurrencySymbol
      , borrowerId :: TokenName
      , lenderAddress :: Address
      , loanAsset :: (CurrencySymbol,TokenName)
      , loanPrinciple :: Integer
      , rolloverFrequency :: Maybe POSIXTime
      , lastCheckpoint :: POSIXTime
      , loanTerm :: POSIXTime
      , loanInterest :: Rational
      , minPayment :: Integer
      , collateralization :: [((CurrencySymbol,TokenName),Rational)]
      , collateralIsSwappable :: Bool
      , claimExpiration :: POSIXTime
      , loanExpiration :: POSIXTime
      , loanOutstanding :: Rational
      , loanId :: TokenName
      }
```

The Ask and Offer datums can be thought of as forms that users fill out in order to tell the
protocol what terms they want enforced on their behalf. The Active datum is created based on the Ask
and Offer datums used to create the loan.

`POSIXTime` is an integer representing POSIX time down to the millisecond.
`Rational` is a fraction of integers. This is how decimals are represented by the dApp.

The datums are used as inline datums so that others can see the necessary information for
negotiating and checking a borrower's credit history. 

- `beaconSym` - The minting policy for the protocol. Every form needs this so that the protocol
knows what beacons to look for. This field is not part of negotiations since it is only used
internally by the validator. 
- `borrowerId` - The borrower's staking credential hash. The credential hash itself is what gets
stored here. This field makes it easy for lenders to know what credential to check for the user's
credit history. Without this field, the lender would need to inspect the address itself to get the
credential hash. The field is also used by the protocol to save on computation since it doesn't need
to parse the address' staking credential each time in order to get this. This field is not part of
negotiations since it much match the address being used.
- `loanAsset` - The policy ID and asset name for the asset being lent out. 
- `loanPrinciple` - The amount of the loan asset that is borrowed.
- `loanTerm` - The loan's duration (ie, how much time will the borrower have to repay the loan).
- `collateral` - A list of assets the borrower is willing to use as collateral for the loan.
- `lenderId` - The lender's pubkey or staking script credential. The protocol requires this to
manage the LenderID beacon token. It is also used to cheaply check for the lender's approval when
required. This field is not part of the negotiations since it is only used internally by the
validator.
- `lenderAddress` - The address where the lender would like loan payments to go. It can either be a
payment pubkey address or a plutus address with staking where the payment credential is the proxy
script. No other payment scripts are allowed.
- `rolloverFrequency` - The frequency in which the interest must be applied. It can be set to
`Nothing` to allow a non-compounding interest loan.
- `loanInterest` - The interest rate for the loan. It can be set to zero for an interest-free loan.
- `minPayment` - The minimum payment borrowers are required to make with each payment. The borrower
always has the option to pay off the remaining balance, even if the remaining balance is less than
the required minimum payment. Lenders can set this to zero if they would like to offer loans with no
minimum payment requirement.
- `collateralization` - The relative values that the lender wants for each collateral asset. The
lender can allow over-collateralized or under-collateralized loans by setting the relative values
for each collateral asset to above or below the market value, respectively. If the lender does not
want that asset to be used as collateral, they can set that asset's relative value to zero. In
essence this, field is *how much of that collateral asset the lender wants per unit of the loan
asset taken.* This is always in units of `collateralAsset/loanAsset`. For example, if
`collateralization` for AGIX/ADA is 2/1, then the borrower must put up 2 AGIX for every 1 ADA
borrowed. If more than one collateral asset is possible, then the collateral can be used in any
combination where the total relative value equals the loan principle. For example, if DUST is also
allowed at a ratio of 10/1, then 10 ADA can be borrowed by putting up 10 AGIX and 50 DUST: `10 / 2 +
50 / 10 = 5 + 5 = 10`.
- `collateralIsSwappable` - Whether or not the lender wants to allow the borrower to swap out
collateral during the loan. This option favors the borrower but lenders can enable it to make their
offers more competitive. Since lenders must explicitly allow this option, it cannot be abused by
borrowers.
- `claimPeriod` - The time period the lender requires to claim the collateral once the loan is
expired. Since the Key NFT can be lost, the collateral could be locked forever without an escape
hatch. The protocol considers collateral "Lost" once the `claimPeriod` has passed and allows the
borrower to reclaim the collateral (while still recording the default).
- `offerDeposit` - How much ADA the lender used as the minimum UTxO value for the Offer UTxO. This
amount will be repaid to the lender when the offer is accepted.
- `lastCheckpoint` - The last time the loan state changed. Loan states change when the loan is
accepted and when the loan is rolled over. It is used to determine the next time a rollover is
required. The next rollover is required at `lastCheckpoint + rolloverFrequency`.
- `claimExpiration` - The absolute time when the lender's claim period will end (and the collateral
will be considered "Lost"). It is calculated as: `startTime + loanTerm + claimPeriod`.
- `loanExpiration` - The absolute time when the loan will expire. It is calculated as: `startTime +
loanTerm`.
- `loanOutstanding` - The remaining unpaid balance for the loan. When the loan is accepted, this
field is initialized to: `loanPrinciple * (1 + loanInterest)`. This field is decremented with each
payment made. It is incremented with each rollover made.
- `loanId` - The asset name for the Lock/Key NFT pair for the loan.


#### Lender Payment Datums

In order to ensure that double satisfaction does not occur when paying a lender directly, each loan
payment includes an inline datum specifying which loan the payment is for. The protocol can safely
enforce the datum for all payments since lender addresses can only be payment pubkey address or
proxy script addresses (which can accept any datum). This is the datum:

```Haskell
newtype PaymentDatum = (CurrencySymbol,TokenName)
```

The `CurrencySymbol` is the beacon policy id and the `TokenName` is that specific loan's LoanID.
Since every LoanID is guaranteed to be unique, the inclusion of this datum guarantees that all loan
payments are unique even when composed with other protocols. **Enforcing the datums like this does
not impact composition with other P2P-DeFi protocols.** For a more detailed discussion of this,
check out the [ADR](architectural-decisions/003-payment-datum.md).

#### Minting Redeemer
The (Haskell) code for the minting policy redeemer is below:

``` Haskell
data BeaconRedeemer
  = CreateAsk 
      Credential -- ^ Borrower's staking credential.
      [(CurrencySymbol,TokenName)] -- ^ Loan assets asked for.
  | CreateOffer 
      Credential -- ^ Lender's credential to be used as the LenderID.
      [(CurrencySymbol,TokenName)] -- ^ Loan assets offered.
  | CreateActive 
      Credential -- ^ Borrower's staking credential.
      [(TxOutRef,TxOutRef)] -- ^ List pairing up Asks and Offers to accept.
  | BurnBeacons
```

- `CreateAsk` - Borrowers can create multiple Ask UTxOs in the same transaction. They do not need to
be for the same loan asset. The list passed with the redeemer is a list of all loan assets the
borrower is creating Ask UTxOs for.
- `CreateOffer` - Lenders can create multiple Offer UTxOs in the same transaction and they do not
need to be for the same loan asset. The list passed with the redeemer is a list of all loan assets
the lender is creating Offer UTxOs for.
- `CreateActive` - Borrowers can accept multiple loans in a single transaction. Each loan accepted
must have the corresponding Ask and Offer UTxOs properly paired up in the redeemer's list. This
redeemer does **not** allow multiple borrowers to accept loans in the same transaction. Each
acceptance transaction must be dedicated to a specific borrower.

#### Loan Validator Redeemer
The (Haskell) code for the loan validator redeemer is below:

``` Haskell
data LoanRedeemer
  = CloseAsk
  | CloseOffer
  | AcceptOffer
  | MakePayment
  | Rollover Integer
  | ClaimExpired
  | UpdateLenderAddress Address Integer
  | Unlock
```

- `CloseAsk` - Allow the borrower to close any Ask UTxOs (valid or invalid). All beacons must be
burned. Multiple Ask UTxOs can be closed in the same transaction.
- `CloseOffer` - Allow the lender to close any valid Offer UTxOs. All beacons must be burned. The
borrower can use this redeemer to spend invalid Offer UTxOs (UTxOs with an `OfferDatum` but no
beacons). Multiple Offer UTxOs can be closed in the same transaction.
- `AcceptOffer` - Allow the borrower to consume valid Ask and Offer UTxOs in order to create a new
loan. Multiple loans can be accepted in the same transaction.
- `MakePayment` - Allow the borrower to make a payment on a loan. Payments can be made on multiple
loans in the same transaction. This redeemer will not allow payments on loans that require a
rollover. 
- `Rollover` - Allow the borrower to rollover a loan and accrue the interest. Multiple loans can be
rolled over in the same transaction. Loans can be rolled over in the same transaction where payments
are made on other loans. Since the UTxO for the loan can grow in size, this redeemer allows
increasing the minUTxOValue deposit by the amount of Lovelace specified by the `Integer`. This
feature is also useful for Cardano protocol parameter changes.
- `ClaimExpired` - Allow the lender to claim expired collateral using the Key NFT. The lender can
claim the collateral for multiple loans in the same transaction as long as all loans are expired and
the lender controls the Key NFT for each loan.
- `UpdateLenderAddress` - Allow the owner of the Key NFT to update the `lenderAddress` field of the
`ActiveDatum` so that new loan payments go to the new address. This is especially useful for when a
Key NFT is bought on the secondary market. This redeemer allows updating the address in the same
transaction where the Key NFT is purchased. Multiple loans can have their addresses updated in the
same transaction as long as the required Key NFTs are present. The `Address` passed with the
redeemer is the desired new address for that loan. Since the UTxO for the loan can grow in size,
this redeemer allows increasing the minUTxOValue deposit by the amount of Lovelace specified by the
`Integer`. This feature is also useful for Cardano protocol parameter changes.
- `Unlock` - Allow the borrower to claim "Lost" collateral, spend invalid Active UTxOs (UTxOs with
an `ActiveDatum` but no beacons), or clean up a finished loan's beacons and reclaim their deposit
for the Active UTxO (from the minUTxOValue for the Cardano blockchain). Multiple UTxOs can be
unlocked in the same transaction.

### The Loan Lifecycle
Cardano-Loans is broken up into five distinct phases:

#### 1. Ask Phase

##### Creating an Ask

Borrowers begin the negotiations by creating an Ask UTxO. Creating an Ask requires minting the Ask
token(s), the Asset token(s), and storing them with a valid datum at the borrower's address. This
step only requires the protocol's minting policy. In order for creation to succeed, all of the
following must be true:

1. The `CreateAsk` minting redeemer must be used.
2. Only Ask tokens and Asset tokens can be minted by the minting policy in this transaction. All
   loan assets whose Asset token will be minted must be found in the list passed with the redeemer.
3. The Ask tokens must have the asset name "Ask".
4. The Asset tokens must have the asset name: 
    `sha2_256( "Asset" ++ loan_asset_policy_id ++ loan_asset_name )`.
5. All tokens must be stored with Ask UTxOs.
6. Each Ask UTxO must have exactly 1 Ask token and 1 Asset token. No other tokens from the minting
   policy can be found in the UTxO.
7. Each Ask UTxO must be locked at an address using the protocol's validator as the payment
   credential and the credential in the redeemer as the staking credential.
8. Each Ask UTxO must have a valid inline `AskDatum`:
    - `beaconSym` must be protocol's minting policy id.
    - `borrowerId` must be the hash from the credential passed with the redeemer.
    - `loanAsset` must correspond to the Asset token stored in the UTxO and the loan asset cannot be
      one of the protocol's tokens.
    - `loanPrinciple` must be > 0.
    - `loanTerm` must be > 0.
    - `collateral` must not be an empty list.
9. The receiving staking credential (ie, the credential in the redeemer) must approve the 
   transaction.

The prefix of "Asset" in the pre-hash (for creating the Asset token's asset name) is used to ensure
that Asset tokens can never be counterfeit LoanIDs. Since the `CreateAsk` redeemer does not check if
the `loanAsset` is a real asset, it is possible to create an Asset token for something that is not
an Asset. For example, one could possibly create an Asset token for a UTxO's output reference. Since
output references are used to generate LoanIDs, this could result in counterfeit LoanIDs. While
this is not necessarily a security risk, it does complicate the beacon querying. Adding the prefix
to the pre-hash string makes it so that names generated for Asset tokens will always be distinct
from names generated for LoanID tokens, even if the same input is used for both.

The borrower is able to use multiple assets as collateral for a given loan. Whatever assets *can*
(but not necessarily *need* to) be used must appear in the `collateral` list.

The approval signal requirement cryptographically ensures no one but the borrower can open a loan
under the same credential.

The requirements allow for creating multiple Ask UTxOs in the same transaction as long as they are
all for the same borrower.

##### Closing an Ask

If the borrower changes their mind about the loan they are asking for, they may close their Ask at
any time. Closing the Ask requires both the protocol's minting policy and its validator. To close
an Ask, all of the following must be true:

1. The `CloseAsk` redeemer must be used for the validator and the `BurnBeacons` redeemer must
   be used for the minting policy.
2. The datum attached to the UTxO must be an `AskDatum`. 
3. The address' staking credential (aka the borrower) must signal approval.
4. All protocol tokens among the transaction inputs must be burned.

Here it is not necessary to check for the presence of an Ask token; the address owner gets custody
of both valid Ask UTxOs and misconfigured Ask UTxOs. If a UTxO has an `AskDatum` but is missing an
`Ask` token, the address owner can spend the UTxO by way of the staking credential. If a UTxO has an
`AskDatum` and an Ask token, the address owner still has custody and can spend the UTxO in the same
way. In either case, the staking credential must approve, so there is no need to check for the `Ask`
token. This can be used to prevent accidental locking of any misconfigured UTxOs that have an
`AskDatum`.

The requirements allow for closing multiple Ask UTxOs in the same transaction.

:important: There is no way to update Ask UTxOs in place. This would dramatically increase the
attack surface of the protocol. Instead, borrowers must first close and then re-open the Ask with
the new terms. Since these actions are cheap due to reference scripts (about 0.4 ADA in total), this
hardly seems like a loss.

##### Querying Asks and Borrower Histories

Prospective lenders can see all current Asks by querying the location of the Ask beacon tokens. For
a more specific query of all Asks for a given loan asset, lenders can query all UTxOs that contain
**both** an Ask beacon token and the corresponding Asset beacon token.

Once the lender finds an Ask they are interested in, they can query the borrower's credit history by
querying the burn transaction history for that borrower's BorrowerID beacon token. All transactions
where the BorrowerID was burned in isolation contain successful repayments - all Active UTxO outputs
in these burn transactions are the fully paid loans. All transactions where the BorrowerID was
**not** burned in isolation contain defaults - all Active UTxO inputs with the BorrowerID are
defaults.

It is also possible for the lender to see all loans a borrower currently has by querying all
BorrowerIDs at the borrower's address - these are all active loans. This may be a useful metric to
avoid potentially over-leveraged borrowers.

#### 2. Offer Phase

##### Querying competing Offers

Before a lender makes an Offer, they can see what other offers have already been made to that
borrower. The borrower's address is returned as part of the response from querying the open Asks.
This allows the lender to query all UTxOs at the borrower's address that contain an Offer beacon
token with the corresponding Asset beacon token - these would only be competing Offers. If no Offers
are returned by this query, they the lender would be the first to make an Offer to this borrower.

##### Create an Offer

Prospective lenders continue the negotiations by creating an Offer UTxO for the borrower. Creating
an Offer UTxO requires minting the Offer token(s), the LenderID(s), and the Asset token(s), and
storing them with a valid `OfferDatum` at a borrower's loan address. This step only requires the
protocol's minting policy. Creating the Offer requires all of the following to be true:

1. The `CreateOffer` redeemer must be used for the minting policy.
2. Only Offer tokens, LenderIDs, and Asset tokens can be minted by the minting policy in this 
   transaction.
3. The Offer tokens must have the asset name "Offer".
4. The LenderIDs must use the hash from the credential passed with the redeemer and be prefixed with
   either "00" or "01" if the credential is a pubkey or script, respectively.
5. The Asset tokens must have the asset name: 
    `sha2_256( "Asset" ++ loan_asset_policy_id ++ loan_asset_name )`.
6. All tokens must be stored in with Offer UTxOs.
7. Each Offer UtxO must have exactly 1 Offer token, 1 LenderID, 1 Asset token, the loan amount
   for the Offer, and the amount of ADA specified in the `offerDeposit` field of the UTxO's
   `OfferDatum`.
8. Each Offer UTxO must be locked at an address using the protocol's validator for the payment
   credential and with a staking credential.
9. Each Offer UTxO must be stored with a valid `OfferDatum`:
    - `beaconSym` must be the policy id for the protocol's minting policy.
    - `lenderId` must be the asset name used for the LenderID token (including the prefix).
    - `lenderAddress` must use either the proxy script or a pubkey for the payment credential. If
      the proxy script is used, the address must have a staking credential.
    - `loanAsset` must be the asset that corresponds to the Asset token stored in the UTxO and it
      cannot be one of the protocol's tokens.
    - `loanPrinciple` must be > 0.
    - `rolloverFrequency` must either be `Nothing` or `Just x` where x is > 0.
    - `loanTerm` must be > 0.
    - `loanInterest` must be >= 0 and have a denominator that is > 0.
    - `minPayment` must be >= 0.
    - `collateralization` must not be an empty list, all relative prices must be >= 0, and all
      relative prices must have a denominator > 0.
    - `claimPeriod` must be > 0.
    - `offerDeposit` must be > 0.
10. The credential passed with the redeemer must signal approve by either signing the transaction or
    being executed as a staking script.

No payment scripts are supported for the lender's credential; it would add too much logic to the
already large smart contracts. Executing a payment script requires actually consuming a UTxO that is
guarded by it. This makes it more difficult to signal approval than if a staking script is used
since no ADA actually needs to be withdrawn from a rewards address to executed the staking script.
Staking scripts are prioritized for this reason.

The `collateralization` list must be in the same order (based on collateral name) as the borrower's
`collateral` list. This is due to how `AcceptOffer` checks if the borrower and lender agree on the
terms. It is too expensive for the protocol to sort them internally, it would hurt composition.

The lender must signal approval of the transaction with the credential used for the LenderID so
that the LenderID token is guaranteed to be unique to that lender. The LenderID is used internally
by the protocol to mediate spending of valid Offer UTxOs (ie, it will look for the credential that
corresponds to the ID when an Offer is being closed). The LenderID also serves as a beacon for
lenders to keep track of their own Offers.

By requiring the lender to store the loan amount with the Offer Token, the borrower is able to
accept the loan without any other input from the lender. The ADA used for `offerDeposit` will be
returned to the lender when the Offer is accepted. 

##### Closing an Offer

If the lender changes their mind about their Offer *prior* to it being accepted by the borrower, or
if the borrower accepts a different Offer, the lender may close the Offer and reclaim their Offer
UTxO assets. This step requires both the protocol's minting policy and the validator. To close and
Offer, all of the following must be true:

1. The `CloseOffer` redeemer must be used for the loan validator and the `BurnBeacons` redeemer must
   be used for the minting policy.
2. The datum attached to the UTxO being consumed must be an `OfferDatum`. If it isn't, then this
   UTxO cannot possibly be an Offer.
3. If the Offer token is present in the UTxO, this is a valid Offer and the LenderID is guaranteed
to be present. It also guarantees that the datum for this UTxO is properly configured and can be
trusted. Custody belongs to the lender associated with the LenderID. Additional checks are required
in this situation:
    - The credential in the LenderID must approve the transaction. The credential hash is the
    LenderIDs asset name, prefix with either "00" if the credential is a pubkey or "01" if the
    credential is a script.
    - All Offer tokens in the transaction inputs must be burned.
    - All of LenderIDs in the transaction inputs must be burned.
    - All Asset tokens in the transaction inputs must be burned.
4. If the Offer token is not present but the UTxO contains an `OfferDatum`, then this is an invalid
Offer UTxO and the address owner gets custody by default. In this scenario, the staking credential
of the address must signal approval.

The requirements still allow the lender to close multiple Offers in the same transaction; they do
not need to be Offers to the same borrower nor do they need to be Offers for the same loan asset.

Likewise, the borrower is able to close multiple invalid Offer UTxOs in the same transaction.

:important: There is no way to update Offer UTxOs in place. This would dramatically increase the
attack surface of the protocol. Instead, borrowers must first close and then re-open the Offer with
the new terms. Since these actions are cheap due to reference scripts (about 0.4 ADA in total), this
hardly seems like a loss.

#### 3. Renegotiations

The borrower is able to keep track of all Offers made to them by periodically querying for UTxOs at
their address with an Offer beacon token. If an Offer is made to the borrower that the borrower does
not like, the borrower can close their Ask and open a new one with a "counter-offer" (eg, one where
the borrower uses a different asset as collateral or asks for a shorter loan).

Likewise, the lender is able to keep track of activity in the borrower's address by periodically
querying UTxOs at the address with either an Ask beacon token or an Offer beacon token. If the
lender sees that the terms of the Ask UTxO have changed, the lender can choose to either match the
terms (by closing their Offer and re-opening it with the new terms) or walk away from the loan.
Similarly, if the lender sees that another lender has made a more enticing Offer to the borrower,
then they can decide whether to walk away from the loan or one-up the other lender.

The protocol allows the borrower and lender(s) to renegotiate the terms for as long as they would
like. Both parties can walk away at any time. Once the borrower and lender settle on terms, the loan
can progress to the Active Phase.

#### 4. Active Phase

##### Accepting a loan offer

Accepting an offer requires both the minting policy and the loan validator script. The idea is to
consume 1 Ask UTxO and 1 Offer UTxO to produce 1 Active UTxO. In order to avoid redundant
executions, the minting policy and validator split the checks.

###### Validator Checks

1. The `AcceptOffer` redeemer must be used.
2. The UTxO input must have either an `AskDatum` or an `OfferDatum`.
3. At least one Active token must be minted in the transaction. 

The check for the Active token can be used by the validator as a proxy for whether or not the
minting policy was executed properly in the transaction. The only way to mint an Active token is
with the proper usage of the minting policy. The minting policy will check the validity of the
inputs.

###### Minting Policy Checks

1. The `CreateActive` redeemer must be used.
2. All Ask UTxOs must have an Ask token.
3. All Offer UTxOs must have an Offer token.
4. All Ask and Offer UTxOs must come from the address using the protocol's validator as the payment
   credential and the credential in the redeemer as the staking credential.
5. No other UTxOs from the address are permitted in the transaction.
6. All Ask UTxOs must be paired up with exactly one Offer UTxO and vice,versa. The pairings must be
   passed as the list to the redeemer.
7. The paired inputs must agree on the terms (ie, the fields that are in both datums must agree).
8. The transaction must specify and invalid-before bound.
9. The proper tokens must be minted/burned by the policy:
    - All Ask tokens among inputs must be burned.
    - All Offer tokens among inputs must be burned.
    - All LenderIDs among inputs must be burned.
    - All Asset tokens from the Offer UTxOs must be burned. The ones from the Ask UTxOs are kept to
    be stored with the new Active UTxO.
    - Exactly 1 Active token must be minted for every loan accepted. It must have "Active" as the
    asset name.
    - Exactly 1 BorrowerID must be minted for every loan accepted. It must have the hash in the
    credential from the redeemer as the asset name. It must not be prefixed with anything.
    - Exactly 2 LoanIDs for each loan accepted. The asset name for the LoanIDs must correspond to
    the Offer UTxO that was used to create it: `sha2_256( offer_tx_hash ++ offer_output_index )`.
10. The corresponding Active UTxO output must have a valid inline `ActiveDatum`.
    - `beaconSym` must be the protocol's minting policy id.
    - `borrowerId` must be the hash of the credential in the redeemer. It must not be prefixed with
    anything.
    - `lenderAddress` must be the `lenderAddress` from the input datums.
    - `loanAsset` must be the `loanAsset` from input datums.
    - `loanPrinciple` must be the `loanPrinciple` from the input datums.
    - `rolloverFrequency` must be the `rolloverFrequency` fromt the Offer UTxO input.
    - `lastCheckpoint` must be the invalid-before specified with this transaction.
    - `loanTerm` must be the `loanTerm` from the input datums.
    - `loanInterest` must be the `loanInterest` from the Offer UTxO input.
    - `minPayment` must be the `minPayment` from the Offer UTxO input.
    - `collateralization` must be the `collateralization` from the Offer UTxO input.
    - `collateralIsSwappable` must be the `collateralIsSwappable` from the Offer UTxO input.
    - `loanExpiration` must be == `invalid-before + loanTerm`
    - `claimExpiration` must be == `loanExpiration + claimPeriod` where the `claimPeriod` is from
    the Offer UTxO input.
    - `loanOutstanding` must be == `loanPrinciple + (1 + loanInterest)`.
    - `loanId` must be == `sha2_256( offer_tx_hash ++ offer_output_index )`.
11. The Active UTxO must have the proper value:
    - Exactly 1 Active Token.
    - Exactly 1 BorrowerID.
    - Exactly 1 Asset token that corresponds to the `loanAsset` in the datum.
    - Exactly 1 LoanID that corresponds to the `loanId` in the datum. This is the Lock NFT.
    - The amount of collateral whose relative value (based on the datum's `collateralization`) is >=
    the datum's `loanPrinciple`.
12. There must be a corresponding output to the `lenderAddress` for each loan accepted. The value
must be the amount of ADA specified in the Offer UTxO's `offerDeposit` field and 1 LoanID token that
corresponds to that loan. This LoanID token is the Key NFT. The datum must be the tuple
`(minting_policy_id,"Accepted")`.
13. The borrower (signified by the credential in the redeemer) must approve the transaction.

In essence, the collateral calculation is:
```
sum { collateralDeposited / relativeValue } >= loanPrinciple offerDatum
```

The lender's output must include a datum just in case the `lenderAddress` is using the proxy plutus
script.

To summarize: the borrower accepts a loan offer via a single transaction with an Offer UTxO, an Ask
UTxO, and however many other inputs are necessary to fulfill the collateral requirements. The
transaction outputs one Active UTxO to the loan address and one output to the lender with the Key
NFT, while the remaining funds (what is actually being borrowed) is output to any address, as
specified by the borrower. The lender can freely trade the Key NFT.

The fact that the BorrowerID asset name is not prefixed with anything while the LenderID asset name
is means that users can safely use the same credential as both a lender and a borrower without
messing with the beacon queries.

Since the LoanID asset name uses the full output reference for each Offer UTxO, every loan is
cryptographically guaranteed to have a unique LoanID pair.

The last requirement ensures that only the borrower can every accept a loan under their credential.
Identity theft is cryptographically impossible (as long as not private keys are physically stolen).

##### Keeping track of Active UTxOs

The borrower can query all UTxOs with an Active token and BorrowerID at their loan address whenever
they want to check on the status of their current loans. 

Lenders can use the Key NFT to look up the associated Active UTxO. For example, since there are only
two LoanIDs with that asset name in existence, the lender can query all UTxOs that have that LoanID
*and* have an Active token. There will never be more than one UTxO that fits this requirement. When
the lender wants to sell the Key NFT on the secondary market, this exact same query can be used by
potential buyers to check the terms of the loan they would be aquiring.

##### Making a Loan Payment

A loan payment can either be a partial payment or a full payment (relative to the remaining
balance). If only partial payments are being made, then only the protocol's validator is needed. If
even one payment is a full payment, then the protocol's minting policy will also be required. The
loan validator script can tell whether a partial or full payment is being made.

To make a payment, all of the following must be true:

1. The `MakePayment` redeemer must be used with the validator and, if necessary, the `BurnBeacons`
redeemer must be used with the minting policy.
2. The input must have an `ActiveDatum`.
3. The address' staking credential (aka the borrower) must signal approval.
4. The BorrowerID must be present in the input being consumed. The presence of this token means the
UTxO is properly configured and the datum can be trusted.
5. The invalid-hereafter bound must be specified in the transaction.
6. The invalid-hereafter time specified must be <= `loanExpiration` from datum.
7. If the datum has `Just x` for the `rolloverFrequency`, then the invalid-hereafter time specified
must be <= `x + lastCheckpoint`.
8. There must be an output to the `lenderAddress` with the payment amount and proper datum. The
datum must be the tuple `(minting_policy_id,loanId)` where the `loanId` comes from the datum. If
no output matching the above is found, the protocol will assume a payment of 0 has been made.
9. There must be an Active UTxO output to the address of origin (aka the borrower's loan address)
with the proper datum: exactly the same as the input's datum except the `loanOutstanding` field
has subtracted off the payment amount.
10. If the amount paid to the lender is >= to the `loanOutstanding` from the input's datum, then
this is a full payment. The following checks are performed:
    - The Active UTxO output must have exactly 1 Active token, exactly 1 Asset
    token that corresponds to the datum's `loanAsset`, and 1 LoanID that corresponds to the datum's
    `loanId`. 
    - The output **cannot** have a BorrowerID token. All collateral can be taken from the input
    UTxO. 
    - BorrowerIDs must be burned in the transaction. **No other tokens can be minted/burned by
    any minting policy in the transaction**. BorrowerIDs must be burned in isolation.
11. Otherwise, this is a partial payment. The following checks are performed:
    - The amount paid must be >= the `minPayment` in the datum.
    - The Active UTxO output must have exactly 1 Active token,
    exactly 1 Asset token that corresponds to the datum's `loanAset`, 1 LoanID that corresponds to
    the datum's `loanId`, and exactly 1 BorrowerID that corresponds to the datum's `borrowerId`. 
    - The proportion of collateral taken from the input UTxO (when compared to this output UTxO) 
    must be <= the proportion of the loan repaid (based on the input's `loanOutstanding` field).
12. Any output with a BorrowerID must also have an Active token.


The collateral that can be reclaimed during a partial payment is determined by the following
equation:
```
sum { collateralTaken / relativeValue }           loanRepaid
--------------------------------------------  <=  --------------------------
sum { startingCollateral / relative value }       startingOutstandingBalance
```

The output to the lender's address determines how much of the loan was repaid. The script only
checks for a single proper output to the lender's address. If there are multiple, only one
will be counted which means the extra amount paid will be missed. This dramatically simplifies the
logic and will likely not be changed. There is no reason for borrowers to use multiple outputs to
lenders for a given loan payment.

The contract only checks if the borrower took *too much* collateral. If the borrower misses an
opportunity to reclaim some collateral, that proportion can only be reclaimed when the loan is fully
paid off. As long as the borrower successfully repays the loan, the borrower is guaranteed to retain
custody of the collateral.

Due to the way the collateral calculation is implemented, it is possible to swap out collateral as
long as the total relative value of the collateral is correct. For example, the borrower can replace
collateral A with collateral B if desired. The total relative values are still enforced by the
script. This "swapping" must be explicitly allowed by the `collateralIsSwappable` field of the
datum. If this field is set to `False`, no collateral can be added to the Active UTxO during the
loan's lifetime.

When a loan is fully paid off, the BorrowerID must be burned by itself, without other tokens being
burned in the same Tx. This includes tokens from other minting policies. This constraint makes it
easy to check (in the future) whether the loan ended in default or was repaid. 

Since there is no cheap way for a single validator execution to know exactly how many full payments
were made in the transaction, there is no cheap way for the validator to know how many BorrowerIDs
should be burned in the transaction. To get around this, the protocol eliminates the possibility for
the BorrowerID from a finished loan to be found in **any** output UTxO. This results in there being
no choice but to burn the BorrowerIDs from finished loans. This is the purpose of the last
requirement.

In order to enforce interest accrual, the protocol will not allow the borrower to make anymore
payments until after the required number of rollovers have occurred. Refusal to do this will only
result in a default as the collateral will be locked until expiration of the loan.

For convenience with using the invalid-hereafter flag, borrowers can set it to the next rollover
time. If no rollovers are required before the expiration, then it can be set to the loan's
expiration time. If there are multiple loan payments being made in the transaction, this flag can be
set to the earliest rollover/expiration.

The Active UTxO needs to be re-output to the address, even if the loan is fully paid off, due to the
way the credit history works. The borrower can clean up these finished loans and reclaim the
minUTxOValue deposits using the `Unlock` redeemer. Any collateral missed in the payment transaction
can still be claimed with the `Unlock` redeemer.

##### Rolling over a loan
Having the loan update its state at set intervals is a cheap way to enable compound interest. The
purpose of this redeemer is to do exactly that: have a loan accrue interest. This action only
involves the protocol's validator since the Active UTxO is just consumed and reproduced at the same
address, only the datum is updated to reflect that interest has been applied. Since the size of the
UTxO may increase with the new datum, the required minimum ADA to be stored with the Active UTxO may
increase. To allow for this growth, the `Integer` passed with the redeemer can specify how much ADA
the UTxO's value needs to increase by. The number can be 0 in the case where the UTxO does not need
more ADA. To rollover a loan, all of the following must be true:

1. The `Rollover` redeemer must be used and the `Integer` passed with it must be >= 0.
2. The input's datum must be an `ActiveDatum`.
3. The input must have a BorrowerID. This implies the Active UTxO is properly configured and the
   datum can be trusted.
4. The invalid-hereafter bound must be specified for the transaction.
5. The invalid-hereafter time specified must be <= the datum's `loanExpiration`.
6. The address' staking credential (aka the borrower) must signal approval.
7. There must be an Active UTxO output to the origin address with the proper value.
    - Value must be == input value + amount of lovelace from redeemer. This means the beacons and
    collateral value cannot change from the input.
8. The Active UTxO output must have the proper datum. The datum should be the same as the input
   datum except:
    - `loanCheckpoint` == `loanCheckpoint` from input's datum + `x` where `x` comes from `Just x` in
    the input datum's `rolloverFrequency` field. If the `rolloverFrequency` field has `Nothing`,
    then the new datum's `loanCheckpoint` should be the same as the input datum's.
    - `loanOutstanding` == `loanOutstanding` from input's datum * (1 + `loanInterest`).

Technically, the borrower can rollover the loan as many times as they like before the expiration.
However, since it means the interest is unnecessarily applied to the balance this just makes it
harder for the borrower to avoid a default. Therefore, the borrower is strongly incentivized **not**
to rollover a loan more than necessary.

This redeemer can only be used to rollover loans one period at a time. If two rollovers are required
before the next payment can be made, the loan must be rolled over in two transactions (one rollover
per transaction) before another payment can be made. This dramatically simplifies the protocol's
logic.

This redeemer allows rolling over multiple loans in a single transaction. The borrower can even
rollover loans in the same transaction where they are making loan payments with the `MakePayment`
redeemer.

As previously stated, since the size of the UTxO can grow with this redeemer, requiring the Active
UTxO output's value to be identical to the input's value would be too restrictive and worse yet,
prevent rollovers that otherwise would occur. This restriction would also be an issue in the
presence of Cardano protocol parameter updates. For this reason, the redeemer can be used to tell
the protocol how much of a value change to expect. **Only the value of ADA can change and it can
only increase.** This feature removes the excessive restriction without requiring any complicated
logic.

For convenience, the borrower can always set the invalid-hereafter flag to the `loanExpiration` of
the loan with the earliest expiration in the transaction.

##### Updating the `lenderAddress` field for an Active loan

When the Key NFT is sold, the lenderAddress in the `ActiveDatum` will need to be updated because
otherwise, the new owner would not be able to receive the loan payments. This can be done using the
`UpdateLenderAddress` redeemer. The `Address` passed with the redeemer is the new address and the
`Integer` is the amount of ADA the UTxO's value increased by (this is necessary since the UTxO's
size can grow, just like with the `Rollover` redeemer).

Updating the `lenderAddress` field only requires the protocol's validator. To update the field, all
of the following must be true:

1. The `UpdateLenderAddress` redeemer must be used. The `Address` passed with it must either use a
payment pubkey (and an optional staking credential) or the proxy script with a staking
credential. The `Integer` passed with the redeemer must be >= 0.
2. The input must have an `ActiveDatum`.
3. The input must have a BorrowerID. This guarantees the Active UTxO is properly configured and the 
datum can be trusted.
4. There must be 1 LoanID among the transactions inputs that is **not** located at the loan address.
The asset name must match the `loanId` field of the input's datum. 
5. There must be an Active UTxO output to the input's origin address with the proper value:
    - Value must be == input value + amount of lovelace specified in redeemer. This means the value
    of the beacons and collateral cannot change.
6. The Active UTxO output must have the proper datum:
    - Exactly the same as the input's datum except the `lenderAddress` is the address from the
    redeemer.

The BorrowerID must be present to prevent tricking the credit history if a borrower is also their
own lender. Otherwise, it could possibly enable double counting loans in the credit history.

This redeemer is designed so that lenders can update the address in the same transaction where they
purchase the Key NFT on the secondary market. It is modeled after
[cardano-secondary-market](https://github.com/fallen-icarus/cardano-secondary-market) where the
buyer unlocks the Key NFT when they make the payment to the seller. Therefore, the buyer is able to
also set the new `lenderAddress` in the same transaction. This is the dominant update scenario that
the Cardano-Loans protocol is intended to support and referencing the Key NFT instead would not
allow updating the address in this transaction. Lenders that are updating the `lenderAddress` for
themselves will need to send the Key NFT to themselves in order to allow the update to occur.

#### 5. Post-Active Phase: Expired or Finished

##### Claiming an expired loan

The `ClaimExpired` redeemer allows the lender to claim expired loans. This action requires both the
protocol's minting policy and its validator. Any Active UTxOs claimed with this redeemer will appear
as defaults for the respective borrowers. To claim an expired loan, all of the following must be
true:

1. The `BurnBeacons` redeemer must be used for the minting policy and the `ClaimExpired` redeemer
   must be used for the validator.
2. The input must have an `ActiveDatum`.
3. The input must have a BorrowerID. This ensures the Active UTxO is properly configured and the 
datum can be trusted.
4. The invalid-before bound must be specified in the transaction.
5. The invalid-before time must be > `loanExpiration`.
6. There must be 1 LoanID in the transaction inputs that is **not** located at the input's origin
address. The asset name for the LoanID must match the input datum's `loanId` field.
7. All of the beacon tokens among the tx inputs must be burned: Active tokens, Asset tokens, 
BorrowerIDs, and LoanIDs.

The BorrowerID is required to be present because the lender only has claims on expired collateral.
The minUTxOValue deposits (and possibly collateral) from finished loans belong to the borrower so
the protocol should not allow the lender to claim it.

The lender has until the claim period ends to claim the defaulted loan. Once the claim period
passes, the lender can still claim it but so can the borrower since the collateral is considered
"Lost" by the protocol. This should not be an issue since the claim period is fully negotiable.

The lender is able to claim multiple expired loans in the transaction as long as they control all
the required Key NFTs.

##### Unlocking Active UTxOs

With the introduction of Lock and Key NFTs, it is possible for collateral to be permanently lost if
the Key NFT is lost. To prevent this, a lender has a set amount of time to claim the collateral.
After this time passes, the collateral is considered "Lost" and is recoverable by the borrower. The
loan still counts as a default against the borrower. Once the claim period passed, the lender can
still claim the collateral with `ClaimExpired` but it is then a race against the borrower. The claim
period is fully negotiable so this should not be an issue. Any "Lost" loans claimed by the borrower
will still appear in the borrower's credit history.

Another usecase for this redeemer is to clean up finished Active UTxOs. Since the borrower's
credit history requires the BorrowerID to be burned in isolation, there is no way to burn the other
beacon tokens in the same transaction. That is why full payments still require an output with the
remaining beacon tokens. This redeemer can be used by the borrower to burn the remaining beacon
tokens and reclaim the ADA that must be stored with the UTxO.

The last usecase for this redeemer is to allow the address owner to spend invalid Active UTxOs (ie,
UTxOs with an `ActiveDatum` but no beacons).

Using the `Unlock` redeemer requires at least the protocol's validator. If any beacons need to be
burned, then the minting policy is also required. To unlock an Active UTxO, all of the following
must be true:

1. The `Unlock` redeemer must be used with the validator and the `BurnBeacons` redeemer must be used
with the minting policy.
2. The input must have an `ActiveDatum`.
3. The address' staking credential (aka the borrower) must signal approval.
4. The invalid-before bound must be specified in the transaction.
4. All beacon tokens among the transaction inputs must be burned.
5. If the BorrowerID is present in the input, then the time specified with invalid-before must be > 
`claimPeriod`.

This redeemer allows unlocking multiple Active UTxOs in the same transaction.

### Benchmarks

Detailed benchmarks can be found [here](Benchmarks.md). Below is a quick summary:

- Borrowers can accept up to 5 loans in a single transaction.
- Borrowers can make 6-7 partial/full payments in a single transaction.
- Borrowers can rollover up to 11 loans in a single transaction.
- Lenders can claim 4 expired loans in a single transaction.


### End-to-End Example (Simplified)
```
Alice would like to use some AGIX and DUST tokens as collateral to borrow 100 ADA for 100 slots. 
She uses Cardano-Loans to ask for a loan with those terms.
Bob queries all open asks and sees Alice's request.
Bob looks up Alice's credit history and sees that she has successfully paid back 3 prior loans.
Bob offers Alice an under-collateralized loan at 10% interest, and AGIX at 2/1 and DUST at 5/1.
Mike also sees Alice's request.
Mike offers Alice an over-collateralized loan at 8% interest, and AGIX at 3/1 and Dust at 10/1.
Alice accepts Bob's offer and locks 140 AGIX and 150 DUST as collateral.
The total amount owed by Alice is 110 ADA.
Mike sees that Alice accepted another offer and closes his own offer.
After 30 slots, Alice repays half the loan and reclaims half her collateral.
Alice's new outstanding balance is 55 ADA and the Active UTxO now has 70 AGIX, 75 DUST, and 55 ADA.
Alice's plans go awry and she defaults on the remainder of the loan.
Once the loan is expired, Bob claims all remaining assets in the Active UTxO.
Alice's default is now recorded on-chain for future lenders to consider.
```


## Features Discussion

Here are some unique features that distinguish Cardano-Loans from other lending/borrowing protocols:

### On-Chain Emergent Credit History

Beacon Tokens can be used as "DID-like" identifiers that attest the (current and past) borrowing
history of their associated credential. The Borrowers' credit history naturally emerges thanks to
the unique properties of fully paid loans and defaulted loans:
- When a loan is fully paid off, the BorrowerID must be burned in isolation - no other tokens can be
minted or burned in the same Tx. Therefore, the number of unique tokens minted/burned will always be
**1**.
- When a loan is defaulted on, the BorrowerID must be burned with at least an Active beacon and the
LoanIDs. Therefore, the number of unique tokens minted/burned will always be **>1**.

Using an off-chain API, it is easy to query whether a loan was repaid in full or defaulted on.
Although the contract logic treats all "defaulted" loans identically, it may be the case that the
loan was almost completely repaid. How much of a defaulted loan was repaid is easily queryable, so
the mere fact of a default is **not** a binary indicator of a borrower's credit-worthiness. Lenders
and/or third-party rating agencies can use this history (possibly in combination with other factors,
such as an associated DID) to determine the credit-worthiness of a borrower. All current and past
loan conditions are visible to the third-party.

The table below shows which API endpoints are used for this with Blockfrost:
| Action | API Endpoint |
|--|--|
| Burn Txs | [API](https://docs.blockfrost.io/#tag/Cardano-Assets/paths/~1assets~1%7Basset%7D~1history/get) |
| Number of Unique Tokens Minted/Burned | [API](https://docs.blockfrost.io/#tag/Cardano-Transactions/paths/~1txs~1%7Bhash%7D/get) |
| Specific Loan Information | [API](https://docs.blockfrost.io/#tag/Cardano-Transactions/paths/~1txs~1%7Bhash%7D~1utxos/get) |

For the second API, the `asset_mint_or_burn_count` value will either be 1 or more, specifying a full
repayment or default, respectively. For the third API, the inputs with the BorrowerID token will
have the datums of the loans attached. Those datums have the terms of that specific loan (you will
also need to use [this
API](https://docs.blockfrost.io/#tag/Cardano-Scripts/paths/~1scripts~1datum~1%7Bdatum_hash%7D/get)
since Blockfrost only returns the hash of the datums in the last query).

Here is how to interpret the query results:
```
1) When a BorrowerID is burned in isolation, all outputs with an Active beacon token but missing a 
     BorrowerID are full payments. The other outputs can be ignored.
2) When a BorrowerID is burned along with other assets, all inputs with the BorrowerID are
     defaults. The other inputs can be ignored.
```

In addition to past loans, lenders can also see the borrower's current loans by looking up all UTxOs
with that borrower's ID beacon (they would all be located at the borrower's loan address). These can
only ever be open loans. This [Blockfrost
API](https://docs.blockfrost.io/#tag/Cardano-Addresses/paths/~1addresses~1%7Baddress%7D~1utxos~1%7Basset%7D/get)
will return that information.

### Trustless p2p Negotiations

Negotiation, acceptance, and repayment of loans occur fully p2p. All assets are always in control of
either the borrower or lender, no middleman contracts/addresses are necessary. Pretty much every
feature is negotiable: interest rates, collateralization, loan durations, etc. No other
lending/borrowing dApp comes close to the expressiveness of Cardano-Loans.

### Partial Repayments

Borrowers may repay their loans incrementally, and withdraw collateral in proportion to the amount
repaid. A loan is considered defaulted if it is not paid back in full by the agreed upon slot, at
which point the lender may claim any remaining collateral. The extent to which the loan was paid off
is visible on-chain, so not all defaults (should) impact creditworthiness in the same way.

### Endogenous Price & Interest Rate Discovery

Cardano-Loans is designed to be independent from the traditional financial system, in favor of
endogenously producing its own. As such, every piece of a loan, including the relative values of
assets to collateral, interest rates, and term length are all negotiated and agreed upon fully p2p -
no oracle feeds required. 

Although this (at first) presents a bootstrapping problem, it may be overcome by the fact that this
protocol may be the fastest way for the most financially underserved peoples to begin building a
credit history. Prospective borrowers are incentivized to build p2p relationships within a global
marketplace of prospective lenders, who themselves are incentivized to lend by the relatively high
rates that financially underserved borrowers would be willing to pay. Since Offers are public,
lenders can see all offers being made to the borrower, resulting in a healthy competition between
lenders.

With enough users & liquidity, this protocol may eventually *serve* as the de-facto oracle for
market-driven rate discovery.

### Tradable Bonds

Lenders can freely trade their Key NFTs on the secondary market. Buyers of the Key NFTs can update
the lender address for those loans in the same transaction where they purchase the Key NFTs.

### Staking Script Support

Both lenders and borrowers can use staking scripts as their credential (the former through the use
of the proxy script). This enables logic like multisig to govern the assets. This feature, when
combined with the on-chain credit history, opens the door to business being able to use
Cardano-Loans while still being compliant with the regulations of their jurisdiction. No other
Cardano DeFi dApp can say the same.

### Democratically Updatable

No one can force users to upgrade to new versions. The only restriction is that negotiations require
the same version to be used by both the lender and the borrower. As new versions come out, the
borrower's credit history is not lost as long as the same credential is used for both versions of
the protocol. The credit history is tied to the borrower's credential, not the protocol's version.

## FAQ

### Does psuedonymity incentivize borrowers to cheat lenders?

This is a common belief that does not match reality. As long as there is a strong enough reason to
be honest, psuedonymity does not lead to borrowers cheating lenders.

Consider the case where the borrower is their own lender and they use the same credential. While the
protocol allows the borrower to be their own lender, the borrower still cannot fake a full payment.
They must still repay the loan truthfully despite being their own lender. This approach can be used
by the borrower to build up good credit history. This is not a bad thing - traditional finance has
similar [mechanisms](https://www.investopedia.com/terms/s/securedcard.asp) where new borrowers can
borrower money from themselves for the sole reason of building up a credit history. Since most
lenders will likely require several years of good credit to qualify for under-collateralized loans,
there is a huge incentive for borrowers to keep the credit history, even if they "gamed" it in the
beginning. The dilemma is basically between: 

- Build up several years of good credit for a few big scores of ripping off a lender that gave you
an under-collateralized loan. 
- Build up several years of good credit for lifetime access to under-collateralized loans. 

If the borrower chooses the former, other lenders will see that they did this and will not offer
them under-collateralized loans going forward. If the borrower chooses the latter, they can retain
access to under-collateralized loans now and in the future. The economic benefits of
under-collaterized loans strongly incentivize borrowers to pick the second option. The fact that
this protocol uses psuedonymous identities does not alter the game theory.

## Conclusion

The Cardano-Loans protocol is a first-attempt at rethinking how the economy of Cardano could evolve.
It forgoes reliance on global/external token prices in favor of incentivizing the creation of a
CSL-native p2p credit-debt market. It is censorship-resistant, scalable, and straightforward in its
design. Wallets and other frontends can integrate the protocol into their UI, and even provide all
necessary API query services. Endogenous price & rate discovery empowers users to create their own
economy, one that is decoupled from the existing financial system, in pursuit of something more
fair, equal, and accessible to all.

