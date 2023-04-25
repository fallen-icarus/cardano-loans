# Cardano-Loans
Cardano-loans is a *distributed dApp* and thus adheres to the distributed dApp standard


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
Cardano-Loans is a proof-of-concept implementation of a *fully p2p* lending/borrowing protocol for the Cardano Settlement Layer (CSL). It empowers users to create and operate a CSL-native credit/debt market via trustlessly negotiable & repayable p2p loans, and on-chain credit history. This circumvents the need for oracles and/or concentrated lending pools in favor of *endogenous* price & interest-rate discovery. Users deploy (and interact with each others') script addresses, so full spending *and* delegation control of assets is maintained, and contract upgrades occur democratically.


## Motivation
A healthy credit-debt market is a vital (and often the largest) component of a thriving economy. A healthy market is one in which prices reflect underlying reality as quickly and fluidly (with as little friction) as possible. For the sake of avoiding a whole treatise on economics, suffice to say that *the best way to achieve fast and frictionless price discovery is through the aggregation of maximally expressive individual sentiments.* This is especially true for credit-debt markets, where money itself is the asset, priced via interest rates. 

Many lending/borrowing dApps on Cardano are implemented in controlled manners that limit users' flexibility in negotiating loan terms. This results in a sub-optimal expression of market sentiment. Furthermore, existing protocols rely on oracles for price feeds, which complicates the trust model, increases the dApp's attack surface, and subjects prices to a broader economy in which interest rates are not set by the market, but by central actors. 

Protocols that offer an alternative to this status quo will likely be perceived as a threat by those in positions of high power, so censorship resistance is an essential feature. Fully p2p dApps, such as Cardano-Loans, offer the highest level of censorship-resistance of any dApp architecture, because users not only have fully custody of their assets, but can also fractionalize and recompose their interactions across an already decentralized ledger, instead of pooling their assets into one or a few contracts.

Finally, p2p lending dApps may be the fastest way for the most financially underserved peoples to begin building credit scores. Those who *need* atypical financing options are in the best position to begin bootstrapping the system, as they would be willing to pay higher interest rates than any other group. This in turn incentivizes yield-seeking lenders to develop new strategies for such unprecedented conditions. In time, borrowers and lenders can build p2p relationships, and begin dis-intermediating the banking system.


## Preliminary Discussion
To appreciate the necessity for new lending/borrowing protocols on Cardano, it is first important to understand the deficiencies of the status quo:

### Current Lending/Borrowing dApp Deficiencies

1. **Oracles** - dApps that rely on off-chain information feeds are subject to the integrity of the underlying oracle network. Oracles are a nuanced topic that is beyond the scope of this document, but in short, oracles increase the attack surface of dApps, and have a long way to go before they can be safely relied upon by ledger-wide distributed dApps.

2. **Non-negotiable Loan Terms** - lending/borrowing dApps that are mediated by a central contract/entity lack negotiability of all loan terms, resulting in inefficient markets. This is especially troubling for a credit-debt market, whose efficiency is a vital component to a healthy economy.
   
3. **Concentrated Lending Pools** - concentrated lending pools (or any concentrated dApp design) are a higher security risk compared to distributed dApps. Aside from catastrophic draining attacks, centralized dApps often have a complex security model that relies on additional entities like DAOs (and associated "dApp Tokens") for upgradeability. This not only increases the technical attack surface, but the social attack surface as well. Furthermore, concentrated dApps do not scale naturally compared to distributed dApp architectures, especially in the context of state channels, like Hydra. 
   
4. **Inefficient Markets** - concentrated lending pools set interest rates formulaically, rather than emergently. Although the formula may be transparent, a healthy economy *continuously discovers* prices and rates via real-time aggregation of individual interactions, not by formulas. There is no single formula that can optimally capture market sentiment. Even if there was, agreeing on it would be a great challenge in and of itself. 


### The Cardano-Loans Protocol
Cardano-Loans gives users the great power (and responsibility) to create a fully on-chain debt market. It is entirely divorced from "global" off-chain markets, in favor of a bottom-up, or *endogenous*, approach to price & interest rate discovery. 

###### The protocol is broadly comprised of three distinct phases:

1. **Ask Phase** - Alice initiates this phase by "broadcasting" her (easily queryable) loan request for a *specific quantity of a certain token, collateralized by a certain token, to be repaid over a specific timeframe.*

2. **Offer Phase** - Bob determines that Alice is creditworthy (via her on-chain credit history) and initiates an offer to fulfill the loan *in the amount and over a timeframe that Alice* has specified, *with a collateral type, relative price-point, collateral ratio and interest rate that Bob* specifies. 
   
3. **Active Phase** - Alice accepts the loan terms Bob has offered by spending his "Offer" UTxO, thereby gaining full control over the requested assets and locking her collateral. Alice may incrementally pay off the loan, and reclaim her collateral in proportion to the price-point and collateral-ratio agreed upon, until the loan is either fully paid off or expired. If the loan expires, Alice can no longer repay the outstanding amount, and Bob can claim the remaining collateral. 

All three of these phases are expanded upon in the [Specification Section](#specification) below

###### Distinguishing features specific to Cardano-loans:

1. **On-Chain Credit History** - the status/conditions of current & past loans associated with a borrower's address are easily queryable by prospective lenders.
2. **Trustless Negotiations** - all loan conditions are negotiated in a fully p2p fashion - interest rates, collateral (token type, relative price, collateralization ratio), and length of the loan - all are negotiable parameters.
3. **Partial Repayments** - borrowers can gradually repay loans and incrementally withdraw associated collateral in proportion to the collateral ratio agreed upon by both parties.
4. **No Auto-liquidation** - All loan terms (including relative token prices) are agreed upon explicitly by both parties, so "margin" is constant throughout the length of the loan, and does *not* change with global price movements. 
5. **Endogenous Price & Interest Rate Discovery** - instead of relying on oracles or trusted actors, relative token prices and interest rates are explicitly agreed upon during loan negotiations, resulting in true market-driven discovery. In due time and with enough users, sufficiently distributed dApps may *serve* as oracles, instead of having to consume them.


###### Features that Cardano-Loans shares with other *distributed dApp* protocols:

- **Full Custody** - lenders/borrowers always maintain full spending *and* delegation control over their assets.
- **Natural Concurrency** - throughput scales *with* the number of users. No specialized batchers/indexers are necessary, though they *may* be used in proprietary backends of large lending providers.
- **Frontend/Backend Agnosticism & Interoperability** - relatively straightforward integration with existing frontends (i.e. wallets). Third-party platforms can build business models atop open-contract p2p protocols. 
- **Censorship Resistance** - due to the aforementioned agnosticism, distributed p2p dApps offer the highest level of censorship-resistance of any dApp architecture.
- **No Superfluous "dApp" Tokens** - ADA is all you need to pay for script/TX fees. 
- **Democratic Upgradability** - users choose if/when to use new contracts.


## Specification
The dApp logic is composed of one minting policy and one validator script that work together. All loans, no matter how different the terms, use the same minting policy and validator script. 

### Loan Components:
This section outlines the different components (Tokens, datums, redeemers) of Cardano-loans. 

#### The Borrower's Address
Borrowers create a unique "loan" address - this is where negotiations take place *and* where collateral is kept until the loan is either repaid or expired. As is common in *distributed dApps*, all such "loan" addresses use the same validator script for the payment credential, and a unique, user-defined staking key for the staking credential. Owner-related actions are "overloaded" to the staking key *by* the validator script, so the user maintains full control of the address.

:note: v1.0 does not support staking scripts for the staking credential.

#### The Beacon Tokens
Cardano-loans uses 5 types of tokens:

1. **Ask Token** - demarcates UTxOs in the "Ask Phase"
2. **Offer Token** - demarcates UTxOs in the "Offer Phase"
3. **Active Token** - demarcates UTxOs in the "Active Phase"
4. **LenderID Token** - demarcates the lender's address and mediates their ability to interact with the loan
5. **BorrowerID Token** - demarcates the borrower's address and mediates their ability to interact with the loan. This token is also used to check the history of loans associated with a borrower's address and determine their creditworthiness.

In addition to mediating dApp logic, all these tokens double as Beacons that users can query (via existing APIs) to interact with each other, without relying on a specialized aggregator/batcher.

#### The Loan Datums
Three different sum-type inline datums (one for each of the phases: Ask, Offer, Active) are used for loan negotiations/agreements. Each of these will be expanded upon further below in the sections detailing each of the 3 phases. For reference, here is the code:

``` Haskell
data LoanDatum
  -- | The datum for the ask phase.
  = AskDatum 
      { askBeacon :: (CurrencySymbol,TokenName)
      , borrowerId :: (CurrencySymbol,TokenName)
      , loanAsset :: (CurrencySymbol,TokenName) -- ^ The asset that will be borrowed.
      , loanPrinciple :: Integer -- ^ The desired loan amount.
      , loanTerm :: POSIXTime  -- ^ How long the loan will be valid for.
      , collateral :: [(CurrencySymbol,TokenName)]  -- ^ A list of assets the borrower is willing to use
                                                    -- as collateral.
      }
  -- | The datum for the offer phase.
  | OfferDatum
      { offerBeacon :: (CurrencySymbol,TokenName)
      , lenderId :: (CurrencySymbol,TokenName)
      , loanAsset :: (CurrencySymbol,TokenName)
      , loanPrinciple :: Integer
      , loanTerm :: POSIXTime
      , loanInterest :: Rational
      , loanBacking :: Integer -- ^ How much of the loan needs to be collateralized. In units of the
                               -- loanAsset.
      , collateralRates :: [((CurrencySymbol,TokenName),Rational)] -- ^ Rates: collateralAsset/loanAsset
      }
  -- | The datum for the active phase. This also has information useful for the credit history.
  | ActiveDatum
      { activeBeacon :: (CurrencySymbol,TokenName)
      , lenderId :: (CurrencySymbol,TokenName)
      , borrowerId :: (CurrencySymbol,TokenName)
      , loanAsset :: (CurrencySymbol,TokenName)
      , loanPrinciple :: Integer
      , loanTerm :: POSIXTime
      , loanInterest :: Rational
      , loanBacking :: Integer
      , collateralRates :: [((CurrencySymbol,TokenName),Rational)]
      , loanExpiration :: POSIXTime
      , loanOutstanding :: Rational
      }
```
`POSIXTime` is an integer representing POSIX time down to the millisecond.
`Rational` is a fraction of integers. This is how decimals are represented by the dApp.

The datums are used as inline datums so that others can see the necessary information for negotiating and checking a borrower's credit history. Each of these datums will be covered as each loan phase is covered below.

#### The Minting Redeemers
Minting redeemers are introduced here, their usage is explained further below.

``` Haskell
-- | The redeemer for the beacons.
data BeaconRedeemer
  -- | Mint the ask token to the borrower's address.
  = MintAskToken PaymentPubKeyHash -- ^ Pubkey for the borrower's STAKING credential. Simplifies logic.
  -- | Mint the Offer Token and lender ID.
  | MintOfferToken PaymentPubKeyHash -- ^ Pubkey for lender ID.
  -- | Mint the active token and the borrower ID.
  -- The first pubkey is the borrower's. The second one is the lender's.
  | MintActiveToken PaymentPubKeyHash PaymentPubKeyHash
  -- | Burn any token/IDs.
  | BurnBeaconToken
```

#### The Loan Validator Redeemers
Validator redeemers are introduced here, their usage is explained further below.

``` Haskell
data LoanRedeemer
  = CloseAsk
  | CloseOffer
  | AcceptOffer
  | RepayLoan
  | Claim
```


### The Loan Lifecycle
Cardano-loans is broken up into three distinct phases:

#### Ask Phase
Prospective borrowers initiate the Ask Phase by minting an `Ask` Token. 

##### Ask Initiation
Minting a valid `Ask` token requires all of the following to be true:

1. The `MintAskToken` redeemer must be used.
2. One and only one token with the token name 'Ask' is minted by the minting policy in the transaction.
3. The `Ask` token must be minted to an address using the dApp's validator script as the payment credential.
4. The `Ask` token must be minted to an address using the staking pubkey that was supplied with the `MintAskToken` redeemer as the staking credential.
5. The `Ask` token must be stored with a valid `Ask` datum:
    - `askBeacon` == (beaconPolicyId, TokenName "Ask")
    - `borrowerId` == (beaconPolicyId, borrower's staking pubkey hash as a token name)
    - `loanPrinciple` > 0
    - `loanTerm` > 0
    - collateral list must not be empty
6. The receiving staking pubkey must sign the transaction.

##### Closing an Ask
If the borrower changes their mind about the loan they are asking for, they may close their Ask accordingly:

1. The `CloseAsk` redeemer must be used for the loan validator and the `BurnBeaconToken` redeemer must be used for the minting policy.
2. The datum attached to the UTxO must be an `AskDatum`. If it isn't, then this UTxO cannot possibly be an Ask.
3. The staking credential must signal approval.
    - pubkey must sign
    - staking script must be executed (this is in case the wrong address is accidentally configured)
4. All Ask Tokens among the transaction inputs must be burned.


#### Offer Phase
Prospective lenders initiate the Offer Phase by minting an `Offer` token. 

##### Offer Initiation
Minting a valid `Offer` token requires all of the following to be true:

1. The `MintOfferToken` redeemer must be used.
2. One and only one token with the token name 'Offer' is minted, *and* one and only one token with the pubkey hash supplied with the redeemer as the token name. This latter token is the `LenderID`.
3. Both the `Offer` and `LenderID` tokens must be minted to an address using the dApp's validator script as the payment credential.
4. Both the `Offer` and `LenderID` tokens must be minted to an address using a staking pubkey as the staking credential.
5. Both tokens must be stored in the same UTxO with the proper inline offer datum:
    - `offerBeacon` == (beaconPolicyId, TokenName "Offer")
    - `lenderId` == (beaconPolicyId, lender's payment pubkey hash as a token name)
    - `loanPrinciple` > 0
    - `loanTerm` > 0
    - `loanInterest` > 0
    - `loanBacking` > 0
    - collateralRates is not empty
    - all collateral rates must be > 0
6. The UTxO containing the datums and tokens must have the loan amount specified in the datum. If the `loanAsset` is ADA, then an additional 3 ADA is also required in the UTxO.
7. The lender's payment pubkey must sign the transaction.

A note on #6: by requiring the lender to store the loan amount with the Offer Token, the borrower is able to accept the loan without any other input from the lender. The additional 3 ADA is required when the loan asset is ADA because storing the beacons requires a minimum amount of ADA, which is unavailable to the borrower when they accept the loan. When the loan is accepted, it will be stored with at least three tokens (the Active token, the LenderID, and the BorrowerID). The minimum amount of ADA required for storing these three tokens is 2.844600 ADA, rounded to 3 for convenience.

Multiple lenders can make an offer to the borrower. Each lender will have their own "Offer UTxO" located at the borrower's address. The borrower can check his/her own loan address and see what other offers were made. From the Offer Datum, the borrower can see:
**1. What the loan's interest rate will be.**
**2. How much of the loan must be backed by collateral.**
**3. What value the lender considers the collateral assets to be.**

##### Closing an Offer
If the lender changes their mind about their offer *prior* to it being accepted by the borrower, or if the borrower accepts a different offer, the lender may close the offer and reclaim their "Offer UTxO" accordingly:

1. The `CloseOffer` redeemer must be used for the loan validator and the `BurnBeaconToken` redeemer must be used for the minting policy.
2. The datum attached to the UTxO must be an `OfferDatum`. If it isn't, then this UTxO cannot possibly be an Offer.
3. If the Offer beacon is present in the UTxO, this is a valid offer and the LenderID is guaranteed to be present. Additional checks are required in this situation:
    - The pubkey hash of the lender must sign the transaction. The pubkey hash can be gotten from the offer datum since the presence of the Offer Token means that the datum is properly configured.
    - All offer beacons in the transaction inputs must be burned.
    - All of the lender's IDs in the transaction inputs must be burned.
4. If the offer beacon is not present, the address owner gets custody by default. This scenario can only happen if the offer is not a valid offer.
5. The staking credential of the address must signal approval.

In this manner, **the lender maintains custody of the "Offer UTxO" despite the UTxO being located in the borrower's address.** Thanks to the LenderID beacon token, it is easy to query all UTxOs associated with that lender.

#### Active Phase
When a borrower is satisfied with an offer that has been made to them, they may accept it, thus transitioning the loan into the Active Phase:

##### Accepting a loan offer
Accepting an offer requires both the minting policy and the loan validator script. The idea is to consume 1 Ask Phase UTxO and 1 Offer Phase UTxO to produce 1 Active Phase UTxO.

- The `AcceptOffer` redeemer is used for the loan validator and the `MintActiveToken` redeemer is used for the minting policy.

###### Validator Checks:

1. The staking credential of the loan address must approve the transaction.
2. There are only two inputs from the loan address.
3. One of the inputs is an ask input with an Ask Datum and Ask beacon.
4. The other input is an offer input with an Offer Datum and Offer Beacon.
5. The ask input and the offer input must agree on the terms (the similar fields must have the same values).
6. No other beacons are allowed in the transaction inputs - this is to prevent double satisfaction.
7. There must/can only be one output to the loan address.
8. The transaction must specify invalid-before as the loan start time.
9. The output must contain the proper inline Active Datum:
    - activeBeacon == (fst $ askBeacon askDatum, TokenName "Active")
    - lenderId == lenderId offerDatum
    - borrowerId == borrowerId askDatum
    - loanAsset == loanAsset askDatum
    - loanPrinciple == loanPrinciple askDatum
    - loanTerm == loanTerm askDatum
    - loanInterest == loanInterest offerDatum
    - loanBacking == loanBacking offerDatum
    - collateralRates == collateralRates offerDatum
    - loanExpiration == loanTerm offerDatum + time specified by invalid-before in transaction
    - loanOutstanding == loanPrinciple askDatum * (1 + loanInterest offerDatum)
10. The amount of collateral posted must equal the loanBacking specified in the Offer Datum.
11. Only one Active Beacon can be minted in the transaction and is must be minted to the same address where the offer and ask inputs originate.

###### Minting Policy Checks:

1. Both the Ask Token and the Offer Token from the inputs must be burned in the transaction.
2. Must mint exactly **one** Active Token with the token name 'Active' and one BorrowerID with the staking pubkey specified in the redeemer as the token name.
3. The Active token, BorrowerID, and LenderID (from the offer input) must be stored in the loan address using the supplied staking pubkey as the staking credential.

 the `Invalid-before` parameter must be used because the script can only know the current time from the validity range. Using invalid-before tells the script that the loan starts as soon as the acceptance transaction is valid.

The collateral calculation is done by this function:
``` Haskell
-- | This checks that enough collateral is posted when a loan offer is accepted. It uses the
-- loanBacking to determine validity.
enoughCollateral :: Bool
enoughCollateral =
  let target = fromInteger (loanBacking offerDatum)
      foo _ acc [] = acc
      foo val !acc ((collatAsset,price):xs) =
        foo val
            (acc + fromInteger (uncurry (valueOf val) collatAsset) * recip price)
            xs
  in foo oVal (fromInteger 0) (collateralRates offerDatum) >= target
```

The `oVal` is the value of the output to the loan address. The `recip` function gives the reciprocal of the `Rational` fraction. In essence, this function does:

```
sum { collateralDeposited / collateralRate } >= loanBacking offerDatum
```

In summary, the borrower accepts a loan offer via a single transaction with one Offer input, one Ask input, and however many other inputs are necessary to fulfill the collateral requirements. The transaction outputs one (and only one) Active UTxO to the loan address, while the remaining funds (what is actually being borrowed) is output to any address, as specified by the borrower.

##### Making a Loan Payment
A loan payment can either be a partial payment or a full payment (relative to the remaining balance). In either case, the `RepayLoan` redeemer is used. The script can tell whether a partial or full payment is being made.

To make a payment, all of the following must be true:

1. The UTxO must have an Active Datum. If it doesn't, then it isn't an "Active UTxO".
2. The staking credential must approve the transaction.
3. If the Active Beacon is present in the UTxO, then this is a valid active loan:
    1. No other beacons can be in the transaction - this means only this Active Beacon, one BorrowerID, and one LenderID are present in the transaction.
    2. There is only one input from this loan address.
    3. The loan is not expired - using the `invalid-hereafter` option to tell the script what time it is.
    4. There is only one output to the loan address.
    5. The output must contain the same Active Datum as the input except the amount paid must be subtracted from the `loanOutstanding` field - the script calculates the new outstanding balance.
    6. If the new outstanding balance calculated by the script is <= 0, this is a full repayment:
        - All collateral in the UTxO is available to take.
        - The BorrowerID must be burned.
        - No other tokens can be minted/burned in the transaction.
        - The output to the address must contain the Active Beacon and the LenderID.
    7. If the new outstanding balance is > 0, this is a partial repayment:
        - No collateral can be taken.
        - The output to the address must have the Active Beacon, the BorrowerID, and the LenderID.
4. If the Active Beacon is missing, then this is not a valid active UTxO. The address owner gets custody of this invalid UTxO. The conditions for spending are satisfied by the presence of the active datum and the staking credential approving.

When a loan is fully paid off, only the BorrowerID must be burned (by itself, without other tokens being burned in the same TX). This makes it easy to check (in the future) whether the loan ended in default or was repaid.

Partial payments require consuming and re-outputing the BorrowerID to the loan address, while fully paying off the loan requires burning the BorrowerID.  Tight coupling of the minting policy with the validator script makes it impossible to subvert this by minting a spare BorrowerID. **The borrower must withdraw all of their collateral in the same transaction that the loan is fully paid off.** Once the BorrowerID is burned, it will no longer be possible to reclaim the collateral.

:note: The loan validator also checks that there is only one input from the loan address (even though it also checks the number of beacons present) to prevent invalid UTxOs from skewing the repayment calculations.

The `invalid-hereafter` option is used instead of `invalid-before` because it is impossible to tell the script that the time is earlier than it actually is with `invalid-hereafter`. The borrower cannot trick the script into thinking the loan is still active when it is actually expired. If `invalid-before` was used, it *would* be possible to tell the script an earlier time that it actually is.

##### Claiming expired or fully paid loans
The last redeemer is the `Claim` redeemer which allows the lender to claim expired or fully paid Active UTxOs. The minting policy is also used with the `BurnBeaconToken` redeemer.

To claim an active UTxO as the lender, all of the following must be true:

1. The UTxO must have an Active Datum. Otherwise, it is *not* an Active UTxO.
2. The input UTxO must have an Active Beacon. Otherwise, it is not a valid Active UTxO and can be claimed by the address owner using the `RepayLoan` redeemer.
3. No other beacons are allowed in the transaction inputs - this ensures only the Active Beacon and LenderID, (and in the case of a partial repayment, the BorrowerID) are present.
4. The loan must either be expired (specified by invalid-before) or the `loanOutstanding` in the datum must be <= 0 (fully paid off).
5. The Active Beacon must be burned.
6. The LenderID must be burned.
7. If the BorrowerID is still present, it must be burned too.
8. No other tokens can be minted/burned in the transaction.
9. The lender must sign the transaction.

As previously mentioned, using `invalid-before` makes it impossible to tell the script the current time is later than it actually is since the transaction would not be valid yet. This means the lender can not trick the script into thinking the loan is expired when it actually isn't.

The presence of the BorrowerID in an expired loan indicates a default. When collateral is reclaimed in a defaulted loan, the BorrowerID is burned along with two other tokens, totaling three. This on its own is enough for future lenders to easily query a borrower's history of repayments/defaults. 


## Features Discussion
Here are some unique features that distinguish Cardano-Loans from other lending/borrowing protocols:

### On-Chain Credit History
Beacon Tokens can be used as "DID-like" identifiers that attest the borrowing history of their associated address. Using an off-chain API, it is easy to query whether a loan was repaid in full or defaulted (indicated by the burning of three Beacon Tokens in the same TX). Although the contract logic treats all "defaulted" loans identically, it may be the case that the loan was almost completely repaid. How much of a defaulted loan was repaid is easily queryable, so the mere fact of a default is **not** a binary indicator of a borrower's credit-worthiness. Lenders and/or third-party rating agencies can use this history (possibly in combination with other factors, such as an associated DID) to determine the credit-worthiness of a borrower.

### Trustless p2p Negotiations
Negotiation, acceptance, and repayment of loans occur fully p2p. All assets are always in control of either the borrower or lender, no middleman contracts/addresses are necessary. Tokens in the "Offer" Phase are held in UTxOs that reside in the borrower's address. The borrower cannot *spend* this UTxO unless they proceed with moving the loan into the "Active" Phase, but they *do* have staking rights over the UTxO. Lenders are therefore incentivized to keep offer periods short, since the offer's stake is controlled by the borrower.

### Partial Repayments
Borrowers may repay their loans and reclaim collateral incrementally, proportional to the agreed upon collateral ratio. A loan is considered defaulted if it is not paid back in full by the agreed upon slot. However, the extent to which the loan was paid off is reflected on-chain, so not all defaults (should) impact creditworthiness in the same way.


### Endogenous Price & Interest Rate Discovery
Cardano-Loans is designed to be independent from the traditional financial system, in favor of endogenously producing its own. As such, every piece of a loan, including the relative values of assets & collateral, interest rates, and collateralization ratios are all negotiated and agreed upon fully p2p. No oracle feeds are necessary. 

Although this (at first) presents a bootstrapping problem, it may be overcome by the fact that this protocol may be the fastest way for the most financially underserved peoples to begin building a credit history. Prospective borrowers are incentivized to build p2p relationships within a global marketplace of prospective lenders, who themselves are incentivized to lend by the relatively high rates that financially underserved borrowers would be willing to pay.  

With enough users & liquidity, this protocol may eventually *serve* as the de-facto oracle for market-driven rate discovery.


## Future Directions and Considerations
Being a PoC, v1 of Cardano-loans is intended to demonstrate the capacity for fully p2p lending/borrowing on the CSL. As such, there are a number of features that may be implemented in future version of the protocol, discussed below.

Note: Cardano-Loans v1 is written in IOG's PlutusTx. Although this is a great choice for prototyping & auditing, it is very resource-intensive. Many of the features discussed in this section are bottlenecked by current script execution limits. Future increases to this limit, as well as utilizing newer, more resource-efficient languages (such as Aiken) can result in up to 100x more headroom for additional features. 

In the future, fully p2p contracts may utilize both approaches: an audit-friendly language (like PlutusTx) for security and ease of upgradeability, followed by translation to a resource-friendly language (like Aiken) for deployment at scale. 

### Potential Future Features
A non-exhaustive list of features that may be implemented in the future:

#### Multiple Collaterals
Loans can be collateralized by a number of different tokens, both of the fungible and non-fungible kind. This is especially useful as more and more real-world assets (such as real estate) become tokenized, in addition to existing digital-native assets.

#### Transferrable Credit/Debt
Borrowers may "refinance" their debt by selling it to another lender in exchange for a new loan with more favorable conditions. Lenders may also sell their credit to a willing third party. The *ability* for either party to engage in such transfers may itself be negotiated in advance. For example, a borrower may not want to ever be paying interest to certain lenders for ethical reasons, so they may want to restrict the initial lender's ability to transfer credit.

Note: This feature will likely require the introduction of additional Beacon Tokens.

#### Term Extensions/Renegotiations
A borrower may renegotiate an active loan with their lender, without closing or defaulting on the loan. This may be to "refinance" the loan, to negotiate a loan term extension, or for whatever other reasons the two parties may agree upon. All such actions would be queryable by prospective lenders in the future, giving them further insight into the nature/creditworthiness of the borrower.

#### Linkable Credit-History
By introducing additional Beacon Tokens (and associated standards), it may be possible to link together previously unrelated/pseudonymous borrower addresses into a set of *related* (but still pseudonymous) addresses, at the borrower's discretion.

#### DID Compatibility
Upon the maturation of standards, decentralized identifiers (DIDs) can be incorporated with Cardano-loans, further amplifying utility and interoperability.

#### Support for Staking Scripts
Due to the aforementioned execution limit, only staking keys may be used for the staking credential; staking scripts are currently unsupported. Support for staking scripts easily be added in the future.

:note: Technically, the loan validator already has logic *in case* a staking script is used, but this is currently only to prevent accidental locking if the wrong address is configured.


### Other Considerations


#### Double Satisfaction Problem
WIP - Redundant Script Execution CPS?


#### Consolidation of Collateral-Ratio & Relative-Price parameters
There was some internal debate about whether or not it is redundant to  have *both* the `collateralization-ratio` *and* `relative-price` fields as part of the loan terms. Arguments for both sides are summarized below, and **community feedback would be especially helpful on resolving this matter.**

###### Argument against having both parameters, in favor of consolidation:
Since both parties must agree on relative token prices during negotiation, collateralization-ratio is already implied in the agreed upon price. For example, if Alice creates an Ask to borrow ADA collateralized by AGIX, and Bob decides that 1 ADA is worth 4 AGIX, and he is willing to offer her a loan for 50% collateral, Bob can simply offer her a loan:collateral ratio of 1 ADA : 2 AGIX. There is no need for an explicit distinction between relative token price and collateralization-ratio because Bob's perception of the tokens' relative values *and* Alice's creditworthiness is implictly captured by his Offer.

Since Bob can *losslessly* express his offer using just one field, requiring both is not only redundant, but may even be obfuscatory. If *effectively* the same loan can be negotiated with different parameters, there would be noise introduced for any lenders/data-miners that are modeling whatever is going on. Since there is no functional benefit from having both fields, consolidation into a single `loan-collateral-ratio` field helps with consistency for all parties involved.

###### Objection to the above argument, in favor of keeping both parameters:
Although a single field is enough to losslessly convey all necessary information, it is not user friendly for any of the parties involved:
- The borrower cannot know *why* the offer is what is it is, because the extent to which the borrower's credit-worthiness *versus* the lender's perceived relative token price was into account is not made explicit.
- The lender must take into account a lot of factors on their end, including current "global" price, relative price, borrower's creditworthiness (and all associated on-chain analytics), express all of this with a single ratio. 
- The third-party data miner, much like the borrower, also does not know *why* the lender made the offer that they did. This would have to be inferred based on all the same information that the lender had access to at the time the offer was made. 



## Conclusion
The Cardano-Loans protocol is a first-attempt at rethinking how the economy of Cardano could evolve. It forgoes reliance on global/external token prices in favor of incentivizing the creation of a CSL-native p2p credit-debt market. It is censorship-resistant, scalable, and straightforward in its design. Wallets and other frontends can integrate the protocol into their UI, and even provide all necessary API query services. Endogenous price & rate discovery empowers users to create their own economy, one that is (at least initially) decoupled from the existing financial system, in pursuit of something more fair, equal, and accessible to all.

