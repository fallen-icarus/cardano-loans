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
Cardano-Loans is a proof-of-concept implementation of a *fully p2p* lending/borrowing protocol for the Cardano Settlement Layer (CSL). It empowers users to create and operate a CSL-native credit/debt market via trustlessly negotiable & repayable p2p loans, and on-chain credit history. This circumvents the need for oracles and/or concentrated lending pools in favor of *endogenous* price & interest-rate discovery. Users deploy (and interact with each others') script addresses, so full spending *and* delegation control of assets is maintained, and contract upgrades occur democratically.

## Motivation
A healthy credit-debt market is a vital (and often the largest) component of a thriving economy. A healthy market is one in which prices reflect underlying reality as quickly and fluidly (with as little friction) as possible. For the sake of avoiding a whole treatise on economics, suffice to say that *the best way to achieve fast and frictionless price discovery is through the aggregation of maximally expressive individual sentiments.* This is especially true for credit-debt markets, where money itself is the asset, priced via interest rates. 

Many lending/borrowing dApps on Cardano are implemented in controlled manners that limit users' flexibility in negotiating loan terms. This results in a sub-optimal expression of market sentiment. Furthermore, existing protocols rely on oracles for price feeds, which complicates the trust model, increases the dApp's attack surface, and subjects prices to a broader economy in which interest rates are not set by the market, but by central actors. 

Protocols that offer an alternative to this status quo will likely be perceived as a threat by those in positions of high power, so censorship resistance is an essential feature. Fully p2p dApps, such as Cardano-Loans, offer the highest level of censorship-resistance of any dApp architecture, because users not only have full custody of their assets, but can also fractionalize and recompose their interactions across an already decentralized ledger, instead of pooling their assets into one or a few contracts.

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

##### The protocol is broadly comprised of three distinct phases:

1. **Ask Phase** - Alice initiates this phase by "broadcasting" her (easily queryable) loan request for a *specific quantity of a certain token, collateralized by a certain token, to be repaid over a specific timeframe.*

2. **Offer Phase** - Bob determines that Alice is creditworthy (via her on-chain credit history) and initiates an offer to fulfill the loan *in the amount and over a timeframe that Alice* has specified, *with a collateral ratio and interest rate that Bob* specifies. 
   
3. **Active Phase** - Alice accepts the loan terms Bob has offered by spending his Offer UTxO, thereby gaining full control over the requested assets, and locking her collateral into a time-locked Active UTxO. Alice may incrementally pay off the loan, and can reclaim her collateral once the loan is fully paid off. If the loan expires, Alice can no longer repay the outstanding amount, and Bob can claim the collateral. Note that Alice maintains staking rights over her locked collateral through the length of the loan.

All three of these phases are expanded upon in the [Specification Section](#specification) below

##### Distinguishing features specific to Cardano-Loans:

1. **On-Chain Credit History** - the status/conditions of current & past loans associated with a borrower are easily queryable by prospective lenders and third-party data miners.
2. **Trustless Negotiations** - all loan conditions are negotiated in a fully p2p fashion - interest rates, collateral (token type(s) and relative prices), and length of the loan - all are negotiable parameters. Multiple tokens can be used as collateral, even NFTs. Over/under collateralization is implied by the relative prices in the lender's offer, which is impacted by the borrower's credit history.
3. **Partial Repayments** - borrowers can repay loans incrementally and withdraw collateral in proportion to the repayment. If an outstanding balance remains at expiry, the lender may withdraw the remaining collateral. 
4. **No Auto-liquidation** - All loan terms (including relative token prices) are agreed upon explicitly by both parties, so "margin" is constant throughout the length of the loan, and does *not* change with global price movements. 
5. **Endogenous Price & Interest Rate Discovery** - instead of relying on oracles or trusted actors, relative token prices and interest rates are explicitly agreed upon during loan negotiations, resulting in true market-driven discovery. In due time and with enough users, sufficiently distributed dApps may *serve* as oracles, instead of having to consume them.


##### Features that Cardano-Loans shares with other *distributed dApp* protocols:

- **Full Custody** - users always maintain full spending *and* delegation control over their assets.
- **Natural Concurrency** - throughput scales *with* the number of users. No specialized batchers/indexers are necessary, though they *may* be used in proprietary backends of large lending providers.
- **Frontend/Backend Agnosticism & Interoperability** - relatively straightforward integration with existing frontends (i.e. wallets). Third-party platforms can build business models atop open-contract p2p protocols. 
- **Censorship Resistance** - due to the aforementioned agnosticism, distributed p2p dApps offer the highest level of censorship-resistance of any dApp architecture.
- **No Superfluous "dApp" Tokens** - ADA is all you need to pay for script/TX fees. 
- **Democratic Upgradability** - users choose if/when to use new contracts.


## Specification
The dApp logic is composed of one minting policy and one validator script that work together. All loans, no matter how different the terms, use the same minting policy and validator script. 

### Loan Components:
This section outlines the different components (tokens, datums, redeemers, time, interest) of Cardano-Loans. 

#### The Borrower's Address
Borrowers create a unique "loan" address - this is where negotiations take place *and* where collateral is kept until the loan is either repaid or expired. As is common in *distributed dApps*, all such "loan" addresses use the same validator script for the payment credential, and a unique, user-defined staking key for the staking credential. Owner-related actions are "overloaded" to the staking key *by* the validator script, so the user maintains full control of the address.

Since loan negotiations and repayments occur in the borrower's address, the borrower maintains delegation control of all assets throughout the life cycle of the loan. For the majority of the time, the only assets located in the address will be the collateral which belong to the borrower unless a default occurs. Therefore, giving delegation control to the borrower seems like the most appropriate configuration. This design choice also incentivizes lenders to *not* use loan addresses as their own personal addresses. In other words, lenders are incentivized to reclaim all their assets as soon as possible so that they do not miss out on staking rewards.

:warning: v1.0 does not support staking scripts for the staking credential. All loans are required to use a pubkey for the borrower's staking credential. This restriction helped to simplify the logic a bit. If the address uses a staking script, these UTxOs can still be recovered but no loans can be made to these addresses. The beacon policy will not mint to an address unless it is using a staking pubkey. Since these loans are meant to be peer-to-peer and not peer-to-dApp, this seemed fine for a v1 design. A future version can allow the use of staking scripts.

:warning: If, at any point, a misconfigured UTxO is locked at a borrower's address, the borrower will get custody. These UTxOs can only come about by misusing the dApp. As long as beacon tokens are always minted when locking a UTxO, these misconfigured UTxOs can never occur.

#### Telling Time
Cardano currently has no sense of time outside of transaction validity intervals. However, these validity intervals are more than enough for telling the dApp information about the current time. The dApp forces the use of `invalid-before` or `invalid-hereafter` when appropriate. By marrying incentives with these bounds, these flags can be used to securely tell the dApp whether or not a certain time has passed.

Consider the (inclusive) slot interval `[5,infinity)`. This transaction will only be valid if slot 5 is currently happening or it has passed. A plutus script can interpret this information as "the current time is guaranteed to be >= slot 5". Cardano-Loans forces the use of `invalid-before` when accepting a loan (specifying the loan start time) and claiming a loan (specifying whether a loan is expired).

Take these two scenarios separately:
- Borrower accepts loan - The borrower cannot access the funds until the loan starts so they want to set the time as early as possible. However, the loan expiration is calculated by adding the loan term length to the start time. If the borrower sets the time to be earlier than it actually is, the loan expiration would also be earlier than it otherwise would be. This means less time for the loan than the borrower wants. If the borrower sets the time to be later than it actually is so that the loan expiration would be later than it otherwise would be, this transaction would not be valid until the start time *actually* occurs. This means the borrower would only be delaying when they can access the loan asset. Therefore, the borrower is incentivized to set `invalid-before` to be as close to the current time as possible.
- Lender claims a finished loan - The lender cannot claim a loan unless it is either fully paid or it is expired. Only expiration depends on time. If the lender set the time to be earlier than it actually is, then the dApp will think the loan is still active and therefore the lender cannot claim it. If the lender set the time to be later than it actually is, then the validity interval would say the transaction isn't valid yet which means they need to wait longer to claim what they are owed. These two together result in the lender being incentivized to set `invalid-before` to be as close as the current time as possible.

Now consider the (inclusive) slot interval `[0,10]`. This transaction will only be valid until slot 10 finishes. The dApp can interpret this information as "the current time is guaranteed to be <= slot 10". This information is useful when a borrower tries to make a payment. The dApp forces the borrower to specify the "current" time with `invalid-hereafter` so that the dApp can appropriately tell whether the loan is expired. Since the borrower wants as much time with the loan as possible, they are incentivized to set the upper bound as late as possible. However, the dApp compares the upper bound to the loan expiration slot. If the upper bound is > the expiration slot, then the dApp will think the loan is expired. Therefore, the latest possible time the borrower can set `invalid-hereafter` to is the loan expiration slot.

Thanks to marrying incentives with dApp logic, it is impossible for users to cheat time.

#### Interest
Unlike other lending/borrowering protocols, Cardano-Loans does not use an algorithm to determine the interest for each loan. Instead, the interest is negotiated between the borrowers and lenders (among other negotiated terms).

**All interest on Cardano-Loans is non-compounding.** While `invalid-hefeafter` can be securely used to determine whether a loan is expired, there is no way to securely determine how many compounding periods have passed using just the validity intervals. If `invalid-hereafter` is set as close to the current time as possible, then it is possible for the transaction to fail due to sitting in the mempool for too long. If `invalid-before` was used, the value can be used to trick the dApp into thinking the compounding period is earlier than it actually is. The validity intervals alone can't be reliably used in this context. 

There is a way to still enable compound interest with this design, but it adds complexity to the overall design. Since lenders can compensate for the lack of compounding by asking for slightly higher interest rates, the loss of functionality due to a lack of compounding interest seems minor. Therefore, the decision was made to not include compound interest in v1 of this dApp. The method to enable compound interest is discussed in the *Potential Future Features* section of the README. 

Given that loans are non-compounding, the total amount owed for all loans is always:
```
principle * (1 + interest)
```

#### The Beacon Tokens
Cardano-Loans uses 5 types of tokens:

1. **Ask Token** - demarcates UTxOs in the "Ask Phase"
2. **Offer Token** - demarcates UTxOs in the "Offer Phase"
3. **Active Token** - demarcates UTxOs in the "Active Phase"
4. **LenderID Token** - demarcates the lender's payment pubkey and mediates their ability to interact with the loan UTxOs.
5. **BorrowerID Token** - demarcates the borrower's staking pubkey and mediates their ability to interact with the loan UTxOs. This token is also used to check the history of loans associated with a borrower and determine their creditworthiness.

In addition to mediating dApp logic, all these tokens double as Beacons that users can query (via existing APIs) to interact with each other, without relying on a specialized aggregator/batcher.

#### The Loan Datums
Three different inline datums (one for each of the phases: Ask, Offer, Active) are used for loan negotiations/agreements. Each of these will be expanded upon further below in the sections detailing each of the 3 phases. For reference, here is the code:

``` Haskell
data LoanDatum
  -- | The datum for the ask phase.
  = AskDatum 
      { loanBeaconSym :: CurrencySymbol -- ^ Policy Id of the beacon minting policy.
      , borrowerId :: TokenName -- ^ The staking pubkey hash for the borrower.
      , loanAsset :: (CurrencySymbol,TokenName) -- ^ The asset to be loaned out.
      , loanPrinciple :: Integer -- ^ The amount of the loan asset to be loaned out.
      , loanTerm :: POSIXTime -- ^ The length of time where the loan will be active.
      , collateral :: [(CurrencySymbol,TokenName)] -- ^ All assets the borrower is willing to use as
                                                   -- collateral.
      }
  -- | The datum for the offer phase.
  | OfferDatum
      { loanBeaconSym :: CurrencySymbol -- ^ Policy Id of the beacon minting policy.
      , lenderId :: TokenName -- ^ The payment pubkey hash for the lender.
      , loanAsset :: (CurrencySymbol,TokenName) -- ^ The asset to be loaned out.
      , loanPrinciple :: Integer -- ^ The amount of the loan asset to be loaned out.
      , loanTerm :: POSIXTime -- ^ The length of time where the loan will be active.
      , loanInterest :: Rational -- ^ The non-compounding interest rate.
      , collateralization :: [((CurrencySymbol,TokenName),Rational)]
          -- ^ The relative value for each collateral asset according to the lender.
      }
  -- | The datum for the active phase. This also has information useful for the credit history.
  | ActiveDatum
      { loanBeaconSym :: CurrencySymbol -- ^ Policy Id of the beacon minting policy.
      , lenderId :: TokenName -- ^ The payment pubkey hash for the lender.
      , borrowerId :: TokenName -- ^ The staking pubkey hash for the borrower.
      , loanAsset :: (CurrencySymbol,TokenName) -- ^ The asset to be loaned out.
      , loanPrinciple :: Integer -- ^ The amount of the loan asset to be loaned out.
      , loanTerm :: POSIXTime -- ^ The length of time where the loan will be active.
      , loanInterest :: Rational -- ^ The non-compounding interest rate.
      , collateralization :: [((CurrencySymbol,TokenName),Rational)]
          -- ^ The relative value for each collateral asset according to the lender.
      , loanExpiration :: POSIXTime -- ^ The time at which the loan will expire.
      , loanOutstanding :: Rational -- ^ The balance still owed on the loan.
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
  = MintAskToken 
      PaymentPubKeyHash -- ^ Pubkey for the borrower's STAKING credential. Simplifies logic.
  
  -- | Mint the Offer Token and lender ID.
  | MintOfferToken 
      PaymentPubKeyHash -- ^ Payment pubkey for lender ID.

  -- | Mint the active token and the borrower ID.
  | MintActiveToken 
      PaymentPubKeyHash  -- ^ This pubkey is the borrower's staking pubkey.
      PaymentPubKeyHash  -- ^ This pubkey is the lender's payment pubkey.

  -- | Burn any tokens/IDs.
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
Cardano-Loans is broken up into three distinct phases:

#### Ask Phase
Prospective borrowers initiate the Ask Phase by minting an `Ask` Token and storing it inside their borrower address with the desired ask terms.

##### Ask Initiation
Minting a valid `Ask` token requires all of the following to be true:

1. The `MintAskToken` redeemer must be used.
2. One and only one token with the token name 'Ask' is minted by the minting policy in the transaction.
3. The `Ask` token must be minted to an address using the dApp's validator script as the payment credential.
4. The `Ask` token must be minted to an address using the staking pubkey that was supplied with the `MintAskToken` redeemer as the staking credential.
5. The `Ask` token must be stored with a valid inline `AskDatum`:
    - `loanBeaconSym` == beaconPolicyId
    - `borrowerId` == borrower's staking pubkey hash as a token name
    - `loanPrinciple` > 0
    - `loanTerm` > 0
    - `collateral` list must not be empty
6. The receiving staking pubkey must sign the transaction.

The beacon policy forces the receiving address to use a pubkey for the staking credential.

The borrower is able to use multiple assets as collateral for a given loan. Whatever assets can be used (but doesn't have to be) must appear in the `collateral` list.

The signature requirement means that no one can open a loan under the borrower's ID except the borrower. In other words, unless the borrower's staking key is stolen, identity theft is cryptographically prevented.

##### Closing an Ask
If the borrower changes their mind about the loan they are asking for, they may close their Ask accordingly:

1. The `CloseAsk` redeemer must be used for the loan validator and the `BurnBeaconToken` redeemer must be used for the minting policy.
2. The datum attached to the UTxO must be an `AskDatum`. If it isn't, then this UTxO cannot possibly be an Ask since real Asks have both an `AskDatum` and an Ask token.
3. The staking credential must signal approval.
    - pubkey must sign
    - staking script must be executed (this is in case the wrong address is accidentally configured)
4. All Ask Tokens among the transaction inputs must be burned.

The presence of the Ask token is not checked here. The reason for this is because the address owner gets custody of both valid Ask UTxOs and misconfigured Ask UTxOs. If a UTxO has an `AskDatum` but is missing an Ask token, then the address owner can spend the UTxO by signalling approval with the staking credential. If a UTxO has an `AskDatum` and an Ask token, the address owner still has custody and can spend the UTxO by signing with the staking pubkey (the Ask token can only be minted to an address using a staking pubkey). Therefore, in either case, the staking credential must approve; there is no need to check for the Ask token.

This redeemer can be used to prevent accidental locking of any misconfigured UTxOs that have an `AskDatum`. It is for this reason that this redeemer checks for both a staking pubkey and a staking script (the latter case is if the address is configured without a staking pubkey).

#### Offer Phase
Prospective lenders initiate the Offer Phase by minting an `Offer` token and storing it in the target borrower's address with the desired offer terms.

##### Offer Initiation
Minting a valid `Offer` token requires all of the following to be true:

1. The `MintOfferToken` redeemer must be used.
2. One and only one token with the token name 'Offer' is minted, *and* one and only one token with the pubkey hash supplied with the redeemer as the token name. This latter token is the `LenderID`.
3. Both the `Offer` and `LenderID` tokens must be minted to an address using the dApp's validator script as the payment credential.
4. Both the `Offer` and `LenderID` tokens must be minted to an address using a staking pubkey as the staking credential.
5. Both tokens must be stored in the same UTxO with the proper inline `OfferDatum`:
    - `loanBeaconSym` == beaconPolicyId
    - `lenderId` == lender's payment pubkey hash as a token name
    - `loanPrinciple` > 0
    - `loanTerm` > 0
    - `loanInterest` > 0
    - `collateralization` list must not empty
    - all collateral relative values must be > 0
6. The UTxO containing the datum and tokens must have the loan amount specified in the datum. If the `loanAsset` is ADA, then an additional 3 ADA is also required in the UTxO.
7. The lender's payment pubkey must sign the transaction.

The `collateralization` field is how much of that collateral asset the lender wants per unit of the loan asset taken. This is always in units of `collateralAsset/loanAsset`. So if the collateralization for AGIX/ADA is 2/1, then the borrower must put up 2 AGIX for every ADA borrowed. If more than one collateral asset is possible, then the collateral can be used in any combination where the total relative value equals the loan principle. For example, if DUST is also allowed at a ratio of 10/1, then 10 ADA can be borrowed by putting up 10 AGIX and 50 DUST:
```
10 / 2 + 50 / 10 = 5 + 5 = 10
```

The lender can offer under-collateralized loans and over-collateralized loans by setting the relative values for the collateral to be under the current market value or over the current market value, respectively.

The `collateralization` list must be in the same order (based on collateral name) as the borrower's `collateral` list. This is due to how `AcceptOffer` checks if the borrower and lender agree on the terms.

If a lender does not want a certain collateral asset to be used, the lender can set an unreasonably high relative value for that collateral to disincentivize the borrower from using it. There is currently no other way to disallow a certain asset. A future version can address this.

A note on #6, by requiring the lender to store the loan amount with the Offer Token, the borrower is able to accept the loan without any other input from the lender. The additional 3 ADA is required when the loan asset is ADA because storing the beacons requires a minimum amount of ADA, which is unavailable to the borrower when they accept the loan. When the loan is accepted, it will be stored with at least three tokens (the Active token, the LenderID, and the BorrowerID). The minimum amount of ADA required for storing these three tokens is 2.844600 ADA, rounded to 3 ADA for convenience.

The lender must sign the transaction with the pubkey hash used for the Lender ID so that the Lender ID beacon token will be cryptographically guaranteed to be unique to that lender.

Multiple lenders can make an offer to the borrower. Each lender will have their own "Offer UTxO" located at the borrower's address. The borrower can check his/her own loan address and see what other offers were made. From the `OfferDatum`s, the borrower can see:
**1. What the loan's interest rate will be.**
**2. What the collateralization ratio is for each collateral asset.**
This breeds natural competition between lenders.

Even though the lender's assets will be at the borrower's address, the lender will maintain spending custody for their Offer UTxO. The only way the borrower can spend the Offer UTxO is if they are accepting the lender's offer and initiating the loan. Thanks to the Lender ID being a beacon token, it is easy to query all UTxOs associated with that lender.

##### Closing an Offer
If the lender changes their mind about their offer *prior* to it being accepted by the borrower, or if the borrower accepts a different offer, the lender may close the offer and reclaim their Offer UTxO accordingly:

1. The `CloseOffer` redeemer must be used for the loan validator and the `BurnBeaconToken` redeemer must be used for the minting policy.
2. The datum attached to the UTxO must be an `OfferDatum`. If it isn't, then this UTxO cannot possibly be an Offer.
3. If the Offer beacon is present in the UTxO, this is a valid offer and the LenderID is guaranteed to be present. Custody belongs to the lender. Additional checks are required in this situation:
    - The pubkey hash of the lender must sign the transaction. The pubkey hash can be gotten from the offer datum since the presence of the Offer Token means that the datum is properly configured.
    - All offer beacons in the transaction inputs must be burned.
    - All of the lender's IDs in the transaction inputs must be burned.
4. If the offer beacon is not present, the address owner gets custody by default. This scenario can only happen if the offer is not a valid offer.
    - The staking credential of the address must signal approval.

Spending custody for the lender is enforced by requiring the lender's signature when appropriate.

This redeemer can be used by the address owner to claim any misconfigured UTxOs that have an `OfferDatum`.

#### Active Phase
When a borrower is satisfied with an offer that has been made to them, they may accept it, thus transitioning the loan into the Active Phase:

##### Accepting a loan offer
Accepting an offer requires both the minting policy and the loan validator script. The idea is to consume 1 Ask Phase UTxO and 1 Offer Phase UTxO to produce 1 Active Phase UTxO.

- The `AcceptOffer` redeemer is used for the loan validator and the `MintActiveToken` redeemer is used for the minting policy.

###### Validator Checks:

1. The staking credential of the loan address must approve the transaction.
2. There are only two inputs from the loan address.
3. One of the inputs is an ask input with an `AskDatum` and Ask beacon.
4. The other input is an offer input with an `OfferDatum` and Offer Beacon.
5. The ask input and the offer input must agree on the terms (the similar fields must have the same values).
    - collateral askDatum == map fst (collateralizaton offerDatum)
6. No other beacons are allowed in the transaction inputs - this is to prevent double satisfaction.
7. There must/can only be one output to the loan address.
8. The transaction must specify `invalid-before` as the loan start time.
9. The output must contain the proper inline `ActiveDatum`:
    - loanBeaconSym == loanBeaconSym askDatum
    - lenderId == lenderId offerDatum
    - borrowerId == borrowerId askDatum
    - loanAsset == loanAsset askDatum
    - loanPrinciple == loanPrinciple askDatum
    - loanTerm == loanTerm askDatum
    - loanInterest == loanInterest offerDatum
    - collateralization == collateralization offerDatum
    - loanExpiration == loanTerm offerDatum + time specified by invalid-before in transaction
    - loanOutstanding == loanPrinciple askDatum * (1 + loanInterest offerDatum)
10. The amount of collateral posted must equal the loanPrinciple specified in the `OfferDatum`.
11. Only one Active Beacon can be minted in the transaction and it must be minted to the same address where the offer and ask inputs originate.

###### Minting Policy Checks:

1. Both the Ask Token and the Offer Token from the inputs must be burned in the transaction.
2. Must mint exactly **one** Active Token with the token name 'Active' and one BorrowerID with the staking pubkey specified in the redeemer as the token name.
3. The Active token, BorrowerID, and LenderID (from the offer input) must be stored in the loan address using the supplied staking pubkey as the staking credential.

In essence, the collateral calculation is this:
```
sum { collateralDeposited / collateralRate } >= loanPrinciple offerDatum
```

In summary, the borrower accepts a loan offer via a single transaction with one Offer UTxO, one Ask UTxO, and however many other inputs are necessary to fulfill the collateral requirements. The transaction outputs one, and only one, Active UTxO to the loan address, while the remaining funds (what is actually being borrowed) is output to any address, as specified by the borrower.

There are no checks to ensure that the borrower only takes the loan asset from the Offer UTxO. It is assumed that the lender put just the loan amount (and the beacons) in the UTxO in the first place.

##### Making a Loan Payment
A loan payment can either be a partial payment or a full payment (relative to the remaining balance). In either case, the `RepayLoan` redeemer is used. If a full payment is being made, the `BurnBeaconToken` redeemer is required for the minting policy. The dApp can tell whether a partial or full payment is being made.

To make a payment, all of the following must be true:

1. The UTxO must have an `ActiveDatum`. If it doesn't, then it isn't an Active UTxO.
2. The staking credential must approve the transaction.
3. If the Active Beacon is present in the UTxO, then this is a valid active loan:
    1. No other beacons can be in the transaction - this means only this Active Beacon, one BorrowerID, and one LenderID are present in the transaction.
    2. There is only one input from this loan address.
    3. The loan is not expired - using the `invalid-hereafter` option to tell the dApp what time it is.
    4. There is only one output to the loan address.
    5. The output must contain the same `ActiveDatum` as the input except the amount paid must be subtracted from the `loanOutstanding` field - the dApp calculates the new outstanding balance.
    6. If the new outstanding balance calculated by the dApp is <= 0, this is a full repayment:
        - All collateral in the Active UTxO is available to take.
        - The BorrowerID must be burned.
        - No other tokens can be minted/burned in the transaction.
        - The output to the address must contain the Active Beacon and the LenderID.
    7. If the new outstanding balance is > 0, this is a partial repayment:
        - The collateral can be reclaimed proportionally to how much was repaid.
        - The output to the address must have the Active Beacon, the BorrowerID, and the LenderID.
4. If the Active Beacon is missing, then this is not a valid Active UTxO. The address owner gets custody of this invalid UTxO. The conditions for spending are satisfied by the presence of the `ActiveDatum` and the staking credential approving.

The collateral that can be reclaimed during a partial payment is determined by the following equation:
```
sum (collateralTaken / collateralization * (1 + interest)) <= loanRepaid
```
In essence, the relative value of the collateral must be <= the value of the loan asset returned. The dApp does not check if the borrower takes any collateral. It only checks if the borrower took *too much* collateral. If the borrower misses an opportunity to reclaim some collateral, that proportion can only be reclaimed when the loan if fully paid off. A future version can add safeguards so that a partial payment fails unless some collateral is taken.

When a loan is fully paid off, the BorrowerID must be burned by itself, without other tokens being burned in the same tx. This makes it easy to check (in the future) whether the loan ended in default or was repaid. **The borrower must withdraw all of their remaining collateral in the same transaction that the loan is fully paid off.** Once the BorrowerID is burned, it will no longer be possible to reclaim the collateral.

:notebook: The loan validator checks that there is only one input from the loan address, even though it also checks the number of beacons present, to prevent invalid UTxOs from skewing the repayment calculations.

This redeemer can be used by the address owner to spend any misconfigured UTxOs with an `ActiveDatum`.

##### Claiming expired or fully paid loans
The last redeemer is the `Claim` redeemer which allows the lender to claim expired or fully paid Active UTxOs. The minting policy is also used with the `BurnBeaconToken` redeemer.

To claim an Active UTxO as the lender, all of the following must be true:

1. The UTxO must have an `ActiveDatum`. Otherwise, it is *not* an Active UTxO.
2. The input UTxO must have an Active Beacon. Otherwise, it is not a valid Active UTxO and can be claimed by the address owner using the `RepayLoan` redeemer.
3. No other beacons are allowed in the transaction inputs - this ensures only the Active Beacon and LenderID, (and in the case of an expired loan, the BorrowerID) are present.
4. The loan must either be expired (specified by `invalid-before`) or the `loanOutstanding` in the datum must be <= 0 (fully paid off).
5. The Active Beacon must be burned.
6. The LenderID must be burned.
7. If the BorrowerID is still present, it must be burned too.
8. No other tokens can be minted/burned in the transaction.
9. The lender must sign the transaction.

The presence of the BorrowerID in an expired loan indicates a default. When collateral is reclaimed in a defaulted loan, the BorrowerID is burned along with two other tokens, totaling three.

### Emergent Credit History

The Borrowers' Credit History naturally emerges thanks to the unique properties of fully paid loans and defaulted loans:
- When a loan is fully paid off, the Borrower ID must be burned in isolation - no other tokens can be minted or burned in the tx. Therefore, the number of unique tokens minted/burned will always be **1**.
- When a loan is defaulted on, the Borrower ID must be burned with the Active beacon and the Lender ID - no other tokens can be minted or burned in the tx. Therefore, the number of unique tokens minted/burned will always be **3**.

Using this information, all burning transactions for a given Borrower ID can be found with an API. If a given transaction had 3 unique tokens minted/burned in it, the borrower defaulted on that loan. If a given transaction had 1 unique token minted/burned in it, the borrower successfully repaid that loan. The APIs can be used to look closer at those transactions to see what the terms of each loan was.

The table below shows which API endpoints are used for this with Blockfrost:

| Action | API Endpoint |
|--|--|
| Burn Txs | [api](https://docs.blockfrost.io/#tag/Cardano-Assets/paths/~1assets~1%7Basset%7D~1history/get) |
| Number of Unique Tokens Minted/Burned | [api](https://docs.blockfrost.io/#tag/Cardano-Transactions/paths/~1txs~1%7Bhash%7D/get) |
| Specific Loan Information | [api](https://docs.blockfrost.io/#tag/Cardano-Transactions/paths/~1txs~1%7Bhash%7D~1utxos/get) |

For the second API, the `asset_mint_or_burn_count` value will either be 1 or 3, specifying a full repayment or default, respectively. For the third API, the input with the Borrower ID token will have the datum of the loan attached. That datum has the terms of that specific loan.

The included `cardano-loans` CLI puts this all together. Here is an example query response (when piped to `jq`):
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

This borrower defaulted on the first loan but successfully paid back the second. From this information, the time of each loan can be deduced (subtract the `term` value from the `expiration_slot` value). Since the `balance_owed` is also returned, lenders can decide for themselves if every default should be treated the same or if exceptions can be made for borrowers who repaid most of the loan before defaulting.

In addition to past loans, lenders can also see the borrower's current loans by looking up all Active UTxOs with that borrower's ID beacon still present (they would all be located at the borrower's loan address). These can only ever be open loans. This Blockfrost [api](https://docs.blockfrost.io/#tag/Cardano-Addresses/paths/~1addresses~1%7Baddress%7D~1utxos~1%7Basset%7D/get) will return that information.

Here is a list of a non-exhaustive list of queries you can make thanks to the beacon tokens:
1. All the borrower's current open Asks.
2. All current Asks on the dApp.
3. All Offers made to a borrower.
4. All the lender's current open Offers.
5. All the borrower's current loans.
6. All the lender's current loans.
7. The borrower's credit history.
8. The lender's loan history.

`cardano-loans` supports all of these queries. To see example responses for all these queries, check out the [GettingStarted](GettingStarted.md).

### Transaction Fee Estimations (YMMV)

All of the following estimations are for loans using a single asset as collateral.

| Action | Fee |
|--|--|
| Create an Ask | 0.499160 ADA |
| Close an Ask | 0.887602 ADA |
| Create an Offer | 0.515948 ADA |
| Close an Offer | 0.905863 ADA |
| Accept an Offer | 1.487373 ADA |
| Make a partial payment | 0.957785 ADA |
| Fully pay off loan | 1.175260 ADA |
| Claim an expired loan | 0.994895 ADA |
| Claim a fully paid loan | 0.920759 ADA |

During testing, it was possible to use 9 different assets as collateral for a loan before hitting the transaction limits. The bottleneck is in the `AcceptOffer` step.


## End-to-End Example (Simplified)

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
Alice's default is now recorded on-chain for future lenders to see.
```

## Features Discussion
Here are some unique features that distinguish Cardano-Loans from other lending/borrowing protocols:

### On-Chain Credit History
Beacon Tokens can be used as "DID-like" identifiers that attest the (current and past) borrowing history of their associated address. Using an off-chain API, it is easy to query whether a loan was repaid in full or defaulted on. Although the contract logic treats all "defaulted" loans identically, it may be the case that the loan was almost completely repaid. How much of a defaulted loan was repaid is easily queryable, so the mere fact of a default is **not** a binary indicator of a borrower's credit-worthiness. Lenders and/or third-party rating agencies can use this history (possibly in combination with other factors, such as an associated DID) to determine the credit-worthiness of a borrower. All current and past loan conditions are visible to the third-party.

### Trustless p2p Negotiations
Negotiation, acceptance, and repayment of loans occur fully p2p. All assets are always in control of either the borrower or lender, no middleman contracts/addresses are necessary. Tokens in the Offer Phase are held in UTxOs that reside in the borrower's address. The borrower cannot *spend* this UTxO unless they proceed with moving the loan into the "Active" Phase, but they *do* have staking rights over the UTxO. Lenders are therefore incentivized to keep offer periods short, since the offer's stake is controlled by the borrower.

### Partial Repayments
Borrowers may repay their loans incrementally, and withdraw collateral in proportion to the amount repaid. A loan is considered defaulted on if it is not paid back in full by the agreed upon slot, at which point the lender may claim any remaining collateral. The extent to which the loan was paid off is visible on-chain, so not all defaults (should) impact creditworthiness in the same way.

### Endogenous Price & Interest Rate Discovery
Cardano-Loans is designed to be independent from the traditional financial system, in favor of endogenously producing its own. As such, every piece of a loan, including the relative values of assets to collateral, interest rates, and term length are all negotiated and agreed upon fully p2p. No oracle feeds are necessary. 

Furthermore, since all Offers are public, lenders can see what other lenders are offering and can decide whether to offer better terms. This natural competition between lenders helps facilitate price and interest rate discovery.

Although this (at first) presents a bootstrapping problem, it may be overcome by the fact that this protocol may be the fastest way for the most financially underserved peoples to begin building a credit history. Prospective borrowers are incentivized to build p2p relationships within a global marketplace of prospective lenders, who themselves are incentivized to lend by the relatively high rates that financially underserved borrowers would be willing to pay.  

With enough users & liquidity, this protocol may eventually *serve* as the de-facto oracle for market-driven rate discovery.


## Future Directions and Considerations
Being a PoC, v1 of Cardano-Loans is intended to demonstrate the capacity for fully p2p lending/borrowing on the CSL using the eUTxO model. As such, there are a number of features that may be implemented in future version of the protocol, discussed below.

Note: Cardano-Loans v1 is written in IOG's PlutusTx. Although this is a great choice for prototyping & auditing, it is very resource-intensive. Many of the features discussed in this section are bottlenecked by current script execution limits. Future increases to this limit, as well as utilizing newer, more resource-efficient languages (such as Aiken) can result in up to 100x more headroom for additional features. 

In the future, fully p2p contracts may utilize both approaches: an audit-friendly language (like PlutusTx) for security and ease of upgradeability, followed by translation to a resource-friendly language (like Aiken) for deployment at scale. 

### Potential Future Features
A non-exhaustive list of features that may be implemented in the future:

#### Transferrable Credit/Debt
Borrowers may "refinance" their debt by using a new loan with more favorable conditions to pay off the first loan. Lenders may also sell their credit to a willing third party. The *ability* for either party to engage in such transfers may itself be negotiated in advance.

Note: This feature will likely require the introduction of additional Beacon Tokens.

#### Multi-Asset Loans
In addition to using multiple collateral assets for loans (which is already implemented), it is possible create loans where multiple assets are being borrowed. This is especially useful in combination with multi-asset collateral, allowing users to create "packaged" loans that are hedged against the "global" price movements of any one of the underlying assets.

#### Term Extensions/Renegotiations
A borrower may renegotiate an active loan with their lender, without closing or defaulting on the loan. This may be to "refinance" the loan, to negotiate a loan term extension, or for whatever other reasons the two parties may agree upon. All such actions would be queryable by prospective lenders in the future, giving them further insight into the nature/creditworthiness of the borrower.

#### Linkable Credit-History
By introducing additional Beacon Tokens (and associated standards), it may be possible to link together previously unrelated/pseudonymous borrower IDs into a set of *related* (but still pseudonymous) IDs, at the borrower's discretion.

#### DID Compatibility
Upon the maturation of standards, decentralized identifiers (DIDs) can be incorporated with Cardano-Loans, further amplifying utility and interoperability.

#### Support for Staking Scripts
Due to trying to keep the v1 PoC simple, staking scripts are not supported. Support for staking scripts can easily be added in the future.

:notebook: Technically, the loan validator already has logic *in case* a staking script is used, but this is currently only to prevent accidental locking if the wrong address is configured.

#### Compound Interest
Compound interest can be enabled by adding "expiration checkpoints" to the design. Then, instead of the dApp using `invalid-hereafter` to check if the overall loan is expired during every payment, the dApp would use it to check if the next expiration checkpoint has passed. If it has, the borrower would have to "rollover" the loan into the next period by updating the `loanOutstanding` field of the datum to reflect the interest that has accrued.

As an example, imagine if Alice borrows 100 ADA from Bob and they agree to checkpoints at slot 100, slot 200, and slot 300. Before slot 100, Alice can make as many payments as she wants. Once slot 100 occurs, the dApp will prevent her from making any more payments until she updates the datum to reflect that interest has accrued on the outstanding balance. Once this is done, she can continue making payments until slot 200 where she must repeat the rollover process. Alice can still fully pay back a loan in any given period (assuming no rollover is necessary before the next payment). If Alice fully paid back the loan at slot 255, then Alice would not need to rollover the loan into the next period; the loan would stop when she fully repays it.

With this design, the dApp would be able to tell if the expiration time is just a checkpoint or if the loan is actually expired (there are no more checkpoints after the current one). Further, Alice would actually be incentivized to accrue the interest since otherwise she would not be able to reclaim any more of her collateral nor would she be able to fully pay off the loan and prevent it from hurting her credit history.

As a final point (alluded to in the above example), these expiration checkpoints can be part of the negotiations.

### Other Considerations

#### Multi Loan Acceptance/Repayments
Currently, borrowers are not able to accept or make payments on multiple loans in a single transaction - this was to keep the first design simple. A future version should ideally allow borrowers to accept or make payments on mulitple loans in a given transaction. This is not an essential feature, but is important for leveling the playing field for small lenders. 

Imagine if Alice wants to borrow 1000 ADA while Mike has 2000 ADA, Bob has 700 ADA, and Charlie has 500 ADA. If Alice asks for 1000 ADA in one loan, only Mike can be her lender due to the all-or-nothing way the dApp works. However, if she instead broke her 1000 ADA loan up into two 500 ADA loans, now all three of them can be her lender. Alice is incentivized to due this because lender competition can result in better terms overall for Alice. This scenario is also better for Charlie and Bob since they are just as competitive as the bigger lender, Mike. However, if Alice must use separate transactions to make payments for each loan, the extra fees can easily add up. These fees actually disincentivize borrowers from "fractionalizing" their loans. In order to level the playing field between large and small lenders as much as possible, using fractional loans must be as cheap as possible.

This design can be adapted to allow for multi-loan acceptance/repayments by using a Loan ID token (a state token that links the input and output for a given loan). The main issue would then be the transaction limits. While using a more efficient language can help, the [Redundant Executions](https://github.com/cardano-foundation/CIPs/pull/418) are also a problem. The full impact of these redundant executions is still being explored.

#### Version Compatibility
Different versions of Cardano-Loans are not compatible with each other. That is, a borrower using v1 of the protocol cannot engage in loans with a lender using v2 of the protocol. However, they may use the same keys for both protocols, which (although resulting in different addresses/beacons) allows them to maintain their pseudonymous identities across versions. 


## Conclusion
The Cardano-Loans protocol is a first-attempt at rethinking how the economy of Cardano could evolve. It forgoes reliance on global/external token prices in favor of incentivizing the creation of a CSL-native p2p credit-debt market. It is censorship-resistant, scalable, and straightforward in its design. Wallets and other frontends can integrate the protocol into their UI, and even provide all necessary API query services. Endogenous price & rate discovery empowers users to create their own economy, one that is (at least initially) decoupled from the existing financial system, in pursuit of something more fair, equal, and accessible to all.