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
Cardano-Loans is a p2p-DeFi protocol for lending/borrowing  assets on the Cardano Settlement Layer (CSL). It empowers users to create and operate a CSL-native credit/debt market via trustlessly negotiable & repayable p2p loans, and an on-chain credit history. This circumvents the need for oracles and/or concentrated lending pools in favor of *endogenous* price & interest-rate discovery. Users deploy (and interact with each others') script addresses, so full spending *and* delegation control of assets is maintained, and contract upgrades occur democratically.

## Motivation
A healthy credit-debt market is a vital (and often the largest) component of a thriving economy. A healthy market is one in which prices reflect underlying reality as quickly and fluidly (with as little friction) as possible. For the sake of avoiding a whole treatise on economics, suffice to say that *the best way to achieve fast and frictionless price discovery is through the aggregation of maximally expressive individual sentiments.* This is especially true for credit-debt markets, where money itself is the asset, priced via interest rates. 

Many lending/borrowing dApps on Cardano are implemented in controlled manners that limit users' flexibility in negotiating loan terms. This results in a sub-optimal expression of market sentiment. Furthermore, existing protocols rely on oracles for price feeds, which complicates the trust model, increases the dApp's attack surface, and subjects prices to a broader economy in which interest rates are not set by the market, but by central actors. 

Finally, p2p lending dApps may be the fastest way for the most financially underserved peoples to begin building credit scores. Those who *need* atypical financing options are in the best position to begin bootstrapping the system, as they would be willing to pay higher interest rates than any other group. This in turn incentivizes yield-seeking lenders to develop new strategies for such unprecedented conditions. In time, borrowers and lenders can build p2p relationships, and begin dis-intermediating the banking system.

## Preliminary Discussion
To appreciate the necessity for p2p lending/borrowing protocols on Cardano, it is first important to understand the deficiencies of the status quo:

### Current Lending/Borrowing dApp Deficiencies

1. **Oracles** - dApps that rely on off-chain information feeds are subject to the integrity of the underlying oracle network. Oracles are a nuanced topic that is beyond the scope of this document, but in short, oracles increase the attack surface of dApps, and have a long way to go before they can be safely relied upon by ledger-wide distributed dApps. See [here]() for more.

2. **Limited Loan Term Negotiability** - lending/borrowing dApps that are mediated by a central contract/entity lack negotiability of all loan terms, resulting in inefficient markets. This is especially troubling for a credit-debt market, whose efficiency is a vital component to a healthy economy.
   
3. **Concentrated Lending Pools** - concentrated lending pools (or any concentrated dApp design) are a higher security risk compared to distributed dApps. Aside from catastrophic draining attacks, centralized dApps often have a complex security model that relies on additional entities like DAOs (and associated "dApp Tokens") for upgradeability. This not only increases the technical attack surface, but the social attack surface as well. Furthermore, concentrated dApps do not scale naturally compared to distributed dApp architectures, especially in the context of state channels, like Hydra. 
   
4. **Inefficient Markets** - concentrated lending pools set interest rates formulaically, rather than emergently. Although the formula may be transparent, a healthy economy *continuously discovers* prices and rates via real-time aggregation of individual interactions, not by formulas. There is no single formula that can optimally capture market sentiment. Even if there was, agreeing on it would be a great challenge in and of itself. 

### The Cardano-Loans Protocol
Cardano-Loans gives users the great power (and responsibility) to create a fully on-chain debt market. It is entirely divorced from "global" off-chain markets, in favor of a bottom-up, or *endogenous*, approach to price & interest rate discovery. 

##### The protocol is broadly comprised of three distinct phases:

1. **Ask Phase** - Alice initiates this phase by "broadcasting" her (easily queryable) loan request for a *specific quantity of a certain token, collateralized by a certain token, to be repaid over a specific timeframe.*

2. **Offer Phase** - Bob determines that Alice is creditworthy (via her on-chain credit history) and initiates an offer to fulfill the loan *in the amount and over a timeframe that Alice* has specified, *with a collateral ratio and interest rate that Bob* specifies. 
   
3. **Active Phase** - Alice accepts the loan terms Bob has offered by spending his Offer UTxO, thereby gaining full control over the requested assets, and locking her collateral into a time-locked Active UTxO. Alice may incrementally pay off the loan, and can reclaim her collateral *in proportion to the amount she just paid off.* If the loan expires, Alice can no longer repay the outstanding amount, and Bob can claim the remaining collateral. Note that Alice maintains staking rights over her locked collateral through the length of the loan.

All three of these phases are expanded upon in the [Specification Section](#specification) below

##### Distinguishing features specific to Cardano-Loans:

1. **On-Chain Credit History** - the status/conditions of current & past loans associated with a borrower's stake pubkey are easily queryable by prospective lenders and third-party data miners.
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

Since negotiations and repayments occur in the borrower's address, the borrower maintains staking rights over all assets throughout the life cycle of the loan. This includes collateral, which makes sense, since collateral does belong to the borrower until they default. It also includes any Offer UTxOs, which incentivizes lenders to be proactive and not leave Offers open for too long. 

:warning: If, at any point, a misconfigured UTxO is sent to a borrower's address, the borrower will get custody. Such UTxOs can only come about by misusing the dApp. As long as beacon tokens are minted in the Tx, the resultant UTxO will be locked appropriately, as per the contract. 

#### Telling Time
Cardano-Loans enforces time-sensitive logic by marrying user incentives with transaction validity intervals (`invalid-before` for accepting and reclaiming loans, and `invalid-hereafter` for repaying loans).

Consider the following transaction scenarios separately:

1. **Borrower accepts loan** - The borrower cannot access the funds until the loan starts, so they want to set the time as early as possible. However, the loan expiration is calculated by adding the loan term length to the start time. If the borrower sets the time to be earlier than it actually is, the loan expiration would also be earlier than it otherwise would be. This means less time for the loan than the borrower wants. If the borrower sets the time to be later than it actually is so that the loan expiration would be later than it otherwise would be, the transaction would not be valid until the start time *actually* occurs. This means the borrower would only be delaying when they can access the loan asset. Therefore, **the borrower is incentivized to set `invalid-before` to be as close to the current time as possible.**
   
2. **Lender claims a finished loan** - The lender cannot claim a loan unless it is either fully paid or it is expired. If the lender set the time to be earlier than it actually is, the loan is still active and the lender cannot claim it. If the lender set the time to be later than it actually is the transaction would not (yet) be valid, so the lender would have to wait longer to claim what they are owed. These two together result in **the lender being incentivized to set `invalid-before` to be as close as the current time as possible.**

3. **Borrower makes a payment** - The borrower cannot make payments once a loan has expired. This is enforced by the borrower having to use `invalid-hereafter` to specify the current time. Since the borrower wants as much time as possible to pay off the loan, but *must* do so prior to expiration slot, **they are incentivized to set `invalid-hereafter` as close the expiration slot as possible.**

#### Interest
Unlike other lending/borrowering protocols, Cardano-Loans does not use an algorithm to determine the interest rates. Instead, rates are one of the explicitly negotiated terms between borrowers and lenders.

**Cardano-Loans v2 utilizes compounding interest rates.** By requiring the loan to be rolled over at certain checkpoints, the interest can be applied to the outstanding balance at each checkpoint. The checkpoints themselves are negotiable.


#### Beacon Tokens
Cardano-Loans v2 uses 6 types of tokens:

1. **Ask Token** - demarcates UTxOs in the "Ask Phase"
2. **Offer Token** - demarcates UTxOs in the "Offer Phase"
3. **Active Token** - demarcates UTxOs in the "Active Phase"
4. **LenderID Token** - demarcates the lender's pubkey or staking script and mediates their ability to interact with their Offer UTxOs.
5. **BorrowerID Token** - demarcates the borrower's staking credential and mediates their ability to interact with the loan UTxOs. This token is also used to check the history of loans associated with a borrower to determine their creditworthiness.
6. **LoanID Token** - demarcates a specific loan and provides a lock/key pair to allow the lender to freely trade the Key NFT on secondary markets.

In addition to mediating dApp logic, all these tokens double as Beacons that users can query (via existing APIs) to interact with each other, without relying on a specialized aggregator/batcher.

#### Loan Datums
Three different inline datums (one for each of the phases: Ask, Offer, Active) are used for loan negotiations/agreements. Each of these will be expanded upon further below in the sections detailing each of the 3 phases. For reference, here is the code:


``` Haskell
data LoanDatum 
  = AskDatum
      { beaconSym :: CurrencySymbol -- ^ Beacon policy symbol.
      , borrowerId :: TokenName -- ^ Borrower's staking credential as a token name.
      , loanAsset :: (CurrencySymbol,TokenName) -- ^ Asset loaned out.
      , loanPrinciple :: Integer -- ^ Amount of the loan.
      , loanTerm :: POSIXTime -- ^ The length of time the loan will be valid.
      , collateral :: [(CurrencySymbol,TokenName)] -- ^ A list of assets to be used as collateral.
      }
  | OfferDatum
      { beaconSym :: CurrencySymbol
      , lenderId :: TokenName -- ^ Lender's payment/staking pubkey or staking script as a token name.
      , lenderAddress :: Address -- ^ Address where loan payments will go.
      , loanAsset :: (CurrencySymbol,TokenName)
      , loanPrinciple :: Integer
      , loanCheckpoints :: [POSIXTime] -- ^ The relative times where the interest must be applied.
      , loanTerm :: POSIXTime
      , loanInterest :: Rational -- ^ The interest rate as a fraction.
      , collateralization :: [((CurrencySymbol,TokenName),Plutus.Rational)]
          -- ^ A list of relative values for each collateral asset.
      , claimPeriod :: POSIXTime -- ^ How long the lender has to claim an expired loan.
      }
  | ActiveDatum
      { beaconSym :: CurrencySymbol
      , borrowerId :: TokenName
      , lenderAddress :: Address
      , loanAsset :: (CurrencySymbol,TokenName)
      , loanPrinciple :: Integer
      , nextCheckpoints :: [POSIXTime] -- ^ The absolute times where the interest must be applied.
      , pastCheckpoints :: [POSIXTime] -- ^ The previous times the interest was applied.
      , loanTerm :: POSIXTime
      , loanInterest :: Plutus.Rational
      , collateralization :: [((CurrencySymbol,TokenName),Plutus.Rational)]
      , claimExpiration :: POSIXTime -- ^ The absolute time the collateral is considered lost.
      , loanExpiration :: POSIXTime -- ^ The absolute time where the loan is considered expired.
      , loanOutstanding :: Rational -- ^ The current outstanding balance of the loan.
      , loanId :: TokenName -- ^ The LoanID for this specific loan.
      }
```
`POSIXTime` is an integer representing POSIX time down to the millisecond.
`Rational` is a fraction of integers. This is how decimals are represented by the dApp.

The datums are used as inline datums so that others can see the necessary information for negotiating and checking a borrower's credit history. Each of these datums will be covered as each loan phase is covered below.

The lenderAddress must use a payment pubkey since the borrower will be making payments to the lender. Enforcing a proper datum adds logic so to simplify this, all payments must go to a pubkey address. A future version can enforce a required datum.

#### Lender Payment Datums

In order to ensure that double satisfaction does not occur when paying a lender directly, each loan payment must include an inline datum specifying which loan the payment is for. This is the datum:
```Haskell
type PaymentDatum = (CurrencySymbol,TokenName)
```

The `CurrencySymbol` is the beacon policy id and the `TokenName` is that specific loan's LoanID. Since every LoanID is guaranteed to be unique, the inclusion of this datum guarantees that all loan payments are unique even when composed with other p2p-DeFi protocols.

#### Minting Redeemers
Minting redeemers are introduced here, their usage is explained further below.

``` Haskell
data BeaconRedeemer
  = MintAskBeacon 
      Credential -- ^ Borrower's staking credential.
  | MintOfferBeacon 
      Credential -- ^ Lender's credential to be used as the LenderID.
  | MintActiveBeacon 
      Credential -- ^ Borrower's staking credential.
      [(TxOutRef,TxOutRef)] -- ^ List pairing up Asks and Offers to accept.
  | BurnBeacons
```

#### Loan Validator Redeemers
Validator redeemers are introduced here, their usage is explained further below.

``` Haskell
data LoanRedeemer
  = CloseAsk
  | CloseOffer
  | AcceptOffer
  | MakePayment
  | Rollover
  | ClaimExpired
  | UpdateLenderAddress
  | UnlockLostCollateral
```

### The Loan Lifecycle
Cardano-Loans is broken up into three distinct phases:

#### 1. Ask Phase
Prospective borrowers initiate the Ask Phase by minting an `Ask` Token and storing it inside their borrower address with the desired Ask terms. 

##### Ask Initiation
Minting a valid `Ask` token requires all of the following to be true:

1. The `MintAskBeacon` redeemer must be used.
2. Only tokens with the token name 'Ask' can be minted by the minting policy in the transaction.
3. The `Ask` tokens must be minted to an address using the dApp's validator script as the payment credential.
4. The `Ask` tokens must be minted to an address using the credential that was supplied with the `MintAskBeacon` redeemer as the staking credential.
5. The `Ask` tokens must be stored with a valid inline `AskDatum`:
    - `loanBeaconSym` == beaconPolicyId
    - `borrowerId` == borrower's credential as a token name
    - `loanPrinciple` > 0
    - `loanTerm` > 0
    - `collateral` list must not be empty
6. The `Ask` tokens must be stored individually.
6. The receiving staking credential must signal approval.

- The borrower can use multiple assets as collateral for a given loan. Whatever assets *can* (but not necessarily *need* to) be used must appear in the `collateral` list.
- The (staking credential) approval signal ensures no one but the borrower can open a loan under the same borrowerID.

##### Closing an Ask
If the borrower changes their mind about the loan they are asking for, they may close their Ask accordingly:

1. The `CloseAsk` redeemer must be used for the loan validator and the `BurnBeacons` redeemer must be used for the minting policy.
2. The datum attached to the UTxO must be an `AskDatum`. If it isn't, then this UTxO cannot be an Ask since valid Asks have both an `AskDatum` and an `Ask` token.
3. The staking credential must signal approval.
    - pubkey must sign
    - staking script must be executed
4. All `Ask` tokens among the transaction inputs must be burned.

Here it is not necessary to check for the presence of an Ask token; the address owner gets custody of both valid Ask UTxOs and misconfigured Ask UTxOs. If a UTxO has an `AskDatum` but is missing an `Ask` token, the address owner can spend the UTxO by way of the staking credential. If a UTxO has an `AskDatum` and an Ask token, the address owner still has custody and can spend the UTxO in the same way. In either case, the staking credential must approve, so there is no need to check for the `Ask` token.

This redeemer can be used to prevent accidental locking of any misconfigured UTxOs that have an `AskDatum`.

#### 2. Offer Phase
Prospective lenders initiate the Offer Phase by minting an `Offer` token and storing it in the target borrower's address with the desired offer terms.

##### Offer Initiation
Minting a valid `Offer` token requires all of the following to be true:

1. The `MintOfferBeacon` redeemer must be used.
2. One and only one token with the token name 'Offer' is minted, *and* one and only one token with the credential supplied with the redeemer as the token name. This latter token is the `LenderID`.
3. Both the `Offer` and `LenderID` tokens must be minted to an address using the dApp's validator script as the payment credential.
4. Both the `Offer` and `LenderID` tokens must be minted to an address using a staking pubkey as the staking credential.
5. Both tokens must be stored in the same UTxO with the proper inline `OfferDatum`:
    - `loanBeaconSym` == beaconPolicyId
    - `lenderId` == lender's credential hash as a token name
    - `lenderAddress` must use a payment pubkey
    - `loanPrinciple` > 0
    - `loanCheckpoints` must all be > 0 and in ascending order. It can be empty. The last checkpoint must be < the `loanTerm`.
    - `loanTerm` > 0
    - `loanInterest` >= 0
    - `collateralization` list must not empty and all collateral relative values must be > 0
    - `claimPeriod` > 0
6. The UTxO containing the datum and tokens must have the loan amount specified in the datum + an additional 5 ADA.
7. The lender's credential in the redeemer must signal approval.


The `collateralization` field is *how much of that collateral asset the lender wants per unit of the loan asset taken.* This is always in units of `collateralAsset/loanAsset`. For example, if `collateralization` for AGIX/ADA is 2/1, then the borrower must put up 2 AGIX for every 1 ADA borrowed. If more than one collateral asset is possible, then the collateral can be used in any combination where the total relative value equals the loan principle. For example, if DUST is also allowed at a ratio of 10/1, then 10 ADA can be borrowed by putting up 10 AGIX and 50 DUST:
```
10 / 2 + 50 / 10 = 5 + 5 = 10
```

- The lender can offer under-collateralized loans and over-collateralized loans by setting the relative values for the collateral to be under the current market value or over the current market value, respectively. If a lender does not want a certain collateral asset to be used, the lender can set the relative value for that collateral to zero.

- The `collateralization` list must be in the same order (based on collateral name) as the borrower's `collateral` list. This is due to how `AcceptOffer` checks if the borrower and lender agree on the terms.

- The lender must signal approval of the transaction with the credential used for the Lender ID so that the Lender ID beacon token is guaranteed to be unique to that lender.

- The `loanCheckpoints` are the times at which the interest must be applied. Interest free loans can be given by leaving the list empty. A future version can offer non-compounding interest by allowing a checkpoint to be 0. This would mean a rollover would be required immediately upon the loan starting and that is the only time the interest gets applied.

By requiring the lender to store the loan amount with the Offer Token, the borrower is able to accept the loan without any other input from the lender. The additional 5 ADA is required when the loan asset is ADA because when the borrower accepts the offer, the script can enforce that the 5 ADA is returned to the lender. If the deposit was variable, the script would have no way to know how much the lender used as the minUTxO deposit.

Multiple lenders can make an offer to the borrower. Each lender will have their own Offer UTxO located at the borrower's address. The borrower can check his/her own loan address and see what other offers were made. The borrower can check the `OfferDatum` for:
1. **The loan's interest rate.**
2. **The relative values of loan asset(s) to collateral asset(s).**
The lenders are also able to see what other offers have been made to the borrower. This breeds natural competition between lenders.

**Even though the lender's assets are stored at the borrower's address, the lender maintains spending custody of their Offer UTxO.** The only way the borrower can spend the Offer UTxO is if they are accepting the lender's offer and initiating the loan. Thanks to the Lender ID being a beacon token, it is easy to query all UTxOs associated with that lender.

##### Closing an Offer
If the lender changes their mind about their offer *prior* to it being accepted by the borrower, or if the borrower accepts a different offer, the lender may close the offer and reclaim their Offer UTxO accordingly:

1. The `CloseOffer` redeemer must be used for the loan validator and the `BurnBeacons` redeemer must be used for the minting policy.
2. The datum attached to the UTxO must be an `OfferDatum`. If it isn't, then this UTxO cannot possibly be an Offer.
3. If the Offer beacon is present in the UTxO, this is a valid offer and the LenderID is guaranteed to be present. Custody belongs to the lender. Additional checks are required in this situation:
    - The credential of the lender must sign the transaction. The credential hash  from the offer datum since the presence of the Offer Token means that the datum is properly configured. It is either a staking key or script.
    - All Offer beacons in the transaction inputs must be burned.
    - All of the lender IDs in the transaction inputs must be burned.
4. If the offer beacon is not present, the address owner gets custody by default. This scenario can only happen if the Offer is not a valid offer.
    - The staking credential of the address must signal approval.

Spending custody for the lender is enforced by requiring the lender's approval when appropriate.

This redeemer can be used by the address owner to claim any misconfigured UTxOs that have an `OfferDatum`.

#### 3. Active Phase
When a borrower is satisfied with an offer that has been made to them, they may accept it, thus transitioning the loan into the Active Phase:

##### Accepting a loan offer
Accepting an offer requires both the minting policy and the loan validator script. The idea is to consume 1 Ask Phase UTxO and 1 Offer Phase UTxO to produce 1 Active Phase UTxO.

- The `AcceptOffer` redeemer is used for the loan validator and the `MintActiveBeacon` redeemer is used for the minting policy.

###### Validator Checks:

1. The input must have either an `AskDatum` or an `OfferDatum`.
2. At least one `Active` beacon must be minted.

The minting policy does the rest of the checks.

###### Minting Policy Checks:

1. All `Offer` beacons must be burned.
2. All `LenderID`s must be burned.
3. All `Ask` beacons must be burned.
4. One `Active` beacon, one BorrowerID, and 2 LoanIDs must be minted for every loan accepted.
5. The BorrowerID must have the loan address' staking credential as the token name.
5. Only one loan address can have inputs in this transaction.
6. All loan address inputs must either have an `Ask` beacon or an `Offer` beacon.
7. All loan address inputs must be paired up exactly once.
8. The LoanID for that loan is the tx hash of the Offer input.
9. The datums of the paired inputs must agree.
10. One of the newly minted LoanIDs must be paid to the corresponding lender + 5 ADA.
11. The borrower must approve the transaction.
12. There must be a collateral output to the loan address with the proper amount of the collateral, one `Active` beacon, one BorrowerID, and the other LoanID for that loan.
13. The transaction must specify `invalid-before` for the loan's start time.
14. The collateral output must have the proper `ActiveDatum`:
  - beaconSym == beaconSym offerDatum
  - borrowerId == borrowerId askDatum
  - lenderAddress == lenderAddress offerDatum
  - loanAsset == loanAsset offerDatum
  - loanPrinciple == loanPrinciple offerDatum
  - nextCheckpoints == map (+ startTime) (loanCheckpoints offerDatum)
  - pastCheckpoints == []
  - loanTerm == loanTerm offerDatum
  - loanInterest == loanInterest offerDatum
  - collateralization == collateralization offerDatum
  - claimExpiration == startTime + loanTerm offerDatum + claimPeriod offerDatum
  - loanExpiration == startTime + loanTerm offerDatum
  - loanOutstanding == loanPrinciple offerDatum
  - loanId == tx hash of offer input

The `MintActiveBeacon` redeemer is used to tell the minting script which loan address should have inputs and what the pairings are.

In essence, the collateral calculation is:
```
sum { collateralDeposited / relativeValue } >= loanPrinciple offerDatum
```

To summarize: the borrower accepts a loan offer via a single transaction with an Offer UTxO, an Ask UTxO, and however many other inputs are necessary to fulfill the collateral requirements. The transaction outputs one Active UTxO to the loan address, while the remaining funds (what is actually being borrowed) is output to any address, as specified by the borrower.

The Aiken version is capable of accepting up to 6 loan offers in a single transaction.

##### Making a Loan Payment
A loan payment can either be a partial payment or a full payment (relative to the remaining balance). In either case, the `MakePayment` redeemer is used. If a full payment is being made, the `BurnBeacons` redeemer is required for the minting policy. The loan validator script can tell whether a partial or full payment is being made.

To make a payment, all of the following must be true:

1. The input must have an `ActiveDatum`.
2. The address' staking credential must signal approval.
3. If the `Active` beacon is present, this is a valid loan:
    1. The loan must not be expired - using the `invalid-hereafter` option to tell the script the time.
    2. The next checkpoint must not have passed - using the `invalid-hereafter` option to tell the time.
    3. The BorrowerID must still be present in the input.
    4. There must be a  new collateral output to the loan address and the datum must be the same as the input except with the outstanding balance updated.
    5. The payment to the lender must include the proper `PaymentDatum`.
    6. If The new outstanding balance is <= 0, this is a full payment:
        - The BorrowerID must be burned.
        - Only BorrowerIDs can be minted/burned in this transaction.
        - All remaining collateral is unlocked.
        - The `Active` beacon and LoanID must be included in the collateral output.
    7. If the new outstanding balance is > 0, this is partial payment:
        - The collateral output must include the `Active` beacon, LoanID, and the BorrowerID.
        - The proportion of collateral taken <= proportion of loan repaid.
4. Otherwise, this is an invalid Active UTxO and the address owner has custody. The staking credential approval was already checked.

The collateral that can be reclaimed during a partial payment is determined by the following equation:
```
sum { collateralTaken / relativeValue }           loanRepaid
--------------------------------------------  <=  --------------------------
sum { startingCollateral / relative value }       startingOutstandingBalance
```

- The output to the lender's address determines how much of the loan was repaid. The script only check for a single output proper output to the lender's address. If there are multiple, only one will be counted which means the extra amount paid will be missed. This dramatically simplifies the logic and will likely not be changed. There is no reason for Borrowers to use multiple outputs to lender's for a given loan payment.

- The contract only checks if the borrower took *too much* collateral. If the borrower misses an opportunity to reclaim some collateral, that proportion can only be reclaimed when the loan is fully paid off. A future version can add safeguards so that a partial payment fails unless *some* collateral is taken.

- Due to the way the collateral calculation is implemented, it is possible to swap out collateral as long as the total relative value of the collateral is correct. For example, the borrower can replace collateral A with collateral B if desired. The total relative values are still enforced by the script.

- When a loan is fully paid off, the BorrowerID must be burned by itself, without other tokens being burned in the same Tx. This makes it easy to check (in the future) whether the loan ended in default or was repaid. **The borrower should withdraw all of their remaining collateral in the same transaction that the loan is fully paid off.** If the Borrower does not reclaim the collateral, they still can by using the `UnlockLostCollateral` redeemer but the lender is also able to claim the collateral by using the `ClaimExpired` redeemer. This was partly to incentivize the clean up of the beacons for finished loans and partly a vestige of the previous version. In hindsight, this is likely unnecessary since only the borrower needs to clean up the beacons (it is their minUTxO deposit anyway). A future version will likely remove the ability for lender's to claim finished collateral UTxOs (while still allowing claiming of defaulted collateral UTxOs).

- This redeemer can be used by the address owner to spend any misconfigured UTxOs with an `ActiveDatum`.

##### Claiming expired or fully paid loans
The `ClaimExpired` redeemer allows the lender to claim expired or fully paid Active UTxOs. The minting policy is also used with the `BurnBeacons` redeemer.

To claim an Active UTxO as the lender, all of the following must be true:

1. The UTxO must have an `ActiveDatum`. Otherwise, it is *not* an Active UTxO.
2. The input UTxO must have an Active Beacon. Otherwise, it is not a valid Active UTxO and can be claimed by the address owner using the `MakePayment` redeemer.
3. The loan must either be expired (specified by `invalid-before`) or the `loanOutstanding` in the datum must be <= 0 (fully paid off).
4. The Active Beacon must be burned.
5. Two LoanIDs must be burned (the Lock and Key NFTs).
6. If the BorrowerID is still present, it must be burned too.

The presence of the BorrowerID in an expired loan indicates a default. When collateral is reclaimed in a defaulted loan, the BorrowerID is burned along at least two other tokens.

The lender has until the claim period ends to claim the defaulted loan. Once the claim period passes, the lender can still claim it but so can the borrower since the collateral is considered lost by the protocol. This should not be an issue since the claim period is fully negotiable.

##### Rolling over a loan
The `Rollover` redeemer is used to apply the interest to the outstanding balance and re-enable payment (until the next checkpoint).

To rollover a loan, all of the following must be true:

1. The input must have an `ActiveDatum`.
2. The address' staking credential must signal approval.
3. The UTxO must have a BorrowerID.
4. The loan must not be expired - using `invalid-hereafter` to tell the script the time.
5. There must be an output to the loan address with the updated datum and exact same value as the input:
    - The outstanding balance must acrue interest.
    - The current checkpoint must be removed from nextCheckpoints and added to pastCheckpoints.

The borrower can rollover multiple loans in a single transaction.

##### Updating the lenderAddress for an active loan
When the Key NFT is sold, the lenderAddress in the `ActiveDatum` will need to be updated. This can be done using the `UpdateLenderAddress` redeemer.

To update the lender's address, all of the following must be true:

1. The input must have an `ActiveDatum`.
2. The input must have a LoanID.
3. There must be two LoanIDs among the tx inputs.
4. The BorrowerID must still be present.
5. The new address must use a payment pubkey.
6. There must be an output to the loan address with the exact same value and datum except the updated address.

The BorrowerID must be present to prevent tricking the credit history if a borrower is also their own lender.

This redeemer is designed so that lenders can update the address in the same transaction where they purchase the Key NFT on the secondary market.

##### Unlocking lost collateral
With the introduction of Lock and Key NFTs, it is possible for collateral to be permanently lost if the Key NFT is lost. To prevent this, a lender has a set amount of time to claim the collateral. After this time passes, the collateral is considered lost and is recoverable by the borrower. The loan still counts as a default against the borrower. Once the claim period passed, the lender can still claim the collateral with `ClaimExpired` but it is then a race against the borrower. The claim period is fully negotiable so this should not be an issue.

To unlock lost collateral, all of the following must be true:

1. The input must have an `ActiveDatum`.
2. The address' staking credential must signal approval.
3. The input must have an `Active` beacon.
4. All `Active` beacons among the inputs must be burned.
5. All LoanIDs among the inputs must be burned.
6. All BorrowerIDs among the inputs must be burned.
7. The claim period must have passed or the loan must be fully repaid.

This redeemer can be used by the borrower to clean up the beacons of finished loans and reclaim the minUTxOValue that must be stored with them.

### Benchmarks

Detailed benchmarks can be found [here](Benchmarks.md). Below is a quick summary:

- Borrowers can accept up to 6 loans in a single transaction.
- Borrowers can make 7-9 partial/full payments in a single transaction.
- Borrowers can rollover 10+ loans in a single transaction.
- Lenders can claim 5 expired loans in a single transaction.


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
Beacon Tokens can be used as "DID-like" identifiers that attest the (current and past) borrowing history of their associated address. The Borrowers' Credit History naturally emerges thanks to the unique properties of fully paid loans and defaulted loans:
- When a loan is fully paid off, the BorrowerID must be burned in isolation - no other tokens can be minted or burned in the same Tx. Therefore, the number of unique tokens minted/burned will always be **1**.
- When a loan is defaulted on, the BorrowerID must be burned with at least an Active beacon and the LoanIDs. Therefore, the number of unique tokens minted/burned will always be **> 1**.

Using an off-chain API, it is easy to query whether a loan was repaid in full or defaulted on. Although the contract logic treats all "defaulted" loans identically, it may be the case that the loan was almost completely repaid. How much of a defaulted loan was repaid is easily queryable, so the mere fact of a default is **not** a binary indicator of a borrower's credit-worthiness. Lenders and/or third-party rating agencies can use this history (possibly in combination with other factors, such as an associated DID) to determine the credit-worthiness of a borrower. All current and past loan conditions are visible to the third-party.

The table below shows which API endpoints are used for this with Blockfrost:

| Action | API Endpoint |
|--|--|
| Burn Txs | [API](https://docs.blockfrost.io/#tag/Cardano-Assets/paths/~1assets~1%7Basset%7D~1history/get) |
| Number of Unique Tokens Minted/Burned | [API](https://docs.blockfrost.io/#tag/Cardano-Transactions/paths/~1txs~1%7Bhash%7D/get) |
| Specific Loan Information | [API](https://docs.blockfrost.io/#tag/Cardano-Transactions/paths/~1txs~1%7Bhash%7D~1utxos/get) |

For the second API, the `asset_mint_or_burn_count` value will either be 1 or more, specifying a full repayment or default, respectively. For the third API, the input with the BorrowerID token will have the datum of the loan attached. That datum has the terms of that specific loan (you will also need to use [this API](https://docs.blockfrost.io/#tag/Cardano-Scripts/paths/~1scripts~1datum~1%7Bdatum_hash%7D/get) since Blockfrost only returns the hash of the datum in the last query).

Here is how to interpret the query results:
```
1) When a BorrowerID is burned in isolation, all outputs with an Active beacon but missing a 
     Borrower ID are full payments. The other outputs can be ignored.
2) When a BorrowerID is burned along with other assets, all inputs with the borrower ID are
     defaults. The other inputs can be ignored.
```

In addition to past loans, lenders can also see the borrower's current loans by looking up all UTxOs with that borrower's ID beacon (they would all be located at the borrower's loan address). These can only ever be open loans. This [Blockfrost API](https://docs.blockfrost.io/#tag/Cardano-Addresses/paths/~1addresses~1%7Baddress%7D~1utxos~1%7Basset%7D/get) will return that information.

Here is a non-exhaustive list of queries you can make thanks to beacon tokens:
1. All of one borrower's current open Asks.
2. All borrowers' open Asks.
3. All Offers made to a borrower.
4. All the lender's current open Offers.
5. All the borrower's current loans.
6. All the lender's current loans based off their Key NFTs.
7. The borrower's credit history.
8. A specific loan based on the LoanID.

`cardano-loans` supports all of these queries.

### Trustless p2p Negotiations
Negotiation, acceptance, and repayment of loans occur fully p2p. All assets are always in control of either the borrower or lender, no middleman contracts/addresses are necessary. Tokens in the "Offer" Phase are held in UTxOs that reside in the borrower's address. The borrower cannot *spend* this UTxO unless they proceed with moving the loan into the "Active" Phase, but they *do* have staking rights over the UTxO. Lenders are therefore incentivized to keep offer periods short, since the offer's stake is controlled by the borrower.

### Partial Repayments
Borrowers may repay their loans incrementally, and withdraw collateral in proportion to the amount repaid. A loan is considered defaulted if it is not paid back in full by the agreed upon slot, at which point the lender may claim any remaining collateral. The extent to which the loan was paid off is visible on-chain, so not all defaults (should) impact creditworthiness in the same way.

### Endogenous Price & Interest Rate Discovery
Cardano-Loans is designed to be independent from the traditional financial system, in favor of endogenously producing its own. As such, every piece of a loan, including the relative values of assets to collateral, interest rates, and term length are all negotiated and agreed upon fully p2p - no oracle feeds required. 

Although this (at first) presents a bootstrapping problem, it may be overcome by the fact that this protocol may be the fastest way for the most financially underserved peoples to begin building a credit history. Prospective borrowers are incentivized to build p2p relationships within a global marketplace of prospective lenders, who themselves are incentivized to lend by the relatively high rates that financially underserved borrowers would be willing to pay. Since Offers are public, lenders can see all offers being made to the borrower, resulting in a healthy competition between lenders.

With enough users & liquidity, this protocol may eventually *serve* as the de-facto oracle for market-driven rate discovery.

### Tradable Bonds
Lenders can freely trade their Key NFTs on the secondary market. Buyers of the Key NFTs can update the lender address for those loans in the same transaction where they purchase the Key NFTs.

### Staking Script Support for BorrowerIDs
Staking scripts allow arbitrary logic for the BorrowerIDs such as a multisig.


## Future Directions and Considerations

#### Term Extensions/Renegotiations
A borrower may renegotiate an active loan with their lender, without closing or defaulting on the loan. This may be to "refinance" the loan, to negotiate a loan term extension, or for whatever other reasons the two parties may agree upon. All such actions would be queryable by prospective lenders in the future, giving them further insight into the borrower's nature/creditworthiness.

#### Multi-Asset Loans
In addition to using multiple collaterals for loans (which is already implemented), it is possible to create "bundled" loans with multiple borrowed assets. This is especially useful in combination with multi-asset collateral; bundled loans can provide a hedge against fluctuations of any one of the underlying assets.

#### Linkable Credit-History
By introducing additional Beacon Tokens (and associated standards), it may be possible to link together previously unrelated/pseudonymous borrower IDs into a set of *related* (but still pseudonymous) IDs, at the borrower's discretion.

#### DID Compatibility
Upon the maturation of standards, decentralized identifiers (DIDs) can be incorporated with Cardano-Loans, further amplifying utility and interoperability.

#### More Expressive Beacon Querying
Currently, all loans, no matter the asset(s), are fetched when querying the beacons. Honing in on specific loan(s) necessitates more filtering, which may become resource intensive if the number of available loans is large. It may be worthwhile to employ unique Beacons per loan-asset, though the trade-offs here are not yet clear.

### Other Considerations

#### Version Compatibility
Different versions of Cardano-Loans are not compatible with each other. That is, a borrower using v1 of the protocol cannot engage in loans with a lender using v2 of the protocol. However, they may use the same keys for both protocols, which (although resulting in different addresses/beacons) allows them to maintain their pseudonymous identities across versions. 

Due to the (potentially) large feature set, it may be the case that not all features can fit into a single script. If high-level language and low level plutus-core optimizations are not enough to address this, it may be worthwhile to split features across multiple versions of Cardano-Loans. Besides a mild inconvenience, there is little downside to doing this; credit history transcends any one version, and users may choose which version to use depending on the features they need.

## Conclusion
The Cardano-Loans protocol is one part in a broad rethinking of how the economy of Cardano could evolve. Users are incentivized to create an *endogenous* CSL-native credit/debt market that is synergistic with other p2p-DeFi protocols. Thanks to its simple design, the protocol can be easily integrated into existing frontend providers, with modular support for various querying pipelines.

