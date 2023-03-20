# Cardano-Loans

## Abstract

By utilizing beacon tokens, it is possible to design a p2p lending dApp where borrowers and lenders can trustlessly negotiate terms, and trustlessly handle the collateral and repayments of loans, all while maintaining custody and full delegation control of the assets. This design allows for an on-chain credit history as well as fully customizable loan terms. This is a step up from the current lending dApp designs where the loan terms are effectively non-negotiable and it is impossible to use one's credit history to get more favorable loans.

## Motivation

Currently, lending dApps are very rigid in their features:

1. What assets can be used as collateral is effectively "hard-coded" into the protocol and requires major governance actions in order to support other assets as collateral.
2. The loan interest rates are algorithmically set and are non-negotiable.
3. Assets are pooled together into a few addresses which results in loss of delegation control and can cause concurrency bottlenecks if batchers cannot join permissionlessly.
4. Oracles are required to tell the protocol the current value of the collateral.
5. Liquidators are needed when collateral values drop too low.
6. Yield farming is used to incentivize high TVL with the idea of being to provide liquidity for loans.
7. The amount of collateral required is effectively hard-coded.

While the above design technically works, it is far from optimal. In general, the design models those of margin loans for stock market loans as opposed to mortgage or car loans. The current design of lending dApps is not well suited for the latter kind of loan. Instead of arguing "why" at this point of the document; the "why" will be saved until the discussion section at the end.

## The Cardano-Loans Protocol
Cardano-Loans uses a similar design paradigm as Cardano-Swaps but with adaptations necessary for lending and borrowing. The idea of Cardano-Loans is to emulate how a loan negotiation occurs in traditional finance. To highlight this, here is simplified example scenario of someone looking for a mortgage.

1. The Ask Phase - Alice walks into Bob's bank and asks Bob if she can take out a $200,000 loan for 5 years. Bob looks up Alice's credit history. If he thinks Alice is a good credit risk, then he will agree to give her a loan.
2. The Offer Phase - Bob offers Alice a 5% interest rate (annual) and asks for a 10% down payment. Alice decides to accept the offer.
3. The Active Phase - Alice gives Bob the 10% down payment and is able to buy the house she wanted. The house is placed as collateral for the mortgage. Alice is required to make payments to Bob until she fully pays of the loan. Alice does not own the house until after the loan is fully paid off.

Using smart contracts, it is possible to follow this same process in a trustlessly p2p fashion. This dApp is a proof-of-concept for this design. Put another way, using smart contracts, it is possible to trustlessly negotiate and carry out loans in a way where one party cannot possibly cheat the other.

Here are some of the key features of Cardano-Loans:
1. Cryptographically protected on-chain credit history.
2. Loan terms are fully negotiable:
      - interest rates
      - what collateral can be used
      - how much the collateral is worth
      - whether the loan is under-collateralized, fully-collateralized, or over-collateralized
      - the length of the loan
3. No oracles are needed since lenders and borrowers can agree on a local price for the collateral.
4. Full delegation control - borrowers maintain delegation control of their collateral while the loan is active.
5. No need for a dApp token.
6. No need for liquidators.
7. Liquidity arises simply through market incentives.
8. No loss of spending custody.
9. Democratic Upgradability.
10. Frontend Agnostic.

## Background Concepts
Before getting into the specification, there are a few terms that need to be defined. Hopefully, these definitions help clarify what is happening in the specification.

*Beacon Token* - a beacon token is a token used to to make off-chain querying more efficient.

*Overloaded Stake Keys* - a stake key that is also required to authorize spending UTxOs from a script address.

With that, we can dive into the specification.

## Specification
The dApp is composed of one minting policy and one validator script that work together to create the desired functionality. Unlike cardano-swaps, the dApp does not have separate scripts for each loan type. Instead, all loans use the same minting policy and validator script.

### The Borrower's Address
All borrower's create their own loan address where 1) the negotiations will take place and 2) the collateral will be kept until the loan either expires or is repaid. Similarly to cardano-swaps, this is accomplished by all borrowers using the dApp's validator script as the payment credential and their own staking pubkey as the staking credential. This staking pubkey is overloaded by the loan validator script so that the borrower can maintain custody when appropriate despite all users using the same payment credential.

**V1.0.0 does not support staking scripts.** However, the code can easily be extended to allow staking scripts if desired. The staking script functionality was not included in V1.0.0 in order to help simplify the dApp logic. Technically, the loan validator does have logic in case a staking script is used, but this is only to prevent accidental locking if the wrong address is configured.

### The dApp Tokens
The dApp uses 5 different beacon tokens:

1. The Ask token - a beacon representing UTxOs in the ask phase.
2. The Offer token - a beacon representing UTxOs in the offer phase.
3. The Active token - a beacon representing UTxOs in the active phase.
4. The LenderID - a beacon token where the token name is the lender's pubkey hash.
5. The BorrowerID - a beacon token where the token name is the borrower's pubkey hash.

Using these tokens, it is possible for users to find all the information needed for interacting with each other without needing to rely on third parties (permissionless or otherwise). No batchers are needed at all. These beacons allow the negotiations and loan payments to be made fully p2p.

The BorrowerID token is also used to establish a credit history. Thanks to the way the dApp logic works, you can use certain characteristics to trustlessly determine whether the borrower defaulted on the loan. All lenders can query the BorrowerID to check the borrower's credit history before agreeing to offer the borrower a loan.

### The Loan Datum
The loan datum is a sum type that can either be an ask datum, an offer datum, or an active datum. Here is the code:

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

The datums are meant to be used as inline datums so that others can see the necessary information for negotiating and checking a borrower's credit history. These datums will be covered as each phase is covered.

`POSIXTime` is just an integer representing POSIX time down to the millisecond.
`Rational` is a fraction of integers. This is how decimals are represented by the dApp.

### The Minting Redeemers
This section just introduces the redeemers here. The usage will be explained later.

``` Haskell
-- | The redeemer for the beacons.
data BeaconRedeemer
  -- | Mint the ask token to the borrower's address.
  = MintAskToken PaymentPubKeyHash -- ^ Pubkey for the borrower's STAKING credential. Simplifies logic.
  -- | Mint the offer token and lender ID.
  | MintOfferToken PaymentPubKeyHash -- ^ Pubkey for lender ID.
  -- | Mint the active token and the borrower ID.
  -- The first pubkey is the borrower's. The second one is the lender's.
  | MintActiveToken PaymentPubKeyHash PaymentPubKeyHash
  -- | Burn any token/IDs.
  | BurnBeaconToken
```

### The Loan Validator Redeemers
Just like with the previous section, this section just introduces the redeemers.

``` Haskell
data LoanRedeemer
  = CloseAsk
  | CloseOffer
  | AcceptOffer
  | RepayLoan
  | Claim
```

### Negotiations and Repayment
Just like the example in the beginning, there are three phases for the negotiations: Ask, Offer, and Active.

#### The Ask Phase
Only the minting policy is used for this phase since it involves depositing the proper UTxO to the borrower's loan address. Successfull initiation of the Ask phase is marked by the successfull minting of the Ask token.

Minting an Ask token requires all of the following to be true:

1. The `MintAskToken` redeemer must be used.
2. Only one token with the token name 'Ask' is minted by the minting policy in the transaction.
3. The Ask token must be minted to an address using the dApp's validator script as the payment credential.
4. The Ask token must be minted to an address using the staking pubkey that was supplied with the `MintAskToken` redeemer as the staking credential.
5. The Ask token must be stored with the proper inline ask datum:
    - askBeacon == (beaconPolicyId, TokenName "Ask")
    - borrowerId == (beaconPolicyId, borrower's staking pubkey hash as a token name)
    - loanPrinciple > 0
    - loanTerm > 0
    - collateral list must not be empty
6. The receiving staking pubkey must sign the transaction.

The last requirement ensures that only the borrower is able to ask for a loan under their ID. If the Ask token was successfully minted, that means the ask datum must has valid information. This means the loan validator script can use the presence of the Ask datum as a stand-in for datum validity.

Once the Ask token is minted, this ask is now discoverable by all potential lenders. Lenders can query the Ask token to easily find all open asks as well as the information for each ask (the datums stored with the Ask tokens).

#### The Offer Phase
Just like with the Ask Phase, only the minting policy is used for this phase since it involves depositing the proper UTxO to the target borrower's loan address. The successfull start of the offer phase is marked by the successfull minting of the Offer token.

Minting an Offer token requires all of the following to be true:

1. The `MintOfferToken` redeemer must be used.
2. Must mint exactly one token with the token name 'Offer' and one token with the pubkey hash supplied with the redeemer as the token name. This latter token is the LenderID.
3. Both tokens must be minted to an address using the dApp's validator script as the payment credential.
4. Both tokens must be minted to an address using a staking pubkey as the staking credential.
5. Both tokens must be stored in the same UTxO with the proper inline offer datum:
    - offerBeacon == (beaconPolicyId, TokenName "Offer")
    - lenderId == (beaconPolicyId, lender's payment pubkey hash as a token name)
    - loanPrinciple > 0
    - loanTerm > 0
    - loanInterest > 0
    - loanBacking > 0
    - collateralRates is not empyt
    - all collaterale rates must be > 0
6. The UTxO containing the datums and tokens must have the loan amount specified in the datum. If the loanAsset is ADA, then an additional 3 ADA is also required in the UTxO.
7. The lender's payment pubkey must sign the transaction.

Number 6 is very important. By requiring the lender to actually store the loan amount with the offer token, the borrower is able to accept the loan without any other input from the lender. The additional 3 ADA is required when the loan asset is ADA because storing the beacons requires a minimum amount of ADA. This ADA would not be available to the borrower when they try to accept the loan. When the loan is accepted, it will be stored with at least three tokens (the Active token, the LenderID, and the BorrowerID). The minimum amount of ADA required for storing these three tokens is 2.844600 ADA. This was rounded to 3 ADA for convenience. If this minimum was not met by the lender, the borrower would instead have to put up the ADA and if the loan asset is ADA, that means the borrower wouldn't be able to borrow the full amount of ADA on net.

Multiple lenders can make an offer to the borrower. Each lender will have their own offer UTxO located at the borrower's address. The borrower can check his/her own loan address and see what offers were made. From the offer datum, the borrower can see:
1. What the loan's interest rate will be.
2. How much of the loan must be backed by collateral.
3. What value the lender considers the collateral assets to be.

The borrower can choose the best loan offer they see. But before that, I will cover the `CloseAsk` and `CloseOffer` redeemers.

#### Closing an Ask
This option is there in case the borrower changes their mind about the loan he/she is asking for. This action requires both the minting policy and the loan validator script.

To close an Ask, all of the following must be true:

1. The `CloseAsk` redeemer must be used for the loan validator and the `BurnBeaconToken` redeemer must be used for the minting policy.
2. The datum attached to the UTxO must be an AskDatum. If it isn't, then this UTxO cannot possibly be an ask.
3. The staking credential must signal approval.
    - pubkey must sign
    - staking script must be executed (this is in case the wrong address is accidentally configured)
4. All ask tokens among the transaction inputs must be burned.

#### Closing an Offer
This option is there so that the lender can reclaim his/her assets if either 1) the borrower chooses another offer or 2) the lender changes his/her mind about the offer.

To close an Offer, all of the following must be true:

1. The `CloseOffer` redeemer must be used for the loan validator and the `BurnBeaconToken` redeemer must be used for the minting policy.
2. The datum attached to the UTxO must be an OfferDatum. If it isn't, then this UTxO cannot possible be an offer.
3. If the offer beacon is present in the UTxO, this is a valid offer and the LenderID is guaranteed to be present. Additional checks are required in this situation:
    1. The pubkey hash of the lender must sign the transaction. The pubkey hash can be gotten from the offer datum since the presence of the offer token means that the datum is properly configured.
    2. All offer beacons in the transaction inputs must be burned.
    3. All of the lender's IDs in the transaction inputs must be burned.
4. If the offer beacon is not present, the address own gets custody by default. This scenario can only happen if the offer is not a valid offer.
    1. The staking credential of the address must signal approval.

As you can see, the lender maintains custody of the UTxO despite the UTxO being located in the borrower's address. Thanks to the LenderID beacon token, finding all UTxOs that belong to that lender is trivial.

Finally, we can see how to accept an offer.

#### Accepting a loan offer
Accepting an offer requires both the minting policy and the loan validator script. The idea is to consume 1 ask phase UTxO and 1 offer phase UTxO in order to produce 1 active phase UTxO.

To accept an offer, all of the following must be true:

1. The `AcceptOffer` redeemer is used for the loan validator and the `MintActiveToken` redeemer is used for the minting policy.

-- validator checks --

2. The staking credential of the loan address must approve the transaction.
3. There are only two inputs from the loan address.
4. One of the inputs is an ask input with an AskDatum and Ask beacon.
5. The other input is an offer input with an OfferDatum and Offer beacon.
6. The ask input and the offer input must agree on the terms (the similar fields must have the same values).
7. No other beacons are allowed in the transaction inputs - this is to prevent double satisfaction.
8. There must/can only be one output to the loan address.
9. The transaction must specify invalid-before as the loan start time.
10. The output must contain the proper inline active datum:
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
11. The amount of collateral posted must equal the loanBacking specified in the offer datum.
12. Only one active beacon can be minted in the transaction and is must be minted to the same address where the offer and ask inputs originate.

-- minting policy checks --

13. Both the Ask token and the offer token from the inputs must be burned in the transaction.
14. Must mint exactly one active token with the token name 'Active' and one BorrowerID with the staking pubkey specified in the redeemer as the token name.
15. The Active token, BorrowerID, and LenderID (from the offer input) must be stored in the loan address using the supplied staking pubkey as the staking credential.

The reason invalid-before must be used is because the script can only know the current time from the validity range. Using invalid-before basically tells the script that the loan starts as soon as the acceptance transaction is valid.

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

The `oVal` is the value of the output to the loan address. The `recip` function gives the reciprical of the `Rational` fraction. In essence, this function does:

```
sum { collateralDeposited / collateralRate } >= loanBacking offerDatum
```

#### Making a Loan Payment
A loan payment can either be a partial payment or a full payment (relative to the remaning balance). They both use the same redeemer `RepayLoan`. The script can tell whether a partial or full payment is being made.

To make a payment, all of the following must be true:

1. The UTxO must have an ActiveDatum. If it doesn't, then it isn't an active UTxO.
2. The staking credential must approve the transaction.
3. If the active beacon is present in the UTxO, then this is a valid active loan:
    1. No other beacons can be in the transaction - this means only this active beacon, one BorrowerID, and one LenderID are present in the transaction.
    2. There can only be one input from this loan address.
    3. The loan is not expired - this uses the invalid-hereafter option to tell the script what time it is.
    4. There can only be one output to the loan address.
    5. The output must contain the same active datum as the input except the amount paid must be subtracted from the loanOutstanding field - the script also calculates the new outstanding balance.
    6. If the new outstanding balance calculated by the script is <= 0, then this is a full payment:
        1. All collateral in the UTxO is available to take.
        2. The BorrowerID must be burned.
        3. No other tokens can be minted/burned in the transaction.
        4. The output to the address must contain the active beacon and the LenderID.
    7. If the new outstanding balance is > 0, then this is a partial payment.
        1. No collateral can be taken.
        2. The output to the address must have the active beacon, the BorrowerID, and the LenderID.
4. If the active beacon is missing, then this is not a valid active UTxO. The address owner gets custody of this invalid UTxO. The conditions for spending are satisfied by the presence of the active datum and the staking credential approving.

It is import to note that when a loan is fully paid off, the BorrowerID must be burned by itself. As you will soon see, this makes it trivial to check whether a borrower defaulted or not.

The reason why the invalid-hereafter option is used instead of invalid-before is because it is impossible to tell the script that the time is earlier than it actually is with invalid-hereafter. This means the borrower cannot trick the script into thinking the loan is still active when it is actually expired. If you told the script the time is 14 POSIXTime when it is really 20 POSIXTime, this transaction wouldn't even be valid. If invalid-before was used, it *would* be possible to tell the script an earlier time that it actually is. However, with invalid-before, it is impossible to tell the script a later time than it actually is. You'll see why this is important in the next section.

Note that once a borrower fully pays off a loan and burned his/her BorrowerID, the Borrower can never again spend that UTxO. Partial payments require re-outputing a BorrowerID with the output to the loan address while fully paying off the loan requires burning the BorrowerID. Neither of these conditions can be met again once the BorrowerID is burned. The tight coupling of the minting policy and the loan validator script also make it impossible to mint a spare BorrowerID to try getting around this. **The borrower must withdraw all his/her collateral in the same transaction where the loan is fully paid off.** As stated, once the BorrowerID is burned, it will no longer be possible to reclaim the collateral.

The reason why the loan validator also checks that there is only one input from the loan address even though it checks the number of beacons present is so that invalid UTxOs cannot skew the repayment calculations.

#### Claiming expired or fully paid loans
The last redeemer is the `Claim` redeemer which allows the lender to claim expired or fully paid active UTxOs. The minting policy is also used with the `BurnBeaconToken` redeemer.

To claim an active UTxO as the lender, all of the following must be true:
1. The UTxO must have an active datum. If it doesn't this is not an active UTxO.
2. The input UTxO must have an active beacon. If it doesn't, this is not a valid active UTxO and can be claimed by the address owner using the `RepayLoan` redeemer.
3. No other beacons are allowed in the transaction inputs - this ensures only the Active beacon, LenderID, and maybe a BorrowerID are present.
4. The loan must either be expired (specified by invalid-before) or the loanOutstanding in the datum must be <= 0 (signifying fully paid off).
5. The active beacon must be burned.
6. The LenderID must be burned.
7. If the BorrowerID is still present, it must be burned too.
8. No other tokens can be minted/burned in the transaction.
9. The lender must sign the transaction.

As previously mentioned, using invalid-before makes it impossible to tell the script the current time is later than it actually is since the transaction would not be valid yet. This means the lender can not trick the script into thinking the loan is expired when it actually isn't.

It is important to note that when the BorrowerID is still present, then by definition, the borrower defaulted since the only way for the BorrowerID to still be present is if the loan expires. Then, when it is burned, the transaction has 3 tokens burned in total. This is important for the next section.

#### On-Chain credit history
Using the beacon tokens, it is possible to locate all relevant UTxOs.

The lender can easily find all the UTxOs that belong to him/her by querying the LenderID. The lender can then sort out UTxOs that have an Offer beacon from those that have an Active Beacon.

The credit history uses the BorrowerID beacon. The Koios and Blockfrost APIs allow one to query all minting/burning transactions for a specific asset:

| Koios | Blockfrost |
|--|--|
| [api](https://api.koios.rest/#get-/asset_history) | [api](https://docs.blockfrost.io/#tag/Cardano-Assets/paths/~1assets~1%7Basset%7D~1history/get) |

From this information, you can figure out whether the borrower defaulted based on whether there were 3 tokens (BorrowerID,LenderID,Active token) burned when the BorrowerID was burned or only one token (the BorrowerID). Blockfrost is better set up for this query since your can use this [api](https://docs.blockfrost.io/#tag/Cardano-Transactions/paths/~1txs~1%7Bhash%7D/get) with the target transaction hash to see the number of unique assets mint/burned in the "asset_mint_or_burn_count" field. You can get the same information with Koios but it requires more queries.

So if the BorrowerID was burned by itself, then the borrower successfully paid back the loan. If the BorrowerID was burned with 2 other tokens (LenderID and Active token), then the borrower defaulted. You can also get the loan information from the burn transactions by finding the input datum that was attached to the BorrowerID. That datum has the snapshot of the loan status when the borrower defaulted.

### Other comments

Unlike with cardano-swaps, the cardano-loans does not convert ADA to lovelace. You are responsible for given units of lovelace whenever ADA is used.