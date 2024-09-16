# Revision history for cardano-loans

## 2.0.0.0rc

#### Bug Fixes

- [On-Chain] Fixed a bug where non-compounding loans could not have incentivized minimum payments.
The previous design required `compoundFrequency` to be `Nothing` in order to enable
non-compounding, but this would also disable the required minimum payments. Now, a separate boolean
flag is used in the datum to toggle compounding of the interest. `compoundFrequency` was renamed
to `epochDuration` and `lastCompounding` was renamed to `lastEpochBoundary` in order to make the
names more representative of their functionality.

## 1.0.0.1rc

#### Bug Fixes

- [Off-Chain] Fixed bug in CLI executable where the `lastCompounding` field in the datum was not
being set properly when multiple interest payments were made in the same transaction. The protocol
would properly reject this transaction.

## 1.0.0.0rc

#### New Features

Most of the features discussed in the *Potential Future Features* section of the previous version's
README have been added:

- *Interest free, non-compounding, and compounding interest loans* - the compounding periods are set
as part of the negotiation phase.
- *Required minimum payments* - a borrower is required to repay a certain amount of the loan during
each compounding period. If the minimum amount is not paid, a penalty will be applied against the
outstanding balance the next time interest is applied to the loan. This penalty incentivizes
borrowers to make regular payments. The penalty is set as part of the negotiation phase.
- *Direct payments to lenders* - when a borrower makes a payment, the payment must go directly to
the lender's specified address. This address can be updated by the lender.
- *Tradable bonds* - when a loan is accepted, the borrower must create a lock & key NFT pair that is
unique to the new loan. The key copy must go to the lender's specified address (the same address
where future payments will go). This key NFT identifies who the lender is at any point in time. If
the lender sells this key NFT to another entity, the new entity becomes the lender in the eyes of
the protocol. The key NFT can be freely traded. The functionality of the Key NFT is specifically
designed to pair with a secondary market like
[cardano-secondary-market](https://github.com/fallen-icarus/cardano-secondary-market); the new
lender can update the payment address in the same transaction where the key NFT is purchased on the
secondary market. This composition dramatically minimizes possible financial risk for buyers of
these key NFTs. Imagine what would happen if Bob purchased the key NFT, but Alice fully paid off the
loan before Bob had a chance to update the payment address...
- *Multi-asset loans* - the original restriction of only one acceptance per transaction has been
lifted. It is now possible to accepted multiple offers in a single transaction. Because of this, it
is now possible for Alice to accept an offer for ada in the same transaction where she accepts an
offer for djed. This composition enables borrowers to create "packaged" loans where they borrow
different assets as a hedge against "global" price movements of any one asset. It is also possible
to make payments on all of these loans in a single transaction. So effectively, these separate loans
can be treated as a single loan that requested multiple different assets as the loan asset.
- *Staking script support* - borrowers and lenders can both use staking credentials for their
identities. This enables the possibility of using multi-sig (or other more complicated logic) to
protect an entity's identity. This is a major step towards enabling corporations to start using the
protocol.
- *Additional beacons* - there is now a loan asset beacon, a loan id beacon, and even the collateral
used can act like a beacon. These new beacons increase the kinds of queries that are possible.
- *Updating Asks/Offers in-place* - Ask and Offer UTxOs can now be updated in place. The first
version required two transactions to do this (closing in one transaction and creating the new UTxO
in another).
- *Relaxed Credit History Method* - Borrower IDs can now be burned in the same transaction where
other tokens are minted/burned.

#### Optimizations

The smart contracts have been re-written in aiken, and all of the "one per tx" restrictions have
been lifted. Every part of the protocol has seen huge performance improvements over the PlutusTx
version.

The logic for the protocol was broken over several smart contracts to allow fitting more features
into the protocol. The universal loan spending script just delegates to one of the other scripts
which can either be executed as minting policies or staking scripts. Since all scripts can be used
as reference scripts, there was no drawback from splitting up the logic like this.

## 1.0.0 (MVP)

* First version. Released on an unsuspecting world.
