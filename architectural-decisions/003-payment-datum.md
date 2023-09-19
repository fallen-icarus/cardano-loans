---
Number: 3
Title: Payment Datums
---

<!-- ADR template adapted from Michael Nygard's -->

## Context

<!-- What is the issue that we're seeing that is motivating this decision or change? -->

When a payment is made to a lender, the Key NFT is not present in the output which means the
protocol needs some other way to guarantee uniqueness of outputs and prevent double satisfaction.

One option is to mint a "receipt NFT" with every payment that is unique to that payment. This would
guarantee uniqueness but it would hurt composability since an extra, unique token must be minted for
every payment in the transaction. Not only that but it would also create more work for the lenders
since this "receipt NFT" would now need to be burned after each payment.

Another option is to use the datum for each output. Since cardano-loans enforces that, if lenders
want to use arbitrary logic for the lender address, lenders must use the pre-approved proxy script
which accepts any datum, the datum can be set by the protocol as needed. The question is: What
should the payment datum be? No matter what, the datum must guarantee uniqueness of outputs. There
are two possible options for using the datum:
1) The datum is just the LoanID for the corresponding loan.
2) The datum is both the LoanID for the corresponding loan **and** the beacon policy id for the
   protocol. 

In both cases, the LoanID is what guarantees uniqueness of payments from the perspective of the
protocol. At first glance, the inclusion of the beacon policy id may seem unnecessary. However,
there is a concern with future protocol compositions. In both cases, the LoanID is determined by the
output reference for the Offer UTxO created by this protocol. What would happen if another protocol
was created that would "watch" this Offer UTxO and require a payment if that Offer UTxO is ever
accepted? That protocol may want to use the target Offer UTxO's output reference in the same way
which would match the LoanID's token name (the actual LoanID token would still have a different
policy id). How would the two protocols be able to tell outputs apart if they go to the same address
and the datum only contains the LoanID? This opens the door for future double satisfaction during
protocol composition. By including the beacon policy id in the datum, each protocol would easily be
able to distinguish between outputs meant for each protocol. The beacon policy id effectively acts
as a protocol id.

## Decision

<!-- What is the change that we're proposing and/or doing? -->

cardano-loans uses the datum instead of a receipt token. The datum includes both the LoanID for the
corresponding loan and the beacon policy id.

## Consequences

<!-- What becomes easier or more difficult to do because of this change? -->

Since a receipt token is not used, lenders don't have to worry about worthless NFTs accumulating
in their addresses. From a usability perspective, this is likely a significant benefit.

While I personally cannot imagine a use case for "watching" Offer UTxOs as described above, that
does not mean there isn't a legimate one. Therefore, including the beacon policy id in the payment
datums would future proof the double satisfaction prevention in the scenario where a "watcher"
protocol does arise.

Finally, since lender addresses can possibly be the proxy plutus script, all outputs require a datum
anyway. This approach naturally takes advantage of that requirement.

The only downside I can see is that payments cannot go directly to another protocol since the output
would be guaranteed to have the wrong datum. However, in the case of cardano-loans, the borrower is
creating the datum on behalf of the lender. Since datums usually are involved in controlling custody
of assets, borrowers should not be trusted to create datums for lenders anyway. See [ADR
#2](002-proxy-script.md) for a more detailed discussion of this. In short, having the borrower
create the datum on the lender's behalf would dramatically hurt the composition of the protocol
(since the protocol would need to guard against the datum being created maliciously) but would not
save the lender much in fees when compared to the lender first receiving the payments directly and
then personally passing the funds onto the next protocol.
