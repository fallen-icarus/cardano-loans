---
Number: 2
Title: Proxy Scripts for Lender Addresses
---

<!-- ADR template adapted from Michael Nygard's -->

## Context

<!-- What is the issue that we're seeing that is motivating this decision or change? -->

Since the borrower can make payments directly to the lender's address of choice, there is an issue
of what datum should be attached to each payment. Since plutus scripts require a datum, if the
lender's address uses a plutus script for the payment credential all outputs must contain a datum.
Native scripts do not have this datum requirement but there is no way for the protocol to
distinguish between a native script and a plutus script; they both just appear as a script hash.
Furthermore, the borrower should not be trusted to create an arbitrary datum on the lender's behalf;
the datum creation must be guarded by the protocol in order to protect the lender's revenue.

One option is to disallow the lender's address from being a script address of any kind. However,
this is extremely undesirable since it would dramatically limit the usecases for the protocol.
Lenders should be able to use scripts for the lender address because it would be easier for
businesses to use the protocol. For example, a business may need to use a multisig to manage the
revenue from the loans it makes for compliance reasons. Other businesses may require more arbitrary
logic. These businesses may wind up being a huge soure of liquidity for the protocol so script based
lender addresses should be a priority.

Another option is to have an extra field in the loan's datum that specifies the datum borrowers must
attach to payment outputs. This is effectively a datum within a datum. This has its own major
drawbacks since it would severly hurt the composability of the protocol. If the datum is effectively
twice as large, almost entirely due to the lender's required datum, the composability of the
protocol could easily be cut in half.

Instead of storing the required datum directly in the loan's datum, the lender can store the
information in another UTxO's datum that must be referenced during payments. The protocol would thus
be able to get the required datum without it being stored directly in the protocol's datum.
Unfortunately, this method still dramatically harms the composability of the protocol. Instead of
having the required datum presented immediately to the protocol upon script execution, the protocol
must traverse the reference inputs to find the required datum for payments. This traversal must
occur with every execution in the transaction. On net, this approach is arguably worse for
composability than storing the required datum directly in the loan datum due to the extra
computation required with each execution.

The last option is to have a pre-approved plutus script that can accept any datum, can be used with
any redeemer, and simply delegates spending to the address' staking credential. If a lender wants to
use a script to manage the revenue, the lender's address **must** use the this pre-approved plutus
script as the payment credential and it **must** have a staking credential. Since the plutus script
just delegates spending to the staking credential, this pre-approved script can effectively allow
for for arbitrary logic. It is effectively a proxy, hence the name.

## Decision

<!-- What is the change that we're proposing and/or doing? -->

cardano-loans uses the proxy script approach. All lender addresses must either use a payment pubkey
(and an optional staking credential) or the proxy plutus script with a staking credential.

## Consequences

<!-- What becomes easier or more difficult to do because of this change? -->

There are a few major benefits to this approach:
1. The protocol can easily enforce all lender addresses use the proxy if they want arbitrary logic.
   There is no need for the protocol to have a white list of available plutus script "templates"
   since the proxy script already supports arbitrary logic.
2. Since the datum for outputs to the proxy script can be any datum, datums for payments can be set
   by the protocol. Using the datums is a very cheap way to prevent double satisfaction of loan
   payments.
3. The same proxy script can be used by all P2P DeFi protocols since it can accept any datum. This
   means users only have one additional address to manage.
4. The use of this proxy script does not impact the composability of P2P DeFi protocols at all.

AFAICT, the only downside of this approach is that borrowers cannot send payments directly to
another protocol on behalf of the lender. The payments must first go to the proxy script and then be
passed on by the lender directly. However, I do not see this as a significant drawback - the fees
associated with spending from the proxy script are extremely low and UTxOs can be spent in batches
of 20-30 UTxOs per transaction (in the case of the staking credential being a multisig native
script). Considering the benefits, this drawback seems almost negligible. 


