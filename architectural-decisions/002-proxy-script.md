---
Number: 2
Title: Proxy Scripts for Lender Addresses
---

<!-- ADR template adapted from Michael Nygard's -->

## Context

<!-- What is the issue that we're seeing that is motivating this decision or change? -->

Since using the datums to guarantee uniqueness of loan payments is currently the cheapest option,
all lender addresses must be able to accept this datum. In the future, it may be possible to cheaply
use the reference script field of the UTxO for uniqueness instead which would free up the datum. For
now, all lender addresses must be able to accept UTxOs with the datum enforced by the protocol.

One option would be allow addresses with any arbitrary payment script and trust the lender to ensure
that the script can accept the required datum. However, mistakes happen and these are financial
transactions so safe guards should be used whenever possible.

Another option is to disallow plutus scripts. Unfortunately, since a plutus script cannot
distinguish between a native script and a plutus script, the only way to actually do this is to
disallow all payment scripts altogether. Not allowing any payment script would prevent the use of
multisig - a desirable feature for pooled lenders. Therefore, this option is too strict.

A different approach would be to allow the lenders to set up a personal, "pre-approved" proxy script
address that can have arbitrary logic for what to do next. It can accept any datum and redeemer. The
proxy payment script would simply delegate spending authority to the staking credential for the
proxy address. This has a few benefits: 1) There can be a single payment script hash that the loan
validator can look for which make the check cheap. 2) The proxy script can still have arbitrary
logic since it can accept any datum and redeemer, and just delegates to the staking script, which
can be a pubkey, native script, or plutus script. 3) The datum in loan payment outputs can still be
determined by the loan validator which is more convenient than using a receipt token. This approach
still allows lenders to use multisig despite being strict on the type of datum used for loan
payments.  

## Decision

<!-- What is the change that we're proposing and/or doing? -->

Cardano-Loans uses the "pre-approved" script approach. All lender addresses must either use a
payment pubkey (and an optional staking credential) or the proxy plutus script with a staking
credential.

## Consequences

<!-- What becomes easier or more difficult to do because of this change? -->

There are a few major benefits to this approach:
1. The protocol can easily enforce all lender addresses use the proxy if they want arbitrary
   logic. There is no need for the protocol to have a white list of available plutus script
   "templates" since the proxy script already supports arbitrary logic.
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
script). Considering the benefits, this drawback seems almost negligible. Once it becomes possible
to use the reference script field for uniqueness, the datum can be freed up for other usages if
desired.


