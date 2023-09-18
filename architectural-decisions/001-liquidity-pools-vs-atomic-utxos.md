---
Number: 1
Title: Liquidity Pools vs Atomic UTxOs
---

<!-- ADR template adapted from Michael Nygard's -->

## Context

<!-- What is the issue that we're seeing that is motivating this decision or change? -->

Price discovery is at the heart of an economy. The health and resilience of an economy largely
depends on the ease with which price discovery can occur. A decentralized economy is no exception
and so price discovery should be front and center when designing DeFi protocols. The process of
price discovery depends on several factors, **not just supply and demand.** To quote
[investopedia](https://www.investopedia.com/terms/p/pricediscovery.asp), "The process of price
discovery looks at a number of tangible and intangible factors, including supply and demand,
investor risk attitudes, and the overall economic and geopolitical environment. Simply put, it is
where a buyer and a seller agree on a price and a transaction occurs."

Liquidity pool designs make price discovery virtually impossible by not allowing users to express
their own conclusions. Instead, they prioritize liquidity from day one and only base price on
supply. While this approach enables market liquidity from day one, the loss of true price discovery
makes it impossible to use it as the foundation for a healthy and resilient economy. Atomic UTxOs,
on the other hand, are very expressive in the choices that users can make and therefore strongly
facilitate price discovery. While atomic UTxOs may not be able to promise the same level of
liquidity from day one, the liquidity of atomic UTxOs naturally grows with adoption. While it may
take a little longer to develop, an economy based on atomic UTxOs will be far healthier and
more resilient to economic shocks than one based on liquidity pools.

While price discovery deals with an economy, the security of Proof-of-Stake blockchains operates
largely the same way. A core security assumption of the [ouroboros
protocol](https://iohk.io/en/research/library/papers/ouroboros-a-provably-secure-proof-of-stake-blockchain-protocol/)
are based on users being able to freely express their preferences of stake pools. Liquidity pools
kill this assumption by confining users to only a few choices:
1. You cannot participate at all in PoS - the liquidity pool address does not have a staking
   credential.
2. You can only choose from a set of pre-approved stake pools - there is a liquidity pool set up at
   addresses which are delegated to the pre-approved pool.
3. You cannot choose at all. Instead the liquidity pool address is delegated to the stake pool
   chosen by the entity behind the protocol. This is akin to stealing your delegation.

You may think that #2 isn't bad but in practice it is. The top lending/borrowering dApp on Cardano
only allows users to delegate to [16 out of the 3,500 available stake
pools](https://liqwid.notion.site/Liqwid-FAQ-4cb0cf5509664e9b83d5d1207ca9a8ac#a68bb55606954b6f8cfc758caeaf7134).
Since liquidity pools require users to pool assets together, it is not possible to have both
liquidity pools and full delegation control for users. It is a fundamental limitation of liquidity
pools.

Another way in which liquidity pools undermine the security of Cardano is with voting. Currently,
voting power is tied to a user's [staking credential](https://cips.cardano.org/cips/cip15/). When a
user deposits assets into an address with a staking credential they do not control, they are giving
their voting power to the entity that actually does the staking credential. Considering how
many assets can wind up being pooled into a few liquidity pools, this is an existential threat to
democracy of Cardano. "Power corrupts; absolute power corrupts absolutely."

## Decision

<!-- What is the change that we're proposing and/or doing? -->

Due to the aforementioned issues with liquidity pools, cardano-loans uses atomic UTxOs. While this
means the liquidity will require time to grow, it will result in a more stable lending/borrowering
economy over the long run. Furthermore, atomic UTxOs naturally align with the security
assumptions of Proof-of-Stake blockchains which means Cardano will become more secure as the economy
develops, not less.

## Consequences

<!-- What becomes easier or more difficult to do because of this change? -->

Since all users can get their own UTxOs, the protocol can offer many personalizable settings.
- Interest rates - compounding, non-compounding, and interest-free loans are all supported.
- Assets used as collateral - borrowers can use any number of assets as collateral.
- Time period of the loan.
- Required collateralization - over-collateralization, under-collateralization, and
  market-value-collateralization are all supported.
- Minimum loan payments - lenders can optionally request minimum loan payments to help them manage
  the risk of the loan.
- Built-in borrower credit history - the history is cryptographically tied to the borrower so as
  long as the borrower's private key is not stolen, their identity cannot be stolen.
- Full delegation control.
- Full spending control - except when the protocol needs to enforce certain logic.
- Full voting control.
- Naturally composable with other DeFi protocols - since atomic UTxOs are significantly smaller than
  liquidity pool UTxOs, it is much easier to combine atomic UTxOs into a single transaction.
- Fully p2p - through the use of beacon tokens, there is no need to use batchers which means
  protocol fees can be up to 10x cheaper than liquidity pool fees.
- Prices are fully endogenous - there is no need to rely on an external information feed; the
  Cardano economy can be fully independent.

There are a few downsides that should be mentioned.
- No way to enforce that requests are executed in the same order in which they are submitted.
- Liquidity may be low in the early stages of adoption.

I believe these downsides are acceptable given the rest of the trade-off profile. Business models
can be built on top of this protocol to provide enforcement of order requests for users that care
about this feature. As for liquidity, low liquidity in the early stages just means that a user may
need to wait a while to find a counterparty. Again, this is only temporary as adoption grows.
Neither of these drawbacks seem significant to me. As someone who intends to use this protocol, I
would much rather be patient and end up with a very healthy and resilient decentralized ecosystem
than rush it and wind up with a very unhealthy and fragile one.
