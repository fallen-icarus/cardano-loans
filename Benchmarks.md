# Benchmarks (YMMV)

All benchmarks were done using reference scripts and the cardano-node emulator that is part of [plutus-apps](https://github.com/input-output-hk/plutus-apps). The scripts for this protocol are too large to be used locally so it is required that reference scripts are used. The emulator used the same parameters as the mainnet.

The loan validator script requires about 57 ADA to be stored on chain as a reference script.
The beacon policy script requires about 65 ADA to be stored on chain as a reference script.

It is recommended that users share reference scripts using something like [cardano-reference-script](https://github.com/fallen-icarus/cardano-reference-scripts) so that there is only one copy of each script stored on chain at a time.

## Creating Ask(s)

| Asks Created | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.264450 ADA | 0.324675 ADA |
| 5 | 0.296074 ADA | 0.444111 ADA |
| 10 | 0.395604 ADA | 0.593406 ADA |
| 15 | 0.495133 ADA | 0.742700 ADA |
| 30 | 0.793811 ADA | 1.190717 ADA |
| 60 | 1.397061 ADA | 2.095592 ADA |

The max number of Asks that can be created in a single transaction is about 70.

## Creating an Offer

Due to the way the LoanIDs work, only one Offer can be created in a single transaction.

Tx Fee = 0.249046 ADA
Req. Collateral = 0.37359 ADA

## Closing Ask(s)

| Asks Closed | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.203082 ADA | 0.304623 ADA |
| 2 | 0.242961 ADA | 0.364442 ADA |
| 3 | 0.305262 ADA | 0.457893 ADA |
| 4 | 0.389984 ADA | 0.584976 ADA |
| 5 | 0.497127 ADA | 0.745691 ADA |
| 6 | 0.626692 ADA | 0.940038 ADA |
| 7 | 0.778677 ADA | 1.168016 ADA |
| 8 | 0.953084 ADA | 1.429626 ADA |
| 9 | 1.149911 ADA | 1.724867 ADA |
| 10 | 1.369160 ADA | 2.053740 ADA |

Closing 11 Asks in a single transaction exceeded the limits.

## Closing Offer(s)

| Offers Closed | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.218414 ADA | 0.327621 ADA |
| 2 | 0.278140 ADA | 0.417210 ADA |
| 3 | 0.366763 ADA | 0.550145 ADA |
| 4 | 0.484282 ADA | 0.726423 ADA |
| 5 | 0.630697 ADA | 0.946046 ADA |
| 6 | 0.807008 ADA | 1.209012 ADA |
| 7 | 1.010215 ADA | 1.515323 ADA |
| 8 | 1.243318 ADA | 1.864977 ADA |

Closing 9 Offers in a single transaction exceeded the limits.

## Accept Offer(s)

| Offers Accepted | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.382032 ADA | 0.573048 ADA |
| 2 | 0.572513 ADA | 0.858770 ADA |
| 3 | 0.787706 ADA | 1.181559 ADA |
| 4 | 1.022435 ADA | 1.533653 ADA |
| 5 | 1.290657 ADA | 1.935986 ADA |
| 6 | 1.589800 ADA | 2.384700 ADA |

Accepting 7 offers in a single transaction exceeded the limits.

## Make Payment(s)

It is possible to have both partial payments and full payments in a single transaction but they were benchmarked separately for simplicity. Combining them will yield results somewhere in between the two homogeneous scenarios.

#### All Full Payments

| Full Payments | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.294732 ADA | 0.442098 ADA |
| 2 | 0.407094 ADA | 0.610641 ADA |
| 3 | 0.543336 ADA | 0.815004 ADA |
| 4 | 0.698230 ADA | 1.047345 ADA |
| 5 | 0.864817 ADA | 1.297226 ADA |
| 6 | 1.051253 ADA | 1.576880 ADA |
| 7 | 1.260839 ADA | 1.891259 ADA |
| 8 | 1.482583 ADA | 2.223875 ADA |

Making 9 full payments in a single transaction exceeded the limits.

#### All Partial Payments

| Partial Payments | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.302913 ADA | 0.454370 ADA |
| 2 | 0.453854 ADA | 0.680781 ADA |
| 3 | 0.642404 ADA | 0.963606 ADA |
| 4 | 0.859706 ADA | 1.289559 ADA |
| 5 | 1.106610 ADA | 1.659915 ADA |
| 6 | 1.380873 ADA | 2.071310 ADA |

Making 7 partial payments in a single transaction exceeded the limits.

## Rollover Loan(s)

| Loans Rolled Over | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.240803 ADA | 0.361205 ADA |
| 2 | 0.308611 ADA | 0.462917 ADA |
| 3 | 0.386348 ADA | 0.579522 ADA |
| 4 | 0.474015 ADA | 0.711023 ADA |
| 5 | 0.571568 ADA | 0.857352 ADA |
| 6 | 0.679095 ADA | 1.018643 ADA |
| 7 | 0.796552 ADA | 1.194828 ADA |
| 8 | 0.923938 ADA | 1.385907 ADA |
| 9 | 1.061210 ADA | 1.591815 ADA |
| 10 | 1.208413 ADA | 1.812620 ADA |

Tests were stopped here but it is likely that a few more could fit in the transaction before exceeding the limits.

## Update Address(s)

For these tests, each Key NFT had a separate input and output UTxO. If they were grouped together into UTxOs (which is possible if they are owned by the same entity), the performance would be better.

| Loans Updated | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.271612 ADA | 0.407418 ADA |
| 2 | 0.465087 ADA | 0.697631 ADA |
| 3 | 0.800727 ADA | 1.201091 ADA |
| 4 | 1.345657 ADA | 2.018486 ADA |

Updating 5 addressses in a single transaction exceeded the limits.

## Claim Expired Loan(s)

For these tests, each Key NFT had a separate input. If they were grouped together into UTxOs (which is possible if they are owned by the same entity), the performance would be better.

| Expired Loans Claimed | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.251218 ADA | 0.376827 ADA |
| 2 | 0.401230 ADA | 0.601845 ADA |
| 3 | 0.683489 ADA | 1.025234 ADA |
| 4 | 1.165032 ADA | 1.747548 ADA |

Claiming 5 expired loans in a single transaction exceeded the limits.

## Unlock Lost Collateral

All loan inputs for these tests were expired loans. Cleaning up the beacons of finished loans is likely about the same.

| Lost Collateral Unlocked | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.233954 ADA | 0.350931 ADA |
| 2 | 0.331971 ADA | 0.497957 ADA |
| 3 | 0.497332 ADA | 0.745998 ADA |
| 4 | 0.760604 ADA | 1.140906 ADA |
| 5 | 1.128233 ADA | 1.692350 ADA |

Unlocking 6 lost collateral UTxOs in a single transaction exceeded the limits.