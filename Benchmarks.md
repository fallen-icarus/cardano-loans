# Benchmarks (YMMV)

All benchmarks were done using reference scripts and the cardano-node emulator that is part of [plutus-apps](https://github.com/input-output-hk/plutus-apps). The scripts for this protocol are too large to be used locally so it is required that reference scripts are used. The emulator used the same parameters as the mainnet.

The loan validator script requires about 62 ADA to be stored on-chain as a reference script.
The beacon policy script requires about 70 ADA to be stored on-chain as a reference script.

It is recommended that users share reference scripts using something like [cardano-reference-script](https://github.com/fallen-icarus/cardano-reference-scripts) so that there is only one copy of each script stored on chain at a time.



## Create Ask(s)
#### All UTxOs are for the same loan asset. One asset was offered for collateral.
| Asks Created | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.227435 ADA | 0.341153 ADA |
| 5 | 0.318064 ADA | 0.477096 ADA |
| 10 | 0.431350 ADA | 0.647025 ADA |
| 15 | 0.544636 ADA | 0.816954 ADA |
| 30 | 0.884626 ADA | 1.326939 ADA |
| 45 | 1.230557 ADA | 1.845836 ADA |
| 59 | 1.547758 ADA | 2.321637 ADA |

The maximum number of Ask UTxOs that could be created was 59. The 60th pushed the transaction over
the maximum transaction size limit.

#### All UTxOs are for different loan assets. One asset was offered for collateral.
| Asks Created | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.227523 ADA | 0.341285 ADA |
| 5 | 0.364423 ADA | 0.546635 ADA |
| 10 | 0.592033 ADA | 0.888050 ADA |
| 15 | 0.843076 ADA | 1.264614 ADA |
| 20 | 1.129576 ADA | 1.694364 ADA |
| 25 | 1.453038 ADA | 2.179557 ADA |
| 28 | 1.699126 ADA | 2.548689 ADA |

The maximum number of Ask UTxOs that could be created was 28. The 29th pushed the transaction over
the limits.

#### All UTxOs are for the same loan asset. Two assets were offered for collateral.
| Asks Created | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.229547 ADA | 0.344321 ADA |
| 5 | 0.328624 ADA | 0.492936 ADA |
| 10 | 0.452471 ADA | 0.678707 ADA |
| 15 | 0.576318 ADA | 0.864477 ADA |
| 30 | 0.947991 ADA | 1.421987 ADA |
| 45 | 1.325603 ADA | 1.988405 ADA |
| 51 | 1.474220 ADA | 2.211330 ADA |

The maximum number of Ask UTxOs that could be created was 51. The 52nd pushed the transaction over
the maximum transaction size limit.

#### All UTxOs are for different loan assets. Two assets were offered for collateral.
| Asks Created | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.229635 ADA | 0.344453 ADA |
| 5 | 0.374983 ADA | 0.562475 ADA |
| 10 | 0.613154 ADA | 0.919731 ADA |
| 15 | 0.874758 ADA | 1.312137 ADA |
| 20 | 1.171818 ADA | 1.757727 ADA |
| 25 | 1.505841 ADA | 2.258762 ADA |
| 28 | 1.758266 ADA | 2.637399 ADA |

The maximum number of Ask UTxOs that could be created was 28. The 29th pushed the transaction over
the limits.

#### All UTxOs are for the same loan asset. Three assets were offered for collateral.
| Asks Created | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.231659 ADA | 0.347389 ADA |
| 5 | 0.339185 ADA | 0.508778 ADA |
| 10 | 0.473593 ADA | 0.710390 ADA |
| 15 | 0.608000 ADA | 0.912000 ADA |
| 30 | 1.011355 ADA | 1.517033 ADA |
| 45 | 1.420650 ADA | 2.130975 ADA |

The maximum number of Ask UTxOs that could be created was 45. The 46th pushed the transaction over
the maximum transaction size limit.

#### All UTxOs are for different loan assets. Three assets were offered for collateral.
| Asks Created | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.231747 ADA | 0.347621 ADA |
| 5 | 0.385544 ADA | 0.578316 ADA |
| 10 | 0.634276 ADA | 0.951414 ADA |
| 15 | 0.906440 ADA | 1.359660 ADA |
| 20 | 1.214061 ADA | 1.821092 ADA |
| 25 | 1.558645 ADA | 2.337968 ADA |
| 28 | 1.817406 ADA | 2.726109 ADA |

The maximum number of Ask UTxOs that could be created was 28. The 29th pushed the transaction over
the limits.



## Close Ask(s)
#### All Asks were for the same loan asset. One asset was offered for collateral.
| Asks Closed | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.210340 ADA | 0.315510 ADA |
| 2 | 0.261983 ADA | 0.392975 ADA |
| 3 | 0.342300 ADA | 0.513450 ADA |
| 4 | 0.451291 ADA | 0.676937 ADA |
| 5 | 0.588954 ADA | 0.883431 ADA |
| 6 | 0.755291 ADA | 1.132937 ADA |
| 7 | 0.950302 ADA | 1.425453 ADA |
| 8 | 1.173985 ADA | 1.760978 ADA |

Closing 9 Asks in a single transaction exceeded the limits.

#### All Asks were for the same loan asset. Three assets were offered for collateral.
| Asks Closed | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.210956 ADA | 0.316434 ADA |
| 2 | 0.263216 ADA | 0.394824 ADA |
| 3 | 0.344149 ADA | 0.516224 ADA |
| 4 | 0.453756 ADA | 0.680634 ADA |
| 5 | 0.592036 ADA | 0.888054 ADA |
| 6 | 0.758989 ADA | 1.138484 ADA |
| 7 | 0.954616 ADA | 1.431924 ADA |
| 8 | 1.178915 ADA | 1.768373 ADA |

Closing 9 Asks in a single transaction exceeded the limits.

#### All Asks were for different loan assets. One asset was offered for collateral.
| Asks Closed | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.210340 ADA | 0.315510 ADA |
| 2 | 0.263918 ADA | 0.395877 ADA |
| 3 | 0.354467 ADA | 0.531701 ADA |
| 4 | 0.496097 ADA | 0.744146 ADA |
| 5 | 0.715361 ADA | 1.073042 ADA |
| 6 | 1.028468 ADA | 1.542702 ADA |

Closing 7 Asks in a single transaction exceeded the limits.

#### All Asks were for different loan assets. Three assets were offered for collateral.
| Asks Closed | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.210956 ADA | 0.316434 ADA |
| 2 | 0.265150 ADA | 0.397725 ADA |
| 3 | 0.356316 ADA | 0.534474 ADA |
| 4 | 0.498562 ADA | 0.747843 ADA |
| 5 | 0.718442 ADA | 1.077663 ADA |
| 6 | 1.032166 ADA | 1.548249 ADA |

Closing 7 Asks in a single transaction exceeded the limits.



## Create Offer(s)
#### All Offers were for the same loan asset. One asset was allowed for collateral.
| Offers Created | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.265886 ADA | 0.398829 ADA |
| 5 | 0.490478 ADA | 0.735717 ADA |
| 10 | 0.769755 ADA | 1.154633 ADA |
| 15 | 1.049031 ADA | 1.573547 ADA |
| 20 | 1.328308 ADA | 1.992462 ADA |
| 25 | 1.607937 ADA | 2.411906 ADA |
| 31 | 1.941854 ADA | 2.912781 ADA |

Closing 32 Offers in a single transaction exceeded the limits.

#### All Offers were for the same loan asset. Three assets were allowed for collateral.
| Offers Created | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.276548 ADA | 0.414822 ADA |
| 5 | 0.543785 ADA | 0.815678 ADA |
| 10 | 0.876368 ADA | 1.314552 ADA |
| 15 | 1.208952 ADA | 1.813428 ADA |
| 20 | 1.541535 ADA | 2.312303 ADA |
| 25 | 1.874471 ADA | 2.811707 ADA |
| 27 | 1.989545 ADA | 2.984318 ADA |

Closing 28 Offers in a single transaction exceeded the limits.

#### All UTxOs are for different loan assets. One asset was offered for collateral.
| Offers Created | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.292198 ADA | 0.438297 ADA |
| 5 | 0.581352 ADA | 0.872028 ADA |
| 10 | 0.984450 ADA | 1.476675 ADA |
| 15 | 1.422016 ADA | 2.133024 ADA |
| 19 | 1.816136 ADA | 2.724204 ADA |

The maximum number of Offer UTxOs that could be created was 19. The 20th pushed the transaction 
over the limits.

#### All UTxOs are for different loan assets. Three assets were offered for collateral.
| Offers Created | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.302860 ADA | 0.454290 ADA |
| 5 | 0.634659 ADA | 0.951989 ADA |
| 10 | 1.091064 ADA | 1.636596 ADA |
| 15 | 1.582113 ADA | 2.373170 ADA |
| 18 | 1.874089 ADA | 2.811134 ADA |

The maximum number of Offer UTxOs that could be created was 18. The 19th pushed the transaction 
over the limits.



## Close Offer(s)
#### All Offers were for the same loan asset. One asset was offered for collateral.
| Offers Closed | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.225894 ADA | 0.338841 ADA |
| 2 | 0.299496 ADA | 0.449244 ADA |
| 3 | 0.410089 ADA | 0.615134 ADA |
| 4 | 0.557675 ADA | 0.836513 ADA |
| 5 | 0.742252 ADA | 1.113378 ADA |
| 6 | 0.963822 ADA | 1.445733 ADA |
| 7 | 1.222383 ADA | 1.833575 ADA |

Closing 8 Offers in a single transaction exceeded the limits.

#### All Offers were for the same loan asset. Three assets were offered for collateral.
| Offers Closed | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.228184 ADA | 0.342276 ADA |
| 2 | 0.304075 ADA | 0.456113 ADA |
| 3 | 0.416959 ADA | 0.625439 ADA |
| 4 | 0.566834 ADA | 0.850251 ADA |
| 5 | 0.753701 ADA | 1.130552 ADA |
| 6 | 0.977560 ADA | 1.466340 ADA |
| 7 | 1.238411 ADA | 1.857617 ADA |

Closing 8 Offers in a single transaction exceeded the limits.

#### All Offers were for different loan assets. One asset was offered for collateral.
| Offers Closed | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.230050 ADA | 0.345075 ADA |
| 2 | 0.327041 ADA | 0.490562 ADA |
| 3 | 0.504117 ADA | 0.756176 ADA |
| 4 | 0.779228 ADA | 1.168842 ADA |
| 5 | 1.199798 ADA | 1.799697 ADA |

Closing 6 Offers in a single transaction exceeded the limits.

#### All Offers were for different loan assets. Three assets were offered for collateral.
| Offers Closed | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.232340 ADA | 0.348510 ADA |
| 2 | 0.331621 ADA | 0.497432 ADA |
| 3 | 0.510987 ADA | 0.766481 ADA |
| 4 | 0.788387 ADA | 1.182581 ADA |
| 5 | 1.211247 ADA | 1.816871 ADA |

Closing 6 Offers in a single transaction exceeded the limits.



## Accept Offer(s)
#### All Offers were for the same loan asset, from the same lender, and one asset was used for collateral.
| Offers Accepted | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.418517 ADA | 0.627776 ADA |
| 2 | 0.650351 ADA | 0.975527 ADA |
| 3 | 0.906127 ADA | 1.359191 ADA |
| 4 | 1.189321 ADA | 1.783982 ADA |
| 5 | 1.492221 ADA | 2.238332 ADA |

Accepting 6 offers in a single transaction exceeded the limits.

#### All Offers were for the same loan asset, from the same lender, and three assets were used for collateral.
| Offers Accepted | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.442928 ADA | 0.664392 ADA |
| 2 | 0.698497 ADA | 1.047746 ADA |
| 3 | 0.982651 ADA | 1.473977 ADA |
| 4 | 1.288828 ADA | 1.933242 ADA |
| 5 | 1.610975 ADA | 2.416463 ADA |

Accepting 6 offers in a single transaction exceeded the limits.

#### All Offers were for different loan assets, from the same lender, and one asset was used for collateral.
| Offers Accepted | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.387062 ADA | 0.580593 ADA |
| 2 | 0.615273 ADA | 0.922910 ADA |
| 3 | 0.882352 ADA | 1.323528 ADA |
| 4 | 1.179185 ADA | 1.768778 ADA |
| 5 | 1.500225 ADA | 2.250338 ADA |

Accepting 6 offers in a single transaction exceeded the limits.

#### All Offers were for different loan assets, from the same lender, and three assets were used for collateral.
| Offers Accepted | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.443804 ADA | 0.665706 ADA |
| 2 | 0.706691 ADA | 1.060037 ADA |
| 3 | 1.004774 ADA | 1.507161 ADA |
| 4 | 1.332616 ADA | 1.998924 ADA |
| 5 | 1.666771 ADA | 2.500157 ADA |

Accepting 6 offers in a single transaction exceeded the limits.

#### All Offers were for the same loan asset, from different lenders, and one asset was used for collateral.
| Offers Accepted | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.418561 ADA | 0.627842 ADA |
| 2 | 0.655377 ADA | 0.983066 ADA |
| 3 | 0.924633 ADA | 1.386950 ADA |
| 4 | 1.230036 ADA | 1.845054 ADA |
| 5 | 1.551010 ADA | 2.326515 ADA |

Accepting 6 offers in a single transaction exceeded the limits.

#### All Offers were for different loan assets, from different lenders, and one asset was used for collateral.
| Offers Accepted | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.415474 ADA | 0.623211 ADA |
| 2 | 0.652224 ADA | 0.978336 ADA |
| 3 | 0.919382 ADA | 1.379073 ADA |
| 4 | 1.217243 ADA | 1.825865 ADA |
| 5 | 1.545238 ADA | 2.317857 ADA |

Accepting 6 offers in a single transaction exceeded the limits.

#### All Offers were for different loan assets, from different lenders, and two assets were used for collateral.
| Offers Accepted | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.434345 ADA | 0.651518 ADA |
| 2 | 0.690142 ADA | 1.035213 ADA |
| 3 | 0.977439 ADA | 1.466159 ADA |
| 4 | 1.296433 ADA | 1.944650 ADA |
| 5 | 1.634345 ADA | 2.451518 ADA |

Accepting 6 offers in a single transaction exceeded the limits.

#### All Offers were for different loan assets, from different lenders, and three assets were used for collateral.
| Offers Accepted | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.448530 ADA | 0.672795 ADA |
| 2 | 0.717110 ADA | 1.075665 ADA |
| 3 | 1.034430 ADA | 1.551645 ADA |
| 4 | 1.377620 ADA | 2.066430 ADA |

Accepting 5 offers in a single transaction exceeded the limits.



## Make Payment(s)
It is possible to have both partial payments and full payments in a single transaction but 
they were benchmarked separately for simplicity. Combining them will yield results somewhere 
between the homogeneous scenarios.

#### All payments were full payments. Each loan used one asset for collateral.
| Payments Made | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.311219 ADA | 0.466829 ADA |
| 2 | 0.447519 ADA | 0.671279 ADA |
| 3 | 0.628243 ADA | 0.972365 ADA |
| 4 | 0.799128 ADA | 1.198692 ADA |
| 5 | 0.989274 ADA | 1.483911 ADA |
| 6 | 1.198649 ADA | 1.797974 ADA |
| 7 | 1.427799 ADA | 2.141699 ADA |

Making 8 full payments in a single transaction exceeded the limits.

#### All payments were full payments. Each loan used three assets for collateral.
| Payments Made | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.322167 ADA | 0.483251 ADA |
| 2 | 0.468257 ADA | 0.702386 ADA |
| 3 | 0.654975 ADA | 0.982463 ADA |
| 4 | 0.834691 ADA | 1.252037 ADA |
| 5 | 1.034870 ADA | 1.552305 ADA |
| 6 | 1.255269 ADA | 1.882904 ADA |
| 7 | 1.495546 ADA | 2.243319 ADA |

Making 8 full payments in a single transaction exceeded the limits.

#### All payments were partial payments. Each loan used one asset for collateral. Half of each loan was paid off and half of the collateral was reclaimed.
| Payments Made | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.317305 ADA | 0.475958 ADA |
| 2 | 0.493646 ADA | 0.740469 ADA |
| 3 | 0.701221 ADA | 1.051832 ADA |
| 4 | 0.952303 ADA | 1.428455 ADA |
| 5 | 1.234710 ADA | 1.852065 ADA |
| 6 | 1.565507 ADA | 2.363261 ADA |

Making 7 partial payments in a single transaction exceeded the limits.

#### All payments were partial payments. Each loan used three assets for collateral. Half of each loan was paid off and half of the collateral was reclaimed.
| Payments Made | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.359122 ADA | 0.538683 ADA |
| 2 | 0.576671 ADA | 0.865007 ADA |
| 3 | 0.823677 ADA | 1.235516 ADA |
| 4 | 1.110209 ADA | 1.665314 ADA |
| 5 | 1.436159 ADA | 2.154239 ADA |

Making 6 partial payments in a single transaction exceeded the limits.



## Rollover Loan(s)
#### All loans used one asset for collateral.
| Loans Rolled Over | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.243335 ADA | 0.365003 ADA |
| 5 | 0.589893 ADA | 0.884840 ADA |
| 10 | 1.248709 ADA | 1.873064 ADA |
| 12 | 1.582226 ADA | 2.373339 ADA |

Rolling over 13 loans in a single transaction exceeded the limits.

#### All loans used three assets for collateral.
| Loans Rolled Over | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.253713 ADA | 0.380570 ADA |
| 5 | 0.666143 ADA | 0.999215 ADA |
| 10 | 1.459463 ADA | 2.189195 ADA |
| 11 | 1.655090 ADA | 2.482635 ADA |

Rolling over 12 loans in a single transaction exceeded the limits.



## Update Address(s)
#### Each loan used one asset for collateral. 
| Loans Updated | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.297398 ADA | 0.446097 ADA |
| 2 | 0.532514 ADA | 0.798771 ADA |
| 3 | 0.878607 ADA | 1.317911 ADA |
| 4 | 1.369222 ADA | 2.053833 ADA |

Updating 5 addressses in a single transaction exceeded the limits.

#### Each loan used three assets for collateral. 
| Loans Updated | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.305097 ADA | 0.457646 ADA |
| 2 | 0.560066 ADA | 0.840099 ADA |
| 3 | 1.944992 ADA | 1.417488 ADA |
| 4 | 1.467222 ADA | 2.200833 ADA |

Updating 5 addressses in a single transaction exceeded the limits.



## Claim Expired Loan(s)
#### Each loan uses one asset for collateral, is for the same loan asset, and is from the same borrower.
| Loans Claimed | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.287762 ADA | 0.431643 ADA |
| 2 | 0.490754 ADA | 0.736131 ADA |
| 3 | 0.800584 ADA | 1.200876 ADA |
| 4 | 1.245395 ADA | 1.868093 ADA |

Claiming 5 expired loans in a single transaction exceeded the limits.

#### Each loan uses three assets for collateral, is for the same loan asset, and is from the same borrower.
| Loans Claimed | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.292980 ADA | 0.439470 ADA |
| 2 | 0.509055 ADA | 0.763583 ADA |
| 3 | 0.846782 ADA | 1.270173 ADA |
| 4 | 1.316171 ADA | 1.974257 ADA |

Claiming 5 expired loans in a single transaction exceeded the limits.

#### Each loan uses one asset for collateral, is for a different loan asset, and is from the same borrower.
| Loans Claimed | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.288557 ADA | 0.432836 ADA |
| 2 | 0.508390 ADA | 0.762585 ADA |
| 3 | 0.883134 ADA | 1.324701 ADA |
| 4 | 1.426678 ADA | 2.140017 ADA |

Claiming 5 expired loans in a single transaction exceeded the limits.

#### Each loan uses three assets for collateral, is for a different loan asset, and is from the same borrower.
| Loans Claimed | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.292983 ADA | 0.439475 ADA |
| 2 | 0.523914 ADA | 0.785871 ADA |
| 3 | 0.924348 ADA | 1.324701 ADA |
| 4 | 1.424498 ADA | 2.136747 ADA |

Claiming 5 expired loans in a single transaction exceeded the limits.

#### Each loan uses one asset for collateral, is for a different loan asset, and is from a different borrower.
| Loans Claimed | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.289549 ADA | 0.434324 ADA |
| 2 | 0.513237 ADA | 0.769856 ADA |
| 3 | 0.883506 ADA | 1.325259 ADA |
| 4 | 1.377212 ADA | 2.065818 ADA |

Claiming 5 expired loans in a single transaction exceeded the limits.

#### Each loan uses three assets for collateral, is for a different loan asset, and is from a different borrower.
| Loans Claimed | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.294414 ADA | 0.441621 ADA |
| 2 | 0.526129 ADA | 0.789194 ADA |
| 3 | 0.888355 ADA | 1.332533 ADA |
| 4 | 1.418012 ADA | 2.127018 ADA |

Claiming 5 expired loans in a single transaction exceeded the limits.



## Unlock
#### All UTxOs were lost collateral. Each UTxO had one collateral asset. Each UTxO was for the same loan asset.
| UTxOs Unlocked | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.241145 ADA | 0.361718 ADA |
| 2 | 0.355943 ADA | 0.533915 ADA |
| 3 | 0.552541 ADA | 0.828812 ADA |
| 4 | 0.861066 ADA | 1.291599 ADA |
| 5 | 1.279617 ADA | 1.919426 ADA |

Unlocking 6 UTxOs in a single transaction exceeded the limits.

#### All UTxOs were lost collateral. Each UTxO had three collateral assets. Each UTxO was for the same loan asset.
| UTxOs Unlocked | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.244490 ADA | 0.366735 ADA |
| 2 | 0.371384 ADA | 0.557056 ADA |
| 3 | 0.594877 ADA | 0.892316 ADA |
| 4 | 0.939752 ADA | 1.409628 ADA |

Unlocking 5 UTxOs in a single transaction exceeded the limits.

#### All UTxOs were lost collateral. Each UTxO had one collateral asset. Each UTxO was for a different loan asset.
| UTxOs Unlocked | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.242088 ADA | 0.363132 ADA |
| 2 | 0.371863 ADA | 0.557795 ADA |
| 3 | 0.630306 ADA | 0.945459 ADA |
| 4 | 1.049157 ADA | 1.573736 ADA |

Unlocking 5 UTxOs in a single transaction exceeded the limits.

#### All UTxOs were lost collateral. Each UTxO had three collateral assets. Each UTxO was for a different loan asset.
| UTxOs Unlocked | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.245522 ADA | 0.368283 ADA |
| 2 | 0.387380 ADA | 0.581070 ADA |
| 3 | 0.671520 ADA | 1.007280 ADA |
| 4 | 1.125220 ADA | 1.687830 ADA |

Unlocking 5 UTxOs in a single transaction exceeded the limits.

#### All UTxOs were finished loans. Each UTxO was for the same loan asset. No collateral was still present.
| UTxOs Unlocked | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.247908 ADA | 0.371862 ADA |
| 2 | 0.334631 ADA | 0.501947 ADA |
| 3 | 0.479526 ADA | 0.719289 ADA |
| 4 | 0.693287 ADA | 1.039931 ADA |
| 5 | 1.002028 ADA | 1.503042 ADA |
| 6 | 1.403347 ADA | 2.105021 ADA |

Unlocking 7 UTxOs in a single transaction exceeded the limits.

#### All UTxOs were finished loans. Each UTxO was for a different loan asset. No collateral was still present.
| UTxOs Unlocked | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.235774 ADA | 0.353661 ADA |
| 2 | 0.329154 ADA | 0.493731 ADA |
| 3 | 0.499139 ADA | 0.748709 ADA |
| 4 | 0.792315 ADA | 1.188573 ADA |
| 5 | 1.306392 ADA | 1.959588 ADA |

Unlocking 6 UTxOs in a single transaction exceeded the limits.



## Proxy Script
The cardano-node emulator does not simulate staking scripts so it was not possible to automate
tests where the proxy script delegated to a staking script. However, it seems fair to estimate
about 20-30 UTxOs in the case where a multisig native script is used as the staking script.

#### The proxy address uses a staking pubkey. Each UTxO has a `PaymentDatum`. The redeemer used to spend was the Unit redeemer.
| UTxOs Spent | Tx Fee | Req. Collateral |
|--|--|--|
| 1 | 0.209316 | 0.313974 ADA |
| 10 | 0.300666 ADA | 0.450999 ADA |
| 20 | 0.460882 ADA | 0.691323 ADA |
| 30 | 0.683416 ADA | 1.025124 ADA |
| 40 | 0.967917 ADA | 1.451876 ADA |
| 50 | 1.314120 ADA | 1.971180 ADA |
| 58 | 1.635509 ADA | 2.453264 ADA |

Unlocking 59 UTxOs in a single transaction exceeded the limits.

