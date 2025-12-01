# Benchmarks (YMMV)

All benchmarks were done using reference scripts and the cardano-node emulator
([github](https://github.com/IntersectMBO/cardano-node-emulator)). The scripts for this protocol are
too large to be used locally so it is required that reference scripts are used. The emulator uses
the same parameters as the mainnet.

## Table of Contents 
- [Required Deposits for Reference Scripts](#required-deposits-for-reference-scripts)
- [Creating Ask UTxOs](#creating-ask-utxos)
- [Updating Ask UTxOs](#updating-ask-utxos)
- [Closing Ask UTxOs](#closing-ask-utxos)
- [Creating Offer UTxOs](#creating-offer-utxos)
- [Updating Offer UTxOs](#updating-offer-utxos)
- [Closing Offer UTxOs](#closing-offer-utxos)
- [Accepting Offers](#accepting-offers)
- [Making Partial Payment](#making-partial-payments)
- [Making Full Payment](#making-full-payments)
- [Updating Lender Addresses](#updating-lender-addresses)
- [Claiming Expired Collateral](#claiming-expired-collateral)
- [Unlocking Lost Collateral](#unlocking-lost-collateral)
- [Spending From Proxy Script](#spending-from-proxy-script)

## Required Deposits for Reference Scripts

| Script | Deposit |
|:------:|:-------:|
| loan spending script | 22 ADA |
| negotiation beacon script | 37 ADA |
| active beacon script | 51 ADA |
| payment observer script | 45 ADA |
| address update observer script | 25 ADA |
| proxy script | 4 ADA |

It is recommended that users share reference scripts using something like
[cardano-reference-scripts](https://github.com/fallen-icarus/cardano-reference-scripts) so that there
is only one copy of each script stored on chain at a time.

## Creating Ask UTxOs
Only the worst case scenario is reported here. Creating Asks for the same loan asset and/or with
fewer collateral could result in performance as high as 32 asks/tx.

#### All UTxOs use different loan assets and each loan uses a different set of three native assets for collateral.
| Asks Created | Tx Fee | Req. Collateral |
|:--:|:--:|:--:|
| 1 | 0.359928 ADA | 0.539892 ADA |
| 5 | 0.564308 ADA | 0.846462 ADA |
| 10 | 0.820442 ADA | 1.230663 ADA |
| 15 | 1.076577 ADA | 1.614866 ADA |
| 20 | 1.332712 ADA | 1.999068 ADA |
| 24 | 1.537752 ADA | 2.306628 ADA |

Max: 24 Asks  
Bottleneck: Tx Size

## Updating Ask UTxOs
#### All UTxOs undergo the same conversion where the requested loan assets is changed. Each Ask uses the same three native assets for collateral.
| Asks Updated | Tx Fee | Req. Collateral |
|:--:|:--:|:--:|
| 1 | 0.268775 ADA | 0.403163 ADA |
| 5 | 0.544855 ADA | 0.817283 ADA |
| 10 | 0.933536 ADA | 1.400304 ADA |
| 15 | 1.370641 ADA | 2.055962 ADA |
| 20 | 1.856170 ADA | 2.784255 ADA |
| 22 | 1.959086 ADA | 2.938629 ADA |

Max: 22 Asks  
Bottleneck: Memory

#### All UTxOs undergo different conversions. They all start with unique loan assets and collateral and finish with unique loan assets and collateral. Three native assets are used as collateral for each Ask.
| Asks Updated | Tx Fee | Req. Collateral |
|:--:|:--:|:--:|
| 1 | 0.572771 ADA | 0.859157 ADA |
| 5 | 0.862931 ADA | 1.294397 ADA |
| 10 | 1.269872 ADA | 1.904808 ADA |
| 13 | 1.537324 ADA | 2.305986 ADA |

Max: 13 Asks  
Bottleneck: Tx Size

## Closing Ask UTxOs
#### All UTxOs use different loan assets and each loan uses a different set of three native assets for collateral.
| Asks Closed | Tx Fee | Req. Collateral |
|:--:|:--:|:--:|
| 1 | 0.310652 ADA | 0.465978 ADA |
| 5 | 0.402420 ADA | 0.603630 ADA |
| 10 | 0.562307 ADA | 0.843461 ADA |
| 15 | 0.768797 ADA | 1.153196 ADA |
| 20 | 1.024599 ADA | 1.536899 ADA |
| 25 | 1.328601 ADA | 1.992902 ADA |
| 30 | 1.681470 ADA | 2.522205 ADA |
| 33 | 1.836266 ADA | 2.754399 ADA |

Max: 33 Asks  
Bottleneck: Memory

## Creating Offer UTxOs
Only the worst case scenario is reported here. Creating Offers for the same loan asset and/or with
fewer collateral could result in performance as high as 33 offers/tx.

#### All UTxOs use different loan assets and each loan uses a different set of three native assets for collateral.
| Offers Created | Tx Fee | Req. Collateral |
|:--:|:--:|:--:|
| 1 | 0.312498 ADA | 0.468747 ADA |
| 5 | 0.544619 ADA | 0.816929 ADA |
| 10 | 0.834946 ADA | 1.252419 ADA |
| 15 | 1.125536 ADA | 1.688304 ADA |
| 20 | 1.416127 ADA | 2.124191 ADA |
| 24 | 1.648776 ADA | 2.473164 ADA |

Max: 24 Offers  
Bottleneck: Tx Size

## Updating Offer UTxOs
#### All UTxOs undergo different conversions. They all start with unique loan assets and finish with unique loan assets. Each Offer uses three native assets for collateral.
| Offers Updated | Tx Fee | Req. Collateral |
|:--:|:--:|:--:|
| 1 | 0.357109 ADA | 0.535664 ADA |
| 5 | 0.697886 ADA | 1.046829 ADA |
| 10 | 1.167328 ADA | 1.750992 ADA |
| 15 | 1.684794 ADA | 2.527191 ADA |
| 19 | 2.018935 ADA | 3.028403 ADA |

Max: 19 Offers  
Bottleneck: Memory

## Closing Offer UTxOs
#### All UTxOs use different loan assets.
| Offers Closed | Tx Fee | Req. Collateral |
|:--:|:--:|:--:|
| 1 | 0.268367 ADA | 0.402551 ADA |
| 5 | 0.378339 ADA | 0.567509 ADA |
| 10 | 0.559919 ADA | 0.839879 ADA |
| 15 | 0.789610 ADA | 1.184415 ADA |
| 20 | 1.069056 ADA | 1.603584 ADA |
| 25 | 1.396791 ADA | 2.095187 ADA |
| 30 | 1.693951 ADA | 2.540927 ADA |

Max: 30 Offers  
Bottleneck: Memory

## Accepting Offers
#### All offers are from the same lender and are for the same terms. Three native assets are used as collateral.
| Offers Accepted | Tx Fee | Req. Collateral |
|:--:|:--:|:--:|
| 1 | 0.379418 ADA | 0.569127 ADA |
| 2 | 0.549781 ADA | 0.824672 ADA |
| 3 | 0.725768 ADA | 1.088652 ADA |
| 4 | 0.903519 ADA | 1.355279 ADA |
| 5 | 1.090400 ADA | 1.635600 ADA |
| 6 | 1.277511 ADA | 1.916267 ADA |
| 7 | 1.478705 ADA | 2.218058 ADA |
| 8 | 1.679143 ADA | 2.518715 ADA |
| 9 | 1.884754 ADA | 2.827131 ADA |

Max: 9 Offers  
Bottleneck: Memory

#### All offers are from different lenders and are for different loan assets. The same three native assets are used as collateral for each loan.
| Offers Accepted | Tx Fee | Req. Collateral |
|:--:|:--:|:--:|
| 1 | 0.383901 ADA | 0.575852 ADA |
| 2 | 0.564930 ADA | 0.847395 ADA |
| 3 | 0.767709 ADA | 1.151564 ADA |
| 4 | 0.987181 ADA | 1.480772 ADA |
| 5 | 1.200567 ADA | 1.800851 ADA |
| 6 | 1.422197 ADA | 2.133296 ADA |
| 7 | 1.662465 ADA | 2.493698 ADA |

Max: 7 Offers  
Bottleneck: Memory

## Making Partial Payments
Only the worst case scenario is shown. If the loans uses fewer assets as collateral, performance
could be as high as 15 payments/tx.

#### Make partial payments on multiple loans where each loan uses a different loan asset and three native assets as collateral.
| Payments Made | Tx Fee | Req. Collateral |
|:--:|:--:|:--:|
| 1 | 0.401901 ADA | 0.602852 ADA |
| 2 | 0.561783 ADA | 0.842675 ADA |
| 3 | 0.722053 ADA | 1.083080 ADA |
| 4 | 0.882711 ADA | 1.324067 ADA |
| 5 | 1.043759 ADA | 1.565639 ADA |
| 6 | 1.205194 ADA | 1.807791 ADA |
| 7 | 1.367019 ADA | 2.050529 ADA |
| 8 | 1.529231 ADA | 2.293847 ADA |
| 9 | 1.691833 ADA | 2.537750 ADA |
| 10 | 1.854998 ADA | 2.782497 ADA |
| 11 | 1.956726 ADA | 2.935089 ADA |

Max: 11 Partial Payments  
Bottleneck: Memory

## Making Full Payments
Only the worst case scenario is shown. If the loans uses fewer assets as collateral, performance
could be as high as 18 payments/tx.

#### Make full payments on multiple loans where each loan uses a different loan asset and three native assets as collateral.
| Payments Made | Tx Fee | Req. Collateral |
|:--:|:--:|:--:|
| 1 | 0.319935 ADA | 0.479903 ADA |
| 2 | 0.389334 ADA | 0.584001 ADA |
| 3 | 0.462881 ADA | 0.694322 ADA |
| 4 | 0.542556 ADA | 0.813834 ADA |
| 5 | 0.620447 ADA | 0.930671 ADA |
| 6 | 0.699034 ADA | 1.048551 ADA |
| 7 | 0.789683 ADA | 1.184525 ADA |
| 8 | 0.871673 ADA | 1.307510 ADA |
| 9 | 0.961766 ADA | 1.442649 ADA |
| 10 | 1.054031 ADA | 1.581047 ADA |
| 11 | 1.154448 ADA | 1.731672 ADA |
| 12 | 1.242162 ADA | 1.863243 ADA |
| 13 | 1.345897 ADA | 2.018846 ADA |
| 14 | 1.474547 ADA | 2.211821 ADA |
| 15 | 1.606533 ADA | 2.409800 ADA |

Max: 15 Full Payments  
Bottleneck: Memory

## Updating Lender Addresses
#### Update the address for multiple loans where each loan uses three assets as collateral. The Key NFTs are all in different UTxOs.
| Loans Updated | Tx Fee | Req. Collateral |
|:--:|:--:|:--:|
| 1 | 0.304445 ADA | 0.456668 ADA |
| 5 | 0.763784 ADA | 1.145676 ADA |
| 10 | 1.370066 ADA | 2.055099 ADA |
| 14 | 1.879126 ADA | 2.818689 ADA |

Max: 14 Loans Updated  
Bottleneck: Tx Size

#### Update the address for multiple loans where each loan uses three assets as collateral. The Key NFTs are all in the same UTxO.
| Loans Updated | Tx Fee | Req. Collateral |
|:--:|:--:|:--:|
| 1 | 0.304445 ADA | 0.456668 ADA |
| 5 | 0.717554 ADA | 1.076331 ADA |
| 10 | 1.239692 ADA | 1.859538 ADA |
| 15 | 1.771197 ADA | 2.656796 ADA |

Max: 15 Loans Updated  
Bottleneck: Tx Size

## Claiming Expired Collateral
The emulator only has 10 keys which makes it difficult to test the scenario where each loan claimed
is from a different borrower. It is possible to claim at least 10 loans/tx. Estimating from other tests,
the upper limit is likely about 13 loans/tx.

If you claim collateral from the same borrower and keep the Key NFTs in a single UTxO, performance
could be as high as 20 loans/tx.

#### Claim the collateral for multiple expired loans where each loan use different loan assets and come from different borrowers. Each loan uses three native assets as collateral. The Key NFTs are in separate UTxOs.
| Loans Claimed | Tx Fee | Req. Collateral |
|:--:|:--:|:--:|
| 1 | 0.253516 ADA | 0.380274 ADA |
| 2 | 0.320241 ADA | 0.480362 ADA |
| 3 | 0.389820 ADA | 0.584730 ADA |
| 4 | 0.470170 ADA | 0.705255 ADA |
| 5 | 0.547434 ADA | 0.821151 ADA |
| 6 | 0.645404 ADA | 0.968106 ADA |
| 7 | 0.748605 ADA | 1.122908 ADA |
| 8 | 0.867221 ADA | 1.300832 ADA |
| 9 | 0.989532 ADA | 1.484298 ADA |
| 10 | 1.107785 ADA | 1.661678 ADA |

Max: unknown  
Bottleneck: unknown

## Unlocking Lost Collateral
#### Unlock multiple lost collateral loans. All loans use the same loan asset and three native assets as collateral.
| Loans Unlocked | Tx Fee | Req. Collateral |
|:--:|:--:|:--:|
| 1 | 0.246439 ADA | 0.369659 ADA |
| 5 | 0.496909 ADA | 0.745364 ADA |
| 10 | 0.881997 ADA | 1.322996 ADA |
| 15 | 1.389349 ADA | 2.084774 ADA |
| 17 | 1.480003 ADA | 2.220005 ADA |

Max: 17 Loans Unlocked  
Bottleneck: Memory

## Spending From Proxy Script
#### The proxy address uses a staking pubkey. Each UTxO has a `PaymentDatum`. The redeemer used to spend was the Unit redeemer.
| UTxOs Spent | Tx Fee | Req. Collateral |
|:--:|:--:|:--:|
| 1 | 0.181917 | 0.272876 ADA |
| 10 | 0.254617 ADA | 0.381926 ADA |
| 20 | 0.380115 ADA | 0.570173 ADA |
| 30 | 0.551719 ADA | 0.827579 ADA |
| 40 | 0.769077 ADA | 1.153616 ADA |
| 50 | 1.031486 ADA | 1.547229 ADA |
| 60 | 1.339384 ADA | 2.009076 ADA |
| 70 | 1.693037 ADA | 2.539556 ADA |
| 71 | 1.730921 ADA | 2.596382 ADA |

Max: 71 UTxOs  
Bottleneck: Memory
