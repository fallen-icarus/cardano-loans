# Revision history for cardano-loans

## 2.0.0

* Contracts written using Aiken.
* Added feature to allow lender's to trade open loans using Lock/Key NFTs.
* Allowed lender to specify a zero value for an undesired collateral asset.
* Increased minimum Offer deposit to 5 ADA to allow for larger Offer UTxOs while still making it easy for the lender to recover this deposit.
* Allowed accepting multiple loans in a single transaction.
* Allowed making multiple full and partial payments in a single transaction.
* Allowed swapping out collateral of active loans as long as lender's collateralization is respected.
* Added rollover steps to allow for compound interest. Interest is applied at each step.
* Loan payments are made directly to the lender's address. This address can be changed by anyone who owns the corresponding loan Key NFT.
* A claim period was added in case the Key NFT is lost to prevent permanently locked collateral UTxOs.

## 1.0.0

* First version. Released on an unsuspecting world.
