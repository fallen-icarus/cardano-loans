# Revision history for cardano-loans

## 2.0.0

#### Core
* Contracts written using Aiken.

#### Features
* Allow lenders to trade open loans using Lock/Key NFTs.
* Allow lender to specify a zero value for an undesired collateral asset.
* Allow lender to specify a custom minUTxOValue for Offers that will be returned to the lender when
  the loan is accepted.
* Allow creating multiple Offers in a single transaction.
* Allow accepting multiple loans in a single transaction.
* Allow making multiple full and partial payments in a single transaction.
* Allow swapping out collateral of active loans as long as lender's collateralization is respected.
  The lender must explicitly enable this to prevent abuse.
* Add compound interest interest through rollover steps. Interest is applied at each step.
* Add option for interest-free and non-compounding interest loans.
* Loan payments are made directly to the lender's address. This address can be changed by anyone who
  owns the corresponding loan Key NFT. By using the proxy script, the lender can safely use arbitrary
  logic to protect revenue from loan payments.
* Add claim period for if the Key NFT is lost. This is to prevent permanently locked collateral.
* Add ability for Borrowers to use either a staking pubkey or a staking script as their credential.
  This enables the borrower to be a business using arbitrary logic to control spending (eg, multisig).
* Add an Asset beacon for more expressive off-chain queries.
* Allow required minimum loan payments as another way for lenders to manage risk.
* Add prefix to LenderIDs to 1) allow the protocol to keep track of the type of credential it is and
  2) keep it distinct from the corresponding BorrowerID for the same credential.

#### Documentation
* Add an Architectural Decision Record (ADR).
* Add extensive benchmarking for typical usage scenarios.

## 1.0.0

* First version. Released on an unsuspecting world.
