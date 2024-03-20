{-# LANGUAGE OverloadedStrings #-}

{-

Active UTxOs are loan UTxOs that contain an ActiveDatum and the required Active beacons. Active UTxOs
can be in one of three states:

1) Live - there is still an outstanding balance and the loan has not expired yet
2) Finished - the outstanding balance has reached 0
3) Expired - the loan expired before the outstanding balance reached 0

Active beacons use a separate policy script than negotiation beacons; this enables the possibility
of composing actions on Active UTxOs with actions on negotiation UTxOs (ie, Ask and Offer UTxOs).

Since the credit history is tied to _how_ a BorrowerId is minted/burned, any actions that mint/burn
BorrowerIds are tightly controlled:
1) Minting BorrowerIds requires burning the exact number of Negotiation beacons and minting the exact
   number of Active beacons. All loan outputs must be Active UTxOs.
2) When a loan is finished, the BorrowerIds must be burned in isolation; no other native assets can be 
   minted/burned in the transaction.
3) If neither of the above scenarios apply, then the Active beacons can be burned along with other
   native assets.

The constraint on minting BorrowerIds is due to how accepting offers is optimized; enforcing exact
mints and burns allows for more efficient logic. You can still mint/burn other native tokens when
BorrowerIds are minted; only the Negotiation beacons are tightly controlled. 

The constraint on burning BorrowerIds is to make it cheap to check credit history off-chain. If the
transaction only burns BorrowerIds, then the loans in that transaction are all loans that have been
successfully paid off. If there are other tokens burned in the transaction, all loans in that
transaction are defaults.

-}
module Test.ActiveUTxOs
  ( 
    tests
  ) where

import Test.Tasty (TestTree,testGroup)

import Test.ActiveUTxOs.AcceptOffer qualified as AcceptOffer
import Test.ActiveUTxOs.MakePayment qualified as MakePayment

tests :: TestTree
tests = testGroup "Active UTxO Tests"
  [ AcceptOffer.tests
  , MakePayment.tests
  ]
