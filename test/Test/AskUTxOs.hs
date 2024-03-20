{-# LANGUAGE OverloadedStrings #-}

{-

Ask UTxOs are loan UTxOs that contain an AskDatum and the required Ask beacons. There are three
possible actions for Ask UTxOs: creation, closing, and updating. All three actions can happen in the
same transaction.

In order to allow updating while still being performant, the script does not actually check which
tokens are minted/burned. Instead, the script assumes all outputs containing negotiation beacons are
meant to be Ask UTxOs. By clearly defining what these Ask UTxOs look like and stipulating that all
negotiation beacons must be in Ask UTxO outputs, the negotiation beacon script should fail if
unnecessary/improper beacons are minted or withdrawn. Burning is always allowed.

Intra-Protocol Compositions:

1) Because the script assumes all outputs with negotiation beacons are meant to be Ask UTxOs, any
actions that result in Ask UTxO outputs cannot be composed with actions that result in Offer UTxO
outputs.

2) Since Active UTxOs contain active beacons instead of negotiation beacons, actions on Ask UTxOs
can be composed with any action on Active UTxOs as long as there is no conflict in what is 
minted/burned. The only two actions that tightly control what gets minted/burned are accepting
Offer UTxOs and fully paying off a loan - accepting Offer UTxOs requires only burning negotiation
beacons while fully paying off a loan require burning the BorrowerId in isolation. All other
compositions with actions on Active UTxOs should succeed.

3) Since the script does not care about inputs or what tokens are burned, it is possible to
_close_ Offer UTxOs in the same transaction where Ask UTxOs are created (assuming the user is the
rightful owner of the Offer UTxOs).

3) Since the script does not care about inputs or what tokens are burned, it is possible to
_close_ Offer UTxOs in the same transaction where Ask UTxOs are created (assuming the user is the
rightful owner of the Offer UTxOs). However, closing Ask UTxOs requires the 
CreateCloseOrUpdateAsk redeemer which is incompatible with closing Offer UTxOs. Therefore,
you cannot close both Ask and Offer UTxOs in the same transaction.

-}
module Test.AskUTxOs
  ( 
    tests
  ) where

import Test.Tasty (TestTree,testGroup)

import Test.AskUTxOs.CreateAsk qualified as Create
import Test.AskUTxOs.CloseAsk qualified as Close
import Test.AskUTxOs.UpdateAsk qualified as Update

tests :: TestTree
tests = testGroup "Ask UTxO Tests"
  [ Create.tests
  , Close.tests
  , Update.tests
  ]
