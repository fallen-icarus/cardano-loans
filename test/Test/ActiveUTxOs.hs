{-# LANGUAGE OverloadedStrings #-}

module Test.ActiveUTxOs where

import Test.Tasty (TestTree,testGroup)

import Test.ActiveUTxOs.AcceptOffer qualified as AcceptOffer
import Test.ActiveUTxOs.MakePayment qualified as MakePayment
import Test.ActiveUTxOs.UpdateLenderAddress qualified as UpdateLenderAddress
import Test.ActiveUTxOs.ClaimDefaultedCollateral qualified as ClaimDefaultedCollateral
import Test.ActiveUTxOs.Unlock qualified as Unlock
import Test.ActiveUTxOs.Misc qualified as Misc

tests :: TestTree
tests = testGroup "Active UTxO Tests"
  [ AcceptOffer.tests
  , MakePayment.tests
  , UpdateLenderAddress.tests
  , ClaimDefaultedCollateral.tests
  , Unlock.tests
  , Misc.tests
  ]
