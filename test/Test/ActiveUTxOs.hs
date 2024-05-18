{-# LANGUAGE OverloadedStrings #-}

module Test.ActiveUTxOs where

import Test.Tasty (TestTree,testGroup)

import Test.ActiveUTxOs.AcceptOffer qualified as AcceptOffer
import Test.ActiveUTxOs.ApplyInterest qualified as ApplyInterest
import Test.ActiveUTxOs.MakePayment qualified as MakePayment
import Test.ActiveUTxOs.UpdateLenderAddress qualified as UpdateLenderAddress
import Test.ActiveUTxOs.ClaimExpiredCollateral qualified as ClaimExpiredCollateral
import Test.ActiveUTxOs.Unlock qualified as Unlock
import Test.ActiveUTxOs.Misc qualified as Misc

tests :: TestTree
tests = testGroup "Active UTxO Tests"
  [ AcceptOffer.tests
  , ApplyInterest.tests
  , MakePayment.tests
  , UpdateLenderAddress.tests
  , ClaimExpiredCollateral.tests
  , Unlock.tests
  , Misc.tests
  ]
