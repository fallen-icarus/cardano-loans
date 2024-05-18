{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.ActiveUTxOs.MakePayment where

import Test.Tasty (TestTree,testGroup)

import Test.ActiveUTxOs.MakePayment.Regressions qualified as Regressions
import Test.ActiveUTxOs.MakePayment.Failures qualified as Failures
import Test.ActiveUTxOs.MakePayment.Benchmarks qualified as Benchmarks

tests :: TestTree
tests = testGroup "Making Payments" $ mconcat
  [ Regressions.tests
  , Failures.tests
  , Benchmarks.tests
  ]
