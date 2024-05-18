{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.ActiveUTxOs.ApplyInterest where

import Test.Tasty (TestTree,testGroup)

import Test.ActiveUTxOs.ApplyInterest.Regressions qualified as Regressions
import Test.ActiveUTxOs.ApplyInterest.Failures qualified as Failures
import Test.ActiveUTxOs.ApplyInterest.Benchmarks qualified as Benchmarks

tests :: TestTree
tests = testGroup "Apply Interest to Loans" $ mconcat
  [ Regressions.tests
  , Failures.tests
  , Benchmarks.tests
  ]
