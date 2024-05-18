{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.ActiveUTxOs.UpdateLenderAddress where

import Test.Tasty (TestTree,testGroup)

import Test.ActiveUTxOs.UpdateLenderAddress.Regressions qualified as Regressions
import Test.ActiveUTxOs.UpdateLenderAddress.Failures qualified as Failures
import Test.ActiveUTxOs.UpdateLenderAddress.Benchmarks qualified as Benchmarks

tests :: TestTree
tests = testGroup "Updating Lender Addresses" $ mconcat
  [ Regressions.tests
  , Failures.tests
  , Benchmarks.tests
  ]
