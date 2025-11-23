{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.ActiveUTxOs.ClaimDefaultedCollateral where

import Test.Tasty (TestTree,testGroup)

import Test.ActiveUTxOs.ClaimDefaultedCollateral.Regressions qualified as Regressions
import Test.ActiveUTxOs.ClaimDefaultedCollateral.Failures qualified as Failures
import Test.ActiveUTxOs.ClaimDefaultedCollateral.Benchmarks qualified as Benchmarks

tests :: TestTree
tests = testGroup "Claim Defaulted Collateral" $ mconcat
  [ Regressions.tests
  , Failures.tests
  , Benchmarks.tests
  ]
