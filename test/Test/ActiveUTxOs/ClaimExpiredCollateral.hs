{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.ActiveUTxOs.ClaimExpiredCollateral where

import Test.Tasty (TestTree,testGroup)

import Test.ActiveUTxOs.ClaimExpiredCollateral.Regressions qualified as Regressions
import Test.ActiveUTxOs.ClaimExpiredCollateral.Failures qualified as Failures
import Test.ActiveUTxOs.ClaimExpiredCollateral.Benchmarks qualified as Benchmarks

tests :: TestTree
tests = testGroup "Claim Expired Collateral" $ mconcat
  [ Regressions.tests
  , Failures.tests
  , Benchmarks.tests
  ]
