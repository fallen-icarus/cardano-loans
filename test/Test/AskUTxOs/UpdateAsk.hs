{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.AskUTxOs.UpdateAsk where

import Test.Tasty (TestTree,testGroup)

import Test.AskUTxOs.UpdateAsk.Regressions qualified as Regressions
import Test.AskUTxOs.UpdateAsk.Failures qualified as Failures
import Test.AskUTxOs.UpdateAsk.Benchmarks qualified as Benchmarks

tests :: TestTree
tests = testGroup "Updating Ask UTxOs" $ mconcat
  [ Regressions.tests
  , Failures.tests
  , Benchmarks.tests
  ]
