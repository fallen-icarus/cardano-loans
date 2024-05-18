{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.AskUTxOs.CreateAsk where

import Test.Tasty (TestTree,testGroup)

import Test.AskUTxOs.CreateAsk.Regressions qualified as Regressions
import Test.AskUTxOs.CreateAsk.Failures qualified as Failures
import Test.AskUTxOs.CreateAsk.Benchmarks qualified as Benchmarks

tests :: TestTree
tests = testGroup "Creating Ask UTxOs" $ mconcat
  [ Regressions.tests
  , Failures.tests
  , Benchmarks.tests
  ]
