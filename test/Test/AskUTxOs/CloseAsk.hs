{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.AskUTxOs.CloseAsk where

import Test.Tasty (TestTree,testGroup)

import Test.AskUTxOs.CloseAsk.Regressions qualified as Regressions
import Test.AskUTxOs.CloseAsk.Failures qualified as Failures
import Test.AskUTxOs.CloseAsk.Benchmarks qualified as Benchmarks

tests :: TestTree
tests = testGroup "Closing Ask UTxOs" $ mconcat
  [ Regressions.tests
  , Failures.tests
  , Benchmarks.tests
  ]
