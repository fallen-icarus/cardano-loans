{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.ActiveUTxOs.Unlock where

import Test.Tasty (TestTree,testGroup)

import Test.ActiveUTxOs.Unlock.Regressions qualified as Regressions
import Test.ActiveUTxOs.Unlock.Failures qualified as Failures
import Test.ActiveUTxOs.Unlock.Benchmarks qualified as Benchmarks

tests :: TestTree
tests = testGroup "Unlock Finished/Lost/Invalid UTxOs" $ mconcat
  [ Regressions.tests
  , Failures.tests
  , Benchmarks.tests
  ]
