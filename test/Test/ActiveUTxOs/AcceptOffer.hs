{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.ActiveUTxOs.AcceptOffer where

import Test.Tasty (TestTree,testGroup)

import Test.ActiveUTxOs.AcceptOffer.Regressions qualified as Regressions
import Test.ActiveUTxOs.AcceptOffer.Failures qualified as Failures
import Test.ActiveUTxOs.AcceptOffer.Benchmarks qualified as Benchmarks

tests :: TestTree
tests = testGroup "Accepting Offer UTxOs" $ mconcat
  [ Regressions.tests
  , Failures.tests
  , Benchmarks.tests
  ]
