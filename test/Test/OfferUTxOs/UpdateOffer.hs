{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.OfferUTxOs.UpdateOffer where

import Test.Tasty (TestTree,testGroup)

import Test.OfferUTxOs.UpdateOffer.Regressions qualified as Regressions
import Test.OfferUTxOs.UpdateOffer.Failures qualified as Failures
import Test.OfferUTxOs.UpdateOffer.Benchmarks qualified as Benchmarks

tests :: TestTree
tests = testGroup "Updating Offer UTxOs" $ mconcat
  [ Regressions.tests
  , Failures.tests
  , Benchmarks.tests
  ]
