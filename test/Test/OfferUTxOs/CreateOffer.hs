{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.OfferUTxOs.CreateOffer where

import Test.Tasty (TestTree,testGroup)

import Test.OfferUTxOs.CreateOffer.Regressions qualified as Regressions
import Test.OfferUTxOs.CreateOffer.Failures qualified as Failures
import Test.OfferUTxOs.CreateOffer.Benchmarks qualified as Benchmarks

tests :: TestTree
tests = testGroup "Creating Offer UTxOs" $ mconcat
  [ Regressions.tests
  , Failures.tests
  , Benchmarks.tests
  ]
