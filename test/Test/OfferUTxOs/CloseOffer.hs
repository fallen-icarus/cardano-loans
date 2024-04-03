{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.OfferUTxOs.CloseOffer where

import Test.Tasty (TestTree,testGroup)

import Test.OfferUTxOs.CloseOffer.Regressions qualified as Regressions
import Test.OfferUTxOs.CloseOffer.Failures qualified as Failures
import Test.OfferUTxOs.CloseOffer.Benchmarks qualified as Benchmarks

tests :: TestTree
tests = testGroup "Closing Offer UTxOs" $ mconcat
  [ Regressions.tests
  , Failures.tests
  , Benchmarks.tests
  ]
