{-# LANGUAGE OverloadedStrings #-}

module Test.OfferUTxOs where

import Test.Tasty (TestTree,testGroup)

import Test.OfferUTxOs.CreateOffer qualified as CreateOffer
import Test.OfferUTxOs.CloseOffer qualified as CloseOffer
import Test.OfferUTxOs.UpdateOffer qualified as UpdateOffer
import Test.OfferUTxOs.Misc qualified as Misc

tests :: TestTree
tests = testGroup "Offer UTxO Tests"
  [ CreateOffer.tests
  , CloseOffer.tests
  , UpdateOffer.tests
  , Misc.tests
  ]
