{-# LANGUAGE OverloadedStrings #-}

module Test.AskUTxOs where

import Test.Tasty (TestTree,testGroup)

import Test.AskUTxOs.CreateAsk qualified as CreateAsk
import Test.AskUTxOs.CloseAsk qualified as CloseAsk
import Test.AskUTxOs.UpdateAsk qualified as UpdateAsk
import Test.AskUTxOs.Misc qualified as Misc

tests :: TestTree
tests = testGroup "Ask UTxO Tests"
  [ CreateAsk.tests
  , CloseAsk.tests
  , UpdateAsk.tests
  , Misc.tests
  ]
