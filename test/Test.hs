{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty

import Test.AskUTxOs qualified as AskUTxOs
import Test.OfferUTxOs qualified as OfferUTxOs
import Test.ActiveUTxOs qualified as ActiveUTxOs
import Test.Beacons qualified as Beacons
import Test.Proxy as Proxy

main :: IO ()
main = defaultMain $ testGroup "Cardano-Loans"
  [ AskUTxOs.tests
  , OfferUTxOs.tests
  , ActiveUTxOs.tests
  , Beacons.tests
  , Proxy.tests
  ]
