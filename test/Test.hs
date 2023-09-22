{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty
-- import CardanoLoans

import Test.CreateAsk as CreateAsk
import Test.CloseAsk as CloseAsk
import Test.CreateOffer as CreateOffer
import Test.CloseOffer as CloseOffer
import Test.AcceptOffer as AcceptOffer
import Test.MakePayment as MakePayment
import Test.Rollover as Rollover
import Test.ClaimExpired as ClaimExpired
import Test.UpdateLenderAddress as UpdateLenderAddress
import Test.Unlock as Unlock
import Test.Proxy as Proxy

{- | Tests are broken into 5 categories:

(1) Regression Tests - tests that prove a certain feature actually works.

(2) Failure Tests - tests that prove a certain action will fail.

(3) Edge Case Tests - tests of unusual usages that should or should not fail.

(4) Benchmark Tests - tests that will fail if the performance of the dApp decreases.

(5) Performance Increase Tests - tests that will fail if the performance of the dApp increases.

-}
main :: IO ()
main = do
  -- print $ scriptSize $ loanScript
  -- print $ scriptSize $ beaconScript
  -- print $ scriptSize $ proxyScript

  defaultMain $ testGroup "Cardano-Loans"
    [ 
      CreateAsk.tests
    , CloseAsk.tests
    , CreateOffer.tests
    , CloseOffer.tests 
    , AcceptOffer.tests 
    , MakePayment.tests
    , Rollover.tests
    , ClaimExpired.tests
    , UpdateLenderAddress.tests 
    , Unlock.tests
    , Proxy.tests
    ]
