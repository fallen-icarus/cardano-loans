{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty

import Test.CreateAsk as CreateAsk
import Test.CloseAsk as CloseAsk
import Test.UpdateAsk as UpdateAsk
import Test.CreateOffer as CreateOffer
import Test.CloseOffer as CloseOffer
import Test.UpdateOffer as UpdateOffer
import Test.AcceptOffer as AcceptOffer
-- import Test.MakePayment as MakePayment
-- import Test.Rollover as Rollover
-- import Test.ClaimExpired as ClaimExpired
-- import Test.UpdateLenderAddress as UpdateLenderAddress
-- import Test.Unlock as Unlock
-- import Test.Proxy as Proxy

main :: IO ()
main = defaultMain $ testGroup "Cardano-Loans"
  [ 
    CreateAsk.tests
  , CloseAsk.tests
  , UpdateAsk.tests
  , CreateOffer.tests
  , CloseOffer.tests 
  , UpdateOffer.tests
  , AcceptOffer.tests 
  -- , MakePayment.tests
  -- , Rollover.tests
  -- , ClaimExpired.tests
  -- , UpdateLenderAddress.tests 
  -- , Unlock.tests
  -- , Proxy.tests
  ]
