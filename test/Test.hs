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
