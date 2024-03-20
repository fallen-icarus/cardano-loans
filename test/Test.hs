{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty

import Test.AskUTxOs qualified as AskUTxOs
import Test.OfferUTxOs qualified as OfferUTxOs
import Test.ActiveUTxOs qualified as ActiveUTxOs
-- import Test.UpdateAsk as UpdateAsk
-- import Test.CreateOffer as CreateOffer
-- import Test.CloseOffer as CloseOffer
-- import Test.UpdateOffer as UpdateOffer
-- import Test.AcceptOffer as AcceptOffer
-- import Test.MakePayment as MakePayment
-- import Test.Rollover as Rollover
-- import Test.ClaimExpired as ClaimExpired
-- import Test.UpdateLenderAddress as UpdateLenderAddress
-- import Test.Unlock as Unlock
-- import Test.Proxy as Proxy

main :: IO ()
main = defaultMain $ testGroup "Cardano-Loans"
  [ 
    AskUTxOs.tests
  , OfferUTxOs.tests
  , ActiveUTxOs.tests
  ]
