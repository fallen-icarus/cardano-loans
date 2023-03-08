module Main where

import Test.Tasty

import Test.Ask as Ask
import Test.Offer as Offer
import Test.AcceptOffer as AcceptOffer

main :: IO ()
main = defaultMain $ testGroup "Cardano-Loans"
  [
    Ask.tests
  , Offer.tests
  , AcceptOffer.tests
  ]