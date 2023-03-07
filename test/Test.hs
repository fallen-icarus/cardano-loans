module Main where

import Test.Tasty

import Test.Ask as Ask
import Test.Offer as Offer

main :: IO ()
main = defaultMain $ testGroup "Cardano-Loans"
  [
    Ask.tests
  , Offer.tests
  ]