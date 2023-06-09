module Main where

import Test.Tasty

import Test.Ask as Ask
import Test.Offer as Offer
import Test.AcceptOffer as AcceptOffer
import Test.CloseAsk as CloseAsk
import Test.CloseOffer as CloseOffer
import Test.RepayLoan as RepayLoan
import Test.Claim as Claim

main :: IO ()
main = defaultMain $ testGroup "Cardano-Loans"
  [
    Ask.tests
  , Offer.tests
  , AcceptOffer.tests
  , CloseAsk.tests
  , CloseOffer.tests
  , RepayLoan.tests
  , Claim.tests
  ]