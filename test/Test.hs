module Main where

import Test.Tasty

import Test.Ask as Ask

main :: IO ()
main = defaultMain $ testGroup "Cardano-Lending"
  [
    Ask.tests 
  ]