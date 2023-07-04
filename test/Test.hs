{-# LANGUAGE OverloadedStrings #-}

module Main where

import Ledger (scriptSize,unValidatorScript)
import Test.Tasty
import Test.Tasty.HUnit
import CardanoLoans

import Test.CreateAsk as CreateAsk
import Test.CloseAsk as CloseAsk
import Test.CreateOffer as CreateOffer
import Test.CloseOffer as CloseOffer
import Test.AcceptOffer as AcceptOffer
import Test.MakePayment as MakePayment
import Test.Rollover as Rollover
import Test.ClaimExpired as ClaimExpired
import Test.UpdateAddress as UpdateAddress
import Test.ClaimLost as ClaimLost

main :: IO ()
main = do
  testScripts <- genScripts <$> readBlueprints "aiken/plutus.json"
  
  -- MakePayment.testTrace testScripts

  -- print $ scriptSize $ unValidatorScript $ spendingValidator testScripts
  -- print $ scriptSize $ unMintingPolicyScript $ beaconPolicy testScripts

  defaultMain $ testGroup "Cardano-Loans"
    [ 
      CreateAsk.tests testScripts
    , CloseAsk.tests testScripts
    , CreateOffer.tests testScripts
    , CloseOffer.tests testScripts
    , AcceptOffer.tests testScripts
    , MakePayment.tests testScripts
    , Rollover.tests testScripts
    , ClaimExpired.tests testScripts
    , UpdateAddress.tests testScripts
    , ClaimLost.tests testScripts
    ]