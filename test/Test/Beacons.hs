{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Beacons where

import qualified Ledger.Address as LA
import qualified PlutusLedgerApi.V2 as PV2
import qualified Ledger.CardanoWallet as Mock 
import Test.Tasty (TestTree,testGroup)
import Test.Tasty.HUnit

import CardanoLoans

import Test.Prelude

-------------------------------------------------
-- Beacon Name Tests
-------------------------------------------------
-- | The borrower id and the lender id should have different names, even if the same credential
-- is used for both.
nameTest1 :: TestTree
nameTest1 = testCase "nameTest1" $ assertBool "The beacon names are the same" $
    _unBorrowerId (genBorrowerId borrowerCred) /= _unLenderId (genLenderId borrowerCred)
  where
    borrowerCred = PV2.PubKeyCredential 
                 $ LA.unPaymentPubKeyHash
                 $ Mock.paymentPubKeyHash
                 $ Mock.knownMockWallet 1
  
-- | Two different borrower credentials should have two different borrower ids.
nameTest2 :: TestTree
nameTest2 = testCase "nameTest2" $ assertBool "The beacon names are the same" $
    genBorrowerId borrowerCred1 /= genBorrowerId borrowerCred2
  where
    borrowerCred1 = PV2.PubKeyCredential 
                  $ LA.unPaymentPubKeyHash
                  $ Mock.paymentPubKeyHash
                  $ Mock.knownMockWallet 1
    borrowerCred2 = PV2.PubKeyCredential 
                  $ LA.unPaymentPubKeyHash
                  $ Mock.paymentPubKeyHash
                  $ Mock.knownMockWallet 2
  
-- | Two different lender credentials should have two different lender ids.
nameTest3 :: TestTree
nameTest3 = testCase "nameTest3" $ assertBool "The beacon names are the same" $
    genLenderId borrowerCred1 /= genLenderId borrowerCred2
  where
    borrowerCred1 = PV2.PubKeyCredential 
                  $ LA.unPaymentPubKeyHash
                  $ Mock.paymentPubKeyHash
                  $ Mock.knownMockWallet 1
    borrowerCred2 = PV2.PubKeyCredential 
                  $ LA.unPaymentPubKeyHash
                  $ Mock.paymentPubKeyHash
                  $ Mock.knownMockWallet 2

-- | Two different assets should have two different Asset beacons.
nameTest4 :: TestTree
nameTest4 = testCase "nameTest4" $ assertBool "The beacon names are the same" $
  genLoanAssetBeaconName (Asset (testTokenSymbol,"Test1")) /= 
    genLoanAssetBeaconName (Asset (testTokenSymbol,"Test2"))

-- | The Asset beacon name should never overlap with the loan id name when given the same input.
nameTest5 :: TestTree
nameTest5 = testCase "nameTest5" $ assertBool "The beacon names are the same" $
  _unAssetBeacon (genLoanAssetBeaconName (Asset (testTokenSymbol,"0"))) /= 
    _unLoanId (genLoanId (TxOutRef (TxId $ unCurrencySymbol testTokenSymbol) 0))

-------------------------------------------------
-- TestTree
-------------------------------------------------
-- | A `TestTree` containing all beacon uniqueness tests.
tests :: TestTree
tests = testGroup "Beacon Names"
  [ nameTest1
  , nameTest2
  , nameTest3
  , nameTest4
  , nameTest5
  ]
