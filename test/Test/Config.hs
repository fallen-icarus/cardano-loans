{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Test.Config where

import qualified Data.Map as Map
import Wallet.Emulator.Wallet
import qualified Cardano.Api as C
import Ledger.Tx.CardanoAPI.Internal (toCardanoValue)
import Plutus.Trace.Emulator

import Test.Internal
import CardanoLoans

-------------------------------------------------
-- Configs
-------------------------------------------------
-- | 51 ADA is the default. This is what the cardano-loans spending validator requires.
minUTxOSpendRef :: Integer
minUTxOSpendRef = 51_000_000

-- | 60 ADA is the default. This is what the cardano-loans minting policy requires.
minUTxOMintRef :: Integer
minUTxOMintRef = 60_000_000

-- | An always succeeding validator address without a staking credential.
refScriptAddress :: Address
refScriptAddress = Address (ScriptCredential alwaysSucceedValidatorHash) Nothing

testToken1 :: (CurrencySymbol,TokenName)
testToken1 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken1")

testToken2 :: (CurrencySymbol,TokenName)
testToken2 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken2")

testToken3 :: (CurrencySymbol,TokenName)
testToken3 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken3")

testToken4 :: (CurrencySymbol,TokenName)
testToken4 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken4")

testToken5 :: (CurrencySymbol,TokenName)
testToken5 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken5")

testToken6 :: (CurrencySymbol,TokenName)
testToken6 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken6")

testToken7 :: (CurrencySymbol,TokenName)
testToken7 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken7")

testToken8 :: (CurrencySymbol,TokenName)
testToken8 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken8")

testToken9 :: (CurrencySymbol,TokenName)
testToken9 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken9")

testToken10 :: (CurrencySymbol,TokenName)
testToken10 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken10")

testToken11 :: (CurrencySymbol,TokenName)
testToken11 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken11")

testToken12 :: (CurrencySymbol,TokenName)
testToken12 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken12")

testToken13 :: (CurrencySymbol,TokenName)
testToken13 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken13")

testToken14 :: (CurrencySymbol,TokenName)
testToken14 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken14")

testToken15 :: (CurrencySymbol,TokenName)
testToken15 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken15")

testToken16 :: (CurrencySymbol,TokenName)
testToken16 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken16")

testToken17 :: (CurrencySymbol,TokenName)
testToken17 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken17")

testToken18 :: (CurrencySymbol,TokenName)
testToken18 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken18")

testToken19 :: (CurrencySymbol,TokenName)
testToken19 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken19")

testToken20 :: (CurrencySymbol,TokenName)
testToken20 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken20")

emConfig :: EmulatorConfig
emConfig = EmulatorConfig (Left $ Map.fromList wallets) def
  where
    user1 :: C.Value
    user1 = unsafeFromRight . toCardanoValue $ lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 1000
         <> (uncurry singleton testToken2) 1000
         <> (uncurry singleton testToken3) 1000
         <> (uncurry singleton testToken4) 1000
         <> (uncurry singleton testToken5) 1000
         <> (uncurry singleton testToken6) 1000
         <> (uncurry singleton testToken7) 1000
         <> (uncurry singleton testToken8) 1000
         <> (uncurry singleton testToken9) 1000
         <> (uncurry singleton testToken10) 1000
         <> (uncurry singleton testToken11) 1000
         <> (uncurry singleton testToken12) 1000
         <> (uncurry singleton testToken13) 1000
         <> (uncurry singleton testToken14) 1000
         <> (uncurry singleton testToken15) 1000
         <> (uncurry singleton testToken16) 1000
         <> (uncurry singleton testToken17) 1000

    user2 :: C.Value
    user2 = unsafeFromRight . toCardanoValue $ lovelaceValueOf 5_000_000_000
         <> (uncurry singleton testToken1) 1000
         <> (uncurry singleton testToken2) 1000
         <> (uncurry singleton testToken3) 1000
         <> (uncurry singleton testToken4) 1000
         <> (uncurry singleton testToken5) 1000
         <> (uncurry singleton testToken6) 1000
         <> (uncurry singleton testToken7) 1000
         <> (uncurry singleton testToken8) 1000
         <> (uncurry singleton testToken9) 1000
         <> (uncurry singleton testToken10) 1000
         <> (uncurry singleton testToken11) 1000
         <> (uncurry singleton testToken12) 1000
         <> (uncurry singleton testToken13) 1000
         <> (uncurry singleton testToken14) 1000
         <> (uncurry singleton testToken15) 1000
         <> (uncurry singleton testToken16) 1000
         <> (uncurry singleton testToken17) 1000
         <> (uncurry singleton testToken18) 1000
         <> (uncurry singleton testToken19) 1000
         <> (uncurry singleton testToken20) 1000
    
    user3 :: C.Value
    user3 = unsafeFromRight . toCardanoValue $ lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 1000
         <> (uncurry singleton testToken2) 1000
         <> (uncurry singleton testToken3) 1000
         <> (uncurry singleton testToken4) 1000
         <> (uncurry singleton testToken5) 1000
         <> (uncurry singleton testToken6) 1000
         <> (uncurry singleton testToken7) 1000
         <> (uncurry singleton testToken8) 1000
         <> (uncurry singleton testToken9) 1000
         <> (uncurry singleton testToken10) 1000
         <> (uncurry singleton testToken11) 1000
         <> (uncurry singleton testToken12) 1000
         <> (uncurry singleton testToken13) 1000
         <> (uncurry singleton testToken14) 1000
         <> (uncurry singleton testToken15) 1000
         <> (uncurry singleton testToken16) 1000
         <> (uncurry singleton testToken17) 1000

    user4 :: C.Value
    user4 = unsafeFromRight . toCardanoValue $ lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 1000
         <> (uncurry singleton testToken2) 1000
         <> (uncurry singleton testToken3) 1000
         <> (uncurry singleton testToken4) 1000
         <> (uncurry singleton testToken5) 1000
         <> (uncurry singleton testToken6) 1000
         <> (uncurry singleton testToken7) 1000
         <> (uncurry singleton testToken8) 1000
         <> (uncurry singleton testToken9) 1000
         <> (uncurry singleton testToken10) 1000
         <> (uncurry singleton testToken11) 1000
         <> (uncurry singleton testToken12) 1000
         <> (uncurry singleton testToken13) 1000
         <> (uncurry singleton testToken14) 1000
         <> (uncurry singleton testToken15) 1000
         <> (uncurry singleton testToken16) 1000
         <> (uncurry singleton testToken17) 1000
         <> (uncurry singleton testToken18) 1000
         <> (uncurry singleton testToken19) 1000
         <> (uncurry singleton testToken20) 1000
    
    user5 :: C.Value
    user5 = unsafeFromRight . toCardanoValue $ lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 1000
         <> (uncurry singleton testToken2) 1000
         <> (uncurry singleton testToken3) 1000
         <> (uncurry singleton testToken4) 1000
         <> (uncurry singleton testToken5) 1000
         <> (uncurry singleton testToken6) 1000
         <> (uncurry singleton testToken7) 1000
         <> (uncurry singleton testToken8) 1000
         <> (uncurry singleton testToken9) 1000
         <> (uncurry singleton testToken10) 1000
         <> (uncurry singleton testToken11) 1000
         <> (uncurry singleton testToken12) 1000
         <> (uncurry singleton testToken13) 1000
         <> (uncurry singleton testToken14) 1000
         <> (uncurry singleton testToken15) 1000
         <> (uncurry singleton testToken16) 1000
         <> (uncurry singleton testToken17) 1000
         <> (uncurry singleton testToken18) 1000
         <> (uncurry singleton testToken19) 1000
         <> (uncurry singleton testToken20) 1000

    user6 :: C.Value
    user6 = unsafeFromRight . toCardanoValue $ lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 1000
         <> (uncurry singleton testToken2) 1000
         <> (uncurry singleton testToken3) 1000
         <> (uncurry singleton testToken4) 1000
         <> (uncurry singleton testToken5) 1000
         <> (uncurry singleton testToken6) 1000
         <> (uncurry singleton testToken7) 1000
         <> (uncurry singleton testToken8) 1000
         <> (uncurry singleton testToken9) 1000
         <> (uncurry singleton testToken10) 1000
         <> (uncurry singleton testToken11) 1000
         <> (uncurry singleton testToken12) 1000
         <> (uncurry singleton testToken13) 1000
         <> (uncurry singleton testToken14) 1000
         <> (uncurry singleton testToken15) 1000
         <> (uncurry singleton testToken16) 1000
         <> (uncurry singleton testToken17) 1000
         <> (uncurry singleton testToken18) 1000
         <> (uncurry singleton testToken19) 1000
         <> (uncurry singleton testToken20) 1000

    user7 :: C.Value
    user7 = unsafeFromRight . toCardanoValue $ lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 1000
         <> (uncurry singleton testToken2) 1000
         <> (uncurry singleton testToken3) 1000
         <> (uncurry singleton testToken4) 1000
         <> (uncurry singleton testToken5) 1000
         <> (uncurry singleton testToken6) 1000
         <> (uncurry singleton testToken7) 1000
         <> (uncurry singleton testToken8) 1000
         <> (uncurry singleton testToken9) 1000
         <> (uncurry singleton testToken10) 1000
         <> (uncurry singleton testToken11) 1000
         <> (uncurry singleton testToken12) 1000
         <> (uncurry singleton testToken13) 1000
         <> (uncurry singleton testToken14) 1000
         <> (uncurry singleton testToken15) 1000
         <> (uncurry singleton testToken16) 1000
         <> (uncurry singleton testToken17) 1000
         <> (uncurry singleton testToken18) 1000
         <> (uncurry singleton testToken19) 1000
         <> (uncurry singleton testToken20) 1000
    
    user8 :: C.Value
    user8 = unsafeFromRight . toCardanoValue $ lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 1000
         <> (uncurry singleton testToken2) 1000
         <> (uncurry singleton testToken3) 1000
         <> (uncurry singleton testToken4) 1000
         <> (uncurry singleton testToken5) 1000
         <> (uncurry singleton testToken6) 1000
         <> (uncurry singleton testToken7) 1000
         <> (uncurry singleton testToken8) 1000
         <> (uncurry singleton testToken9) 1000
         <> (uncurry singleton testToken10) 1000
         <> (uncurry singleton testToken11) 1000
         <> (uncurry singleton testToken12) 1000
         <> (uncurry singleton testToken13) 1000
         <> (uncurry singleton testToken14) 1000
         <> (uncurry singleton testToken15) 1000
         <> (uncurry singleton testToken16) 1000
         <> (uncurry singleton testToken17) 1000
         <> (uncurry singleton testToken18) 1000
         <> (uncurry singleton testToken19) 1000
         <> (uncurry singleton testToken20) 1000
    
    user9 :: C.Value
    user9 = unsafeFromRight . toCardanoValue $ lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 1000
         <> (uncurry singleton testToken2) 1000
         <> (uncurry singleton testToken3) 1000
         <> (uncurry singleton testToken4) 1000
         <> (uncurry singleton testToken5) 1000
         <> (uncurry singleton testToken6) 1000
         <> (uncurry singleton testToken7) 1000
         <> (uncurry singleton testToken8) 1000
         <> (uncurry singleton testToken9) 1000
         <> (uncurry singleton testToken10) 1000
         <> (uncurry singleton testToken11) 1000
         <> (uncurry singleton testToken12) 1000
         <> (uncurry singleton testToken13) 1000
         <> (uncurry singleton testToken14) 1000
         <> (uncurry singleton testToken15) 1000
         <> (uncurry singleton testToken16) 1000
         <> (uncurry singleton testToken17) 1000
         <> (uncurry singleton testToken18) 1000
         <> (uncurry singleton testToken19) 1000
         <> (uncurry singleton testToken20) 1000

    user10 :: C.Value
    user10 = unsafeFromRight . toCardanoValue $ lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 1000
         <> (uncurry singleton testToken2) 1000
         <> (uncurry singleton testToken3) 1000
         <> (uncurry singleton testToken4) 1000
         <> (uncurry singleton testToken5) 1000
         <> (uncurry singleton testToken6) 1000
         <> (uncurry singleton testToken7) 1000
         <> (uncurry singleton testToken8) 1000
         <> (uncurry singleton testToken9) 1000
         <> (uncurry singleton testToken10) 1000
         <> (uncurry singleton testToken11) 1000
         <> (uncurry singleton testToken12) 1000
         <> (uncurry singleton testToken13) 1000
         <> (uncurry singleton testToken14) 1000
         <> (uncurry singleton testToken15) 1000
         <> (uncurry singleton testToken16) 1000
         <> (uncurry singleton testToken17) 1000
         <> (uncurry singleton testToken18) 1000
         <> (uncurry singleton testToken19) 1000
         <> (uncurry singleton testToken20) 1000

    wallets :: [(Wallet,C.Value)]
    wallets = 
      [ (knownWallet 1, user1)
      , (knownWallet 2, user2)
      , (knownWallet 3, user3)
      , (knownWallet 4, user4)
      , (knownWallet 5, user5)
      , (knownWallet 6, user6)
      , (knownWallet 7, user7)
      , (knownWallet 8, user8)
      , (knownWallet 9, user9)
      , (knownWallet 10, user10)
      ]

benchConfig :: EmulatorConfig
benchConfig = emConfig & params .~ params'
  where 
    params' :: Params
    params' = def{emulatorPParams = pParams'}

    pParams' :: PParams
    pParams' = pParamsFromProtocolParams protoParams

    protoParams :: ProtocolParameters
    protoParams = def
      { 
        protocolParamMaxTxExUnits = 
          Just (C.ExecutionUnits { C.executionSteps = 10000000000
                                 , C.executionMemory = 13500000
                                 }
               )
      , protocolParamMaxTxSize = 11300
      }
