{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module CLI.Query
(
  runQueryAllAsks,
  runQueryOwnAsks,
  runQueryAllOffers,
  runQueryOwnOffers,
  runQueryAllBorrowerLoans,
  runQueryAllLenderLoans,
  runQueryBorrowerHistory
) where

import Servant.Client
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Control.Exception

import CLI.BlockfrostApi as Blockfrost
import CLI.Types
import CardanoLoans (CurrencySymbol(),PaymentPubKeyHash())

runQueryAllAsks :: Network -> CurrencySymbol -> IO [LoanInfo]
runQueryAllAsks (PreProdTestnet apiKey) currSym = do
  manager' <- newManager tlsManagerSettings
  let env = mkClientEnv manager' (BaseUrl Https "cardano-preprod.blockfrost.io" 443 "api/v0")
      apiKey' = BlockfrostApiKey apiKey
  res <- runClientM (Blockfrost.queryAllAsks apiKey' (show currSym)) env
  case res of
    Right r -> return r
    Left err -> throw err

runQueryOwnAsks :: Network -> CurrencySymbol -> LoanAddress -> IO [LoanInfo]
runQueryOwnAsks (PreProdTestnet apiKey) currSym addr = do
  manager' <- newManager tlsManagerSettings
  let env = mkClientEnv manager' (BaseUrl Https "cardano-preprod.blockfrost.io" 443 "api/v0")
      apiKey' = BlockfrostApiKey apiKey
  res <- runClientM (Blockfrost.queryOwnAsks apiKey' (show currSym) (show addr)) env
  case res of
    Right r -> return r
    Left err -> throw err

runQueryAllOffers :: Network -> CurrencySymbol -> LoanAddress -> IO [LoanInfo]
runQueryAllOffers (PreProdTestnet apiKey) currSym addr = do
  manager' <- newManager tlsManagerSettings
  let env = mkClientEnv manager' (BaseUrl Https "cardano-preprod.blockfrost.io" 443 "api/v0")
      apiKey' = BlockfrostApiKey apiKey
  res <- runClientM (Blockfrost.queryAllOffers apiKey' (show currSym) (show addr)) env
  case res of
    Right r -> return r
    Left err -> throw err

runQueryOwnOffers :: Network -> CurrencySymbol -> PaymentPubKeyHash -> IO [LoanInfo]
runQueryOwnOffers (PreProdTestnet apiKey) currSym lenderPubKeyHash = do
  manager' <- newManager tlsManagerSettings
  let env = mkClientEnv manager' (BaseUrl Https "cardano-preprod.blockfrost.io" 443 "api/v0")
      apiKey' = BlockfrostApiKey apiKey
  res <- runClientM (Blockfrost.queryOwnOffers apiKey' (show currSym) (show lenderPubKeyHash)) env
  case res of
    Right r -> return r
    Left err -> throw err

runQueryAllBorrowerLoans :: Network -> CurrencySymbol -> PaymentPubKeyHash -> LoanAddress -> IO [LoanInfo]
runQueryAllBorrowerLoans (PreProdTestnet apiKey) currSym borrowerStakeKeyHash addr = do
  manager' <- newManager tlsManagerSettings
  let env = mkClientEnv manager' (BaseUrl Https "cardano-preprod.blockfrost.io" 443 "api/v0")
      apiKey' = BlockfrostApiKey apiKey
  res <- runClientM 
            (Blockfrost.queryAllBorrowerLoans apiKey' 
              (show currSym) 
              (show borrowerStakeKeyHash) 
              (show addr)
            ) 
            env
  case res of
    Right r -> return r
    Left err -> throw err

runQueryAllLenderLoans :: Network -> CurrencySymbol -> PaymentPubKeyHash -> IO [LoanInfo]
runQueryAllLenderLoans (PreProdTestnet apiKey) currSym lenderPubKeyHash = do
  manager' <- newManager tlsManagerSettings
  let env = mkClientEnv manager' (BaseUrl Https "cardano-preprod.blockfrost.io" 443 "api/v0")
      apiKey' = BlockfrostApiKey apiKey
  res <- runClientM (Blockfrost.queryAllLenderLoans apiKey' (show currSym) (show lenderPubKeyHash)) env
  case res of
    Right r -> return r
    Left err -> throw err

runQueryBorrowerHistory :: Network -> CurrencySymbol -> PaymentPubKeyHash -> IO [LoanHistory]
runQueryBorrowerHistory (PreProdTestnet apiKey) currSym borrowerPubKeyHash = do
  manager' <- newManager tlsManagerSettings
  let env = mkClientEnv manager' (BaseUrl Https "cardano-preprod.blockfrost.io" 443 "api/v0")
      apiKey' = BlockfrostApiKey apiKey
  res <- runClientM ( Blockfrost.queryBorrowerHistory 
                        apiKey' 
                        (show currSym) 
                        (show borrowerPubKeyHash)
                    ) 
                    env
  case res of
    Right r -> return r
    Left err -> throw err