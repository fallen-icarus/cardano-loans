{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module CLI.Query
(
  runAllAsksQuery,
  runBorrowerAsksQuery,
  runAllOffersFromLenderQuery,
  runAllOffersToBorrowerQuery,
  runAllLendersActiveLoansQuery,
  runAllBorrowersActiveLoansQuery,
  runQueryBorrowerHistory,
) where

import Servant.Client
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Control.Exception

import CLI.BlockfrostApi as Blockfrost
import CLI.Types
import CardanoLoans

runAllAsksQuery :: Network -> IO [AvailableAsk]
runAllAsksQuery (PreProdTestnet apiKey) = do
  manager' <- newManager tlsManagerSettings
  let env = mkClientEnv manager' (BaseUrl Https "cardano-preprod.blockfrost.io" 443 "api/v0")
      apiKey' = BlockfrostApiKey apiKey
  res <- runClientM (Blockfrost.queryAllAsks apiKey') env
  case res of
    Right r -> return r
    Left err -> throw err

runBorrowerAsksQuery :: String -> Network -> IO [AvailableAsk]
runBorrowerAsksQuery addr (PreProdTestnet apiKey) = do
  manager' <- newManager tlsManagerSettings
  let env = mkClientEnv manager' (BaseUrl Https "cardano-preprod.blockfrost.io" 443 "api/v0")
      apiKey' = BlockfrostApiKey apiKey
  res <- runClientM (Blockfrost.queryBorrowerAsks apiKey' addr) env
  case res of
    Right r -> return r
    Left err -> throw err

runAllOffersFromLenderQuery :: String -> Network -> IO [AvailableOffer]
runAllOffersFromLenderQuery lenderPubKeyHash (PreProdTestnet apiKey) = do
  manager' <- newManager tlsManagerSettings
  let env = mkClientEnv manager' (BaseUrl Https "cardano-preprod.blockfrost.io" 443 "api/v0")
      apiKey' = BlockfrostApiKey apiKey
  res <- runClientM (Blockfrost.queryAllOffersFromLender apiKey' lenderPubKeyHash) env
  case res of
    Right r -> return r
    Left err -> throw err

runAllOffersToBorrowerQuery :: String -> Network -> IO [AvailableOffer]
runAllOffersToBorrowerQuery addr (PreProdTestnet apiKey) = do
  manager' <- newManager tlsManagerSettings
  let env = mkClientEnv manager' (BaseUrl Https "cardano-preprod.blockfrost.io" 443 "api/v0")
      apiKey' = BlockfrostApiKey apiKey
  res <- runClientM (Blockfrost.queryAllOffersToBorrower apiKey' addr) env
  case res of
    Right r -> return r
    Left err -> throw err

runAllLendersActiveLoansQuery :: String -> Network -> IO [AvailableActive]
runAllLendersActiveLoansQuery lenderPubKeyHash (PreProdTestnet apiKey) = do
  manager' <- newManager tlsManagerSettings
  let env = mkClientEnv manager' (BaseUrl Https "cardano-preprod.blockfrost.io" 443 "api/v0")
      apiKey' = BlockfrostApiKey apiKey
  res <- runClientM (Blockfrost.queryAllLendersActiveLoans apiKey' lenderPubKeyHash) env
  case res of
    Right r -> return r
    Left err -> throw err

runAllBorrowersActiveLoansQuery :: String -> Network -> IO [AvailableActive]
runAllBorrowersActiveLoansQuery borrowerPubKeyHash (PreProdTestnet apiKey) = do
  manager' <- newManager tlsManagerSettings
  let env = mkClientEnv manager' (BaseUrl Https "cardano-preprod.blockfrost.io" 443 "api/v0")
      apiKey' = BlockfrostApiKey apiKey
  res <- runClientM (Blockfrost.queryAllBorrowersActiveLoans apiKey' borrowerPubKeyHash) env
  case res of
    Right r -> return r
    Left err -> throw err

runQueryBorrowerHistory :: String -> Network -> IO [LoanHistory]
runQueryBorrowerHistory borrowerPubKeyHash (PreProdTestnet apiKey) = do
  manager' <- newManager tlsManagerSettings
  let env = mkClientEnv manager' (BaseUrl Https "cardano-preprod.blockfrost.io" 443 "api/v0")
      apiKey' = BlockfrostApiKey apiKey
  res <- runClientM (Blockfrost.queryBorrowerHistory apiKey' borrowerPubKeyHash) env
  case res of
    Right r -> return r
    Left err -> throw err