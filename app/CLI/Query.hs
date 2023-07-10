{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module CLI.Query
(
  runQueryAllAsks,
  runQueryOwnAsks,
  runQueryAllOffers,
  runQueryOwnOffers,
  runQueryAllBorrowersActiveLoans,
  runQueryAllBorrowersFinishedLoans,
  runQuerySpecificLoan,
  runQueryOwnKeys,
  runQueryBorrowersHistory
) where

import Servant.Client
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Control.Exception

import CLI.BlockfrostApi as Blockfrost
import CLI.Types
import CardanoLoans

runQueryAllAsks :: Network 
                -> ApiEndpoint 
                -> CurrencySymbol 
                -> IO [LoanUTxO]
runQueryAllAsks network api sym = do
  manager' <- newManager tlsManagerSettings
  case (network,api) of
    (PreProdTestnet,Koios) -> undefined
    (PreProdTestnet,Blockfrost apiKey) -> do
      let env = mkClientEnv manager' (BaseUrl Https "cardano-preprod.blockfrost.io" 443 "api/v0")
          apiKey' = BlockfrostApiKey apiKey
      res <- runClientM (Blockfrost.queryAllAsks apiKey' $ show sym) env
      case res of
        Right r -> return r
        Left err -> throw err

runQueryOwnAsks :: Network 
                -> ApiEndpoint 
                -> CurrencySymbol
                -> String
                -> IO [LoanUTxO]
runQueryOwnAsks network api sym addr = do
  manager' <- newManager tlsManagerSettings
  case (network,api) of
    (PreProdTestnet,Koios) -> undefined
    (PreProdTestnet,Blockfrost apiKey) -> do
      let env = mkClientEnv manager' (BaseUrl Https "cardano-preprod.blockfrost.io" 443 "api/v0")
          apiKey' = BlockfrostApiKey apiKey
      res <- runClientM (Blockfrost.queryOwnAsks apiKey' (show sym) addr) env
      case res of
        Right r -> return r
        Left err -> throw err

runQueryAllOffers :: Network 
                  -> ApiEndpoint 
                  -> CurrencySymbol
                  -> String
                  -> IO [LoanUTxO]
runQueryAllOffers network api sym addr = do
  manager' <- newManager tlsManagerSettings
  case (network,api) of
    (PreProdTestnet,Koios) -> undefined
    (PreProdTestnet,Blockfrost apiKey) -> do
      let env = mkClientEnv manager' (BaseUrl Https "cardano-preprod.blockfrost.io" 443 "api/v0")
          apiKey' = BlockfrostApiKey apiKey
      res <- runClientM (Blockfrost.queryAllOffers apiKey' (show sym) addr) env
      case res of
        Right r -> return r
        Left err -> throw err

runQueryOwnOffers :: Network 
                  -> ApiEndpoint 
                  -> CurrencySymbol
                  -> String
                  -> IO [LoanUTxO]
runQueryOwnOffers network api sym lenderCredential = do
  manager' <- newManager tlsManagerSettings
  case (network,api) of
    (PreProdTestnet,Koios) -> undefined
    (PreProdTestnet,Blockfrost apiKey) -> do
      let env = mkClientEnv manager' (BaseUrl Https "cardano-preprod.blockfrost.io" 443 "api/v0")
          apiKey' = BlockfrostApiKey apiKey
      res <- runClientM (Blockfrost.queryOwnOffers apiKey' (show sym) lenderCredential) env
      case res of
        Right r -> return r
        Left err -> throw err

runQueryAllBorrowersActiveLoans :: Network 
                                -> ApiEndpoint 
                                -> CurrencySymbol
                                -> String
                                -> String
                                -> IO [LoanUTxO]
runQueryAllBorrowersActiveLoans network api sym borrowerCredential addr = do
  manager' <- newManager tlsManagerSettings
  case (network,api) of
    (PreProdTestnet,Koios) -> undefined
    (PreProdTestnet,Blockfrost apiKey) -> do
      let env = mkClientEnv manager' (BaseUrl Https "cardano-preprod.blockfrost.io" 443 "api/v0")
          apiKey' = BlockfrostApiKey apiKey
      res <- 
        runClientM (Blockfrost.queryAllBorrowersActiveLoans apiKey' (show sym) borrowerCredential addr) env
      case res of
        Right r -> return r
        Left err -> throw err

runQueryAllBorrowersFinishedLoans :: Network 
                                -> ApiEndpoint 
                                -> CurrencySymbol
                                -> String
                                -> String
                                -> IO [LoanUTxO]
runQueryAllBorrowersFinishedLoans network api sym borrowerCredential addr = do
  manager' <- newManager tlsManagerSettings
  case (network,api) of
    (PreProdTestnet,Koios) -> undefined
    (PreProdTestnet,Blockfrost apiKey) -> do
      let env = mkClientEnv manager' (BaseUrl Https "cardano-preprod.blockfrost.io" 443 "api/v0")
          apiKey' = BlockfrostApiKey apiKey
      res <- 
        runClientM (Blockfrost.queryAllBorrowersFinishedLoans apiKey' (show sym) borrowerCredential addr) env
      case res of
        Right r -> return r
        Left err -> throw err

runQuerySpecificLoan :: Network 
                     -> ApiEndpoint 
                     -> CurrencySymbol
                     -> String
                     -> IO [LoanUTxO]
runQuerySpecificLoan network api sym targetLoanId = do
  manager' <- newManager tlsManagerSettings
  case (network,api) of
    (PreProdTestnet,Koios) -> undefined
    (PreProdTestnet,Blockfrost apiKey) -> do
      let env = mkClientEnv manager' (BaseUrl Https "cardano-preprod.blockfrost.io" 443 "api/v0")
          apiKey' = BlockfrostApiKey apiKey
      res <- 
        runClientM (Blockfrost.querySpecificLoan apiKey' (show sym) targetLoanId) env
      case res of
        Right r -> return r
        Left err -> throw err

runQueryOwnKeys :: Network 
                -> ApiEndpoint 
                -> CurrencySymbol
                -> String
                -> IO [KeyUTxO]
runQueryOwnKeys network api sym addr = do
  manager' <- newManager tlsManagerSettings
  case (network,api) of
    (PreProdTestnet,Koios) -> undefined
    (PreProdTestnet,Blockfrost apiKey) -> do
      let env = mkClientEnv manager' (BaseUrl Https "cardano-preprod.blockfrost.io" 443 "api/v0")
          apiKey' = BlockfrostApiKey apiKey
      res <- runClientM (Blockfrost.queryOwnKeys apiKey' (show sym) addr) env
      case res of
        Right r -> return r
        Left err -> throw err

runQueryBorrowersHistory :: Network 
                         -> ApiEndpoint 
                         -> CurrencySymbol
                         -> String
                         -> IO [LoanHistory]
runQueryBorrowersHistory network api sym borrowerCredential = do
  manager' <- newManager tlsManagerSettings
  case (network,api) of
    (PreProdTestnet,Koios) -> undefined
    (PreProdTestnet,Blockfrost apiKey) -> do
      let env = mkClientEnv manager' (BaseUrl Https "cardano-preprod.blockfrost.io" 443 "api/v0")
          apiKey' = BlockfrostApiKey apiKey
      res <- runClientM (Blockfrost.queryBorrowersHistory apiKey' (show sym) borrowerCredential) env
      case res of
        Right r -> return r
        Left err -> throw err