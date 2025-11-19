{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}

module CLI.Query
  (
    runQuerySlotTip
  , runQueryPersonalAddress
  , runQueryAsks
  , runQueryOffers
  , runQueryActives
  , runQuerySpecificLoanUTxO
  , runQueryBorrowerHistory
  , runQueryLoanHistory
  , runSubmitTx
  , runEvaluateTx
  , runGetParams
  ) where

import Relude
import Servant.Client 
import Network.HTTP.Client hiding (responseBody)
import Network.HTTP.Client.TLS
import Control.Exception
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson

import CLI.Query.Koios as Koios
import CLI.Data.Bech32Address
import CLI.Data.Network
import CLI.Data.ApiService
import CLI.Data.PersonalUTxO
import CLI.Data.LoanUTxO
import CLI.Data.CreditHistory
import CLI.Data.LoanHistory
import CLI.Data.TxCBOR

import CardanoLoans

runQuerySlotTip :: Network -> ApiService -> IO Integer
runQuerySlotTip network api = do
  manager' <- newManager tlsManagerSettings
  either throw return =<< case (network,api) of
    (PreProdTestnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "preprod.koios.rest" 443 "api/v1")
      runClientM Koios.querySlotTip env
    (Mainnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "api.koios.rest" 443 "api/v1")
      runClientM Koios.querySlotTip env

runQueryPersonalAddress :: Network -> ApiService -> PaymentAddress -> Bool -> IO [PersonalUTxO]
runQueryPersonalAddress network api addr keysOnly = do
  manager' <- newManager tlsManagerSettings
  either throw return =<< case (network,api) of
    (PreProdTestnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "preprod.koios.rest" 443 "api/v1")
      runClientM (Koios.queryPersonalAddress addr keysOnly) env
    (Mainnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "api.koios.rest" 443 "api/v1")
      runClientM (Koios.queryPersonalAddress addr keysOnly) env

runQueryAsks 
  :: Network 
  -> ApiService
  -> Maybe AssetBeacon 
  -> Collateral 
  -> Maybe PaymentAddress 
  -> IO [LoanUTxO]
runQueryAsks network api mAssetBeacon collateral mBorrowerAddr = do
  manager' <- newManager tlsManagerSettings
  either throw return =<< case (network,api) of
    (PreProdTestnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "preprod.koios.rest" 443 "api/v1")
      runClientM (Koios.queryAsks mAssetBeacon collateral mBorrowerAddr) env
    (Mainnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "api.koios.rest" 443 "api/v1")
      runClientM (Koios.queryAsks mAssetBeacon collateral mBorrowerAddr) env

runQueryOffers 
  :: Network 
  -> ApiService 
  -> Maybe AssetBeacon 
  -> Maybe PaymentAddress 
  -> Maybe LenderId 
  -> IO [LoanUTxO]
runQueryOffers network api mAssetBeacon mBorrowerAddr mLenderId = do
  manager' <- newManager tlsManagerSettings
  either throw return =<< case (network,api) of
    (PreProdTestnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "preprod.koios.rest" 443 "api/v1")
      runClientM (Koios.queryOffers mAssetBeacon mBorrowerAddr mLenderId) env
    (Mainnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "api.koios.rest" 443 "api/v1")
      runClientM (Koios.queryOffers mAssetBeacon mBorrowerAddr mLenderId) env

runQueryActives
  :: Network 
  -> ApiService 
  -> Maybe AssetBeacon 
  -> Maybe PaymentAddress 
  -> Maybe LoanId
  -> IO [LoanUTxO]
runQueryActives network api mAssetBeacon mBorrowerAddr mLoanId = do
  manager' <- newManager tlsManagerSettings
  either throw return =<< case (network,api) of
    (PreProdTestnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "preprod.koios.rest" 443 "api/v1")
      runClientM (Koios.queryActives mAssetBeacon mBorrowerAddr mLoanId) env
    (Mainnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "api.koios.rest" 443 "api/v1")
      runClientM (Koios.queryActives mAssetBeacon mBorrowerAddr mLoanId) env

runQuerySpecificLoanUTxO :: Network -> ApiService -> TxOutRef -> IO [LoanUTxO]
runQuerySpecificLoanUTxO network api outRef = do
  manager' <- newManager tlsManagerSettings
  either throw return =<< case (network,api) of
    (PreProdTestnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "preprod.koios.rest" 443 "api/v1")
      runClientM (Koios.querySpecificLoanUTxO outRef) env
    (Mainnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "api.koios.rest" 443 "api/v1")
      runClientM (Koios.querySpecificLoanUTxO outRef) env

runQueryBorrowerHistory :: Network -> ApiService -> BorrowerId -> IO [CreditHistory]
runQueryBorrowerHistory network api borrowerId = do
  manager' <- newManager tlsManagerSettings
  either throw return =<< case (network,api) of
    (PreProdTestnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "preprod.koios.rest" 443 "api/v1")
      runClientM (Koios.queryBorrowerHistory borrowerId) env
    (Mainnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "api.koios.rest" 443 "api/v1")
      runClientM (Koios.queryBorrowerHistory borrowerId) env

runQueryLoanHistory :: Network -> ApiService -> LoanId -> IO [LoanHistory]
runQueryLoanHistory network api loanId = do
  manager' <- newManager tlsManagerSettings
  either throw return =<< case (network,api) of
    (PreProdTestnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "preprod.koios.rest" 443 "api/v1")
      runClientM (Koios.queryLoanHistory loanId) env
    (Mainnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "api.koios.rest" 443 "api/v1")
      runClientM (Koios.queryLoanHistory loanId) env

runSubmitTx :: Network -> ApiService -> FilePath -> IO Value
runSubmitTx network api txFile = do
  tx' <- decode @TxCBOR <$> LBS.readFile txFile
  case tx' of
    Nothing -> return "Failed to deserialise transaction file"
    Just tx -> do
      manager' <- newManager tlsManagerSettings
      res <- case (network,api) of
        (PreProdTestnet,Koios) -> do
          let env = mkClientEnv manager' (BaseUrl Https "preprod.koios.rest" 443 "api/v1/ogmios")
          runClientM (Koios.submitTx tx) env
        (Mainnet,Koios) -> do
          let env = mkClientEnv manager' (BaseUrl Https "api.koios.rest" 443 "api/v1/ogmios")
          runClientM (Koios.submitTx tx) env
      case res of
        Right r -> return r
        Left e@(FailureResponse _ err) -> case decode $ responseBody err of
          Just response -> return response
          Nothing -> throw e
        Left err -> throw err

runEvaluateTx :: Network -> ApiService -> FilePath -> IO Value
runEvaluateTx network api txFile = do
  tx' <- decode @TxCBOR <$> LBS.readFile txFile
  case tx' of
    Nothing -> return "Failed to deserialise transaction file"
    Just tx -> do
      manager' <- newManager tlsManagerSettings
      res <- case (network,api) of
        (PreProdTestnet,Koios) -> do
          let env = mkClientEnv manager' (BaseUrl Https "preprod.koios.rest" 443 "api/v1/ogmios")
          runClientM (Koios.evaluateTx tx) env
        (Mainnet,Koios) -> do
          let env = mkClientEnv manager' (BaseUrl Https "api.koios.rest" 443 "api/v1/ogmios")
          runClientM (Koios.evaluateTx tx) env
      case res of
        Right r -> return r
        Left e@(FailureResponse _ err) -> case decode $ responseBody err of
          Just response -> return response
          Nothing -> throw e
        Left err -> throw err

runGetParams :: Network -> IO Value
runGetParams network = do
  manager' <- newManager tlsManagerSettings
  either throw return =<< case network of
    PreProdTestnet -> do
      let env = mkClientEnv manager' (BaseUrl Https "preprod.koios.rest" 443 "api/v1")
      runClientM Koios.getParams env
    Mainnet -> do
      let env = mkClientEnv manager' (BaseUrl Https "api.koios.rest" 443 "api/v1")
      runClientM Koios.getParams env
