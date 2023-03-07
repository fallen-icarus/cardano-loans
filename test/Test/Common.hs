{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE StrictData #-}

module Test.Common where

import qualified Data.Map as Map
import Control.Lens hiding (from,to)
import Data.Default
import Data.Void (Void)
import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Text (Text)
import Ledger hiding (singleton,mintingPolicyHash)
import Ledger.Constraints as Constraints
import qualified Ledger.Constraints.TxConstraints as Constraints
import Plutus.Contract
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), foldMap)
import Ledger.Value (singleton)
import Ledger.Ada (lovelaceValueOf)
import Plutus.Script.Utils.V2.Scripts as UScripts
import Plutus.Trace
import Wallet.Emulator.Wallet
import Data.List (foldl')
import Prelude as Haskell (Semigroup (..), String)
import Plutus.Script.Utils.V2.Generators
import Cardano.Api.Shelley (ExecutionUnits (..),ProtocolParameters (..))
import PlutusPrelude (Natural)

import CardanoLoans

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
toRedeemer :: PlutusTx.ToData a => a -> Redeemer
toRedeemer = Redeemer . PlutusTx.dataToBuiltinData . PlutusTx.toData

toDatum :: PlutusTx.ToData a => a -> Datum
toDatum = Datum . PlutusTx.dataToBuiltinData . PlutusTx.toData

mustPayToAddressWith :: Address -> Maybe (TxOutDatum Datum) -> Value -> TxConstraints i o
mustPayToAddressWith addr maybeDatum val =
  Constraints.singleton $ MustPayToAddress addr maybeDatum Nothing val

data LoanDatum'
  -- | The datum for the ask ask.
  = AskDatum'
      { askBeacon' :: (CurrencySymbol,TokenName)
      , borrowerId' :: (CurrencySymbol,TokenName)
      , loanAsset' :: (CurrencySymbol,TokenName)
      , loanQuantity' :: Integer
      , loanTerm' :: POSIXTime
      , collateral' :: [(CurrencySymbol,TokenName)]
      }
  -- | The datum for the offer ask.
  | OfferDatum'
      { offerBeacon' :: (CurrencySymbol,TokenName)
      , lenderId' :: (CurrencySymbol,TokenName)
      , loanAsset' :: (CurrencySymbol,TokenName)
      , loanQuantity' :: Integer
      , loanTerm' :: POSIXTime
      , loanInterest' :: Rational
      , collateralRates' :: [((CurrencySymbol,TokenName),Rational)]
      }
  -- | The datum for the active ask. This also has information useful for the credit history.
  | ActiveDatum'
      { activeBeacon' :: (CurrencySymbol,TokenName)
      , lenderId' :: (CurrencySymbol,TokenName)
      , borrowerId' :: (CurrencySymbol,TokenName)
      , loanAsset' :: (CurrencySymbol,TokenName)
      , loanQuantity' :: Integer
      , loanTerm' :: POSIXTime
      , loanInterest' :: Rational
      , collateralRates' :: [((CurrencySymbol,TokenName),Rational)]
      , loanExpiration' :: POSIXTime
      , loanOutstanding' :: Rational
      }
  deriving (Generic,ToJSON,FromJSON)

convert2LoanDatum :: LoanDatum' -> LoanDatum
convert2LoanDatum (AskDatum' a b c d e f) = AskDatum a b c d e f
convert2LoanDatum (OfferDatum' a b c d e f g) = OfferDatum a b c d e f g
convert2LoanDatum (ActiveDatum' a b c d e f g h i j) = ActiveDatum a b c d e f g h i j

-- | The redeemer for the beacons.
data BeaconRedeemer'
  -- | Mint the ask token to the borrower's address.
  = MintAskToken' PaymentPubKeyHash -- ^ Pubkey for the borrower's staking credential. Simplifies logic.
  -- | Mint the offer token and lender ID.
  | MintOfferToken' PaymentPubKeyHash -- ^ Pubkey for lender ID.
  -- | Mint the active token and the borrower ID.
  -- The first pubkey is the borrower's. The second one is the lender's.
  | MintActiveToken' PaymentPubKeyHash PaymentPubKeyHash
  -- | Burn any token/IDs.
  | BurnBeaconToken'
  deriving (Generic,ToJSON,FromJSON)

convert2BeaconRedeemer :: BeaconRedeemer' -> BeaconRedeemer
convert2BeaconRedeemer (MintAskToken' pkh) = MintAskToken pkh
convert2BeaconRedeemer (MintOfferToken' pkh) = MintOfferToken pkh
convert2BeaconRedeemer (MintActiveToken' pkh1 pkh2) = MintActiveToken pkh1 pkh2
convert2BeaconRedeemer BurnBeaconToken' = BurnBeaconToken

-------------------------------------------------
-- Params
-------------------------------------------------
data AskParams = AskParams
  { askBeaconsMinted :: [(TokenName,Integer)]
  , askBeaconRedeemer :: BeaconRedeemer'
  , askBeaconPolicy :: MintingPolicy
  , askAddress :: Address
  , askInfo :: [(Maybe LoanDatum', Value)]
  , askAsInline :: Bool
  } deriving (Generic,ToJSON,FromJSON)

-- data OfferParams :: OfferParams
--   { offerBeaconsMinted :: 
--   }

type TraceSchema =
      Endpoint "ask" AskParams

-------------------------------------------------
-- Configs
-------------------------------------------------
testToken1 :: (CurrencySymbol,TokenName)
testToken1 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken1")

testToken2 :: (CurrencySymbol,TokenName)
testToken2 = ("c0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561d","TestToken2")

emConfig :: EmulatorConfig
emConfig = EmulatorConfig (Left $ Map.fromList wallets) def
  where
    user1 :: Value
    user1 = lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 100
         <> (uncurry singleton testToken2) 100

    user2 :: Value
    user2 = lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 100
         <> (uncurry singleton testToken2) 100
    
    user3 :: Value
    user3 = lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 100
    
    user4 :: Value
    user4 = lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 100
  
    wallets :: [(Wallet,Value)]
    wallets = 
      [ (knownWallet 1, user1)
      , (knownWallet 2, user2)
      , (knownWallet 3, user3)
      , (knownWallet 4, user4)
      ]

-------------------------------------------------
-- Trace Models
-------------------------------------------------
askForLoan :: AskParams -> Contract () TraceSchema Text ()
askForLoan AskParams{..} = do
  userPubKeyHash <- ownFirstPaymentPubKeyHash
  let beaconPolicyHash = mintingPolicyHash askBeaconPolicy
      beaconRedeemer = toRedeemer $ convert2BeaconRedeemer askBeaconRedeemer
      
      toDatum'
        | askAsInline = TxOutDatumInline . toDatum . convert2LoanDatum
        | otherwise = TxOutDatumHash . toDatum . convert2LoanDatum
      
      lookups = plutusV2MintingPolicy askBeaconPolicy
      
      tx' =
        -- | Mint Beacons
        (foldl' 
          (\acc (t,i) -> acc <> mustMintCurrencyWithRedeemer beaconPolicyHash beaconRedeemer t i) 
          mempty
          askBeaconsMinted
        )
        -- | Add ask
        <> (foldl'
              (\acc (d,v) -> acc <> mustPayToAddressWith askAddress (fmap toDatum' d) v)
              mempty
              askInfo
           )
        -- | Must be signed by receiving pubkey
        <> mustBeSignedBy userPubKeyHash
  
  ledgerTx <- submitTxConstraintsWith @Void lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String "Asked for a loan"

-------------------------------------------------
-- Endpoints
-------------------------------------------------
endpoints :: Contract () TraceSchema Text ()
endpoints = selectList choices >> endpoints
  where
    askForLoan' = endpoint @"ask" askForLoan
    choices = 
      [ askForLoan'
      ]