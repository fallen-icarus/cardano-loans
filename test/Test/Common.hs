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
      , loanPrinciple' :: Integer
      , loanTerm' :: POSIXTime
      , collateral' :: [(CurrencySymbol,TokenName)]
      }
  -- | The datum for the offer ask.
  | OfferDatum'
      { offerBeacon' :: (CurrencySymbol,TokenName)
      , lenderId' :: (CurrencySymbol,TokenName)
      , loanAsset' :: (CurrencySymbol,TokenName)
      , loanPrinciple' :: Integer
      , loanTerm' :: POSIXTime
      , loanInterest' :: Rational
      , loanBacking' :: Integer
      , collateralRates' :: [((CurrencySymbol,TokenName),Rational)]
      }
  -- | The datum for the active ask. This also has information useful for the credit history.
  | ActiveDatum'
      { activeBeacon' :: (CurrencySymbol,TokenName)
      , lenderId' :: (CurrencySymbol,TokenName)
      , borrowerId' :: (CurrencySymbol,TokenName)
      , loanAsset' :: (CurrencySymbol,TokenName)
      , loanPrinciple' :: Integer
      , loanTerm' :: POSIXTime
      , loanInterest' :: Rational
      , loanBacking' :: Integer
      , collateralRates' :: [((CurrencySymbol,TokenName),Rational)]
      , loanExpiration' :: POSIXTime
      , loanOutstanding' :: Rational
      }
  deriving (Generic,ToJSON,FromJSON)

convert2LoanDatum :: LoanDatum' -> LoanDatum
convert2LoanDatum (AskDatum' a b c d e f) = AskDatum a b c d e f
convert2LoanDatum (OfferDatum' a b c d e f g h) = OfferDatum a b c d e f g h
convert2LoanDatum (ActiveDatum' a b c d e f g h i j k) = ActiveDatum a b c d e f g h i j k

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

data OfferParams = OfferParams
  { offerBeaconsMinted :: [(TokenName,Integer)]
  , offerBeaconRedeemer :: BeaconRedeemer'
  , offerBeaconPolicy :: MintingPolicy
  , offerAddress :: Address
  , offerInfo :: [(Maybe LoanDatum', Value)]
  , offerAsInline :: Bool
  } deriving (Generic,ToJSON,FromJSON)

data AcceptParams = AcceptParams
  { acceptBeaconsMinted :: [(TokenName,Integer)]
  , acceptBeaconRedeemer :: BeaconRedeemer'
  , acceptBeaconPolicy :: MintingPolicy
  , acceptLoanVal :: Validator
  , acceptLoanAddress :: Address
  , acceptSpecificUTxOs :: [(LoanDatum',Value)]
  , acceptChangeAddress :: Address
  , acceptChangeOutput :: [(Maybe LoanDatum',Value)]
  , acceptDatumAsInline :: Bool
  , acceptWithTTL :: Bool
  } deriving (Generic,ToJSON,FromJSON)

data CloseAskParams = CloseAskParams
  { closeAskBeaconsBurned :: [(TokenName,Integer)]
  , closeAskBeaconRedeemer :: BeaconRedeemer'
  , closeAskBeaconPolicy :: MintingPolicy
  , closeAskLoanVal :: Validator
  , closeAskLoanAddress :: Address
  , closeAskSpecificUTxOs :: [(LoanDatum',Value)]
  } deriving (Generic,ToJSON,FromJSON)

data CloseOfferParams = CloseOfferParams
  { closeOfferBeaconsBurned :: [(TokenName,Integer)]
  , closeOfferBeaconRedeemer :: BeaconRedeemer'
  , closeOfferBeaconPolicy :: MintingPolicy
  , closeOfferLoanVal :: Validator
  , closeOfferLoanAddress :: Address
  , closeOfferSpecificUTxOs :: [(LoanDatum',Value)]
  } deriving (Generic,ToJSON,FromJSON)

data RepayLoanParams = RepayLoanParams
  { repayLoanBeaconsBurned :: [(TokenName,Integer)]
  , repayLoanBeaconRedeemer :: BeaconRedeemer'
  , repayLoanBeaconPolicy :: MintingPolicy
  , repayLoanVal :: Validator
  , repayLoanAddress :: Address
  , repayLoanSpecificUTxOs :: [(LoanDatum',Value)]
  , repayLoanChangeAddress :: Address
  , repayLoanChangeOutputs :: [(Maybe LoanDatum',Value)]
  , repayLoanDatumAsInline :: Bool
  , repayLoanWithTTE :: Bool
  , repayLoanOtherMint :: [(TokenName,Integer)]
  , repayLoanOtherMintPolicy :: MintingPolicy
  } deriving (Generic,ToJSON,FromJSON)

type TraceSchema =
      Endpoint "ask" AskParams
  .\/ Endpoint "offer" OfferParams
  .\/ Endpoint "accept" AcceptParams
  .\/ Endpoint "close-ask" CloseAskParams
  .\/ Endpoint "close-offer" CloseOfferParams
  .\/ Endpoint "repay-loan" RepayLoanParams

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
         <> singleton beaconPolicySymbol "Offer" 5
         <> singleton beaconPolicySymbol "Ask" 5
    
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

offerLoan :: OfferParams -> Contract () TraceSchema Text ()
offerLoan OfferParams{..} = do
  userPubKeyHash <- ownFirstPaymentPubKeyHash
  let beaconPolicyHash = mintingPolicyHash offerBeaconPolicy
      beaconRedeemer = toRedeemer $ convert2BeaconRedeemer offerBeaconRedeemer
      
      toDatum'
        | offerAsInline = TxOutDatumInline . toDatum . convert2LoanDatum
        | otherwise = TxOutDatumHash . toDatum . convert2LoanDatum
      
      lookups = plutusV2MintingPolicy offerBeaconPolicy
      
      tx' =
        -- | Mint Beacons
        (foldl' 
          (\acc (t,i) -> acc <> mustMintCurrencyWithRedeemer beaconPolicyHash beaconRedeemer t i) 
          mempty
          offerBeaconsMinted
        )
        -- | Add offer
        <> (foldl'
              (\acc (d,v) -> acc <> mustPayToAddressWith offerAddress (fmap toDatum' d) v)
              mempty
              offerInfo
           )
        -- | Must be signed by receiving pubkey
        <> mustBeSignedBy userPubKeyHash
  
  ledgerTx <- submitTxConstraintsWith @Void lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String "Offered a loan"

acceptLoan :: AcceptParams -> Contract () TraceSchema Text ()
acceptLoan AcceptParams{..} = do
  offerUtxos <- utxosAt acceptLoanAddress
  (start,_) <- currentNodeClientTimeRange
  userPubKeyHash <- ownFirstPaymentPubKeyHash

  let beaconPolicyHash = mintingPolicyHash acceptBeaconPolicy
      beaconRedeemer = toRedeemer $ convert2BeaconRedeemer acceptBeaconRedeemer
      
      toDatum'
        | acceptDatumAsInline = TxOutDatumInline . toDatum . convert2LoanDatum
        | otherwise = TxOutDatumHash . toDatum . convert2LoanDatum

      acceptRedeemer = toRedeemer AcceptOffer

      lookups = Constraints.unspentOutputs offerUtxos
             <> plutusV2OtherScript acceptLoanVal
             <> plutusV2MintingPolicy acceptBeaconPolicy
      
      tx' =
        -- | Mint/burn Beacons
        (foldl' 
          (\acc (t,i) -> acc <> mustMintCurrencyWithRedeemer beaconPolicyHash beaconRedeemer t i) 
          mempty
          acceptBeaconsMinted
        )
        -- | Must spend all utxos to be accepted
        <> foldl' (\a (d,v) -> 
                      a <>
                      mustSpendScriptOutputWithMatchingDatumAndValue 
                        (UScripts.validatorHash acceptLoanVal) 
                        (== toDatum (convert2LoanDatum d))
                        (==v) 
                        acceptRedeemer
                  ) 
                  mempty 
                  acceptSpecificUTxOs
        -- | Return change to loan address
        <> (foldl'
              (\acc (d,v) -> acc <> mustPayToAddressWith acceptChangeAddress (fmap toDatum' d) v)
              mempty
              acceptChangeOutput
           )
        -- | Must tell script current time
        <> (if acceptWithTTL
            then mustValidateIn (from start)
            else mempty)
        -- | Must be signed by borrower
        <> mustBeSignedBy userPubKeyHash

  ledgerTx <- submitTxConstraintsWith @Void lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String "Loan accepted"

closeAsk :: CloseAskParams -> Contract () TraceSchema Text ()
closeAsk CloseAskParams{..} = do
  userPubKeyHash <- ownFirstPaymentPubKeyHash
  askUtxos <- utxosAt closeAskLoanAddress

  let beaconPolicyHash = mintingPolicyHash closeAskBeaconPolicy
      beaconRedeemer = toRedeemer $ convert2BeaconRedeemer closeAskBeaconRedeemer

      closeRedeemer = toRedeemer CloseAsk
      
      lookups = plutusV2MintingPolicy closeAskBeaconPolicy
             <> plutusV2OtherScript closeAskLoanVal
             <> Constraints.unspentOutputs askUtxos
      
      tx' =
        -- | Burn Beacons
        (foldl' 
          (\acc (t,i) -> acc <> mustMintCurrencyWithRedeemer beaconPolicyHash beaconRedeemer t i) 
          mempty
          closeAskBeaconsBurned
        )
        -- | Must spend all utxos to be closed
        <> foldl' (\a (d,v) -> 
                      a <>
                      mustSpendScriptOutputWithMatchingDatumAndValue 
                        (UScripts.validatorHash closeAskLoanVal) 
                        (== toDatum (convert2LoanDatum d))
                        (==v) 
                        closeRedeemer
                  ) 
                  mempty 
                  closeAskSpecificUTxOs
        -- | Must be signed by receiving pubkey
        <> mustBeSignedBy userPubKeyHash
  
  ledgerTx <- submitTxConstraintsWith @Void lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String "Closed an ask"

closeOffer :: CloseOfferParams -> Contract () TraceSchema Text ()
closeOffer CloseOfferParams{..} = do
  userPubKeyHash <- ownFirstPaymentPubKeyHash
  offerUtxos <- utxosAt closeOfferLoanAddress

  let beaconPolicyHash = mintingPolicyHash closeOfferBeaconPolicy
      beaconRedeemer = toRedeemer $ convert2BeaconRedeemer closeOfferBeaconRedeemer

      closeRedeemer = toRedeemer CloseOffer
      
      lookups = plutusV2MintingPolicy closeOfferBeaconPolicy
             <> plutusV2OtherScript closeOfferLoanVal
             <> Constraints.unspentOutputs offerUtxos
      
      tx' =
        -- | Burn Beacons
        (foldl' 
          (\acc (t,i) -> acc <> mustMintCurrencyWithRedeemer beaconPolicyHash beaconRedeemer t i) 
          mempty
          closeOfferBeaconsBurned
        )
        -- | Must spend all utxos to be closed
        <> foldl' (\a (d,v) -> 
                      a <>
                      mustSpendScriptOutputWithMatchingDatumAndValue 
                        (UScripts.validatorHash closeOfferLoanVal) 
                        (== toDatum (convert2LoanDatum d))
                        (==v) 
                        closeRedeemer
                  ) 
                  mempty 
                  closeOfferSpecificUTxOs
        -- | Must be signed by receiving pubkey
        <> mustBeSignedBy userPubKeyHash
  
  ledgerTx <- submitTxConstraintsWith @Void lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String "Closed an offer"

repayLoan :: RepayLoanParams -> Contract () TraceSchema Text ()
repayLoan RepayLoanParams{..} = do
  loanUtxos <- utxosAt repayLoanAddress
  (_,end) <- currentNodeClientTimeRange
  userPubKeyHash <- ownFirstPaymentPubKeyHash

  let beaconPolicyHash = mintingPolicyHash repayLoanBeaconPolicy
      beaconRedeemer = toRedeemer $ convert2BeaconRedeemer repayLoanBeaconRedeemer
      
      toDatum'
        | repayLoanDatumAsInline = TxOutDatumInline . toDatum . convert2LoanDatum
        | otherwise = TxOutDatumHash . toDatum . convert2LoanDatum

      repayRedeemer = toRedeemer RepayLoan

      otherMintPolicyHash = mintingPolicyHash repayLoanOtherMintPolicy
      otherMintRedeemer = toRedeemer ()

      lookups = Constraints.unspentOutputs loanUtxos
             <> plutusV2OtherScript repayLoanVal
             <> plutusV2MintingPolicy repayLoanBeaconPolicy
             <> plutusV2MintingPolicy repayLoanOtherMintPolicy
      
      tx' =
        -- | Burn Beacons
        (foldl' 
          (\acc (t,i) -> acc <> mustMintCurrencyWithRedeemer beaconPolicyHash beaconRedeemer t i) 
          mempty
          repayLoanBeaconsBurned
        )
        -- | Must spend all utxos to be repaid
        <> foldl' (\a (d,v) -> 
                      a <>
                      mustSpendScriptOutputWithMatchingDatumAndValue 
                        (UScripts.validatorHash repayLoanVal) 
                        (== toDatum (convert2LoanDatum d))
                        (==v) 
                        repayRedeemer
                  ) 
                  mempty 
                  repayLoanSpecificUTxOs
        -- | Return change to loan address
        <> (foldl'
              (\acc (d,v) -> acc <> mustPayToAddressWith repayLoanChangeAddress (fmap toDatum' d) v)
              mempty
              repayLoanChangeOutputs
           )
        -- | Must tell script current time
        <> (if repayLoanWithTTE
            then mustValidateIn (to end)
            else mempty)
        -- | Must be signed by borrower
        <> mustBeSignedBy userPubKeyHash
        -- | Burn Beacons
        <> (foldl' 
              (\acc (t,i) -> acc <> mustMintCurrencyWithRedeemer otherMintPolicyHash otherMintRedeemer t i) 
              mempty
              repayLoanOtherMint
           )

  ledgerTx <- submitTxConstraintsWith @Void lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String "Made loan payment"

-------------------------------------------------
-- Endpoints
-------------------------------------------------
endpoints :: Contract () TraceSchema Text ()
endpoints = selectList choices >> endpoints
  where
    askForLoan' = endpoint @"ask" askForLoan
    offerLoan' = endpoint @"offer" offerLoan
    acceptLoan' = endpoint @"accept" acceptLoan
    closeAsk' = endpoint @"close-ask" closeAsk
    closeOffer' = endpoint @"close-offer" closeOffer
    repayLoan' = endpoint @"repay-loan" repayLoan
    choices = 
      [ askForLoan'
      , offerLoan'
      , acceptLoan'
      , closeAsk'
      , closeOffer'
      , repayLoan'
      ]