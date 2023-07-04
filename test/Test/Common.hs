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

{-# OPTIONS_GHC -Wno-orphans -Wno-incomplete-patterns #-}

module Test.Common where

import qualified Data.Map as Map
import Control.Lens hiding (from,index,to)
import Data.Default
import Data.Void (Void)
import Control.Monad (void)
import GHC.Generics (Generic)
import Data.Text (Text)
import Ledger hiding (singleton,mintingPolicyHash,Value,lovelaceValueOf,from,validatorHash)
import Ledger.Tx.Constraints as Constraints
import qualified Ledger.Tx.Constraints.TxConstraints as Constraints
import Ledger.Tx.Constraints.TxConstraints (TxOutDatum(..),mustMintCurrencyWithRedeemerAndReference)
import Plutus.Contract
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), foldMap)
import Plutus.Script.Utils.Value
import Plutus.Script.Utils.Ada (lovelaceValueOf)
import Plutus.Trace
import Wallet.Emulator.Wallet
import Data.List (foldl')
import Prelude as Haskell (Semigroup (..), String)
import Cardano.Api.Shelley (ProtocolParameters (..))
import qualified Cardano.Api as C
import Cardano.Api hiding (TxOutDatum(..),TxOutDatumInline,TxOutDatumHash,Address,TxId,Value)
import Cardano.Node.Emulator.Params
import Ledger.Tx.CardanoAPI.Internal
import Ledger.Tx.Internal as I
import Plutus.Script.Utils.V2.Generators (alwaysSucceedValidator)
import Plutus.Script.Utils.V2.Scripts
import Ledger.Tx.Constraints.ValidityInterval

import CardanoLoans

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
txOutRefToLoanId :: TxOutRef -> TokenName
txOutRefToLoanId (TxOutRef (TxId txid) _) = TokenName txid

txOutRefWithValue :: Value -> EmulatorTrace TxOutRef
txOutRefWithValue value' = do
  state <- chainState
  let xs = Map.toList $ getIndex (state ^. index)
      findTxId v ((ref,o):ys)
        | fromCardanoValue (I.txOutValue o) == v = ref
        | otherwise = findTxId v ys
  return $ findTxId value' xs

txOutRefWithValueAndDatum :: PlutusTx.ToData a => Value -> a -> EmulatorTrace TxOutRef
txOutRefWithValueAndDatum value' datum = do
  state <- chainState
  let xs = Map.toList $ getIndex (state ^. index)
      dHash = datumHash $ toDatum datum
      findTxId v dh ((ref,o):ys)
        | fromCardanoValue (I.txOutValue o) == v = 
            case I.txOutDatumHash o of
              Just d' -> if d' == dh then ref else findTxId v dh ys
              Nothing -> findTxId v dh ys
        | otherwise = findTxId v dh ys
  return $ findTxId value' dHash xs

toRedeemer :: PlutusTx.ToData a => a -> Redeemer
toRedeemer = Redeemer . PlutusTx.dataToBuiltinData . PlutusTx.toData

toDatum :: PlutusTx.ToData a => a -> Datum
toDatum = Datum . PlutusTx.dataToBuiltinData . PlutusTx.toData

unValidatorHash :: ValidatorHash -> Ledger.ScriptHash
unValidatorHash (ValidatorHash h) = ScriptHash h

unMintingPolicyHash :: MintingPolicyHash -> Ledger.ScriptHash
unMintingPolicyHash (MintingPolicyHash h) = ScriptHash h

mustPayToAddressWith :: Address -> Maybe (TxOutDatum Datum) -> Maybe (Ledger.ScriptHash) -> Value -> TxConstraints i o
mustPayToAddressWith addr maybeDatum maybeScript val =
  Constraints.singleton $ MustPayToAddress addr maybeDatum maybeScript val

instance ToJSON LoanDatum
instance FromJSON LoanDatum

instance ToJSON LoanRedeemer
instance FromJSON LoanRedeemer

instance ToJSON BeaconRedeemer
instance FromJSON BeaconRedeemer

instance ToJSON DappScripts
instance FromJSON DappScripts

instance ToJSON PaymentDatum
instance FromJSON PaymentDatum

-------------------------------------------------
-- Params
-------------------------------------------------
data CreateReferenceScriptParams = CreateReferenceScriptParams
  { createReferenceScriptScript :: Ledger.Script
  , createReferenceScriptAddress :: Address
  , createReferenceScriptUTxO :: Value
  } deriving (Generic,ToJSON,FromJSON)

data CreateAskParams = CreateAskParams
  { createAskBeaconsMinted :: [(TokenName,Integer)]
  , createAskBeaconRedeemer :: BeaconRedeemer
  , createAskLoanAddress :: Address
  , createAskUTxOs :: [(Maybe LoanDatum, Value)]
  , createAskAsInline :: Bool
  , createAskScripts :: DappScripts
  , createAskWithRefScript :: Bool
  , createAskRefScript :: TxOutRef
  , createAskRefAddress :: Address
  } deriving (Generic,ToJSON,FromJSON)

data CloseAskParams = CloseAskParams
  { closeAskBeaconsBurned :: [(TokenName,Integer)]
  , closeAskBeaconRedeemer :: BeaconRedeemer
  , closeAskLoanAddress :: Address
  , closeAskUTxOs :: [TxOutRef]
  , closeAskScripts :: DappScripts
  , closeAskWithRefScripts :: Bool
  , closeAskSpendRefScript :: TxOutRef
  , closeAskMintRefScript :: TxOutRef
  , closeAskRefAddress :: Address
  } deriving (Generic,ToJSON,FromJSON)

data CreateOfferParams = CreateOfferParams
  { createOfferBeaconsMinted :: [(TokenName,Integer)]
  , createOfferBeaconRedeemer :: BeaconRedeemer
  , createOfferLoanAddress :: Address
  , createOfferUTxOs :: [(Maybe LoanDatum, Value)]
  , createOfferAsInline :: Bool
  , createOfferScripts :: DappScripts
  , createOfferWithRefScript :: Bool
  , createOfferRefScript :: TxOutRef
  , createOfferRefAddress :: Address
  } deriving (Generic,ToJSON,FromJSON)

data CloseOfferParams = CloseOfferParams
  { closeOfferBeaconsBurned :: [(TokenName,Integer)]
  , closeOfferBeaconRedeemer :: BeaconRedeemer
  , closeOfferLoanAddress :: Address
  , closeOfferUTxOs :: [TxOutRef]
  , closeOfferScripts :: DappScripts
  , closeOfferWithRefScripts :: Bool
  , closeOfferSpendRefScript :: TxOutRef
  , closeOfferMintRefScript :: TxOutRef
  , closeOfferRefAddress :: Address
  } deriving (Generic,ToJSON,FromJSON)

data AcceptOfferParams = AcceptOfferParams
  { acceptOfferBeaconsMinted :: [[(TokenName,Integer)]]
  , acceptOfferBeaconRedeemers :: [BeaconRedeemer]
  , acceptOfferLoanAddresses :: [Address]
  , acceptOfferLoanRedeemers :: [LoanRedeemer]
  , acceptOfferUTxOs :: [[TxOutRef]]
  , acceptOfferCollateralAddresses :: [Address]
  , acceptOfferCollateralUTxOs :: [[(Maybe LoanDatum,Value)]]
  , acceptOfferCollateralAsInline :: Bool
  , acceptOfferLenderAddresses :: [Address]
  , acceptOfferLenderUTxOs :: [[(Maybe TxOutRef,Value)]]
  , acceptOfferLenderAsInline :: Bool
  , acceptOfferWithTTL :: Bool
  , acceptOfferScripts :: DappScripts
  , acceptOfferWithRefScripts :: Bool
  , acceptOfferSpendRefScript :: TxOutRef
  , acceptOfferMintRefScript :: TxOutRef
  , acceptOfferRefAddress :: Address
  } deriving (Generic,ToJSON,FromJSON)

data MakePaymentParams = MakePaymentParams
  { makePaymentBeaconsMinted :: [(TokenName,Integer)]
  , makePaymentBeaconRedeemer :: BeaconRedeemer
  , makePaymentLoanAddress :: Address
  , makePaymentLoanRedeemer :: LoanRedeemer
  , makePaymentUTxOs :: [TxOutRef]
  , makePaymentCollateralAddress :: Address
  , makePaymentCollateralUTxOs :: [(Maybe LoanDatum,Value)]
  , makePaymentCollateralAsInline :: Bool
  , makePaymentLenderAddresses :: [Address]
  , makePaymentLenderUTxOs :: [[(Maybe PaymentDatum,Value)]]
  , makePaymentLenderAsInline :: Bool
  , makePaymentWithTTE :: Bool
  , makePaymentScripts :: DappScripts
  , makePaymentWithRefScripts :: Bool
  , makePaymentSpendRefScript :: TxOutRef
  , makePaymentMintRefScript :: TxOutRef
  , makePaymentRefAddress :: Address
  } deriving (Generic,ToJSON,FromJSON)

data RolloverParams = RolloverParams
  { rolloverLoanAddress :: Address
  , rolloverRedeemer :: LoanRedeemer
  , rolloverInputs :: [TxOutRef]
  , rolloverOutputAddress :: Address
  , rolloverOutputs :: [(Maybe LoanDatum,Value)]
  , rolloverOutputsAsInline :: Bool
  , rolloverWithTTE :: Bool
  , rolloverScripts :: DappScripts
  , rolloverWithRefScripts :: Bool
  , rolloverSpendRefScript :: TxOutRef
  , rolloverMintRefScript :: TxOutRef
  , rolloverRefAddress :: Address
  } deriving (Generic,ToJSON,FromJSON)

data ClaimExpiredParams = ClaimExpiredParams
  { claimExpiredBeaconsBurned :: [(TokenName,Integer)]
  , claimExpiredBeaconRedeemer :: BeaconRedeemer
  , claimExpiredLoanAddress :: Address
  , claimExpiredRedeemer :: LoanRedeemer
  , claimExpiredInputs :: [TxOutRef]
  , claimExpiredWithTTL :: Bool
  , claimExpiredScripts :: DappScripts
  , claimExpiredWithRefScripts :: Bool
  , claimExpiredSpendRefScript :: TxOutRef
  , claimExpiredMintRefScript :: TxOutRef
  , claimExpiredRefAddress :: Address
  } deriving (Generic,ToJSON,FromJSON)

data UpdateAddressParams = UpdateAddressParams
  { updateAddressLoanAddress :: Address
  , updateAddressRedeemer :: LoanRedeemer
  , updateAddressInputs :: [TxOutRef]
  , updateAddressKeyAddress :: Address
  , updateAddressLoanId :: TokenName
  , updateAddressIncludeKeyNFT :: Bool
  , updateAddressOutputAddress :: Address
  , updateAddressOutputs :: [(Maybe LoanDatum,Value)]
  , updateAddressAsInline :: Bool
  , updateAddressScripts :: DappScripts
  , updateAddressWithRefScripts :: Bool
  , updateAddressSpendRefScript :: TxOutRef
  , updateAddressMintRefScript :: TxOutRef
  , updateAddressRefAddress :: Address
  } deriving (Generic,ToJSON,FromJSON)

data ClaimLostParams = ClaimLostParams
  { claimLostBeaconsBurned :: [(TokenName,Integer)]
  , claimLostBeaconRedeemer :: BeaconRedeemer
  , claimLostLoanAddress :: Address
  , claimLostRedeemer :: LoanRedeemer
  , claimLostInputs :: [TxOutRef]
  , claimLostWithTTL :: Bool
  , claimLostScripts :: DappScripts
  , claimLostWithRefScripts :: Bool
  , claimLostSpendRefScript :: TxOutRef
  , claimLostMintRefScript :: TxOutRef
  , claimLostRefAddress :: Address
  } deriving (Generic,ToJSON,FromJSON)

type TraceSchema =
      Endpoint "create-reference-script" CreateReferenceScriptParams
  .\/ Endpoint "create-ask" CreateAskParams
  .\/ Endpoint "close-ask" CloseAskParams
  .\/ Endpoint "create-offer" CreateOfferParams
  .\/ Endpoint "close-offer" CloseOfferParams
  .\/ Endpoint "accept-offer" AcceptOfferParams
  .\/ Endpoint "make-payment" MakePaymentParams
  .\/ Endpoint "rollover" RolloverParams
  .\/ Endpoint "claim-expired" ClaimExpiredParams
  .\/ Endpoint "update-address" UpdateAddressParams
  .\/ Endpoint "claim-lost" ClaimLostParams

-------------------------------------------------
-- Configs
-------------------------------------------------
minUTxOSpendRef :: Integer
minUTxOSpendRef = 57_000_000

minUTxOMintRef :: Integer
minUTxOMintRef = 65_000_000

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
    user2 = unsafeFromRight . toCardanoValue $ lovelaceValueOf 1_000_000_000
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
    
    user3 :: C.Value
    user3 = unsafeFromRight . toCardanoValue $ lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 1000
    
    user4 :: C.Value
    user4 = unsafeFromRight . toCardanoValue $ lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 1000
    
    user5 :: C.Value
    user5 = unsafeFromRight . toCardanoValue $ lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 1000

    user6 :: C.Value
    user6 = unsafeFromRight . toCardanoValue $ lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 1000

    user7 :: C.Value
    user7 = unsafeFromRight . toCardanoValue $ lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 1000
    
    user8 :: C.Value
    user8 = unsafeFromRight . toCardanoValue $ lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 1000
    
    user9 :: C.Value
    user9 = unsafeFromRight . toCardanoValue $ lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 1000

    user10 :: C.Value
    user10 = unsafeFromRight . toCardanoValue $ lovelaceValueOf 1_000_000_000
         <> (uncurry singleton testToken1) 1000

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
    protoParams = def{ protocolParamMaxTxExUnits = Just (ExecutionUnits {executionSteps = 10000000000
                                                                        ,executionMemory = 13000000})
                    --  , protocolParamMaxTxSize = 12300
                     }

-------------------------------------------------
-- Trace Models
-------------------------------------------------
createReferenceScript :: CreateReferenceScriptParams -> Contract () TraceSchema Text ()
createReferenceScript CreateReferenceScriptParams{..} = do
  let d = Just $ TxOutDatumInline $ toDatum ()

      val = Validator $ createReferenceScriptScript

      refScript = unValidatorHash $ validatorHash val
      
      lookups = plutusV2OtherScript val

      tx' =
        mustPayToAddressWith createReferenceScriptAddress d (Just refScript) createReferenceScriptUTxO

  ledgerTx <- submitTxConstraintsWith @Void lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String "Reference script created"

createAsk :: CreateAskParams -> Contract () TraceSchema Text ()
createAsk CreateAskParams{createAskScripts=DappScripts{..},..} = do
  refUTxOs <- utxosAt $ unsafeFromRight $ toCardanoAddressInEra Mainnet createAskRefAddress
  userPubKeyHash <- ownFirstPaymentPubKeyHash
  let beaconRedeemer = toRedeemer createAskBeaconRedeemer
      
      toDatum'
        | createAskAsInline = TxOutDatumInline . toDatum
        | otherwise = TxOutDatumHash . toDatum

      lookups = plutusV2MintingPolicy beaconPolicy
             <> Constraints.unspentOutputs refUTxOs

      tx' =
        (if createAskWithRefScript then
          -- | Must reference scripts
          mustReferenceOutput createAskRefScript
          -- | Mint beacons
          <> foldl' 
                (\acc (t,i) -> acc <>
                  mustMintCurrencyWithRedeemerAndReference 
                    (Just createAskRefScript) 
                    beaconPolicyHash 
                    beaconRedeemer 
                    t 
                    i
                )
                mempty
                createAskBeaconsMinted
        else
          foldl'
            (\acc (t,i) -> acc <>
              mustMintCurrencyWithRedeemer beaconPolicyHash beaconRedeemer t i
            ) 
            mempty
            createAskBeaconsMinted
        )
        -- | Create Ask UTxOs
        <> (foldl'
              (\acc (d,v) -> acc <>
                mustPayToAddressWith createAskLoanAddress (fmap toDatum' d) Nothing v
              )
              mempty
              createAskUTxOs
           )
        -- | Must be signed by receiving pubkey
        <> mustBeSignedBy userPubKeyHash
  
  ledgerTx <- submitTxConstraintsWith @Void lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String "Ask(s) created"

closeAsk :: CloseAskParams -> Contract () TraceSchema Text ()
closeAsk CloseAskParams{closeAskScripts=DappScripts{..},..} = do
  refUTxOs <- utxosAt $ unsafeFromRight $ toCardanoAddressInEra Mainnet closeAskRefAddress
  loanUTxOs <- utxosAt $ unsafeFromRight $ toCardanoAddressInEra Mainnet closeAskLoanAddress
  userPubKeyHash <- ownFirstPaymentPubKeyHash

  let beaconRedeemer = toRedeemer closeAskBeaconRedeemer
      loanRedeemer = toRedeemer CloseAsk

      lookups = plutusV2MintingPolicy beaconPolicy
             <> plutusV2OtherScript spendingValidator
             <> Constraints.unspentOutputs refUTxOs
             <> Constraints.unspentOutputs loanUTxOs

      tx' =
        -- | Must be signed by borrower
        mustBeSignedBy userPubKeyHash

        <> if closeAskWithRefScripts then
          -- | Must reference scripts
          (mustReferenceOutput closeAskSpendRefScript <> mustReferenceOutput closeAskMintRefScript)

          -- | Burn beacons
          <> (foldl' 
                (\acc (t,i) -> acc <>
                  mustMintCurrencyWithRedeemerAndReference 
                    (Just closeAskMintRefScript) 
                    beaconPolicyHash 
                    beaconRedeemer 
                    t 
                    i
                )
                mempty
                closeAskBeaconsBurned
             )
          -- | Spend UTxOs to close
          <> (foldl' 
                (\acc i -> acc <>
                   mustSpendScriptOutputWithReference
                     i
                     loanRedeemer
                     closeAskSpendRefScript
                )
                mempty
                closeAskUTxOs
             )
        else
          -- | Burn beacons
          (foldl'
              (\acc (t,i) -> acc <>
                mustMintCurrencyWithRedeemer beaconPolicyHash beaconRedeemer t i
              ) 
              mempty
              closeAskBeaconsBurned
          )
          -- | Spend UTxOs to close
          <> (foldl' 
                (\acc i -> acc <>
                   mustSpendScriptOutput
                     i
                     loanRedeemer
                )
                mempty
                closeAskUTxOs
             )

  ledgerTx <- submitTxConstraintsWith @Void lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String "Ask(s) closed"

createOffer :: CreateOfferParams -> Contract () TraceSchema Text ()
createOffer CreateOfferParams{createOfferScripts=DappScripts{..},..} = do
  refUTxOs <- utxosAt $ unsafeFromRight $ toCardanoAddressInEra Mainnet createOfferRefAddress
  userPubKeyHash <- ownFirstPaymentPubKeyHash
  let beaconRedeemer = toRedeemer createOfferBeaconRedeemer
      
      toDatum'
        | createOfferAsInline = TxOutDatumInline . toDatum
        | otherwise = TxOutDatumHash . toDatum

      lookups = plutusV2MintingPolicy beaconPolicy
             <> Constraints.unspentOutputs refUTxOs

      tx' =
        (if createOfferWithRefScript then
          -- | Must reference scripts
          mustReferenceOutput createOfferRefScript
          -- | Mint beacons
          <> foldl' 
                (\acc (t,i) -> acc <>
                  mustMintCurrencyWithRedeemerAndReference 
                    (Just createOfferRefScript) 
                    beaconPolicyHash 
                    beaconRedeemer 
                    t 
                    i
                )
                mempty
                createOfferBeaconsMinted
        else
          foldl'
            (\acc (t,i) -> acc <>
              mustMintCurrencyWithRedeemer beaconPolicyHash beaconRedeemer t i
            ) 
            mempty
            createOfferBeaconsMinted
        )
        -- | Create Offer UTxOs
        <> (foldl'
              (\acc (d,v) -> acc <>
                mustPayToAddressWith createOfferLoanAddress (fmap toDatum' d) Nothing v
              )
              mempty
              createOfferUTxOs
           )
        -- | Must be signed by lender
        <> mustBeSignedBy userPubKeyHash
  
  ledgerTx <- submitTxConstraintsWith @Void lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String "Offer created"

closeOffer :: CloseOfferParams -> Contract () TraceSchema Text ()
closeOffer CloseOfferParams{closeOfferScripts=DappScripts{..},..} = do
  refUTxOs <- utxosAt $ unsafeFromRight $ toCardanoAddressInEra Mainnet closeOfferRefAddress
  loanUTxOs <- utxosAt $ unsafeFromRight $ toCardanoAddressInEra Mainnet closeOfferLoanAddress
  userPubKeyHash <- ownFirstPaymentPubKeyHash

  let beaconRedeemer = toRedeemer closeOfferBeaconRedeemer
      loanRedeemer = toRedeemer CloseOffer

      lookups = plutusV2MintingPolicy beaconPolicy
             <> plutusV2OtherScript spendingValidator
             <> Constraints.unspentOutputs refUTxOs
             <> Constraints.unspentOutputs loanUTxOs

      tx' =
        -- | Must be signed by lender
        mustBeSignedBy userPubKeyHash

        <> if closeOfferWithRefScripts then
          -- | Must reference scripts
          (mustReferenceOutput closeOfferSpendRefScript <> mustReferenceOutput closeOfferMintRefScript)

          -- | Burn beacons
          <> (foldl' 
                (\acc (t,i) -> acc <>
                  mustMintCurrencyWithRedeemerAndReference 
                    (Just closeOfferMintRefScript) 
                    beaconPolicyHash 
                    beaconRedeemer 
                    t 
                    i
                )
                mempty
                closeOfferBeaconsBurned
             )
          -- | Spend UTxOs to close
          <> (foldl' 
                (\acc i -> acc <>
                   mustSpendScriptOutputWithReference
                     i
                     loanRedeemer
                     closeOfferSpendRefScript
                )
                mempty
                closeOfferUTxOs
             )
        else
          -- | Burn beacons
          (foldl'
              (\acc (t,i) -> acc <>
                mustMintCurrencyWithRedeemer beaconPolicyHash beaconRedeemer t i
              ) 
              mempty
              closeOfferBeaconsBurned
          )
          -- | Spend UTxOs to close
          <> (foldl' 
                (\acc i -> acc <>
                   mustSpendScriptOutput
                     i
                     loanRedeemer
                )
                mempty
                closeOfferUTxOs
             )

  ledgerTx <- submitTxConstraintsWith @Void lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String "Offer(s) closed"

acceptOffer :: AcceptOfferParams -> Contract () TraceSchema Text ()
acceptOffer AcceptOfferParams{acceptOfferScripts=DappScripts{..},..} = do
  refUTxOs <- utxosAt $ unsafeFromRight $ toCardanoAddressInEra Mainnet acceptOfferRefAddress
  loanUTxOs <- Map.unions 
    <$> mapM (utxosAt . unsafeFromRight . toCardanoAddressInEra Mainnet) acceptOfferLoanAddresses
  userPubKeyHash <- ownFirstPaymentPubKeyHash
  (start,_) <- currentNodeClientTimeRange

  let beaconRedeemers = map toRedeemer acceptOfferBeaconRedeemers
      loanRedeemers = map toRedeemer acceptOfferLoanRedeemers

      toCollateralDatum'
        | acceptOfferCollateralAsInline = TxOutDatumInline . toDatum
        | otherwise = TxOutDatumHash . toDatum
      
      toLenderDatum'
        | acceptOfferLenderAsInline = TxOutDatumInline . toDatum
        | otherwise = TxOutDatumHash . toDatum

      lookups = plutusV2MintingPolicy beaconPolicy
             <> plutusV2OtherScript spendingValidator
             <> Constraints.unspentOutputs refUTxOs
             <> Constraints.unspentOutputs loanUTxOs

      tx' =
        -- | Must be signed by borrower
        mustBeSignedBy userPubKeyHash

        -- | Must tell script current time
        <> (if acceptOfferWithTTL
            then mustValidateInTimeRange (from start)
            else mempty)

        -- | Post collateral to loan address
        <> (mconcat $ zipWith (\z b -> foldl'
              (\acc (d,v) -> acc <> mustPayToAddressWith z (fmap toCollateralDatum' d) Nothing v)
              mempty
              b)
              acceptOfferCollateralAddresses
              acceptOfferCollateralUTxOs
          )
        -- | Pay the lenders
        <> (mconcat $ zipWith (\z b -> foldl'
              (\acc (d,v) -> acc <> mustPayToAddressWith z (fmap toLenderDatum' d) Nothing v)
              mempty
              b)
              acceptOfferLenderAddresses
              acceptOfferLenderUTxOs
          )

        <> if acceptOfferWithRefScripts then
          -- | Must reference scripts
          (mustReferenceOutput acceptOfferSpendRefScript <> mustReferenceOutput acceptOfferMintRefScript)

          -- | Burn beacons
          <> (mconcat $ zipWith (\b r -> foldl' 
                (\acc (t,i) -> acc <> 
                  mustMintCurrencyWithRedeemerAndReference 
                    (Just acceptOfferMintRefScript)
                    beaconPolicyHash
                    r
                    t 
                    i
                ) 
                mempty
                b)
                acceptOfferBeaconsMinted
                beaconRedeemers
             )
          -- | Must spend all utxos to be accepted
          <> ( mconcat $ zipWith (\r is -> (foldl' (\a i -> 
                        a <>
                        mustSpendScriptOutputWithReference 
                          i
                          r
                          acceptOfferSpendRefScript
                    ) 
                    mempty
                    is))
                    loanRedeemers
                    acceptOfferUTxOs
             )
        else
          -- | Burn beacons
          (mconcat $ zipWith (\b r -> foldl' 
                (\acc (t,i) -> acc <> 
                  mustMintCurrencyWithRedeemer
                    beaconPolicyHash
                    r 
                    t 
                    i
                ) 
                mempty
                b)
                acceptOfferBeaconsMinted
                beaconRedeemers
          )
          -- | Must spend all utxos to be accepted
          <> ( mconcat $ zipWith (\r is -> (foldl' (\a i -> 
                        a <>
                        mustSpendScriptOutput
                          i
                          r
                    ) 
                    mempty
                    is))
                    loanRedeemers
                    acceptOfferUTxOs
             )

  ledgerTx <- submitTxConstraintsWith @Void lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String "Offer(s) accepted"

makePayment :: MakePaymentParams -> Contract () TraceSchema Text ()
makePayment MakePaymentParams{makePaymentScripts=DappScripts{..},..} = do
  refUTxOs <- utxosAt $ unsafeFromRight $ toCardanoAddressInEra Mainnet makePaymentRefAddress
  loanUTxOs <- utxosAt $ unsafeFromRight $ toCardanoAddressInEra Mainnet makePaymentLoanAddress
  userPubKeyHash <- ownFirstPaymentPubKeyHash
  (_,end) <- currentNodeClientTimeRange

  let beaconRedeemer = toRedeemer makePaymentBeaconRedeemer
      loanRedeemer = toRedeemer makePaymentLoanRedeemer

      toCollateralDatum'
        | makePaymentCollateralAsInline = TxOutDatumInline . toDatum
        | otherwise = TxOutDatumHash . toDatum
      
      toLenderDatum'
        | makePaymentLenderAsInline = TxOutDatumInline . toDatum
        | otherwise = TxOutDatumHash . toDatum

      lookups = plutusV2MintingPolicy beaconPolicy
             <> plutusV2OtherScript spendingValidator
             <> Constraints.unspentOutputs refUTxOs
             <> Constraints.unspentOutputs loanUTxOs

      tx' =
        -- | Must be signed by borrower
        mustBeSignedBy userPubKeyHash

        -- | Must tell script current time
        <> (if makePaymentWithTTE
            then mustValidateInTimeRange (lessThan $ 1 + end)
            else mempty)

        -- | Post collateral to loan address
        <> (foldl'
              (\acc (d,v) -> 
                acc <> mustPayToAddressWith 
                        makePaymentCollateralAddress 
                        (fmap toCollateralDatum' d) 
                        Nothing 
                        v
              )
              mempty
              makePaymentCollateralUTxOs
          )
        -- | Pay the lenders
        <> (mconcat $ zipWith (\z b -> foldl'
              (\acc (d,v) -> acc <> mustPayToAddressWith z (fmap toLenderDatum' d) Nothing v)
              mempty
              b)
              makePaymentLenderAddresses
              makePaymentLenderUTxOs
          )

        <> if makePaymentWithRefScripts then
          -- | Must reference scripts
          (mustReferenceOutput makePaymentSpendRefScript <> mustReferenceOutput makePaymentMintRefScript)

          -- | Burn beacons
          <> (foldl' 
                (\acc (t,i) -> acc <> 
                  mustMintCurrencyWithRedeemerAndReference 
                    (Just makePaymentMintRefScript)
                    beaconPolicyHash
                    beaconRedeemer
                    t 
                    i
                ) 
                mempty
                makePaymentBeaconsMinted
             )
          -- | Must spend all utxos to be accepted
          <> (foldl' (\a i -> 
                        a <>
                        mustSpendScriptOutputWithReference 
                          i
                          loanRedeemer
                          makePaymentSpendRefScript
                    ) 
                    mempty
                    makePaymentUTxOs
             )
        else
          -- | Burn beacons
          (foldl' 
                (\acc (t,i) -> acc <> 
                  mustMintCurrencyWithRedeemer
                    beaconPolicyHash
                    beaconRedeemer
                    t 
                    i
                ) 
                mempty
                makePaymentBeaconsMinted
          )
          -- | Must spend all utxos to be accepted
          <> ( foldl' (\a i -> 
                        a <>
                        mustSpendScriptOutput
                          i
                          loanRedeemer
                    ) 
                    mempty
                    makePaymentUTxOs
             )

  ledgerTx <- submitTxConstraintsWith @Void lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String "Payment(s) made"

rollover :: RolloverParams -> Contract () TraceSchema Text ()
rollover RolloverParams{rolloverScripts=DappScripts{..},..} = do
  refUTxOs <- utxosAt $ unsafeFromRight $ toCardanoAddressInEra Mainnet rolloverRefAddress
  loanUTxOs <- utxosAt $ unsafeFromRight $ toCardanoAddressInEra Mainnet rolloverLoanAddress
  userPubKeyHash <- ownFirstPaymentPubKeyHash
  (_,end) <- currentNodeClientTimeRange

  let loanRedeemer = toRedeemer rolloverRedeemer

      toCollateralDatum'
        | rolloverOutputsAsInline = TxOutDatumInline . toDatum
        | otherwise = TxOutDatumHash . toDatum
      
      lookups = plutusV2MintingPolicy beaconPolicy
             <> plutusV2OtherScript spendingValidator
             <> Constraints.unspentOutputs refUTxOs
             <> Constraints.unspentOutputs loanUTxOs

      tx' =
        -- | Must be signed by borrower
        mustBeSignedBy userPubKeyHash

        -- | Must tell script current time
        <> (if rolloverWithTTE
            then mustValidateInTimeRange (lessThan $ 1 + end)
            else mempty)

        -- | Post collateral to loan address
        <> (foldl'
              (\acc (d,v) -> 
                acc <> mustPayToAddressWith 
                        rolloverOutputAddress 
                        (fmap toCollateralDatum' d) 
                        Nothing 
                        v
              )
              mempty
              rolloverOutputs
          )

        <> if rolloverWithRefScripts then
          -- | Must reference scripts
          (mustReferenceOutput rolloverSpendRefScript <> mustReferenceOutput rolloverMintRefScript)

          -- | Must spend all utxos to be accepted
          <> (foldl' (\a i -> 
                        a <>
                        mustSpendScriptOutputWithReference 
                          i
                          loanRedeemer
                          rolloverSpendRefScript
                    ) 
                    mempty
                    rolloverInputs
             )
        else
          -- | Must spend all utxos to be accepted
          ( foldl' (\a i -> 
                      a <>
                      mustSpendScriptOutput
                        i
                        loanRedeemer
                  ) 
                  mempty
                  rolloverInputs
          )

  ledgerTx <- submitTxConstraintsWith @Void lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String "Loan(s) evolved"

claimExpired :: ClaimExpiredParams -> Contract () TraceSchema Text ()
claimExpired ClaimExpiredParams{claimExpiredScripts=DappScripts{..},..} = do
  refUTxOs <- utxosAt $ unsafeFromRight $ toCardanoAddressInEra Mainnet claimExpiredRefAddress
  loanUTxOs <- utxosAt $ unsafeFromRight $ toCardanoAddressInEra Mainnet claimExpiredLoanAddress
  (start,_) <- currentNodeClientTimeRange

  let loanRedeemer = toRedeemer claimExpiredRedeemer
      beaconRedeemer = toRedeemer claimExpiredBeaconRedeemer

      lookups = plutusV2MintingPolicy beaconPolicy
             <> plutusV2OtherScript spendingValidator
             <> Constraints.unspentOutputs refUTxOs
             <> Constraints.unspentOutputs loanUTxOs

      tx' =
        -- | Must tell script current time
        (if claimExpiredWithTTL
         then mustValidateInTimeRange (from start)
         else mempty)

        <> if claimExpiredWithRefScripts then
          -- | Must reference scripts
          (mustReferenceOutput claimExpiredSpendRefScript <> mustReferenceOutput claimExpiredMintRefScript)

          -- | Must spend all utxos to be accepted
          <> (foldl' (\a i -> 
                        a <>
                        mustSpendScriptOutputWithReference 
                          i
                          loanRedeemer
                          claimExpiredSpendRefScript
                    ) 
                    mempty
                    claimExpiredInputs
             )

          -- | Burn beacons
          <> (foldl' 
                (\acc (t,i) -> acc <>
                  mustMintCurrencyWithRedeemerAndReference 
                    (Just claimExpiredMintRefScript) 
                    beaconPolicyHash 
                    beaconRedeemer 
                    t 
                    i
                )
                mempty
                claimExpiredBeaconsBurned
             )
        else
          -- | Must spend all utxos to be accepted
          ( foldl' (\a i -> 
                      a <>
                      mustSpendScriptOutput
                        i
                        loanRedeemer
                  ) 
                  mempty
                  claimExpiredInputs
          )
          -- | Burn beacons
          <> (foldl'
                (\acc (t,i) -> acc <>
                  mustMintCurrencyWithRedeemer beaconPolicyHash beaconRedeemer t i
                ) 
                mempty
                claimExpiredBeaconsBurned
             )

  ledgerTx <- submitTxConstraintsWith @Void lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String "Expired Loan(s) claimed"

updateAddress :: UpdateAddressParams -> Contract () TraceSchema Text ()
updateAddress UpdateAddressParams{updateAddressScripts=DappScripts{..},..} = do
  refUTxOs <- utxosAt $ unsafeFromRight $ toCardanoAddressInEra Mainnet updateAddressRefAddress
  loanUTxOs <- utxosAt $ unsafeFromRight $ toCardanoAddressInEra Mainnet updateAddressLoanAddress

  let loanRedeemer = toRedeemer updateAddressRedeemer

      toCollateralDatum'
        | updateAddressAsInline = TxOutDatumInline . toDatum
        | otherwise = TxOutDatumHash . toDatum
      
      lookups = plutusV2MintingPolicy beaconPolicy
             <> plutusV2OtherScript spendingValidator
             <> Constraints.unspentOutputs refUTxOs
             <> Constraints.unspentOutputs loanUTxOs

      tx' =
        -- | Must include Key NFT among inputs.
        (if updateAddressIncludeKeyNFT then 
          mustPayToAddress 
            updateAddressKeyAddress 
            (lovelaceValueOf 3_000_000 <> singleton beaconCurrencySymbol updateAddressLoanId 1)
        else mempty)

        -- | Post collateral to loan address
        <> (foldl'
              (\acc (d,v) -> 
                acc <> mustPayToAddressWith 
                        updateAddressOutputAddress 
                        (fmap toCollateralDatum' d) 
                        Nothing 
                        v
              )
              mempty
              updateAddressOutputs
          )

        <> if updateAddressWithRefScripts then
          -- | Must reference scripts
          (mustReferenceOutput updateAddressSpendRefScript <> mustReferenceOutput updateAddressMintRefScript)

          -- | Must spend all utxos to be accepted
          <> (foldl' (\a i -> 
                        a <>
                        mustSpendScriptOutputWithReference 
                          i
                          loanRedeemer
                          updateAddressSpendRefScript
                    ) 
                    mempty
                    updateAddressInputs
             )
        else
          -- | Must spend all utxos to be accepted
          ( foldl' (\a i -> 
                      a <>
                      mustSpendScriptOutput
                        i
                        loanRedeemer
                  ) 
                  mempty
                  updateAddressInputs
          )

  ledgerTx <- submitTxConstraintsWith @Void lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String "Lender Address Updated"

claimLost :: ClaimLostParams -> Contract () TraceSchema Text ()
claimLost ClaimLostParams{claimLostScripts=DappScripts{..},..} = do
  refUTxOs <- utxosAt $ unsafeFromRight $ toCardanoAddressInEra Mainnet claimLostRefAddress
  loanUTxOs <- utxosAt $ unsafeFromRight $ toCardanoAddressInEra Mainnet claimLostLoanAddress
  userPubKeyHash <- ownFirstPaymentPubKeyHash
  (start,_) <- currentNodeClientTimeRange

  let loanRedeemer = toRedeemer claimLostRedeemer
      beaconRedeemer = toRedeemer claimLostBeaconRedeemer

      lookups = plutusV2MintingPolicy beaconPolicy
             <> plutusV2OtherScript spendingValidator
             <> Constraints.unspentOutputs refUTxOs
             <> Constraints.unspentOutputs loanUTxOs

      tx' =
        -- | Must be signed by borrower
        mustBeSignedBy userPubKeyHash

        -- | Must tell script current time
        <> (if claimLostWithTTL
            then mustValidateInTimeRange (from start)
            else mempty)

        <> if claimLostWithRefScripts then
          -- | Must reference scripts
          (mustReferenceOutput claimLostSpendRefScript <> mustReferenceOutput claimLostMintRefScript)

          -- | Must spend all utxos to be accepted
          <> (foldl' (\a i -> 
                        a <>
                        mustSpendScriptOutputWithReference 
                          i
                          loanRedeemer
                          claimLostSpendRefScript
                    ) 
                    mempty
                    claimLostInputs
             )

          -- | Burn beacons
          <> (foldl' 
                (\acc (t,i) -> acc <>
                  mustMintCurrencyWithRedeemerAndReference 
                    (Just claimLostMintRefScript) 
                    beaconPolicyHash 
                    beaconRedeemer 
                    t 
                    i
                )
                mempty
                claimLostBeaconsBurned
             )
        else
          -- | Must spend all utxos to be accepted
          ( foldl' (\a i -> 
                      a <>
                      mustSpendScriptOutput
                        i
                        loanRedeemer
                  ) 
                  mempty
                  claimLostInputs
          )
          -- | Burn beacons
          <> (foldl'
                (\acc (t,i) -> acc <>
                  mustMintCurrencyWithRedeemer beaconPolicyHash beaconRedeemer t i
                ) 
                mempty
                claimLostBeaconsBurned
             )

  ledgerTx <- submitTxConstraintsWith @Void lookups tx'
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @String "Lost collateral claimed"

-------------------------------------------------
-- Endpoints
-------------------------------------------------
endpoints :: Contract () TraceSchema Text ()
endpoints = selectList choices >> endpoints
  where
    createReferenceScript' = endpoint @"create-reference-script" createReferenceScript
    createAsk' = endpoint @"create-ask" createAsk
    closeAsk' = endpoint @"close-ask" closeAsk
    createOffer' = endpoint @"create-offer" createOffer
    closeOffer' = endpoint @"close-offer" closeOffer
    acceptOffer' = endpoint @"accept-offer" acceptOffer
    makePayment' = endpoint @"make-payment" makePayment
    rollover' = endpoint @"rollover" rollover
    claimExpired' = endpoint @"claim-expired" claimExpired
    updateAddress' = endpoint @"update-address" updateAddress
    claimLost' = endpoint @"claim-lost" claimLost
    choices = 
      [ createReferenceScript'
      , createAsk'
      , closeAsk'
      , createOffer'
      , closeOffer'
      , acceptOffer'
      , makePayment'
      , rollover'
      , claimExpired'
      , updateAddress'
      , claimLost'
      ]