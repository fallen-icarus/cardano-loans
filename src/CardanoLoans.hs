{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module CardanoLoans
(
  proxyScript,
  proxyValidator,
  proxyValidatorHash,

  loanScript,
  loanValidator,
  loanValidatorHash,

  beaconScript,
  beaconMintingPolicy,
  beaconMintingPolicyHash,
  beaconCurrencySymbol,

  LoanDatum(..),
  LoanRedeemer(..),
  BeaconRedeemer(..),
  PaymentDatum(..),

  writeData,
  writeScript,
  decodeDatum,

  genAssetBeaconName,
  genLoanId,
  credentialAsToken,
  (.*.),
  (.+.),
  (.-.),
  fromInt,

  unsafeFromRight,
  dataFromCBOR,
  toCBOR,

  Ledger.scriptSize,
  slotToBeginPOSIXTime


  -- LoanDatum(..),
  -- LoanRedeemer(..),
  -- BeaconRedeemer(..),
  -- PaymentDatum(..),
  -- Blueprints,
  -- CurrencySymbol(..),
  -- TokenName(..),
  -- Address(..),
  -- Slot(..),
  -- Credential(..),
  -- POSIXTime(..),
  -- PlutusRational,
  -- StakingCredential(..),
  -- Api.TxId(..),
  -- TxOutRef(..),
  -- unValidatorScript,
  -- unMintingPolicyScript,
  -- unsafeRatio,
  -- fromInteger,
  -- (Plutus.-),
  -- (Plutus.*),
  -- (Plutus.+),
  -- Plutus.divide,

  -- adaSymbol,
  -- adaToken,
  -- unsafeFromRight,

  -- dataFromCBOR,
  -- toCBOR,

  -- writeData,
  -- writeScript,
  -- decodeDatum,

  -- credentialAsToken,
  -- readCurrencySymbol,
  -- readTokenName,
  -- readValidatorHash,
  -- readPubKeyHash,
  -- readTxId,
  -- posixTimeToSlot,
  -- slotToPOSIXTime,
  -- getValidatorHash,
  -- CardanoLoans.getPubKeyHash,
  -- toEncodedText,
  -- toAsset,
  -- idToString,
  -- tokenAsPubKey,
  -- toStakePubKeyHash,
  -- toStakeValidatorHash,
  -- toValidatorHash,
  -- toPubKeyHash,
) where

import Prelude hiding (fromInteger)
import Data.Aeson as Aeson
import Control.Monad
import Data.Text (pack)
import Plutus.V2.Ledger.Api as Api
import qualified PlutusTx
import qualified PlutusTx.Prelude as Plutus
import GHC.Generics (Generic)
import Codec.Serialise hiding (decode,encode)
import Ledger (Script(..),applyArguments,scriptSize)
import Cardano.Api hiding (Script,Address)
import Cardano.Api.Shelley (PlutusScript (..))
import Data.ByteString.Lazy (fromStrict,toStrict)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Data.String (fromString)
import Ledger.Bytes (fromHex,bytes,encodeByteString)
import qualified Data.Map as Map
import Ledger.Tx.CardanoAPI.Internal
import Plutus.Script.Utils.V2.Scripts
import Ledger.Slot
import PlutusTx.Ratio (unsafeRatio, fromInteger)
import Cardano.Node.Emulator.TimeSlot
import PlutusTx.Builtins.Internal (BuiltinByteString(..))
import Data.Maybe (fromJust)
import Ledger.Address
import Data.FileEmbed

-------------------------------------------------
-- Blueprints
-------------------------------------------------
newtype Blueprints = Blueprints (Map.Map String String)
  deriving (Show)

instance FromJSON Blueprints where
  parseJSON (Object o) = 
    Blueprints . Map.fromList <$> 
      (o .: "validators" >>= mapM (\(Object o') -> (,) <$> o' .: "title" <*> o' .: "compiledCode"))
  parseJSON _ = mzero

blueprints :: Map.Map String String
blueprints = 
  case decode $ LBS.fromStrict $(embedFile "aiken/plutus.json") of
    Nothing -> error "Failed to decode cardano-loan's blueprint file"
    Just (Blueprints bs) -> bs

parseScriptFromCBOR :: String -> Ledger.Script
parseScriptFromCBOR cbor = 
  case fmap (deserialise . fromStrict . bytes) . fromHex $ fromString cbor of
    Left err -> error err
    Right script -> script

proxyScript :: Ledger.Script
proxyScript = parseScriptFromCBOR $ blueprints Map.! "cardano_loans.proxy"

proxyValidator :: Validator
proxyValidator = Validator proxyScript

proxyValidatorHash :: ValidatorHash
proxyValidatorHash = validatorHash proxyValidator

loanScript :: Ledger.Script
loanScript = 
  applyArguments 
    (parseScriptFromCBOR $ blueprints Map.! "cardano_loans.spend")
    [toData proxyValidatorHash]

loanValidator :: Validator
loanValidator = Validator loanScript

loanValidatorHash :: ValidatorHash
loanValidatorHash = validatorHash loanValidator

beaconScript :: Ledger.Script
beaconScript =
  applyArguments
    (parseScriptFromCBOR $ blueprints Map.! "cardano_loans.mint")
    [toData proxyValidatorHash, toData loanValidatorHash]

beaconMintingPolicy :: MintingPolicy
beaconMintingPolicy = MintingPolicy beaconScript

beaconMintingPolicyHash :: MintingPolicyHash
beaconMintingPolicyHash = mintingPolicyHash beaconMintingPolicy

beaconCurrencySymbol :: CurrencySymbol
beaconCurrencySymbol = scriptCurrencySymbol beaconMintingPolicy

-------------------------------------------------
-- On-Chain Data Types
-------------------------------------------------
data LoanDatum 
  = AskDatum
      { beaconSym :: CurrencySymbol
      , borrowerId :: TokenName
      , loanAsset :: (CurrencySymbol,TokenName)
      , loanPrinciple :: Integer
      , loanTerm :: POSIXTime
      , collateral :: [(CurrencySymbol,TokenName)]
      }
  | OfferDatum
      { beaconSym :: CurrencySymbol
      , lenderId :: TokenName
      , lenderAddress :: Address
      , loanAsset :: (CurrencySymbol,TokenName)
      , loanPrinciple :: Integer
      , rolloverFrequency :: Maybe POSIXTime
      , loanTerm :: POSIXTime
      , loanInterest :: Plutus.Rational
      , minPayment :: Integer
      , collateralization :: [((CurrencySymbol,TokenName),Plutus.Rational)]
      , collateralIsSwappable :: Bool
      , claimPeriod :: POSIXTime
      , offerDeposit :: Integer
      }
  | ActiveDatum
      { beaconSym :: CurrencySymbol
      , borrowerId :: TokenName
      , lenderAddress :: Address
      , loanAsset :: (CurrencySymbol,TokenName)
      , loanPrinciple :: Integer
      , rolloverFrequency :: Maybe POSIXTime
      , lastCheckpoint :: POSIXTime
      , loanTerm :: POSIXTime
      , loanInterest :: Plutus.Rational
      , minPayment :: Integer
      , collateralization :: [((CurrencySymbol,TokenName),Plutus.Rational)]
      , collateralIsSwappable :: Bool
      , claimExpiration :: POSIXTime
      , loanExpiration :: POSIXTime
      , loanOutstanding :: Plutus.Rational
      , loanId :: TokenName
      }
  deriving (Generic,Show)

-- | Custom instance is needed since Aiken uses a different representation.
instance ToData LoanDatum where
  toBuiltinData AskDatum{..} = dataToBuiltinData $
    Constr 0 [ toData beaconSym
             , toData borrowerId
             , List [toData $ fst loanAsset, toData $ snd loanAsset]
             , toData loanPrinciple
             , toData loanTerm
             , Map $ map (\(x,y) -> (toData x, toData y)) collateral
             ]
  toBuiltinData OfferDatum{..} = dataToBuiltinData $
    Constr 1 [ toData beaconSym
             , toData lenderId
             , toData lenderAddress
             , List [toData $ fst loanAsset, toData $ snd loanAsset]
             , toData loanPrinciple
             , toData rolloverFrequency
             , toData loanTerm
             , toData loanInterest
             , toData minPayment
             , Map $ map (\((x,y),r) -> (List [toData x, toData y],toData r)) collateralization
             , toData collateralIsSwappable
             , toData claimPeriod
             , toData offerDeposit
             ]
  toBuiltinData ActiveDatum{..} = dataToBuiltinData $ 
    Constr 2 [ toData beaconSym
             , toData borrowerId
             , toData lenderAddress
             , List [toData $ fst loanAsset, toData $ snd loanAsset]
             , toData loanPrinciple
             , toData rolloverFrequency
             , toData lastCheckpoint
             , toData loanTerm
             , toData loanInterest
             , toData minPayment
             , Map $ map (\((x,y),r) -> (List [toData x, toData y],toData r)) collateralization
             , toData collateralIsSwappable
             , toData claimExpiration
             , toData loanExpiration
             , toData loanOutstanding
             , toData loanId
             ]

fromData' :: (FromData a) => Data -> a
fromData' = fromJust . fromData

instance FromData LoanDatum where
  fromBuiltinData 
    (BuiltinData 
      (Constr 0 
        [ sym
        , bId
        , List [lsym,lname]
        , lPrinciple
        , lterm
        , Map collats
        ]
      )
    ) = Just $ AskDatum
      { beaconSym = fromData' sym
      , borrowerId = fromData' bId
      , loanAsset = (fromData' lsym, fromData' lname)
      , loanPrinciple = fromData' lPrinciple
      , loanTerm = fromData' lterm
      , collateral = 
          map (\(collatsym,collatname) -> (fromData' collatsym, fromData' collatname)) collats
      }
  fromBuiltinData 
    (BuiltinData 
      (Constr 1
        [ sym
        , lId
        , lAddress
        , List [lsym,lname]
        , lPrinciple
        , lRolloverFrequency
        , lterm
        , lInterest
        , lMinPayment
        , Map collats
        , lSwappable
        , lClaimPeriod
        , lOfferDeposit
        ]
      )
    ) = Just $ OfferDatum
      { beaconSym = fromData' sym
      , lenderId = fromData' lId
      , lenderAddress = fromData' lAddress
      , loanAsset = (fromData' lsym, fromData' lname)
      , loanPrinciple = fromData' lPrinciple
      , rolloverFrequency = fromData' lRolloverFrequency
      , loanTerm = fromData' lterm
      , loanInterest = fromData' lInterest
      , minPayment = fromData' lMinPayment
      , collateralization = 
          map (\(List [collatsym,collatname],price) -> 
                ((fromData' collatsym, fromData' collatname),fromData' price)) collats
      , collateralIsSwappable = fromData' lSwappable
      , claimPeriod = fromData' lClaimPeriod
      , offerDeposit = fromData' lOfferDeposit
      }
  fromBuiltinData 
    (BuiltinData 
      (Constr 2
        [ sym
        , bId
        , lAddress
        , List [lsym,lname]
        , lPrinciple
        , lRolloverFrequency
        , lLastCheckpoint
        , lterm
        , lInterest
        , lMinPayment
        , Map collats
        , lSwappable
        , lClaimExpired
        , lExpired
        , lOutstanding
        , lloanId
        ]
      )
    ) = Just $ ActiveDatum
      { beaconSym = fromData' sym
      , borrowerId = fromData' bId
      , lenderAddress = fromData' lAddress
      , loanAsset = (fromData' lsym, fromData' lname)
      , loanPrinciple = fromData' lPrinciple
      , rolloverFrequency = fromData' lRolloverFrequency
      , lastCheckpoint = fromData' lLastCheckpoint
      , loanTerm = fromData' lterm
      , loanInterest = fromData' lInterest
      , minPayment = fromData' lMinPayment
      , collateralization = 
          map (\(List [collatsym,collatname],price) -> 
                ((fromData' collatsym, fromData' collatname),fromData' price)) collats
      , collateralIsSwappable = fromData' lSwappable
      , claimExpiration = fromData' lClaimExpired
      , loanExpiration = fromData' lExpired
      , loanOutstanding = fromData' lOutstanding
      , loanId = fromData' lloanId
      }
  fromBuiltinData _ = Nothing

data LoanRedeemer
  = CloseAsk
  | CloseOffer
  | AcceptOffer
  | MakePayment
  | Rollover
  | ClaimExpired
  | UpdateLenderAddress Address
  | Unlock
  deriving (Generic,Show)

data BeaconRedeemer
  = CreateAsk 
      Credential -- ^ Borrower's staking credential.
      [(CurrencySymbol,TokenName)] -- ^ Loan assets asked for.
  | CreateOffer
      Credential -- ^ Lender's staking credential.
      [(CurrencySymbol,TokenName)] -- ^ Loan assets offered.
  | CreateActive 
      Credential -- ^ Borrower's staking credential.
      [(TxOutRef,TxOutRef)] -- ^ Ask UTxO and Offer UTxO pairing. (Ask,Offer)
  | BurnBeacons
  deriving (Generic,Show)

instance ToData BeaconRedeemer where
  toBuiltinData (CreateAsk cred assets) = 
    dataToBuiltinData $ 
      Constr 0 [ toData cred
               , Map $ map (\(x,y) -> (toData x, toData y)) assets
               ]
  toBuiltinData (CreateOffer cred assets) = 
    dataToBuiltinData $ 
      Constr 1 [ toData cred
               , Map $ map (\(x,y) -> (toData x, toData y)) assets
               ]
  toBuiltinData (CreateActive cred pairing) = dataToBuiltinData $
    Constr 2 [ toData cred
             , Map $ map (\(x,y) -> (toData x, toData y)) pairing
             ]
  toBuiltinData BurnBeacons = dataToBuiltinData $ Constr 3 []

newtype PaymentDatum = PaymentDatum (CurrencySymbol,TokenName)
  deriving (Generic)

instance ToData PaymentDatum where
  toBuiltinData (PaymentDatum (sym,name)) = dataToBuiltinData $ List [toData sym, toData name]

PlutusTx.unstableMakeIsData ''LoanRedeemer

-------------------------------------------------
-- Serialization
-------------------------------------------------
dataToScriptData :: Data -> ScriptData
dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (Map xs)      = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (List xs)     = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (I n)         = ScriptDataNumber n
dataToScriptData (B bs)        = ScriptDataBytes bs

toJSONValue :: PlutusTx.ToData a => a -> Aeson.Value
toJSONValue = scriptDataToJson ScriptDataJsonDetailedSchema
           . dataToScriptData
           . PlutusTx.toData

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . encode . toJSONValue

serialisedScript :: Script -> PlutusScript PlutusScriptV2
serialisedScript = PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise

writeScript :: FilePath -> Script -> IO (Either (FileError ()) ())
writeScript file script = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing
                         $ serialisedScript script

writeData :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeData = writeJSON

decodeDatum :: (FromData a) => Aeson.Value -> Maybe a
decodeDatum = unsafeFromRight . fmap (PlutusTx.fromBuiltinData . fromCardanoScriptData)
            . scriptDataFromJson ScriptDataJsonDetailedSchema

-------------------------------------------------
-- Off-Chain Helper Functions and Types
-------------------------------------------------
genAssetBeaconName :: (CurrencySymbol,TokenName) -> TokenName
genAssetBeaconName ((CurrencySymbol sym),(TokenName name)) =
  TokenName $ Plutus.sha2_256 $ "Asset" <> sym <> name

genLoanId :: TxOutRef -> TokenName
genLoanId (TxOutRef (Api.TxId txHash) index) = 
  TokenName $ Plutus.sha2_256 $ txHash <> index'
  where TokenName index' = fromString $ show index

credentialAsToken :: Bool -> Credential -> TokenName
credentialAsToken addIdentifier (PubKeyCredential (PubKeyHash pkh)) = 
  if addIdentifier
  then TokenName $ (unsafeToBuiltinByteString "00") <> pkh
  else TokenName pkh
credentialAsToken addIdentifier (ScriptCredential (ValidatorHash vh)) = 
  if addIdentifier
  then TokenName $ (unsafeToBuiltinByteString "01") <> vh
  else TokenName vh

type PlutusRational = Plutus.Rational

slotToPOSIXTime :: Slot -> POSIXTime
slotToPOSIXTime = slotToBeginPOSIXTime preprodConfig

posixTimeToSlot :: POSIXTime -> Slot
posixTimeToSlot = posixTimeToEnclosingSlot preprodConfig

-- | The preproduction testnet has not always had 1 second slots. Therefore, the default settings
-- for SlotConfig are not usable on the testnet. To fix this, the proper SlotConfig must be
-- normalized to "pretend" that the testnet has always used 1 second slot intervals.
--
-- The normalization is done by taking a slot time and subtracting the slot number from it.
-- For example, slot 23210080 occurred at 1678893280 POSIXTime. So subtracting the slot number 
-- from the time yields the normalized 0 time.
preprodConfig :: SlotConfig
preprodConfig = SlotConfig 1000 (POSIXTime 1655683200000)

unsafeToBuiltinByteString :: String -> BuiltinByteString
unsafeToBuiltinByteString = (\(LedgerBytes bytes) -> bytes)
                          . unsafeFromRight
                          . fromHex
                          . fromString

-- | Parse Currency from user supplied String
readCurrencySymbol :: String -> Either String CurrencySymbol
readCurrencySymbol s = case fromHex $ fromString s of
  Right (LedgerBytes bytes') -> Right $ CurrencySymbol bytes'
  Left msg                   -> Left $ "could not convert: " <> msg

-- | Parse TokenName from user supplied String
readTokenName :: String -> Either String TokenName
readTokenName s = case fromHex $ fromString s of
  Right (LedgerBytes bytes') -> Right $ TokenName bytes'
  Left msg                   -> Left $ "could not convert: " <> msg

-- | Parse PubKeyHash from user supplied String
readPubKeyHash :: String -> Either String PubKeyHash
readPubKeyHash s = case fromHex $ fromString s of
  Right (LedgerBytes bytes') -> Right $ PubKeyHash bytes'
  Left msg                   -> Left $ "could not convert: " <> msg

-- | Parse ValidatorHash from user supplied String
readValidatorHash :: String -> Either String ValidatorHash
readValidatorHash s = case fromHex $ fromString s of
  Right (LedgerBytes bytes') -> Right $ ValidatorHash bytes'
  Left msg                   -> Left $ "could not convert: " <> msg

-- | Parse TxId from user supplied String
readTxId :: String -> Either String Api.TxId
readTxId s = case fromHex $ fromString s of
  Right (LedgerBytes bytes') -> Right $ Api.TxId bytes'
  Left msg                   -> Left $ "could not convert: " <> msg

getValidatorHash :: ValidatorHash -> B.ByteString
getValidatorHash (ValidatorHash (BuiltinByteString vh)) = vh

getPubKeyHash :: PubKeyHash -> B.ByteString
getPubKeyHash pkh = (\(BuiltinByteString z) -> z) $ Api.getPubKeyHash pkh

toEncodedText :: TokenName -> Text
toEncodedText (TokenName (BuiltinByteString tn)) = encodeByteString tn

toAsset :: (CurrencySymbol,TokenName) -> Text
toAsset (currSym,tokName)
  | currSym == adaSymbol = "lovelace"
  | otherwise = pack (show currSym) <> "." <> toEncodedText tokName

idToString :: TokenName -> String
idToString tn = show $ tokenAsPubKey tn

-- | This is used to convert the LenderID and BorrowerID to strings for displaying in the JSON. It
-- does not matter whether the ID is actually for a pubkey or a script.
tokenAsPubKey :: TokenName -> PubKeyHash
tokenAsPubKey (TokenName pkh) = PubKeyHash pkh

toStakePubKeyHash :: Address -> Maybe PubKeyHash
toStakePubKeyHash (Address _ (Just (StakingHash (PubKeyCredential pkh)))) = Just pkh
toStakePubKeyHash _ = Nothing

toStakeValidatorHash :: Address -> Maybe ValidatorHash
toStakeValidatorHash (Address _ (Just (StakingHash (ScriptCredential vh)))) = Just vh
toStakeValidatorHash _ = Nothing

(.*.) :: PlutusRational -> PlutusRational -> PlutusRational
num1 .*. num2 = num1 Plutus.* num2

(.+.) :: PlutusRational -> PlutusRational -> PlutusRational
num1 .+. num2 = num1 Plutus.+ num2

(.-.) :: PlutusRational -> PlutusRational -> PlutusRational
num1 .-. num2 = num1 Plutus.- num2

fromInt :: Integer -> PlutusRational
fromInt = Plutus.fromInteger

-------------------------------------------------
-- Misc
-------------------------------------------------
unsafeFromRight :: Either a b -> b
unsafeFromRight (Right x) = x
unsafeFromRight _ = error "unsafeFromRight used on Left"

dataFromCBOR :: String -> Either String Data
dataFromCBOR = fmap (deserialise . fromStrict . bytes) . fromHex . fromString

toCBOR :: Serialise a => a -> Text
toCBOR = encodeByteString . toStrict . serialise
