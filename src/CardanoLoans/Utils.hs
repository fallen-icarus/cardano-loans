{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module CardanoLoans.Utils
  ( 
    -- * On-Chain Data Types
    PlutusRational

    -- * Generate Beacon Names
  , genAssetBeaconName
  , genLoanId
  , credentialAsToken

    -- * Serialization
  , writeData
  , writeScript
  , decodeDatum
  , dataFromCBOR
  , decodeHex
  , toCBOR
  , parseScriptFromCBOR
  , unsafeFromRight
  , unsafeFromData

    -- * Parsing User Inputs
    -- This is just so that certain things do not need to be re-exported.
  , readTokenName
  , readCurrencySymbol
  , readTxId
  , readPubKeyHash
  , readValidatorHash

    -- * Time
  , slotToPOSIXTime
  , posixTimeToSlot
  , preprodConfig
  , mainnetConfig
  , slotToBeginPOSIXTime -- This is needed for the test-suite

    -- * Misc
  , (.*.)
  , (.+.)
  , (.-.)
  , fromInt
  , showTokenName

  -- * Re-exports
  , applyArguments
  , Ledger.scriptSize
  , CurrencySymbol(..)
  , TokenName(..)
  , unsafeRatio
  , adaSymbol
  , adaToken
  , numerator
  , denominator
  , TxOutRef(..)
  , TxId(..)
  ) where

import Data.Aeson as Aeson
import Plutus.V2.Ledger.Api as Api
import qualified PlutusTx
import qualified PlutusTx.Prelude as Plutus
import Codec.Serialise hiding (decode,encode)
import Ledger (Script(..),applyArguments,scriptSize)
import Cardano.Api hiding (TxId,Script,Address)
import Cardano.Api.Shelley (PlutusScript (..))
import Data.ByteString.Lazy (fromStrict,toStrict)
import Data.Text (Text)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Data.String (fromString)
import Ledger.Bytes (fromHex,bytes,encodeByteString)
import Ledger.Tx.CardanoAPI.Internal
import PlutusTx.Ratio (unsafeRatio,numerator,denominator)
import Prettyprinter
import Cardano.Node.Emulator.TimeSlot
import Ledger.Slot
import Data.Maybe (fromJust)

-------------------------------------------------
-- On-Chain Data Types
-------------------------------------------------
type PlutusRational = Plutus.Rational

instance Pretty PlutusRational where
  pretty num = pretty (numerator num) <> " / " <> pretty (denominator num)

-------------------------------------------------
-- Generate Beacon Names
-------------------------------------------------
genAssetBeaconName :: (CurrencySymbol,TokenName) -> TokenName
genAssetBeaconName ((CurrencySymbol sym),(TokenName name)) =
  TokenName $ Plutus.sha2_256 $ "Asset" <> sym <> name

genLoanId :: TxOutRef -> TokenName
genLoanId (TxOutRef (Api.TxId txHash) index) = 
  TokenName $ Plutus.sha2_256 $ txHash <> index'
  where TokenName index' = fromString $ show index

-- | Convert a credential to a token name and optionally add the identifier
-- prefix (used for LenderIDs).
credentialAsToken :: Bool -> Credential -> TokenName
credentialAsToken addIdentifier (PubKeyCredential (PubKeyHash pkh)) = 
  if addIdentifier
  then TokenName $ (unsafeToBuiltinByteString "00") <> pkh
  else TokenName pkh
credentialAsToken addIdentifier (ScriptCredential (ValidatorHash vh)) = 
  if addIdentifier
  then TokenName $ (unsafeToBuiltinByteString "01") <> vh
  else TokenName vh

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

parseScriptFromCBOR :: String -> Ledger.Script
parseScriptFromCBOR cbor = 
  case fmap (deserialise . fromStrict . bytes) . fromHex $ fromString cbor of
    Left err -> error err
    Right script -> script

dataFromCBOR :: String -> Either String Data
dataFromCBOR = fmap deserialise . decodeHex

decodeHex :: String -> Either String LBS.ByteString
decodeHex = fmap (fromStrict . bytes) . fromHex . fromString

toCBOR :: Serialise a => a -> Text
toCBOR = encodeByteString . toStrict . serialise

unsafeToBuiltinByteString :: String -> BuiltinByteString
unsafeToBuiltinByteString = (\(LedgerBytes bytes') -> bytes')
                          . unsafeFromRight
                          . fromHex
                          . fromString

unsafeFromData :: (FromData a) => Data -> a
unsafeFromData = fromJust . fromData

-------------------------------------------------
-- Functions for parsing user input.
-------------------------------------------------
-- | Parse `CurrencySymbol` from user supplied `String`.
readCurrencySymbol :: String -> Either String CurrencySymbol
readCurrencySymbol s = case fromHex $ fromString s of
  Right (LedgerBytes bytes') -> Right $ CurrencySymbol bytes'
  Left msg                   -> Left $ "could not convert: " <> msg

-- | Parse `TokenName` from user supplied `String`.
readTokenName :: String -> Either String TokenName
readTokenName s = case fromHex $ fromString s of
  Right (LedgerBytes bytes') -> Right $ TokenName bytes'
  Left msg                   -> Left $ "could not convert: " <> msg

-- | Parse `TxId` from user supplied `String`.
readTxId :: String -> Either String TxId
readTxId s = case fromHex $ fromString s of
  Right (LedgerBytes bytes') -> Right $ TxId bytes'
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

-------------------------------------------------
-- Time
-------------------------------------------------
slotToPOSIXTime :: SlotConfig -> Slot -> POSIXTime
slotToPOSIXTime = slotToBeginPOSIXTime

posixTimeToSlot :: SlotConfig -> POSIXTime -> Slot
posixTimeToSlot = posixTimeToEnclosingSlot

-- | The preproduction testnet has not always had 1 second slots. Therefore, the default settings
-- for SlotConfig are not usable on the testnet. To fix this, the proper SlotConfig must be
-- normalized to "pretend" that the testnet has always used 1 second slot intervals.
--
-- The normalization is done by taking a slot time and subtracting the slot number from it.
-- For example, slot 23210080 occurred at 1678893280 POSIXTime. So subtracting the slot number 
-- from the time yields the normalized 0 time.
preprodConfig :: SlotConfig
preprodConfig = SlotConfig 1000 (POSIXTime 1655683200000)

-- | The normalized mainnet `SlotConfig`. The normalization is done by taking a slot time and 
-- subtracting the slot number from it. For example, slot 107368296 occurred at 1698934587 
-- POSIXTime. So subtracting the slot number from the time yields the normalized 0 time.
mainnetConfig :: SlotConfig
mainnetConfig = SlotConfig 1000 (POSIXTime 1591566291000)

-------------------------------------------------
-- Misc
-------------------------------------------------
(.*.) :: PlutusRational -> PlutusRational -> PlutusRational
num1 .*. num2 = num1 Plutus.* num2

(.+.) :: PlutusRational -> PlutusRational -> PlutusRational
num1 .+. num2 = num1 Plutus.+ num2

(.-.) :: PlutusRational -> PlutusRational -> PlutusRational
num1 .-. num2 = num1 Plutus.- num2

fromInt :: Integer -> PlutusRational
fromInt = Plutus.fromInteger

unsafeFromRight :: Either a b -> b
unsafeFromRight (Right x) = x
unsafeFromRight _ = error "unsafeFromRight used on Left"

-- | Show the token name in hexidecimal.
showTokenName :: TokenName -> String
showTokenName (TokenName name) = show $ PubKeyHash name

