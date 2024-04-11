{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module CLI.Data.Bech32Address
  ( -- * Bech32 Encoded Addresses
    PaymentAddress(..)
  , readPaymentAddress
  , unPaymentAddress
  , StakeAddress(..)
  , readStakeAddress
  , unStakeAddress

    -- * Conversions
  , plutusToBech32
  , paymentAddressToPlutusAddress
  , stakeAddressToPlutusCredential

    -- * Inspecting Bech32 Addresses
  , Address.AddressInfo(..)
  , inspectBech32Address
  ) where

import Relude
import Data.Aeson qualified as Aeson
import Data.Maybe (fromJust)
import Prettyprinter (Pretty(..))

import Cardano.Address.Style.Shelley qualified as Address
import Cardano.Address (fromBech32,bech32,bech32With)
import Cardano.Address.Script qualified as Address
import Cardano.Address.Derivation (Depth(..))
import Cardano.Codec.Bech32.Prefixes (stake_test,stake)

import CardanoLoans

import CLI.Data.Network

-- | A type synonym to improve type readability.
type PlutusAddress = Address

-------------------------------------------------
-- Bech32 Encoded Payment Addresses
-------------------------------------------------
-- | A wrapper around a bech32 encoded payment address.
newtype PaymentAddress = PaymentAddress Text
  deriving (Show,Eq,Ord)

instance Aeson.ToJSON PaymentAddress where
  toJSON (PaymentAddress addr) = Aeson.toJSON addr

instance Aeson.FromJSON PaymentAddress where
  parseJSON = Aeson.withText "PaymentAddress" (return . PaymentAddress)

instance Pretty PaymentAddress where
  pretty (PaymentAddress addr) = pretty addr

instance IsString PaymentAddress where
  fromString = PaymentAddress . toText

instance ToText PaymentAddress where
  toText (PaymentAddress addr) = addr

instance ToString PaymentAddress where
  toString (PaymentAddress addr) = toString addr

unPaymentAddress :: PaymentAddress -> Text
unPaymentAddress (PaymentAddress addr) = addr

-------------------------------------------------
-- Bech32 Encoded Stake Addresses
-------------------------------------------------
-- | A wrapper around a bech32 encoded stake address.
newtype StakeAddress = StakeAddress Text
  deriving (Show,Eq,Ord)

instance Aeson.ToJSON StakeAddress where
  toJSON (StakeAddress addr) = Aeson.toJSON addr

instance Aeson.FromJSON StakeAddress where
  parseJSON = Aeson.withText "StakeAddress" (return . StakeAddress)

instance Pretty StakeAddress where
  pretty (StakeAddress addr) = pretty addr

instance IsString StakeAddress where
  fromString = StakeAddress . toText

instance ToText StakeAddress where
  toText (StakeAddress addr) = addr

instance ToString StakeAddress where
  toString (StakeAddress addr) = toString addr

unStakeAddress :: StakeAddress -> Text
unStakeAddress (StakeAddress addr) = addr

-------------------------------------------------
-- Inspecting Bech32 Addresses
-------------------------------------------------
inspectBech32Address :: Text -> Either Text Address.AddressInfo
inspectBech32Address addr = do
    bechAddr <- maybeToRight "Not a valid bech32 address." $ fromBech32 addr

    inspectInfo <- 
      first (const "Could not inspect bech32 address.") $
        Address.eitherInspectAddress Nothing bechAddr

    case inspectInfo of
      Address.InspectAddressShelley info -> Right info
      _ -> Left "Bech32 address is not a shelley address."

-------------------------------------------------
-- Parsing Bech32 Addresses
-------------------------------------------------
-- | Try to convert a user supplied payment address to `PaymentAddress`. Since bech32 addresses
-- also encode the network used, this function requires specifying what network to expect
-- with the address. Only shelley addresses are supported.
readPaymentAddress :: Network -> Text -> Either Text PaymentAddress
readPaymentAddress targetNetwork rawAddr = do
    Address.AddressInfo{infoSpendingKeyHash,infoScriptHash,infoNetworkTag} <- 
      inspectBech32Address rawAddr

    -- The address should have exactly one payment credential.
    when (length (catMaybes [infoScriptHash,infoSpendingKeyHash]) /= 1) $
      Left "Address is not a valid bech32 payment address."

    -- The address must be for the proper network.
    if networkTag == infoNetworkTag
    then Right $ PaymentAddress rawAddr
    else Left $ "Address is not a " <> toText targetNetwork <> " address."
  where
    networkTag = case targetNetwork of
      Mainnet -> Address.shelleyMainnet
      PreProdTestnet -> Address.shelleyTestnet

-- | Try to convert a user supplied stake address to `StakeAddress`. Since bech32 addresses
-- also encode the network used, this function requires specifying what network to expect
-- with the address. Only shelley addresses are supported.
readStakeAddress :: Network -> Text -> Either Text StakeAddress
readStakeAddress targetNetwork rawAddr = do
    Address.AddressInfo{..} <- inspectBech32Address rawAddr
    
    -- The address should not have any payment credentials.
    when (isJust $ infoScriptHash <|> infoSpendingKeyHash) $ 
      Left "Address is not a stake address."

    -- The address should have exactly one staking credential.
    when (length (catMaybes [infoStakeScriptHash,infoStakeKeyHash]) /= 1) $
      Left "Address is not a valid bech32 stake address."

    -- The address must be for the proper network.
    if networkTag == infoNetworkTag
    then Right $ StakeAddress rawAddr
    else Left $ "Address is not a " <> toText targetNetwork <> " address."
  where
    networkTag = case targetNetwork of
      Mainnet -> Address.shelleyMainnet
      PreProdTestnet -> Address.shelleyTestnet

-------------------------------------------------
-- PlutusAddress <-> Bech32
-------------------------------------------------
-- | Convert a plutus address to the corresponding bech32 addresses for the specific network. 
-- Pointer addresses are not supported. This is usually used when parsing datums.
plutusToBech32 :: Network -> PlutusAddress -> Either Text (PaymentAddress, Maybe StakeAddress)
plutusToBech32 network (Address pCred msCred) = case msCred of
    Just sCred -> do
      stakeCred <- toBech32StakeCredential sCred
      payCred <- toBech32PaymentCredential pCred
      stakeAddr <-
        fmap (StakeAddress . bech32With stakePrefix) $ 
          first (const "Could not create stake address from plutus StakingCredential.") $
            Address.stakeAddress networkTag stakeCred
      return ( PaymentAddress $ bech32 $ Address.delegationAddress networkTag payCred stakeCred
             , Just stakeAddr
             )
    Nothing -> do
      payCred <- toBech32PaymentCredential pCred
      return ( PaymentAddress $ bech32 $ Address.paymentAddress networkTag payCred
             , Nothing
             )
  where
    (stakePrefix, networkTag) = 
      case network of
        Mainnet -> (stake, Address.shelleyMainnet)
        PreProdTestnet -> (stake_test, Address.shelleyTestnet)
      
    toBech32StakeCredential 
      :: StakingCredential 
      -> Either Text (Address.Credential 'DelegationK)
    toBech32StakeCredential sCred = case sCred of
      StakingHash (PubKeyCredential pkh) -> 
        fmap Address.DelegationFromKeyHash $ 
          maybeToRight "Could not get stake keyHash from bytes" $ 
            Address.keyHashFromBytes 
              (Address.Delegation, unBuiltinByteString $ getPubKeyHash pkh)
      StakingHash (ScriptCredential vh) -> 
        fmap Address.DelegationFromScript $ 
          maybeToRight "Could not get stake scriptHash from bytes" $ 
            Address.scriptHashFromBytes $ unBuiltinByteString $ getScriptHash vh
      _ -> Left "Pointer addresses are not supported."

    toBech32PaymentCredential :: Credential -> Either Text (Address.Credential 'PaymentK)
    toBech32PaymentCredential payCred = case payCred of
      PubKeyCredential pkh ->
        fmap Address.PaymentFromKeyHash $ 
          maybeToRight "Could not get payment keyHash from bytes" $ 
            Address.keyHashFromBytes (Address.Payment, unBuiltinByteString $ getPubKeyHash pkh)
      ScriptCredential vh ->
        fmap Address.PaymentFromScript $ 
          maybeToRight "Could not get payment scriptHash from bytes" $ 
            Address.scriptHashFromBytes $ unBuiltinByteString $ getScriptHash vh

-- | Convert a bech32 payment address to a plutus address. This cannot be used with stake addresses
-- since plutus addresses __must__ have payment credentials.
paymentAddressToPlutusAddress :: PaymentAddress -> Either Text PlutusAddress
paymentAddressToPlutusAddress (PaymentAddress addr) = do
    Address.AddressInfo{..} <- inspectBech32Address addr
    return $ 
      Address
          { addressCredential =
              let mScriptCred = ScriptCredential 
                              . ScriptHash 
                              . BuiltinByteString 
                            <$> infoScriptHash
                  mPayCred = PubKeyCredential 
                           . PubKeyHash 
                           . BuiltinByteString
                         <$> infoSpendingKeyHash
              in fromJust $ mPayCred <|> mScriptCred
          , addressStakingCredential = case (infoStakeScriptHash,infoStakeKeyHash) of
              (Nothing, Just keyHash) -> Just $ 
                StakingHash $ PubKeyCredential $ PubKeyHash $ 
                  BuiltinByteString keyHash
              (Just scrptHash, Nothing) -> Just $ 
                StakingHash $ ScriptCredential $ ScriptHash $ 
                  BuiltinByteString scrptHash
              _ -> Nothing
          }

-- | Convert a bech32 stake address to a plutus credential (either a pubkey or a script credential). 
stakeAddressToPlutusCredential :: StakeAddress -> Either Text Credential
stakeAddressToPlutusCredential (StakeAddress addr) = do
  Address.AddressInfo{..} <- inspectBech32Address addr

  let mStakeKeyHash = 
        PubKeyCredential . PubKeyHash . BuiltinByteString <$> infoStakeKeyHash
      mStakeScriptHash = 
        ScriptCredential . ScriptHash . BuiltinByteString <$> infoStakeScriptHash

  maybeToRight "Address is not a valid bech32 stake address." $ 
    mStakeScriptHash <|> mStakeKeyHash
