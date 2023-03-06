{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
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


module CardanoLoans
(
  loanValidator
) where

import Data.Aeson hiding (Value)
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import Prelude (IO,FilePath,seq) 
import qualified Prelude as Haskell
import Data.String (fromString)

import           Cardano.Api hiding (Script,Value,TxOut,Address,ScriptHash)
import           Cardano.Api.Shelley   (PlutusScript (..))
import Plutus.V2.Ledger.Contexts
import Plutus.V2.Ledger.Api
import qualified PlutusTx
import PlutusTx.Prelude
import Ledger.Address
import Plutus.Script.Utils.V2.Scripts as Scripts
import Plutus.Script.Utils.V2.Typed.Scripts
import Ledger.Bytes (fromHex)
import qualified Plutonomy
import Ledger.Value (valueOf,flattenValue)
import PlutusTx.Numeric as Num
import PlutusTx.Ratio (fromGHC,recip)
import PlutusTx.Ratio as Ratio
import PlutusPrelude (foldl')
import qualified PlutusTx.AssocMap as Map

-------------------------------------------------
-- Data Types
-------------------------------------------------
data LoanDatum
  -- | The datum for the ask ask.
  = AskDatum 
      { askBeacon :: (CurrencySymbol,TokenName)
      , borrowerId :: (CurrencySymbol,TokenName)
      , loanAsset :: (CurrencySymbol,TokenName)
      , loanQuantity :: Integer
      , loanTerm :: POSIXTime
      , collateral :: [(CurrencySymbol,TokenName)]
      }
  -- | The datum for the offer ask.
  | OfferDatum
      { offerBeacon :: (CurrencySymbol,TokenName)
      , lenderId :: (CurrencySymbol,TokenName)
      , loanAsset :: (CurrencySymbol,TokenName)
      , loanQuantity :: Integer
      , loanTerm :: POSIXTime
      , loanInterest :: Rational
      , collateralRates :: [((CurrencySymbol,TokenName),Rational)]
      , offerExperation :: POSIXTime
      }
  -- | The datum for the active ask.
  | ActiveDatum
      { activeBeacon :: (CurrencySymbol,TokenName)
      , lenderId :: (CurrencySymbol,TokenName)
      , borrowerId :: (CurrencySymbol,TokenName)
      , loanAsset :: (CurrencySymbol,TokenName)
      , loanQuantity :: Integer
      , loanTerm :: POSIXTime
      , loanInterest :: Rational
      , collateralRates :: [((CurrencySymbol,TokenName),Rational)]
      , loanExpiration :: POSIXTime
      , loanOutstanding :: Integer
      }

instance Eq LoanDatum where
  {-# INLINABLE (==) #-}
  (AskDatum a b c d e f) == (AskDatum a' b' c' d' e' f') =
    a == a' && b == b' && c == c' && d == d' && e == e' && f == f'
  (OfferDatum a b c d e f g h) == (OfferDatum a' b' c' d' e' f' g' h') =
    a == a' && b == b' && c == c' && d == d' && e == e' && f == f' && g == g' && h == h'
  (ActiveDatum a b c d e f g h i j) == (ActiveDatum a' b' c' d' e' f' g' h' i' j') =
    a == a' && b == b' && c == c' && d == d' && e == e' && f == f' && g == g' && h == h' && 
    i == i' && j == j'
  _ == _ = False

data LoanRedeemer
  = CloseAsk
  | CloseOffer
  | AcceptOffer
  | RepayLoan
  | Claim

-- | The redeemer for the Borrower ID policy.
data BorrowerIdRedeemer
  -- | Borrower can mint one of their IDs.
  = MintBorrowerId PaymentPubKeyHash
  -- | Burn IDs.
  | BurnBorrowerId

-- | The redeemer for the lender ID policy.
data LenderIdRedeemer
  = MintLenderId PaymentPubKeyHash
  | BurnLenderId

-- | The redeemer for the phase beacon.
data PhaseRedeemer
  = MintAskToken
  | MintOfferToken PaymentPubKeyHash
  | MintActiveToken
  | BurnPhaseToken

-- | A helper type used to create testing beacons.
type AppName = BuiltinString

PlutusTx.unstableMakeIsData ''LoanDatum
PlutusTx.unstableMakeIsData ''LoanRedeemer
PlutusTx.unstableMakeIsData ''BorrowerIdRedeemer
PlutusTx.unstableMakeIsData ''LenderIdRedeemer
PlutusTx.unstableMakeIsData ''PhaseRedeemer

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
-- | Used to create a testing set of beacons/IDs without having to change the logic.
app :: AppName
app = "testing"

{-# INLINABLE pubKeyAsToken #-}
pubKeyAsToken :: PaymentPubKeyHash -> TokenName
pubKeyAsToken = TokenName . getPubKeyHash . unPaymentPubKeyHash

{-# INLINABLE tokenAsPubKey #-}
tokenAsPubKey :: TokenName -> PubKeyHash
tokenAsPubKey (TokenName pkh) = PubKeyHash pkh

{-# INLINABLE scriptHashAsToken #-}
scriptHashAsToken :: ValidatorHash -> TokenName
scriptHashAsToken (ValidatorHash vh) = TokenName vh

{-# INLINABLE tokenAsScriptHash #-}
tokenAsScriptHash :: TokenName -> ValidatorHash
tokenAsScriptHash (TokenName vh) = ValidatorHash vh

{-# INLINABLE encodeDatum #-}
-- | This is a convenient way to check what kind of datum it is.
encodeDatum :: LoanDatum -> Integer
encodeDatum (AskDatum _ _ _ _ _ _) = 0
encodeDatum (OfferDatum _ _ _ _ _ _ _ _) = 1
encodeDatum (ActiveDatum _ _ _ _ _ _ _ _ _ _) = 2

{-# INLINABLE signed #-}
signed :: [PubKeyHash] -> PubKeyHash -> Bool
signed [] _ = False
signed (k:ks) k'
  | k == k' = True
  | otherwise = signed ks k'

{-# INLINABLE stakingCredApproves #-}
stakingCredApproves :: Address -> TxInfo -> Bool
stakingCredApproves addr info = case addressStakingCredential addr of
  -- | This is to prevent permanent locking of funds.
  -- The DEX is not meant to be used without a staking credential.
  Nothing -> True

  -- | Check if staking credential signals approval.
  Just stakeCred@(StakingHash cred) -> case cred of
    PubKeyCredential pkh -> signed (txInfoSignatories info) pkh
    ScriptCredential _ -> isJust $ Map.lookup stakeCred $ txInfoWdrl info
  
  Just _ -> traceError "Wrong kind of staking credential."

{-# INLINABLE ownInput #-}
ownInput :: ScriptContext -> TxOut
ownInput (ScriptContext info (Spending ref)) = getScriptInput (txInfoInputs info) ref
ownInput _ = traceError "script input error"

{-# INLINABLE getScriptInput #-}
getScriptInput :: [TxInInfo] -> TxOutRef -> TxOut
getScriptInput [] _ = traceError "script input error"
getScriptInput ((TxInInfo iRef ot) : tl) ref
  | iRef == ref = ot
  | otherwise = getScriptInput tl ref

{-# INLINABLE parseLoanDatum #-}
parseLoanDatum :: OutputDatum -> LoanDatum
parseLoanDatum d = case d of
  (OutputDatum (Datum d')) -> unsafeFromBuiltinData d'
  _ -> traceError "All loan datums must be inline datums."

-------------------------------------------------
-- On-Chain Loan Validator
-------------------------------------------------
-- | This validator uses the presence or absence of the phase tokens to judge the validity of
-- the datums. This is due to the phase tokens only being mintable when the datums are valid.
-- 
-- If there is ever a datum present WITHOUT a phase token, the staking credential of the address
-- has custody rights. This is to protect the address owner from malicious datums. It is therefore
-- up to the lenders to ensure proper use of this validator.
--
-- It is technically possible for a malicious user to create their own phase beacon policy for use
-- with this validator. However, this would be an entirely different token than the actual beacons
-- which means they would not even be discoverable by other users.
--
-- Since the active utxo is time-locked for the borrower, there is no need to ensure that ONLY the
-- collateral assets ever leave. Those assets come from the borrower and the borrower has custody
-- of that utxo until the loan expires.
mkLoan :: LoanDatum -> LoanRedeemer -> ScriptContext -> Bool
mkLoan loanDatum r ctx@ScriptContext{scriptContextTxInfo=info} = case r of
    CloseAsk ->
      -- | The datum must be an AskDatum.
      traceIfFalse "Datum is not an AskDatum" (encodeDatum loanDatum == 0) &&
      -- | There can only be one utxo being spent from this address in this tx.
      traceIfFalse "Only one utxo can be spent from this address in this tx." (length allInputs == 1) &&
      -- | The staking credential must signal approval - this is required regardless of the presence
      -- of the ask phase beacon since the borrowerID is assumed to be the same as the staking cred.
      traceIfFalse "Staking credential did not approve" stakingCredApproves'
    CloseOffer ->
      -- | The datum must be an OfferDatum.
      traceIfFalse "Datum is not an OfferDatum" (encodeDatum loanDatum == 1) &&
      -- | There can only be one utxo being spent from this address in this tx.
      traceIfFalse "Only one utxo can be spent from this address in this tx." (length allInputs == 1) &&
      -- | If the offer phase beacon is present
      if uncurry (valueOf inputValue) (offerBeacon loanDatum) == 1
      then 
        -- | The pubkey associated with the lender ID must sign the tx.
        traceIfFalse "Lender did not sign" 
             (signed (txInfoSignatories info) (tokenAsPubKey $ snd $ lenderId loanDatum))
      else 
        -- | The staking credential must signal approval.
        traceIfFalse "Staking credential did not approve" stakingCredApproves'
    AcceptOffer -> True
      -- | There must only be two inputs from this address:
      --     - one ask input signified by the AskDatum
      --     - one offer input signified by the OfferDatum
      -- | The ask input must have the ask phase beacon.
      -- | The offer input must have the offer phase beacon.
      -- | The ask phase beacon must have the same currency symbol as the offer phase beacon.
      -- | There must only be one output to this address.
      -- | The output to this address must have an active phase beacon with the same currency symbol
      -- as the ask and offer phase beacons - this requirement delegates a lot of the datum checking 
      -- to the phase beacon policy.
      -- | The staking credential must signal approval.
    RepayLoan ->
      -- | The input must have an ActiveDatum.
      traceIfFalse "Datum is not an ActiveDatum" (encodeDatum loanDatum == 2) &&
      -- | There can only be one utxo spent from this address.
      traceIfFalse "Only one utxo can be spent from this address in this tx." (length allInputs == 1) &&
      -- | If the input has the active phase beacon (a sign of a valid loan)
      if uncurry (valueOf inputValue) (offerBeacon loanDatum) == 1
      then
        -- | The loan must not be expired.
        traceIfFalse "Loan is expired" (not $ loanIsExpired $ loanExpiration loanDatum) &&
        -- | repaymentCheck ensures:
        --     - there can only be one output to the address.
        --     - sum (collateral asset taken * collateralRate) * (1 + interest) <= loan asset repaid
        traceIfFalse "Fail: collateralTaken / collateralization * (1 + interest) <= loanRepaid"
          repaymentCheck &&
        -- | The output must have the proper datum
        --     - same as input datum except must subtract loan repaid from loanOutstanding.
        traceIfFalse "Output to address has wrong datum" 
          (parseLoanDatum (txOutDatum output) == loanDatum{loanOutstanding = newOutstanding}) &&
        -- | If new loanOutstanding <= 0, the borrower ID must be burned
        if newOutstanding <= 0
        then uncurry (valueOf $ txInfoMint info) (borrowerId loanDatum) == -1
        else True
      else
        -- | The staking credential must signal approval.
        traceIfFalse "Staking credential did not approve" stakingCredApproves' &&
        -- | The borrower and lender IDs must be burned.
        traceIfFalse "IDs not burned" 
          ( uncurry (valueOf inputValue) (borrowerId loanDatum) == 
             Num.negate (uncurry (valueOf $ txInfoMint info) (borrowerId loanDatum)) &&

            uncurry (valueOf inputValue) (lenderId loanDatum) == 
             Num.negate (uncurry (valueOf $ txInfoMint info) (lenderId loanDatum))
          )
    Claim ->
      -- | The datum must be an ActiveDatum.
      traceIfFalse "Datum is not an ActiveDatum" (encodeDatum loanDatum == 2) &&
      -- | There can only be one utxo spent from this address.
      traceIfFalse "Only one utxo can be spent from this address in this tx." (length allInputs == 1) &&
      -- | The utxo must have an active phase beacon.
      traceIfFalse "UTxO does not have active phase beacon" 
        (uncurry (valueOf inputValue) (activeBeacon loanDatum) == 1) &&
      -- | The loan must be expired.
      traceIfFalse "Loan not expired" (loanIsExpired $ loanExpiration loanDatum) &&
      -- | The lender ID must be burned.
      traceIfFalse "Lender ID not burned" 
        (uncurry (valueOf $ txInfoMint info) (lenderId loanDatum) == -1) &&
      -- | The lender must sign the tx.
      traceIfFalse "Lender did not sign" 
        (signed (txInfoSignatories info) (tokenAsPubKey $ snd $ lenderId loanDatum)) &&
      -- | If the borrower ID is still present, it must be burned.
      if (uncurry (valueOf inputValue) (borrowerId loanDatum) == 1)
      then traceIfFalse "Borrower ID not burned" 
             (uncurry (valueOf $ txInfoMint info) (borrowerId loanDatum) == -1)
      else True
  
  where
    -- | Get the credential for this input as well as its value.
    -- Credential used to check asset flux for address and ensure staking credential approves 
    -- when necessary. The value is used to check for phase beacon tokens.
    (inputCredentials,inputValue) = 
      let TxOut{txOutAddress=addr,txOutValue=iVal} = ownInput ctx
      in (addr,iVal)

    stakingCredApproves' :: Bool
    !stakingCredApproves' = stakingCredApproves inputCredentials info

    -- | Returns a list of inputs from this address.
    allInputs :: [TxOut]
    allInputs =
      let inputs = txInfoInputs info
          foo iCred !acc (TxInInfo{txInInfoResolved=x@TxOut{txOutAddress=addr}}:xs) =
            if addr == iCred
            then x : acc
            else acc
      in foo inputCredentials [] inputs

    -- | Check if the expiration has passed.
    loanIsExpired :: POSIXTime -> Bool
    loanIsExpired endTime = repaymentTime > endTime

    -- | Get the loan repayment time from the tx's validity range.
    -- This one uses to upper bound of the tx's validity range so that a borrower can't
    -- set a lower limit than has already passed to trick the script.
    repaymentTime :: POSIXTime
    repaymentTime = case (\(UpperBound t _) -> t) $ ivTo $ txInfoValidRange info of
      PosInf -> traceError "TTE not specified"
      Finite t -> t
      _ -> traceError "Shouldn't be NegInf."

    -- | Gets the output to this address. 
    -- Throws an error if there is more than one.
    output :: TxOut
    output = 
      let outputs = txInfoOutputs info
          foo iCred !acc (x@TxOut{txOutAddress = addr}:xs) =
            if addr == iCred
            then if null acc
                 then x : acc
                 else traceError "There can only be one output to address"
            else acc
      in head $ foo inputCredentials [] outputs

    -- | The value flux of this address.
    -- Positive values mean the address gained the asset.
    -- Negative values mean the address lost the asset.
    addrDiff :: Value
    addrDiff = txOutValue output <> Num.negate inputValue

    repaidAmount :: Integer
    repaidAmount = uncurry (valueOf addrDiff) $ loanAsset loanDatum

    -- | This is converted into units of the loan asset by dividing by the collateral rates.
    collateralReclaimed :: Rational
    collateralReclaimed = 
      let foo val !acc ((collatAsset,price):xs) = 
            acc + Ratio.negate (fromInteger $ uncurry (valueOf val) collatAsset) * recip price 
      in foo addrDiff (fromInteger 0) (collateralRates loanDatum)

    repaymentCheck :: Bool
    repaymentCheck = 
      collateralReclaimed * (fromInteger 1 + loanInterest loanDatum) <= fromInteger repaidAmount

    -- | New total of loan outstanding.
    newOutstanding :: Integer
    newOutstanding = loanOutstanding loanDatum - repaidAmount

    

data Loan
instance ValidatorTypes Loan where
  type instance RedeemerType Loan = LoanRedeemer
  type instance DatumType Loan = LoanDatum

loanValidator :: Validator
loanValidator = Plutonomy.optimizeUPLC $ validatorScript $ mkTypedValidator @Loan
    $$(PlutusTx.compile [|| mkLoan ||])
    $$(PlutusTx.compile [|| wrap ||])
  where wrap = mkUntypedValidator

loanScript :: Script
loanScript = unValidatorScript loanValidator

loanValidatorHash :: ValidatorHash
loanValidatorHash = Scripts.validatorHash loanValidator

-------------------------------------------------
-- On-Chain Borrower ID Policy
-------------------------------------------------
mkBorrowerIdPolicy :: AppName -> ValidatorHash -- ^ Extra parameters.
                   -> BorrowerIdRedeemer -> ScriptContext -> Bool
mkBorrowerIdPolicy appName loanValHash r ctx@ScriptContext{scriptContextTxInfo=info} = True

borrowerIdPolicy :: MintingPolicy
borrowerIdPolicy = Plutonomy.optimizeUPLC $ mkMintingPolicyScript
    ($$(PlutusTx.compile [|| wrap ||])
      `PlutusTx.applyCode` PlutusTx.liftCode app
      `PlutusTx.applyCode` PlutusTx.liftCode loanValidatorHash)
  where
    wrap x y = mkUntypedMintingPolicy $ mkBorrowerIdPolicy x y

borrowerIdPolicyScript :: Script
borrowerIdPolicyScript = unMintingPolicyScript borrowerIdPolicy

borrowerIdPolicySymbol :: CurrencySymbol
borrowerIdPolicySymbol = scriptCurrencySymbol borrowerIdPolicy

-------------------------------------------------
-- On-Chain Lender ID Policy
-------------------------------------------------
mkLenderIdPolicy :: AppName -> ValidatorHash -- ^ Extra parameters.
                 -> LenderIdRedeemer -> ScriptContext -> Bool
mkLenderIdPolicy appName loanValHash r ctx@ScriptContext{scriptContextTxInfo=info} = True

lenderIdPolicy :: MintingPolicy
lenderIdPolicy = Plutonomy.optimizeUPLC $ mkMintingPolicyScript
    ($$(PlutusTx.compile [|| wrap ||])
      `PlutusTx.applyCode` PlutusTx.liftCode app
      `PlutusTx.applyCode` PlutusTx.liftCode loanValidatorHash)
  where
    wrap x y = mkUntypedMintingPolicy $ mkLenderIdPolicy x y

lenderIdPolicyScript :: Script
lenderIdPolicyScript = unMintingPolicyScript lenderIdPolicy

lenderIdPolicySymbol :: CurrencySymbol
lenderIdPolicySymbol = scriptCurrencySymbol lenderIdPolicy

-------------------------------------------------
-- On-Chain Phase Beacon Policy
-------------------------------------------------
mkPhaseBeaconPolicy :: AppName -> ValidatorHash  -> CurrencySymbol -> CurrencySymbol -- ^ Extra parameters
                    -> PhaseRedeemer -> ScriptContext -> Bool
mkPhaseBeaconPolicy appName dappHash borrowerIdSym lenderIdSym
                    r ctx@ScriptContext{scriptContextTxInfo = info} = case r of
    MintAskToken -> True
      -- | Must be minted to an address protected by the dappHash.
      -- | Must be minted to an address with a staking credential.
      -- | Only one ask token can be minted this tx.
      -- | The ask token must be stored with the proper ask datum.
      --     - askBeacon == (phaseBeaconSym,"Ask")
      --     - borrowerId == (borrowerIdSym,staking cred as token)
      --     - loanQuantity > 0
      --     - loanTerm > 0
      --     - collateral list not empty
      -- | The ask token must be stored with the proper borrower ID.
      --     - The borrower ID must match that of the staking credential for the destination address.
      -- | The receiving staking credential must signal approval.
    MintOfferToken pkh -> True
      -- | Must be minted to an address protected by the dappHash.
      -- | Must be minted to an address with a staking credential.
      -- | Only one offer token can be minted this tx.
      -- | The offer token must be stored with the proper offer datum.
      --     - offerBeacon == (phaseBeaconSym,"Offer")
      --     - lenderId == (lenderIdSym,pubKeyAsToken pkh)
      --     - loanQuantity > 0.
      --     - loanTerm > 0.
      --     - loanInterest > 0.
      --     - all collaterale rates > 0.
      --     - offerExperation > 0.
      -- | The offer token must be stored with the proper lender ID.
      --     - The lender ID must match the pkh passed with the redeemer.
      -- | The lender must sign the tx.
    MintActiveToken -> True
      -- | There must only be one ask token among the inputs.
      -- | There must only be one offer token among the inputs.
      -- | The ask token and the offer token inputs must come from the same address.
      -- | The ask datum and offer datum must agree.
      --     - loanAsset in ask datum == loanAsset in offer datum
      --     - loanQuantity in ask datum == loanQuantity in offer datum
      --     - loanTerm in ask datum == loan term in offer datum
      --     - collateral in ask datum == map fst collateralRates in offer datum
      -- | The ask token must be burned in the tx.
      -- | The offer token must be burned in the tx.
      -- | The active token must be minted to the address where the ask and offer tokens originate.
      -- | The active token must be stored with the proper active datum.
      --     - activeBeacon == (phaseBeaconSym,"Active")
      --     - lenderId == lenderId from offer datum in input
      --     - borrowerId == borrowerId from ask datum in input
      --     - loanAsset == loanAsset from ask datum (same as offer datum)
      --     - loanQuantity == loanQuantity from ask datum (same as offer datum)
      --     - loanTerm == loanTerm from ask datum (same as offer datum)
      --     - loanInterest == loanInterest from offer datum
      --     - collateralRates == collateralRates from offer datum
      --     - loanExpiration == ttl + loanTerm
      --     - loanOutstanding == loan
    BurnPhaseToken -> True
      -- | Allowed as long as redeemer only used to burn.
                
phaseBeaconPolicy :: MintingPolicy
phaseBeaconPolicy = Plutonomy.optimizeUPLC $ mkMintingPolicyScript
  ($$(PlutusTx.compile [|| wrap ||])
    `PlutusTx.applyCode` PlutusTx.liftCode app
    `PlutusTx.applyCode` PlutusTx.liftCode loanValidatorHash
    `PlutusTx.applyCode` PlutusTx.liftCode borrowerIdPolicySymbol
    `PlutusTx.applyCode` PlutusTx.liftCode lenderIdPolicySymbol)
  where
    wrap x y z w = mkUntypedMintingPolicy $ mkPhaseBeaconPolicy x y z w

phaseBeaconPolicyScript :: Script
phaseBeaconPolicyScript = unMintingPolicyScript phaseBeaconPolicy

phaseBeaconPolicySymbol :: CurrencySymbol
phaseBeaconPolicySymbol = scriptCurrencySymbol phaseBeaconPolicy