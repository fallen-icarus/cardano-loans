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
  LoanDatum(..),
  LoanRedeemer(..),
  BeaconRedeemer(..),
  CurrencySymbol(..),
  TokenName(..),
  PaymentPubKeyHash(..),
  pubKeyAsToken,
  adaSymbol,
  adaToken,


  loanValidator,
  loanValidatorHash,
  loanValidatorScript,

  beaconPolicy,
  beaconPolicyScript,
  beaconPolicySymbol,

  writeData,
  writeScript,
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
      , loanPrinciple :: Integer
      , loanTerm :: POSIXTime
      , collateral :: [(CurrencySymbol,TokenName)]
      }
  -- | The datum for the offer ask.
  | OfferDatum
      { offerBeacon :: (CurrencySymbol,TokenName)
      , lenderId :: (CurrencySymbol,TokenName)
      , loanAsset :: (CurrencySymbol,TokenName)
      , loanPrinciple :: Integer
      , loanTerm :: POSIXTime
      , loanInterest :: Rational
      , loanDownPayment :: Integer
      , collateralRates :: [((CurrencySymbol,TokenName),Rational)]
      }
  -- | The datum for the active ask. This also has information useful for the credit history.
  | ActiveDatum
      { activeBeacon :: (CurrencySymbol,TokenName)
      , lenderId :: (CurrencySymbol,TokenName)
      , borrowerId :: (CurrencySymbol,TokenName)
      , loanAsset :: (CurrencySymbol,TokenName)
      , loanPrinciple :: Integer
      , loanTerm :: POSIXTime
      , loanInterest :: Rational
      , loanDownPayment :: Integer
      , collateralRates :: [((CurrencySymbol,TokenName),Rational)]
      , loanExpiration :: POSIXTime
      , loanOutstanding :: Rational
      }

instance Eq LoanDatum where
  {-# INLINABLE (==) #-}
  (AskDatum a b c d e f) == (AskDatum a' b' c' d' e' f') =
    a == a' && b == b' && c == c' && d == d' && e == e' && f == f'
  (OfferDatum a b c d e f g h) == (OfferDatum a' b' c' d' e' f' g' h') =
    a == a' && b == b' && c == c' && d == d' && e == e' && f == f' && g == g' && h == h'
  (ActiveDatum a b c d e f g h i j k) == (ActiveDatum a' b' c' d' e' f' g' h' i' j' k') =
    a == a' && b == b' && c == c' && d == d' && e == e' && f == f' && g == g' && h == h' && 
    i == i' && j == j' && k == k'
  _ == _ = False

data LoanRedeemer
  = CloseAsk
  | CloseOffer
  | AcceptOffer
  | RepayLoan
  | Claim

-- | The redeemer for the beacons.
data BeaconRedeemer
  -- | Mint the ask token to the borrower's address.
  = MintAskToken PaymentPubKeyHash -- ^ Pubkey for the borrower's staking credential. Simplifies logic.
  -- | Mint the offer token and lender ID.
  | MintOfferToken PaymentPubKeyHash -- ^ Pubkey for lender ID.
  -- | Mint the active token and the borrower ID.
  -- The first pubkey is the borrower's. The second one is the lender's.
  | MintActiveToken PaymentPubKeyHash PaymentPubKeyHash
  -- | Burn any token/IDs.
  | BurnBeaconToken

-- | A helper type used to create testing beacons.
type AppName = BuiltinString

PlutusTx.unstableMakeIsData ''LoanDatum
PlutusTx.unstableMakeIsData ''LoanRedeemer
PlutusTx.unstableMakeIsData ''BeaconRedeemer

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

{-# INLINABLE encodeDatum #-}
-- | This is a convenient way to check what kind of datum it is.
encodeDatum :: LoanDatum -> Integer
encodeDatum (AskDatum _ _ _ _ _ _) = 0
encodeDatum (OfferDatum _ _ _ _ _ _ _ _) = 1
encodeDatum (ActiveDatum _ _ _ _ _ _ _ _ _ _ _) = 2

{-# INLINABLE signed #-}
signed :: [PubKeyHash] -> PubKeyHash -> Bool
signed [] _ = False
signed (k:ks) k'
  | k == k' = True
  | otherwise = signed ks k'

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

-- | This is only used by the validator to prevent permanent locking when a staking script
-- is accidentally used. The beacons require that the address uses a staking pubkey.
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

-------------------------------------------------
-- On-Chain Loan Validator
-------------------------------------------------
-- | The purpose of this validator is to guarantee that loan negotiations and repayment go
-- smoothing without needing to trust the other party.
--
-- This validator uses the presence or absence of the beacon tokens to judge the validity of
-- the datums. This is due to the beacon tokens only being mintable when the datums are valid.
-- 
-- If there is ever a datum present WITHOUT the proper beacon token, the staking credential of 
-- the address has custody rights. This is to protect the address owner from malicious datums. 
-- It is therefore up to the lenders to ensure proper usage of this validator.
--
-- It is technically possible for a malicious user to create their own beacon beacon policy for use
-- with this validator. However, this would be an entirely different token than the actual beacons
-- which means they would not even be discoverable by other users.
--
-- Since the active utxo is time-locked for the borrower, there is no need to ensure that ONLY the
-- collateral assets ever leave. Those assets come from the borrower and the borrower has custody
-- of that utxo until the loan expires.
--
-- The beacon policy requires that the beacons can only be minted to an address with a staking
-- pubkey. However, there is no way to enforce this from the validator's side which means it is
-- possible to send funds to an address instance for this validator that uses a staking script.
-- Note that it would be impossible to actually broadcast this address with the beacons. However,
-- the funds would be permanently locked unless the validator allowed spending with a staking script
-- as well as a staking pubkey. To prevent this locking, the validator still checks if the staking 
-- script signals approval, too.
--
-- The interest for these loans are non-compounding.
mkLoan :: LoanDatum -> LoanRedeemer -> ScriptContext -> Bool
mkLoan loanDatum r ctx@ScriptContext{scriptContextTxInfo=info} = case r of
    CloseAsk ->
      -- | The datum must be an AskDatum.
      traceIfFalse "Datum is not an AskDatum" (encodeDatum loanDatum == 0) &&
      -- | There can only be one utxo spent from this address in this tx.
      traceIfFalse "Only one utxo can be spent from this address in this tx." (length allInputs == 1) &&
      -- | The address' staking credential must signal approval.
      traceIfFalse "Staking credential did not approve" stakingCredApproves' &&
      -- | If the ask beacon is present, it must be burned.
      traceIfFalse "Ask beacon not burned."
        (uncurry (valueOf inputValue) (askBeacon loanDatum) == 
           Num.negate (uncurry (valueOf $ txInfoMint info) (askBeacon loanDatum)))
    CloseOffer ->
      -- | The datum must be an OfferDatum.
      traceIfFalse "Datum is not an OfferDatum" (encodeDatum loanDatum == 1) &&
      -- | There can only be one utxo spent from this address in this tx.
      traceIfFalse "Only one utxo can be spent from this address in this tx." (length allInputs == 1) &&
      -- | If the offer beacon is present (also means lender ID is present):
      if uncurry (valueOf inputValue) (offerBeacon loanDatum) == 1
      then
        -- | The lender in the lender ID must sign the tx.
        traceIfFalse "Lender did not sign" 
             (signed (txInfoSignatories info) (tokenAsPubKey $ snd $ lenderId loanDatum)) &&
        -- | The offer beacon must be burned.
        traceIfFalse "Offer beacon not burned"
          (uncurry (valueOf $ txInfoMint info) (offerBeacon loanDatum) == -1) &&
        -- | The lender ID must be burned.
        traceIfFalse "Lender ID not burned"
          (uncurry (valueOf $ txInfoMint info) (lenderId loanDatum) == -1)
      -- Else (a sign of an invalid offer utxo):
      else
        -- | The staking credential must signal approval.
        traceIfFalse "Staking credential did not approve" stakingCredApproves'
    AcceptOffer ->
      -- | The staking credential must signal approval.
      traceIfFalse "Staking credential did not approve" stakingCredApproves' &&
      -- | The following function checks:
      -- 1) There must only be two inputs from this address.
      -- 2) One input must be an ask input.
      -- 3) One input must be an offer input.
      -- 4) The input datums must agree.
      -- 5) The ask input must have the ask beacon.
      -- 6) The offer input must have the offer beacon.
      validInputs &&
      -- | No other ask and offer beacons are in the tx inputs.
      noOtherInputBeacons &&
      -- | The following function checks:
      -- 1) There must only be one output to this address (checked implicitly).
      -- 2) The output must contain the proper datum.
      traceIfFalse "output datum is incorrect: must be ActiveDatum with correct info" 
        validActiveDatum &&
      -- | The required amount of collateral must be posted.
      traceIfFalse "Not enough collateral posted for loan" enoughCollateral &&
      -- | The following function checks:
      -- 1) The active beacon must be minted and stored in this address.
      -- 2) The borrower ID is minted.
      -- 3) The output contains the active beacon, the lender ID, and the borrower ID.
      -- 4) The ask beacon and the offer beacon are burned.
      -- 5) The ask beacon must have the same currency symbol as the offer beacon.
      traceIfFalse "Active beacon not minted to this address"
        (((valueOf $ txInfoMint info) (fst $ askBeacon askDatum) (TokenName "Active") == 1) &&
         ((valueOf oVal) (fst $ askBeacon askDatum) (TokenName "Active") == 1))
    RepayLoan ->
      -- | The input must have an ActiveDatum.
      traceIfFalse "Datum is not an ActiveDatum" (encodeDatum loanDatum == 2) &&
      -- | There can only be one utxo spent from this address.
      traceIfFalse "Only one utxo can be spent from this address in this tx." (length allInputs == 1) &&
      -- | If the input has the active beacon:
      if uncurry (valueOf inputValue) (activeBeacon loanDatum) == 1
      then
        -- | The loan must not be expired.
        traceIfFalse "Loan is expired" (not $ loanIsExpired $ loanExpiration loanDatum) &&
        -- | There can only be one output to this address. Checked by the next check.
        -- | The output must have the proper datum.
        --     - same as input datum except must subtract loan repaid from loanOutstanding.
        traceIfFalse "Output to address has wrong datum" 
          ((parseLoanDatum od) == loanDatum{loanOutstanding = newOutstanding}) &&
        -- | sum (collateral asset taken * collateralRate) * (1 + interest) <= loan asset repaid
        traceIfFalse "Fail: collateralTaken / collateralization * (1 + interest) <= loanRepaid"
          repaymentCheck &&
        -- | If new loanOutstanding <= 0):
        if newOutstanding <= fromInteger 0
        then
          -- | The borrower ID must be burned.
          traceIfFalse "Borrower ID not burned" 
            (uncurry (valueOf $ txInfoMint info) (borrowerId loanDatum) == -1) &&
          -- | The output must have the active beacon and the lender ID.
          traceIfFalse "Output must have active beacon and lender ID"
            (uncurry (valueOf oVal) (lenderId loanDatum) == 1 &&
             uncurry (valueOf oVal) (activeBeacon loanDatum) == 1)
        -- Else:
        else
          -- | The output must have the active beacon, borrower ID, and lender ID.
          traceIfFalse "Output must have active beacon, borrower ID, and lender ID"
            (uncurry (valueOf oVal) (lenderId loanDatum) == 1 &&
             uncurry (valueOf oVal) (activeBeacon loanDatum) == 1 &&
             uncurry (valueOf oVal) (borrowerId loanDatum) == 1)
      -- Else (a sign of an invalid active utxo):
      else
        -- | The staking credential must signal approval.
        traceIfFalse "Staking credential did not approve" stakingCredApproves'
    Claim ->
      -- | The datum must be an ActiveDatum.
      traceIfFalse "Datum is not an ActiveDatum" (encodeDatum loanDatum == 2) &&
      -- | There can only be one utxo spent from this address. This is due to how
      -- the credit history is updated.
      traceIfFalse "Only one utxo can be spent from this address in this tx." (length allInputs == 1) &&
      -- | The input utxo must have an active beacon. This also ensures a lender ID is present.
      traceIfFalse "UTxO does not have an active beacon" 
        (uncurry (valueOf inputValue) (activeBeacon loanDatum) == 1) &&
      -- | The active beacon must be burned.
      traceIfFalse "Active beacon not burned" 
            (uncurry (valueOf $ txInfoMint info) (activeBeacon loanDatum) == -1) &&
      -- | The lender ID must be burned.
      traceIfFalse "Lender ID not burned" 
            (uncurry (valueOf $ txInfoMint info) (lenderId loanDatum) == -1) &&
      -- | The loan must be expired.
      traceIfFalse "Loan not expired" (loanIsExpired $ loanExpiration loanDatum) &&
      -- | The lender must sign the tx.
      traceIfFalse "Lender did not sign" 
        (signed (txInfoSignatories info) (tokenAsPubKey $ snd $ lenderId loanDatum)) &&
      -- | If the borrower ID is still present, it must be burned.
      traceIfFalse "Borrower ID not burned."
        (uncurry (valueOf inputValue) (borrowerId loanDatum) == 
           Num.negate (uncurry (valueOf $ txInfoMint info) (borrowerId loanDatum)))
  where
    -- | Get the credential for this input as well as its value.
    -- Credential is used to check asset flux for address and ensure staking credential approves 
    -- when necessary. The value is used to quickly check for beacon tokens.
    (inputCredentials,inputValue) = 
      let TxOut{txOutAddress=addr,txOutValue=iVal} = ownInput ctx
      in (addr,iVal)

    -- | This tends to build up a thunk so its evaluation is forced even though it is not always
    -- needed.
    stakingCredApproves' :: Bool
    !stakingCredApproves' = stakingCredApproves inputCredentials info

    -- | Returns a list of inputs from this address.
    allInputs :: [TxOut]
    allInputs =
      let inputs = txInfoInputs info
          foo _ acc [] = acc
          foo iCred !acc (TxInInfo{txInInfoResolved=x@TxOut{txOutAddress=addr}}:xs) =
            if addr == iCred
            then foo iCred (x : acc) xs
            else acc
      in foo inputCredentials [] inputs

    -- | Get the loan repayment time from the tx's validity range.
    -- It uses to upper bound of the tx's validity range so that a borrower can't
    -- set an earlier time than has already passed to trick the script.
    repaymentTime :: POSIXTime
    repaymentTime = case (\(UpperBound t _) -> t) $ ivTo $ txInfoValidRange info of
      PosInf -> traceError "TTE not specified"
      Finite t -> t
      _ -> traceError "Shouldn't be NegInf."

    -- | Check if the expiration has passed.
    loanIsExpired :: POSIXTime -> Bool
    loanIsExpired endTime = repaymentTime > endTime

    -- | Gets the output to this address. 
    -- Throws an error if there is more than one since all redeemers require no more than
    -- one output to this address.
    TxOut{txOutValue=oVal,txOutDatum = od} = 
      let outputs = txInfoOutputs info
          foo _ acc [] = acc
          foo iCred !acc (x@TxOut{txOutAddress = addr}:xs) =
            if addr == iCred
            then if null acc
                 then foo iCred (x : acc) xs
                 else traceError "There can only be one output to address"
            else acc
      in head $ foo inputCredentials [] outputs

    -- | The value flux of this address.
    -- Positive values mean the address gained the asset.
    -- Negative values mean the address lost the asset.
    addrDiff :: Value
    addrDiff = oVal <> Num.negate inputValue

    repaidAmount :: Rational
    repaidAmount = fromInteger $ uncurry (valueOf addrDiff) $ loanAsset loanDatum

    -- | This is converted into units of the loan asset by dividing by the collateral rates.
    collateralReclaimed :: Rational
    collateralReclaimed = 
      let foo _ acc [] = acc
          foo val !acc ((collatAsset,price):xs) = 
            foo val 
                (acc + Ratio.negate (fromInteger $ uncurry (valueOf val) collatAsset) * recip price)
                xs
      in foo addrDiff (fromInteger 0) (collateralRates loanDatum)

    -- | This checks that enough collateral is posted when a loan offer is accepted. It uses the
    -- loanDownPayment to determine validity.
    enoughCollateral :: Bool
    enoughCollateral =
      let foo _ acc [] = acc
          foo val !acc ((collatAsset,price):xs) =
            foo val
                (acc + (fromInteger $ uncurry (valueOf val) collatAsset) * recip price)
                xs
      in foo oVal (fromInteger 0) (collateralRates offerDatum) >= fromInteger (loanDownPayment offerDatum)

    repaymentCheck :: Bool
    repaymentCheck = 
      collateralReclaimed * (fromInteger 1 + loanInterest loanDatum) <= repaidAmount

    -- | New total of loan outstanding.
    newOutstanding :: Rational
    newOutstanding = loanOutstanding loanDatum - repaidAmount

    -- | This will throw an error if there are not only two inputs or if the two inputs are not
    -- an ask input and an offer input.
    (askVal,askDatum,offerVal,offerDatum) =
      case allInputs of
        [TxOut{txOutDatum=xd,txOutValue=xVal},TxOut{txOutDatum=yd,txOutValue=yVal}] ->
          let xd' = parseLoanDatum xd
              yd' = parseLoanDatum yd
          in if encodeDatum xd' == 0 && encodeDatum yd' == 1 then (xVal,xd',yVal,yd')
             else if encodeDatum xd' == 1 && encodeDatum yd' == 0 then (yVal,yd',xVal,xd')
             else traceError "Inputs are not the right phases"
        _ -> traceError "There must be two and only two inputs from this address"

    -- | Ensures that the borrower and lender are agreeing to the same loan.
    datumsAgree :: Bool
    datumsAgree
      | fst (askBeacon askDatum) /= fst (offerBeacon offerDatum) =
          traceError "Datums are using different beacons"
      | loanAsset askDatum /= loanAsset offerDatum =
          traceError "Datums have different loan assets"
      | loanPrinciple askDatum /= loanPrinciple offerDatum =
          traceError "Datums have different loan quantities"
      | loanTerm askDatum /= loanTerm offerDatum =
          traceError "Datums have different loan terms"
      | collateral askDatum /= map fst (collateralRates offerDatum) =
          traceError "Datums have different collateral assets"
      | otherwise = True

    -- | Checks that the datums agree and the proper beacons are present.
    validInputs :: Bool
    validInputs = datumsAgree && 
      traceIfFalse "Ask beacon is missing" (uncurry (valueOf askVal) (askBeacon askDatum) == 1) &&
      traceIfFalse "Offer beacon is missing" (uncurry (valueOf offerVal) (offerBeacon offerDatum) == 1)

    noOtherInputBeacons :: Bool
    noOtherInputBeacons =
      let allVal = valueSpent info
          offerBeacon' = offerBeacon offerDatum
          askBeacon' = askBeacon askDatum
      in traceIfFalse "No other ask and offer beacons are allowed in tx" $
        uncurry (valueOf allVal) askBeacon' == uncurry (valueOf askVal) askBeacon' &&
        uncurry (valueOf allVal) offerBeacon' == uncurry (valueOf offerVal) offerBeacon'
    
    -- | Get the loan expiration time from the tx's validity range. Based off the lower bound.
    expirationTime :: POSIXTime
    expirationTime = case (\(LowerBound t _) -> t) $ ivFrom $ txInfoValidRange info of
      NegInf -> traceError "TTL not specified"
      Finite t -> t + loanTerm offerDatum
      _ -> traceError "Shouldn't be PosInf."

    -- | Ensures the new active datum has the proper information.
    validActiveDatum :: Bool
    validActiveDatum = parseLoanDatum od == ActiveDatum
      { activeBeacon = (fst $ askBeacon askDatum,TokenName "Active")
      , lenderId = lenderId offerDatum
      , borrowerId = borrowerId askDatum
      , loanAsset = loanAsset askDatum
      , loanPrinciple = loanPrinciple askDatum
      , loanTerm = loanTerm askDatum
      , loanInterest = loanInterest offerDatum
      , loanDownPayment = loanDownPayment offerDatum
      , collateralRates = collateralRates offerDatum
      , loanExpiration = expirationTime
      , loanOutstanding = 
          fromInteger (loanPrinciple askDatum) * (fromInteger 1 + loanInterest offerDatum)
      }

data Loan
instance ValidatorTypes Loan where
  type instance RedeemerType Loan = LoanRedeemer
  type instance DatumType Loan = LoanDatum

loanValidator :: Validator
loanValidator = Plutonomy.optimizeUPLC $ validatorScript $ mkTypedValidator @Loan
    $$(PlutusTx.compile [|| mkLoan ||])
    $$(PlutusTx.compile [|| wrap ||])
  where wrap = mkUntypedValidator

loanValidatorScript :: Script
loanValidatorScript = unValidatorScript loanValidator

loanValidatorHash :: ValidatorHash
loanValidatorHash = Scripts.validatorHash loanValidator

-------------------------------------------------
-- On-Chain Beacon Policy
-------------------------------------------------
mkBeaconPolicy :: AppName -> ValidatorHash  -- ^ Extra parameters
               -> BeaconRedeemer -> ScriptContext -> Bool
mkBeaconPolicy appName dappHash r ctx@ScriptContext{scriptContextTxInfo = info} = case r of
    MintAskToken (PaymentPubKeyHash pkh) ->
      -- | Only one ask token can be minted this tx and it must have the token name "Ask".
      mintCheck r &&
      -- | The following function checks:
      -- 1) Must be minted to an address protected by the dappHash.
      -- 2) Must be minted to an address with a staking pubkey matching the supplied pubkey.
      -- 3) The ask token must be stored with the proper ask datum.
      --     - askBeacon == (beaconSym, TokenName "Ask")
      --     - borrowerId == (beaconSym,pubkeyAsToken pkh)
      --     - loanPrinciple > 0
      --     - loanTerm > 0
      --     - collateral list not empty
      destinationCheck r &&
      -- | The receiving staking pubkey must sign.
      traceIfFalse "Receiving staking pubkey did not sign" (signed (txInfoSignatories info) pkh)
    MintOfferToken (PaymentPubKeyHash pkh) ->
      -- | The following function checks:
      -- 1) Must mint exactly one offer token and exactly one lender ID.
      -- 2) The offer token must have the token name "Offer".
      -- 3) The lender token must have the lender's pubkey as its token name.
      mintCheck r &&
      -- | The following function checks:
      -- 1) Must be minted to an address protected by the dappHash.
      -- 2) Must be minted to an address with a staking pubkey.
      -- 3) The offer token and lender ID must be stored in the same utxo at the dapp address.
      -- 4) The tokens must be stored with the proper offer datum.
      --     - offerBeacon == (beaconSym,TokenName "Offer")
      --     - lenderId == (beaconSym,pubKeyAsToken pkh)
      --     - loanPrinciple > 0.
      --     - loanTerm > 0.
      --     - loanInterest > 0.
      --     - collateralRates not null
      --     - all collaterale rates > 0.
      -- 5) The offer token must be stored with the amount of the loan asset specified in
      -- the datum. If the loan asset is ADA, an additional 3 ADA is required to account for
      -- the presence of the beacon tokens.
      destinationCheck r &&
      -- | The lender pkh must sign the tx.
      traceIfFalse "Lender did not sign tx" (signed (txInfoSignatories info) pkh)
    MintActiveToken _ _ ->
      -- | The following function checks:
      -- 1) Both the ask token and the offer token must be burned in the tx.
      -- 2) Must mint exactly one active token and one borrower ID.
      -- 3) The active token must have the token name "Active".
      -- 4) The borrower ID must have the borrower's pubkey as the token name.
      mintCheck r &&
      -- | The active token, the borrower ID, and the lender ID (from the offer input) must
      -- be stored in the same utxo at the dapp address using the supplied staking pubkey.
      destinationCheck r
    BurnBeaconToken ->
      -- | Allowed as long as redeemer only used to burn.
      mintCheck r
  where
    beaconSym :: CurrencySymbol
    beaconSym = ownCurrencySymbol ctx

    -- | Returns only the beacons minted/burned. This is useful for ensuring only
    -- the required beacons are minting.
    beaconMint :: [(CurrencySymbol,TokenName,Integer)]
    beaconMint = case Map.lookup beaconSym $ getValue $ txInfoMint info of
      Nothing -> traceError "MintError"
      Just bs -> flattenValue $ Value $ Map.insert beaconSym bs Map.empty -- ^ a Value with only beacons

    mintSatisfied :: TokenName -> Integer -> [(CurrencySymbol,TokenName,Integer)] -> Bool
    mintSatisfied _ _ [] = False
    mintSatisfied tn n ((_,tn',n'):cs)
      | tn == tn' && n == n' = True
      | otherwise = mintSatisfied tn n cs

    mintCheck :: BeaconRedeemer -> Bool
    mintCheck r' = case (r',beaconMint) of
      (MintAskToken _, [(_,tn,n)]) ->
        traceIfFalse "Only the ask beacon must have the token name 'Ask'" (tn == (TokenName "Ask")) &&
        traceIfFalse "Only one ask beacon can be minted" (n == 1)
      (MintAskToken _, _) -> 
        traceError "Only one beacon can be minted and it must be the ask beacon."
      (MintOfferToken pkh, [(_,tn1,n1),(_,tn2,n2)]) ->
        if tn1 == TokenName "Offer" then
          traceIfFalse "Only one offer beacon can be minted" (n1 == 1) &&
          traceIfFalse "The lender ID does not have the lender's pubkey as token name" 
            (tn2 == pubKeyAsToken pkh) &&
          traceIfFalse "Only one lender ID can be minted" (n2 == 1)
        else if tn2 == TokenName "Offer" then
          traceIfFalse "Only one offer beacon can be minted" (n2 == 1) &&
          traceIfFalse "The lender ID does not have the lender's pubkey as token name" 
            (tn1 == pubKeyAsToken pkh) &&
          traceIfFalse "Only one lender ID can be minted" (n1 == 1)
        else traceError "Only thhe offer beacon and lender ID can be minted"
      (MintOfferToken _, _) ->
        traceError "Only the offer beacon and lender ID can be minted"
      (MintActiveToken pkh _, xs@[_,_,_,_]) ->
        traceIfFalse "One ask beacon not burned" (mintSatisfied (TokenName "Ask") (-1) xs) &&
        traceIfFalse "One offer beacon not burned" (mintSatisfied (TokenName "Offer") (-1) xs) &&
        traceIfFalse "One active beacon not minted" (mintSatisfied (TokenName "Active") 1 xs) &&
        traceIfFalse "One borrower ID not minted" (mintSatisfied (pubKeyAsToken pkh) 1 xs)
      (MintActiveToken _ _, _) ->
        traceError 
          "The offer and ask beacon must be burned while the active beacon and borrower ID must be minted"
      (BurnBeaconToken, xs) ->
        traceIfFalse "Beacons can only be burned with this redeemer" (all (\(_,_,n) -> n < 0) xs)

    -- | A helper function for destinationCheck to make the code easier to reason about.
    -- This uses the appName in the error message.
    validDestination :: ValidatorHash -> Bool
    validDestination spendVh
      | spendVh /= dappHash = 
          traceError ("Beacon not minted to the proper " <> appName <> " address")
      | otherwise = True

    validDatum :: BeaconRedeemer -> LoanDatum -> Bool
    validDatum (MintAskToken pkh) (AskDatum ab bi _ lq lt c)
      | ab /= (beaconSym,TokenName "Ask") = traceError "Invalid AskDatum askBeacon"
      | bi /= (beaconSym,pubKeyAsToken pkh) = traceError "Invalid AskDatum borrowerId"
      | lq <= 0 = traceError "AskDatum loanPrinciple not > 0"
      | lt <= 0 = traceError "AskDatum loanTerm not > 0"
      | null c = traceError "AskDatum collateral is empty"
      | otherwise = True
    validDatum (MintAskToken _) _ = traceError "Ask beacon not stored with an AskDatum"
    validDatum (MintOfferToken pkh) (OfferDatum ob li _ lq lt i dp cr)
      | ob /= (beaconSym, TokenName "Offer") = traceError "Invalid OfferDatum offerBeacon"
      | li /= (beaconSym, pubKeyAsToken pkh) = traceError "OfferDatum lenderId not correct"
      | lq <= 0 = traceError "OfferDatum loanPrinciple not > 0"
      | lt <= 0 = traceError "OfferDatum loanTerm not > 0"
      | i <= fromInteger 0 = traceError "OfferDatum loanInterest not > 0"
      | dp <= 0 = traceError "OfferDatum loanDownPayment not > 0"
      | null cr = traceError "OfferDatum collateralRates is empty"
      | not $ all (\(_,p) -> p > fromInteger 0) cr = traceError "All collateralRates must be > 0"
      | otherwise = True
    validDatum (MintOfferToken _) _ = traceError "Offer beacon not stored with an OfferDatum"
    validDatum _ _ = True -- ^ This is to stop the pattern match compile warning. It is not used
                          --   for any other redeemers.

    -- | This checks that the offer beacon is stored with the desired loan amount.
    loanPrincipleMet :: Value -> LoanDatum -> Bool
    loanPrincipleMet oVal d
      | loanAsset d == (adaSymbol,adaToken) = 
          uncurry (valueOf oVal) (loanAsset d) >= loanPrinciple d * 1_000_000 + 3_000_000
      | otherwise = uncurry (valueOf oVal) (loanAsset d) >= loanPrinciple d

    -- | Check if the beacons are going to the proper address and are stored properly (together and 
    -- with proper datum). This is only used for MintOfferToken and MintAskToken.
    destinationCheck :: BeaconRedeemer -> Bool
    destinationCheck r' =
      let outputs = txInfoOutputs info
          foo acc TxOut{txOutDatum=d
                       ,txOutValue=oVal
                       ,txOutAddress=Address{addressCredential=addrCred
                                            ,addressStakingCredential=maybeStakeCred
                                            }
                       } =
            case r' of
              MintAskToken (PaymentPubKeyHash pkh)->
                if valueOf oVal beaconSym (TokenName "Ask") == 1 then
                  case (addrCred,maybeStakeCred) of
                    (ScriptCredential vh, Just (StakingHash (PubKeyCredential pkh'))) ->
                      -- | validDestination and validDatum will both fail with traceError
                      -- unless True.
                      acc && validDestination vh && 
                      validDatum r' (parseLoanDatum d) && 
                      traceIfFalse "Receiving address does not match redeemer pubkey hash" (pkh == pkh')
                    _ -> traceError "Ask beacon must go to a dapp address using a staking pubkey"
                else acc
              MintOfferToken pkh ->
                if valueOf oVal beaconSym (TokenName "Offer") == 1 then 
                  if valueOf oVal beaconSym (pubKeyAsToken pkh) == 1 then
                    case (addrCred,maybeStakeCred) of
                      (ScriptCredential vh, Just (StakingHash (PubKeyCredential _))) ->
                        -- | validDestination and validDatum will both fail with traceError
                        -- unless True.
                        let datum = parseLoanDatum d
                        in 
                          acc && validDestination vh && validDatum r' datum && 
                          traceIfFalse "Offer beacon not stored with required loan amount" 
                            (loanPrincipleMet oVal datum)
                      _ -> traceError "Offer beacon must go to a dapp address using a staking pubkey"
                  else traceError "Offer token and lender ID must be stored in the same utxo."
                else acc
              MintActiveToken borrowerPkh@(PaymentPubKeyHash pkh) lenderPkh ->
                if valueOf oVal beaconSym (TokenName "Active") == 1 then 
                  if valueOf oVal beaconSym (pubKeyAsToken borrowerPkh) == 1 then
                    if valueOf oVal beaconSym (pubKeyAsToken lenderPkh) == 1 then
                      case (addrCred,maybeStakeCred) of
                        (ScriptCredential vh, Just (StakingHash (PubKeyCredential pkh'))) ->
                          -- | validDestination will fail with traceError unless True.
                          acc && validDestination vh && pkh == pkh'
                        _ -> traceError 
                          "Active token must go to the dapp address using the supplied staking pubkey"
                    else traceError "Lender ID not stored with borrower ID and active beacon"
                  else traceError "Borrower ID not stored with active beacon"
                else acc
              _ -> False
      in foldl' foo True outputs


beaconPolicy :: MintingPolicy
beaconPolicy = Plutonomy.optimizeUPLC $ mkMintingPolicyScript
  ($$(PlutusTx.compile [|| wrap ||])
    `PlutusTx.applyCode` PlutusTx.liftCode app
    `PlutusTx.applyCode` PlutusTx.liftCode loanValidatorHash)
  where
    wrap x y = mkUntypedMintingPolicy $ mkBeaconPolicy x y

beaconPolicyScript :: Script
beaconPolicyScript = unMintingPolicyScript beaconPolicy

beaconPolicySymbol :: CurrencySymbol
beaconPolicySymbol = scriptCurrencySymbol beaconPolicy

-------------------------------------------------
-- Serialization
-------------------------------------------------
dataToScriptData :: Data -> ScriptData
dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (Map xs)      = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (List xs)     = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (I n)         = ScriptDataNumber n
dataToScriptData (B bs)        = ScriptDataBytes bs

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file 
               . encode 
               . scriptDataToJson ScriptDataJsonDetailedSchema 
               . dataToScriptData 
               . PlutusTx.toData

serialisedScript :: Script -> PlutusScript PlutusScriptV2
serialisedScript = PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise

writeScript :: FilePath -> Script -> IO (Either (FileError ()) ())
writeScript file script = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing
                         $ serialisedScript script

writeData :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeData = writeJSON