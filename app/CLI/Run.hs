{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module CLI.Run
  (
    runCommand
  ) where

import Relude
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Prettyprinter
import Prettyprinter.Render.Terminal
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import qualified Data.ByteString as SBS
import Data.FileEmbed

import CardanoLoans

import CLI.Data.Commands
import CLI.Data.Output
import CLI.Data.Network
import CLI.Data.PersonalUTxO
import CLI.Data.LoanUTxO
import CLI.Data.Bech32Address
import CLI.Data.CreditHistory
import CLI.Data.LoanHistory
import CLI.Query

preprodParams :: SBS.ByteString
preprodParams = $(embedFile "preprod-params.json")

mainnetParams :: SBS.ByteString
mainnetParams = $(embedFile "mainnet-params.json")

runCommand :: Command -> IO ()
runCommand cmd = case cmd of
  ExportScript script file -> runExportScriptCmd script file
  CreateDatum protocolDatum file -> runCreateDatum protocolDatum file
  CreateRedeemer newRedeemer file -> runCreateRedeemer newRedeemer file
  BeaconName info output -> runBeaconName info output
  Query query -> runQuery query
  ConvertTime convert network -> runTimeConversion convert network
  SubmitTx network api txFile -> 
    runSubmitTx network api txFile >>= LBS.putStr . encode
  EvaluateTx network api txFile -> 
    runEvaluateTx network api txFile >>= LBS.putStr . encode
  ExportParams network output -> runExportParams network output

runExportParams :: Network -> Output -> IO ()
runExportParams network output = case (network,output) of
  (PreProdTestnet,Stdout) -> SBS.putStr preprodParams
  (PreProdTestnet,File file) -> SBS.writeFile file preprodParams
  (Mainnet,Stdout) -> SBS.putStr mainnetParams
  (Mainnet,File file) -> SBS.writeFile file mainnetParams

runExportScriptCmd :: Script -> FilePath -> IO ()
runExportScriptCmd script file = do
  flip whenLeftM_ (\e -> putStrLn $ "There was an error: " <> show e) $
    writeScript file $ case script of
      NegotiationBeaconScript -> negotiationBeaconScript
      ActiveBeaconScript -> activeBeaconScript
      PaymentObserverScript -> paymentObserverScript
      InterestObserverScript -> interestObserverScript
      AddressUpdateObserverScript -> addressUpdateObserverScript
      LoanScript -> loanScript
      ProxyScript -> proxyScript

runCreateRedeemer :: NewRedeemer -> FilePath -> IO ()
runCreateRedeemer (NewNegotiation redeemer) file = writeData file redeemer
runCreateRedeemer (NewLoan redeemer) file = writeData file redeemer
runCreateRedeemer (NewActive redeemer) file = writeData file redeemer
runCreateRedeemer (NewPaymentObserver redeemer) file = writeData file redeemer
runCreateRedeemer (NewInterestObserver redeemer) file = writeData file redeemer
runCreateRedeemer (NewAddressUpdateObserver redeemer) file = writeData file redeemer

runBeaconName :: BeaconName -> Output -> IO ()
runBeaconName name output = 
    displayName $ case name of
      NegotiationPolicyId -> show negotiationBeaconCurrencySymbol
      ActivePolicyId -> show activeBeaconCurrencySymbol
      AssetBeaconName loanAsset -> 
        showTokenName $ _unAssetBeacon $ genLoanAssetBeaconName loanAsset
      AskBeaconName -> showTokenName askBeaconName
      OfferBeaconName -> showTokenName offerBeaconName
      ActiveBeaconName -> showTokenName activeBeaconName
      LenderIdName lenderCred -> 
        showTokenName $ _unLenderId $ genLenderId lenderCred
      LoanIdName offerId -> 
        showTokenName $ _unLoanId $ genLoanId offerId
  where
    displayName :: String -> IO ()
    displayName = case output of
      Stdout -> putStr
      File file -> writeFile file

runTimeConversion :: ConvertTime -> Network -> IO ()
runTimeConversion time network =
    case time of
      (POSIXTimeToSlot p) -> print $ getSlot $ posixTimeToSlot config p
      (SlotToPOSIXTime s) -> print $ getPOSIXTime $ slotToPOSIXTime config s
  where
    config = case network of
      Mainnet -> mainnetConfig
      PreProdTestnet -> preprodConfig

runCreateDatum :: NewDatum -> FilePath -> IO ()
runCreateDatum (NewAsk newAsk) file = writeData file $ unsafeCreateAskDatum newAsk
runCreateDatum (NewOffer newOffer) file = writeData file $ unsafeCreateOfferDatum newOffer
runCreateDatum (NewActiveManual newActive) file = writeData file $ unsafeCreateActiveDatum newActive
runCreateDatum (NewActiveAuto network endpoint offerId start borrowerCred) file = do
  utxo <- runQuerySpecificLoanUTxO network endpoint offerId
  case utxo of
    [LoanUTxO{_loanDatum=Just (Offer datum)}] -> 
      writeData file $ createAcceptanceDatumFromOffer borrowerCred offerId start datum
    _ -> error "Not an Offer UTxO."
runCreateDatum (NewPayment loanId) file = 
  writeData file $ PaymentDatum (activeBeaconCurrencySymbol,_unLoanId loanId)
runCreateDatum (NewPostPaymentActiveManual datum) file = 
  writeData file $ unsafeCreatePostPaymentActiveDatum datum
runCreateDatum (NewPostPaymentActiveAuto network endpoint loanRef paymentAmount) file = do
  utxo <- runQuerySpecificLoanUTxO network endpoint loanRef
  case utxo of
    [LoanUTxO{_loanDatum=Just (Active datum)}] -> 
      writeData file $ createPostPaymentActiveDatum paymentAmount datum
    _ -> error "Not an Active UTxO."
runCreateDatum (NewPostInterestActiveManual datum) file = 
  writeData file $ unsafeCreatePostInterestActiveDatum datum
runCreateDatum (NewPostInterestActiveAuto network endpoint loanRef numberOfTimes) file = do
  utxo <- runQuerySpecificLoanUTxO network endpoint loanRef
  case utxo of
    [LoanUTxO{_loanDatum=Just (Active datum)}] -> 
      writeData file $ createPostInterestActiveDatum numberOfTimes datum
    _ -> error "Not an Active UTxO."
runCreateDatum (NewPostAddressUpdateActiveManual datum) file = 
  writeData file $ unsafeCreatePostAddressUpdateActiveDatum datum
runCreateDatum (NewPostAddressUpdateActiveAuto network endpoint loanRef newAddr) file = do
  utxo <- runQuerySpecificLoanUTxO network endpoint loanRef
  case utxo of
    [LoanUTxO{_loanDatum=Just (Active datum)}] -> 
      writeData file $ (datum{_lenderAddress = newAddr} :: ActiveDatum)
    _ -> error "Not an Active UTxO."

runQuery :: Query -> IO ()
runQuery query = case query of
  QueryCurrentSlot network api -> runQuerySlotTip network api >>= print
  QueryPersonal network api addr keysOnly format output ->
    runQueryPersonalAddress network api addr keysOnly >>= 
      case format of
        JSON -> toJSONOutput output
        Pretty -> toPrettyOutput output 
                . (<> hardline) 
                . (personalHeader <>) 
                . vsep 
                . map prettyPersonalUTxO 
        Plain -> toPlainOutput output 
                . (<> hardline) 
                . (personalHeader <>) 
                . vsep 
                . map prettyPersonalUTxO
  QueryAsks network api mLoanAsset collateral mBorrowerCred format output -> do
    let mBorrowerAddr = genLoanAddress network <$> mBorrowerCred
        mAssetBeacon = genLoanAssetBeaconName <$> mLoanAsset
    runQueryAsks network api mAssetBeacon collateral mBorrowerAddr >>= case format of
      JSON -> toJSONOutput output
      Pretty -> toPrettyOutput output . (<> hardline) . vsep . map (prettyLoanUTxO network)
      Plain -> toPlainOutput output . (<> hardline) . vsep . map (prettyLoanUTxO network)
  QueryOffers network api mLoanAsset mBorrowerCred mLenderCred format output -> do
    let mBorrowerAddr = genLoanAddress network <$> mBorrowerCred
        mLenderId = genLenderId <$> mLenderCred
        mAssetBeacon = genLoanAssetBeaconName <$> mLoanAsset
    runQueryOffers network api mAssetBeacon mBorrowerAddr mLenderId >>= case format of
      JSON -> toJSONOutput output
      Pretty -> toPrettyOutput output . (<> hardline) . vsep . map (prettyLoanUTxO network)
      Plain -> toPlainOutput output . (<> hardline) . vsep . map (prettyLoanUTxO network)
  QueryActives network api mLoanAsset mBorrowerCred mLoanId format output -> do
    let mBorrowerAddr = genLoanAddress network <$> mBorrowerCred
        mAssetBeacon = genLoanAssetBeaconName <$> mLoanAsset
    runQueryActives network api mAssetBeacon mBorrowerAddr mLoanId >>= case format of
      JSON -> toJSONOutput output
      Pretty -> toPrettyOutput output . (<> hardline) . vsep . map (prettyLoanUTxO network)
      Plain -> toPlainOutput output . (<> hardline) . vsep . map (prettyLoanUTxO network)
  QueryBorrowerCreditHistory network api borrowerId format output -> do
    runQueryBorrowerHistory network api borrowerId >>= case format of
      JSON -> toJSONOutput output
      Pretty -> toPrettyOutput output . (<> hardline) . vsep . map (prettyCreditHistory network)
      Plain -> toPlainOutput output . (<> hardline) . vsep . map (prettyCreditHistory network)
  QueryLoanHistory network api loanId format output -> do
    runQueryLoanHistory network api loanId >>= case format of
      JSON -> toJSONOutput output
      Pretty -> toPrettyOutput output . (<> hardline) . vsep . map (prettyLoanHistory network)
      Plain -> toPlainOutput output . (<> hardline) . vsep . map (prettyLoanHistory network)

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
toPlainOutput :: Output -> Doc AnsiStyle -> IO ()
toPlainOutput Stdout xs = TIO.putStr $ T.pack $ show $ unAnnotate xs
toPlainOutput (File file) xs = TIO.writeFile file $ T.pack $ show xs

toPrettyOutput :: Output -> Doc AnsiStyle -> IO ()
toPrettyOutput Stdout xs = putDoc xs
toPrettyOutput (File file) xs = 
  TIO.writeFile file $ renderStrict $ layoutPretty defaultLayoutOptions xs

toJSONOutput :: (ToJSON a) => Output -> [a] -> IO ()
toJSONOutput Stdout xs = LBS.putStr $ encode xs
toJSONOutput (File file) xs = LBS.writeFile file $ encode xs

genLoanAddress :: Network -> Credential -> PaymentAddress
genLoanAddress network borrowerCred = 
  either error fst $ plutusToBech32 network $
    Address (ScriptCredential $ scriptHash loanScript) (Just $ StakingHash borrowerCred)

