module CLI.Run
(
  runCommand
) where

import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as BL

import CardanoLoans
import CLI.Query
import CLI.Types

runCommand :: Command -> IO ()
runCommand cmd = case cmd of
  ExportScript script file -> runExportScriptCmd script file
  BorrowerCmd borrowerCmd -> runBorrowerCmd borrowerCmd
  LenderCmd lenderCmd -> runLenderCmd lenderCmd

runExportScriptCmd :: Script -> FilePath -> IO ()
runExportScriptCmd script file = do
  let (name,script') = case script of
        Spending -> ("Loan validator",loanValidatorScript)
        Policy -> ("Beacon policy",beaconPolicyScript)
  res <- writeScript file script'
  case res of
    Right _ -> putStrLn $ name <> " script exported successfully."
    Left err -> putStrLn $ "There was an error: " <> show err

runBorrowerCmd :: BorrowerCmd -> IO ()
runBorrowerCmd borrowerCmd = case borrowerCmd of
  BorrowerAskDatum d file -> writeData file d >> putStrLn "Ask datum created successfully."
  BorrowerPaymentActiveDatum d file -> 
    writeData file d >> putStrLn "Active datum for loan payment created successfully."
  BorrowerAcceptActiveDatum d file ->
    writeData file d >> putStrLn "Active datum for accepting a loan created successfully."
  CreateAskBeaconRedeemer borrowerPubKey file -> do
    let redeemer = MintAskToken borrowerPubKey
    writeData file redeemer
    putStrLn "Beacon policy redeemer for minting Ask beacons created successfully."
  CreateActiveBeaconRedeemer borrowerPubKey lenderPubKey file -> do
    let redeemer = MintActiveToken borrowerPubKey lenderPubKey
    writeData file redeemer
    putStrLn "Beacon policy redeemer for minting Active beacons created successfully."
  CreateCloseAskRedeemer file -> do
    writeData file CloseAsk
    putStrLn "Loan validator redeemer for closing an Ask created successfully."
  CreateBorrowerBurnBeaconRedeemer file -> do
    writeData file BurnBeaconToken
    putStrLn "Beacon policy redeemer for burning beacons and IDs created successfully."
  CreateAcceptOfferRedeemer file -> do
    writeData file AcceptOffer
    putStrLn "Loan validator redeemer for accepting an Offer created successfully."
  CreateRepayRedeemer file -> do
    writeData file RepayLoan
    putStrLn "Loan validator redeemer for making a loan payment created successfully."
  QueryBorrowerCurrentAsks addr network output -> 
    runBorrowerAsksQuery addr network >>= toOutput output
  QueryBorrowerCurrentOffers addr network output -> 
    runAllOffersToBorrowerQuery addr network >>= toOutput output
  QueryBorrowerCurrentLoans borrowerPubKey network output -> 
    runAllBorrowersActiveLoansQuery (show borrowerPubKey) network >>= toOutput output
  ConvertPOSIXToSlot time -> print $ getSlot $ posixTimeToSlot time

runLenderCmd :: LenderCmd -> IO ()
runLenderCmd lenderCmd = case lenderCmd of
  CreateOfferDatum d file -> 
    writeData file d >> putStrLn "Offer datum for a new loan offer created successfully."
  CreateClaimRedeemer file ->
    writeData file Claim >> putStrLn "Claim redeemer created successfully."
  CreateOfferBeaconRedeemer lenderPubKey file -> do
    let redeemer = MintOfferToken lenderPubKey
    writeData file redeemer
    putStrLn "Beacon redeemer for minting offer beacons and IDs created successfully."
  CreateBurnBeaconRedeemer file -> do
    writeData file BurnBeaconToken
    putStrLn "Beacon policy redeemer for burning beacons and IDs created successfully."
  CreateCloseOfferRedeemer file -> do
    writeData file CloseOffer
    putStrLn "CloseOffer redeemer created successfully."
  QueryAllAsks network output -> runAllAsksQuery network >>= toOutput output
  QueryLenderCurrentOffers lenderPubKey network output -> 
    runAllOffersFromLenderQuery (show lenderPubKey) network >>= toOutput output
  QueryLenderCurrentLoans lenderPubKey network output -> 
    runAllLendersActiveLoansQuery (show lenderPubKey) network >>= toOutput output
  QueryBorrowerHistory borrowerPubKey network output ->
    runQueryBorrowerHistory (show borrowerPubKey) network >>= toOutput output 

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
toOutput :: (ToJSON a) => Output -> a -> IO ()
toOutput output xs = case output of
  Stdout -> BL.putStr $ encode xs
  File file -> BL.writeFile file $ encodePretty xs
