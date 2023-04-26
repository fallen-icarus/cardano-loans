module CLI.Run
(
  runCommand
) where

import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as BL

import CLI.Types
import CLI.Query
import CardanoLoans

runCommand :: Command -> IO ()
runCommand cmd = case cmd of
  ExportScript script file -> runExportScriptCmd script file
  CreateLoanDatum d file -> writeData file d
  CreateLoanRedeemer r file -> writeData file r
  CreateBeaconRedeemer r file -> writeData file r
  QueryBeacons query -> runQuery query
  Convert convert -> runConversion convert

runExportScriptCmd :: Script -> FilePath -> IO ()
runExportScriptCmd script file = do
  let script' = case script of
        LoanScript -> loanValidatorScript
        BeaconPolicy -> beaconPolicyScript
  res <- writeScript file script'
  case res of
    Right _ -> return ()
    Left err -> putStrLn $ "There was an error: " <> show err

runQuery :: Query -> IO ()
runQuery query = case query of
  QueryAllAsks network policyId output -> runQueryAllAsks network policyId >>= toOutput output
  QueryOwnAsks network policyId addr output ->
    runQueryOwnAsks network policyId addr >>= toOutput output
  QueryAllOffers network policyId addr output ->
    runQueryAllOffers network policyId addr >>= toOutput output
  QueryOwnOffers network policyId lenderPubKeyHash output -> 
    runQueryOwnOffers network policyId lenderPubKeyHash >>= toOutput output
  QueryAllBorrowerLoans network policyId borrowerStakeKeyHash addr output ->
    runQueryAllBorrowerLoans network policyId borrowerStakeKeyHash addr >>= toOutput output
  QueryAllLenderLoans network policyId lenderPubKeyHash output -> 
    runQueryAllLenderLoans network policyId lenderPubKeyHash >>= toOutput output
  QueryBorrowerHistory network policyId borrowerStakeKeyHash output ->
    runQueryBorrowerHistory network policyId borrowerStakeKeyHash >>= toOutput output
  QueryLenderHistory network policyId lenderPubKeyHash output ->
    runQueryLenderHistory network policyId lenderPubKeyHash >>= toOutput output

runConversion :: Convert -> IO ()
runConversion (POSIXTimeToSlot p) = print $ getSlot $ posixTimeToSlot p
runConversion (SlotToPOSIXTime s) = print $ getPOSIXTime $ slotToPOSIXTime s

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
toOutput :: (ToJSON a) => Output -> a -> IO ()
toOutput output xs = case output of
  Stdout -> BL.putStr $ encode xs
  File file -> BL.writeFile file $ encodePretty xs