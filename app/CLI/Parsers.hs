module CLI.Parsers
(
  parseCommand
) where

import Options.Applicative
import Relude
import qualified Data.String as String

import CardanoLoans

import CLI.Data.Commands
import CLI.Data.Bech32Address
import CLI.Data.Network
import CLI.Data.ApiService
import CLI.Data.Output

-------------------------------------------------
-- Main Parsers
-------------------------------------------------
parseCommand :: Parser Command
parseCommand = hsubparser $ mconcat
  [ command "scripts" $
      info parseExportScript $ progDesc "Export a protocol plutus script."
  , command "datums" $
      info parseCreateDatum $ progDesc "Create a datum for the protocol."
  , command "redeemers" $
      info parseCreateRedeemer $ progDesc "Create a redeemer for the protocol."
  , command "beacon-name" $
      info parseBeaconName $ progDesc "Calculate a beacon policy id or asset name."
  , command "convert-time" $
      info pConvertTime $ progDesc "Convert POSIXTime <--> Slot."
  , command "query" $
      info parseQuery $ progDesc "Query the blockchain."
  , command "submit" $
      info pSubmitTx $ progDesc "Submit a transaction to the blockchain."
  , command "protocol-params" $
      info pExportParams $ progDesc "Export the current protocol parameters."
  , command "evaluate-tx" $
      info pEvaluateTx $ progDesc "Estimate script execution units for a transaction."
  ]

-------------------------------------------------
-- Scripts Parser
-------------------------------------------------
parseExportScript :: Parser Command
parseExportScript = 
    ExportScript
      <$> pScript
      <*> pOutputFile
  where
    pScript :: Parser Script
    pScript = pNegotiationScript
          <|> pActiveScript
          <|> pPaymentScript
          <|> pInterestScript
          <|> pAddressUpdateScript
          <|> pLoanScript
          <|> pProxyScript

    pNegotiationScript :: Parser Script
    pNegotiationScript = flag' NegotiationBeaconScript
      (  long "negotiation-script"
      <> help "Export the negotiation beacon script."
      )

    pActiveScript :: Parser Script
    pActiveScript = flag' ActiveBeaconScript
      (  long "active-script"
      <> help "Export the active beacon script."
      )

    pPaymentScript :: Parser Script
    pPaymentScript = flag' PaymentObserverScript
      (  long "payment-script"
      <> help "Export the payment observer script."
      )

    pInterestScript :: Parser Script
    pInterestScript = flag' InterestObserverScript
      (  long "interest-script"
      <> help "Export the interest observer script."
      )

    pAddressUpdateScript :: Parser Script
    pAddressUpdateScript = flag' AddressUpdateObserverScript
      (  long "address-update-script"
      <> help "Export the address update observer script."
      )

    pLoanScript :: Parser Script
    pLoanScript = flag' LoanScript
      (  long "loan-script"
      <> help "Export the loan spending script."
      )

    pProxyScript :: Parser Script
    pProxyScript = flag' ProxyScript
      (  long "proxy-script"
      <> help "Export the proxy script."
      )

-------------------------------------------------
-- CreateDatum Parser
-------------------------------------------------
parseCreateDatum :: Parser Command
parseCreateDatum = hsubparser $ mconcat
    [ command "ask" $
        info pCreateNewAskInfo $ progDesc "Create an AskDatum."
    , command "offer" $
        info pCreateNewOfferInfo $ progDesc "Create an OfferDatum."
    , command "active" $
        info pCreateActiveDatum $ progDesc "Create an ActiveDatum."
    , command "payment" $
        info pCreatePaymentDatum $ progDesc "Create a PaymentDatum."
    ]

pCreatePaymentDatum :: Parser Command
pCreatePaymentDatum = 
  CreateDatum 
    <$> (NewPayment <$> pLoanId)
    <*> pOutputFile

pCreateNewAskInfo :: Parser Command
pCreateNewAskInfo =
    CreateDatum
      <$> pNewAskInfo
      <*> pOutputFile
  where
    pNewAskInfo :: Parser NewDatum
    pNewAskInfo =
      fmap NewAsk $ NewAskInfo
        <$> pUserCredential "borrower"
        <*> pAsset "loan"
        <*> pPrincipal
        <*> pLoanTerm
        <*> some (pAsset "collateral")

pCreateNewOfferInfo :: Parser Command
pCreateNewOfferInfo =
    CreateDatum
      <$> pNewOfferInfo
      <*> pOutputFile
  where
    pNewOfferInfo :: Parser NewDatum
    pNewOfferInfo =
      fmap NewOffer $ NewOfferInfo
        <$> pUserCredential "lender"
        <*> pPaymentAddress
        <*> pAsset "loan"
        <*> pPrincipal
        <*> (pCompoundFrequency <|> pure Nothing)
        <*> pLoanTerm
        <*> pInterest
        <*> pMinPayment
        <*> pPenalty
        <*> pCollateralization
        <*> pIsSwappable
        <*> pClaimPeriod
        <*> pOfferDeposit
        <*> (pOfferExpiration <|> pure Nothing)

pCreateActiveDatum :: Parser Command
pCreateActiveDatum = hsubparser $ mconcat
  [ command "new" $
      info pCreateNewActiveInfo $ progDesc "Create a new ActiveDatum."
  , command "post-payment" $
      info pCreatePostPaymentActive $ progDesc "Create a post-payment ActiveDatum."
  , command "post-interest" $
      info pCreatePostInterestActive $ progDesc "Create a post-interest ActiveDatum."
  , command "post-address-update" $
      info pCreatePostAddressUpdateActive $ progDesc "Create a post-address-update ActiveDatum."
  ]

pCreateNewActiveInfo :: Parser Command
pCreateNewActiveInfo = hsubparser $ mconcat
    [ command "manual" $
        info pCreateNewActiveInfoManual $ progDesc "Create a new ActiveDatum manually."
    , command "auto" $
        info pCreateNewActiveInfoAuto $ progDesc "Create a new ActiveDatum by looking up the offer UTxO."
    ]

pCreateNewActiveInfoManual :: Parser Command
pCreateNewActiveInfoManual =
    CreateDatum
      <$> pNewActiveInfo
      <*> pOutputFile
  where
    pNewActiveInfo :: Parser NewDatum
    pNewActiveInfo =
      fmap NewActiveManual $ NewActiveInfo
        <$> pPaymentAddress
        <*> pAsset "loan"
        <*> pPrincipal
        <*> (pCompoundFrequency <|> pure Nothing)
        <*> pLoanTerm
        <*> pInterest
        <*> pMinPayment
        <*> pPenalty
        <*> pCollateralization
        <*> pIsSwappable
        <*> pClaimPeriod
        <*> pStartTime
        <*> pUserCredential "borrower"
        <*> pOfferId

pCreateNewActiveInfoAuto :: Parser Command
pCreateNewActiveInfoAuto =
    CreateDatum
      <$> pNewActive
      <*> pOutputFile
  where
    pNewActive :: Parser NewDatum
    pNewActive =
      NewActiveAuto
        <$> pNetwork
        <*> pApiService
        <*> pOfferId
        <*> pStartTime
        <*> pUserCredential "borrower"

pCreatePostPaymentActive :: Parser Command
pCreatePostPaymentActive = hsubparser $ mconcat
    [ command "manual" $
        info pCreatePostPaymentActiveManual $ 
          progDesc "Create a post-payment ActiveDatum manually."
    , command "auto" $
        info pCreatePostPaymentActiveAuto $ 
          progDesc "Create a post-payment ActiveDatum by looking up the loan UTxO."
    ]

pCreatePostPaymentActiveManual :: Parser Command
pCreatePostPaymentActiveManual =
    CreateDatum
      <$> pNewPaymentInfo
      <*> pOutputFile
  where
    pNewPaymentInfo :: Parser NewDatum
    pNewPaymentInfo =
      fmap NewPostPaymentActiveManual $ NewPaymentInfo
        <$> pUserCredential "borrower"
        <*> pPaymentAddress
        <*> pAsset "loan"
        <*> pPrincipal
        <*> (pCompoundFrequency <|> pure Nothing)
        <*> pLastCompounding
        <*> pLoanTerm
        <*> pInterest
        <*> pMinPayment
        <*> pPenalty
        <*> pCollateralization
        <*> pIsSwappable
        <*> pClaimExpiration
        <*> pLoanExpiration
        <*> pLoanOutstanding
        <*> pTotalEpochPayments
        <*> pLoanId
        <*> pPaymentAmount

pCreatePostPaymentActiveAuto :: Parser Command
pCreatePostPaymentActiveAuto =
    CreateDatum
      <$> pNewPayment
      <*> pOutputFile
  where
    pNewPayment :: Parser NewDatum
    pNewPayment =
      NewPostPaymentActiveAuto
        <$> pNetwork
        <*> pApiService
        <*> pLoanRef
        <*> pPaymentAmount

pCreatePostInterestActive :: Parser Command
pCreatePostInterestActive = hsubparser $ mconcat
    [ command "manual" $
        info pCreatePostInterestActiveManual $ 
          progDesc "Create a post-interest ActiveDatum manually."
    , command "auto" $
        info pCreatePostInterestActiveAuto $ 
          progDesc "Create a post-interest ActiveDatum by looking up the loan UTxO."
    ]

pCreatePostInterestActiveManual :: Parser Command
pCreatePostInterestActiveManual =
    CreateDatum
      <$> pNewInterestInfo
      <*> pOutputFile
  where
    pNewInterestInfo :: Parser NewDatum
    pNewInterestInfo =
      fmap NewPostInterestActiveManual $ NewInterestInfo
        <$> pUserCredential "borrower"
        <*> pPaymentAddress
        <*> pAsset "loan"
        <*> pPrincipal
        <*> (pCompoundFrequency <|> pure Nothing)
        <*> pLastCompounding
        <*> pLoanTerm
        <*> pInterest
        <*> pMinPayment
        <*> pPenalty
        <*> pCollateralization
        <*> pIsSwappable
        <*> pClaimExpiration
        <*> pLoanExpiration
        <*> pLoanOutstanding
        <*> pTotalEpochPayments
        <*> pLoanId
        <*> pNumberOfInterestApplications

pCreatePostInterestActiveAuto :: Parser Command
pCreatePostInterestActiveAuto =
    CreateDatum
      <$> pNewInterest
      <*> pOutputFile
  where
    pNewInterest :: Parser NewDatum
    pNewInterest =
      NewPostInterestActiveAuto
        <$> pNetwork
        <*> pApiService
        <*> pLoanRef
        <*> pNumberOfInterestApplications

pCreatePostAddressUpdateActive :: Parser Command
pCreatePostAddressUpdateActive = hsubparser $ mconcat
    [ command "manual" $
        info pCreatePostAddressUpdateActiveManual $ 
          progDesc "Create a post-address-update ActiveDatum manually."
    , command "auto" $
        info pCreatePostAddressUpdateActiveAuto $ 
          progDesc "Create a post-address-update ActiveDatum by looking up the loan UTxO."
    ]

pCreatePostAddressUpdateActiveManual :: Parser Command
pCreatePostAddressUpdateActiveManual =
    CreateDatum
      <$> pNewAddressUpdateInfo
      <*> pOutputFile
  where
    pNewAddressUpdateInfo :: Parser NewDatum
    pNewAddressUpdateInfo =
      fmap NewPostAddressUpdateActiveManual $ NewAddressInfo
        <$> pUserCredential "borrower"
        <*> pPaymentAddress
        <*> pAsset "loan"
        <*> pPrincipal
        <*> (pCompoundFrequency <|> pure Nothing)
        <*> pLastCompounding
        <*> pLoanTerm
        <*> pInterest
        <*> pMinPayment
        <*> pPenalty
        <*> pCollateralization
        <*> pIsSwappable
        <*> pClaimExpiration
        <*> pLoanExpiration
        <*> pLoanOutstanding
        <*> pTotalEpochPayments
        <*> pLoanId

pCreatePostAddressUpdateActiveAuto :: Parser Command
pCreatePostAddressUpdateActiveAuto =
    CreateDatum
      <$> pNewAddressUpdate
      <*> pOutputFile
  where
    pNewAddressUpdate :: Parser NewDatum
    pNewAddressUpdate =
      NewPostAddressUpdateActiveAuto
        <$> pNetwork
        <*> pApiService
        <*> pLoanRef
        <*> pPaymentAddress

-------------------------------------------------
-- CreateRedeemer Parser
-------------------------------------------------
parseCreateRedeemer :: Parser Command
parseCreateRedeemer = hsubparser $ mconcat
    [ command "negotiation-script" $
        info pNegotiationRedeemer $ progDesc "Create a redeemer for the negotiation script."
    , command "loan-script" $
        info pLoanRedeemer $ progDesc "Create a redeemer for the loan script."
    , command "active-script" $
        info pActiveRedeemer $ progDesc "Create a redeemer for the active script."
    , command "payment-script" $
        info pPaymentObserverRedeemer $ progDesc "Create a redeemer for the payment observer script."
    , command "interest-script" $
        info pInterestObserverRedeemer $ progDesc "Create a redeemer for the interest observer script."
    , command "address-update-script" $
        info pAddressUpdateObserverRedeemer $ 
          progDesc "Create a redeemer for the address update observer script."
    ]

pNegotiationRedeemer :: Parser Command
pNegotiationRedeemer = hsubparser $ mconcat
    [ command "manage-asks" $
        info pCreateCloseOrUpdateAsk $ 
          progDesc "Create the redeemer for creating/updating/closing Ask UTxOs."
    , command "manage-offers" $
        info pCreateCloseOrUpdateOffer $ 
          progDesc "Create the redeemer for creating/updating/closing Offer UTxOs."
    , command "burn-all" $
        info pBurnNegotiationBeacons $ progDesc "Create the redeemer for burning all beacons."
    , command "register" $
        info pRegisterNegotiationScript $ progDesc "Create the redeemer for registering the script."
    ]
  where
    pCreateCloseOrUpdateAsk :: Parser Command
    pCreateCloseOrUpdateAsk = 
      CreateRedeemer
        <$> (NewNegotiation . CreateCloseOrUpdateAsk <$> pUserCredential "borrower")
        <*> pOutputFile

    pCreateCloseOrUpdateOffer :: Parser Command
    pCreateCloseOrUpdateOffer = 
      CreateRedeemer
        <$> (NewNegotiation . CreateCloseOrUpdateOffer <$> pUserCredential "lender")
        <*> pOutputFile

    pBurnNegotiationBeacons :: Parser Command
    pBurnNegotiationBeacons = 
      CreateRedeemer 
        <$> pure (NewNegotiation BurnNegotiationBeacons)
        <*> pOutputFile

    pRegisterNegotiationScript :: Parser Command
    pRegisterNegotiationScript =
      CreateRedeemer 
        <$> pure (NewNegotiation RegisterNegotiationScript)
        <*> pOutputFile

pActiveRedeemer :: Parser Command
pActiveRedeemer = hsubparser $ mconcat
    [ command "accept-offers" $
        info pAcceptOffers $ 
          progDesc "Create the redeemer for accepting loan offers."
    , command "claim-expired" $
        info pBurnKeyAndClaimExpired $ 
          progDesc "Create the redeemer for claiming expired collateral."
    , command "unlock" $
        info pBurnRemainderOrUnlockLost $ 
          progDesc "Create the redeemer for unlocking Active UTxOs."
    , command "burn-all" $
        info pBurnActiveBeacons $ 
          progDesc "Create the redeemer for burning active beacons."
    ]
  where
    pAcceptOffers :: Parser Command
    pAcceptOffers = 
      CreateRedeemer
        <$> pure (NewActive $ CreateActive negotiationBeaconCurrencySymbol)
        <*> pOutputFile

    pBurnKeyAndClaimExpired :: Parser Command
    pBurnKeyAndClaimExpired = 
      CreateRedeemer
        <$> pure (NewActive BurnKeyAndClaimExpired)
        <*> pOutputFile

    pBurnRemainderOrUnlockLost :: Parser Command
    pBurnRemainderOrUnlockLost = 
      CreateRedeemer
        <$> pure (NewActive BurnAndUnlockLost)
        <*> pOutputFile

    pBurnActiveBeacons :: Parser Command
    pBurnActiveBeacons = 
      CreateRedeemer
        <$> pure (NewActive BurnActiveBeacons)
        <*> pOutputFile

pLoanRedeemer :: Parser Command
pLoanRedeemer = hsubparser $ mconcat
    [ command "manage-ask" $
        info pCloseOrUpdateAsk $ 
          progDesc "Create the redeemer for updating/closing Ask UTxOs."
    , command "manage-offer" $
        info pCloseOrUpdateOffer $ 
          progDesc "Create the redeemer for updating/closing Offer UTxOs."
    , command "accept-offer" $
        info pAcceptOffer $ 
          progDesc "Create the redeemer for accepting a lender's offer."
    , command "make-payment" $
        info pMakePayment $ 
          progDesc "Create the redeemer for making a loan payment."
    , command "apply-interest" $
        info pApplyInterest $ 
          progDesc "Create the redeemer for applying interest to a loan."
    , command "claim-expired-collateral" $
        info pSpendWithKeyNFT $ 
          progDesc "Create the redeemer for claiming an expired loan's collateral."
    , command "update-lender-address" $
        info pUpdateLenderAddress $ 
          progDesc "Create the redeemer for changing the lender address."
    , command "unlock-active" $
        info pUnlock $ 
          progDesc "Create the redeemer for unlocking lost/finished/invalid Active UTxOs."
    ]
  where
    pCloseOrUpdateAsk :: Parser Command
    pCloseOrUpdateAsk = 
      CreateRedeemer
        <$> pure (NewLoan CloseOrUpdateAsk)
        <*> pOutputFile

    pCloseOrUpdateOffer :: Parser Command
    pCloseOrUpdateOffer = 
      CreateRedeemer 
        <$> pure (NewLoan CloseOrUpdateOffer) 
        <*> pOutputFile

    pAcceptOffer :: Parser Command
    pAcceptOffer = 
      CreateRedeemer 
        <$> pure (NewLoan AcceptOffer) 
        <*> pOutputFile

    pMakePayment :: Parser Command
    pMakePayment = 
      CreateRedeemer
        <$> (NewLoan . MakePayment <$> pPaymentAmount)
        <*> pOutputFile

    pApplyInterest :: Parser Command
    pApplyInterest = 
      CreateRedeemer
        <$> (fmap NewLoan . ApplyInterest <$> pDepositIncrease <*> pNumberOfInterestApplications)
        <*> pOutputFile

    pUpdateLenderAddress :: Parser Command
    pUpdateLenderAddress = 
      CreateRedeemer
        <$> (fmap NewLoan . UpdateLenderAddress <$> pPaymentAddress <*> pDepositIncrease)
        <*> pOutputFile

    pSpendWithKeyNFT :: Parser Command
    pSpendWithKeyNFT = 
      CreateRedeemer 
        <$> pure (NewLoan SpendWithKeyNFT) 
        <*> pOutputFile

    pUnlock :: Parser Command
    pUnlock =
      CreateRedeemer
        <$> pure (NewLoan Unlock)
        <*> pOutputFile

pPaymentObserverRedeemer :: Parser Command
pPaymentObserverRedeemer = hsubparser $ mconcat
    [ command "observe-payment" $
        info pObservePayment $ 
          progDesc "Create the redeemer for observing loan payments."
    , command "register" $
        info pRegisterPaymentObserverScript $ 
          progDesc "Create the redeemer for registering the script."
    ]
  where
    pObservePayment :: Parser Command
    pObservePayment = 
      CreateRedeemer 
        <$> pure (NewPaymentObserver ObservePayment)
        <*> pOutputFile

    pRegisterPaymentObserverScript :: Parser Command
    pRegisterPaymentObserverScript =
      CreateRedeemer 
        <$> pure (NewPaymentObserver RegisterPaymentObserverScript)
        <*> pOutputFile

pInterestObserverRedeemer :: Parser Command
pInterestObserverRedeemer = hsubparser $ mconcat
    [ command "observe-interest" $
        info pObserveInterest $ 
          progDesc "Create the redeemer for observing loan interest applications."
    , command "register" $
        info pRegisterInterestObserverScript $ 
          progDesc "Create the redeemer for registering the script."
    ]
  where
    pObserveInterest :: Parser Command
    pObserveInterest = 
      CreateRedeemer 
        <$> pure (NewInterestObserver ObserveInterest)
        <*> pOutputFile

    pRegisterInterestObserverScript :: Parser Command
    pRegisterInterestObserverScript =
      CreateRedeemer 
        <$> pure (NewInterestObserver RegisterInterestObserverScript)
        <*> pOutputFile

pAddressUpdateObserverRedeemer :: Parser Command
pAddressUpdateObserverRedeemer = hsubparser $ mconcat
    [ command "observe-address-update" $
        info pObserveAddressUpdate $ 
          progDesc "Create the redeemer for observing a lender address update."
    , command "register" $
        info pRegisterAddressUpdateObserverScript $ 
          progDesc "Create the redeemer for registering the script."
    ]
  where
    pObserveAddressUpdate :: Parser Command
    pObserveAddressUpdate = 
      CreateRedeemer 
        <$> pure (NewAddressUpdateObserver ObserveAddressUpdate)
        <*> pOutputFile

    pRegisterAddressUpdateObserverScript :: Parser Command
    pRegisterAddressUpdateObserverScript =
      CreateRedeemer 
        <$> pure (NewAddressUpdateObserver RegisterAddressUpdateObserverScript)
        <*> pOutputFile

-------------------------------------------------
-- Beacon Name Parsers
-------------------------------------------------
parseBeaconName :: Parser Command
parseBeaconName = hsubparser $ mconcat
    [ command "policy-id" $
        info pPolicyId $ progDesc "Calculate a beacon policy id."
    , command "asset-name" $
        info pAssetName $ progDesc "Calculate a beacon asset name."
    ]
  where
    pNegotiationId :: Parser BeaconName
    pNegotiationId = flag' NegotiationPolicyId
      (  long "negotiation-beacons"
      <> help "Calculate the policy id for the negotiation beacons."
      )

    pActiveId :: Parser BeaconName
    pActiveId = flag' ActivePolicyId
      (  long "active-beacons"
      <> help "Calculate the policy id for the active beacons."
      )

    pAssetBeaconName :: Parser BeaconName
    pAssetBeaconName = AssetBeaconName <$> pAsset "loan"

    pAskBeaconName :: Parser BeaconName
    pAskBeaconName = flag' AskBeaconName
      (  long "ask-beacon"
      <> help "Calculate the asset name for the Ask beacon."
      )

    pOfferBeaconName :: Parser BeaconName
    pOfferBeaconName = flag' OfferBeaconName
      (  long "offer-beacon"
      <> help "Calculate the asset name for the Offer beacon."
      )

    pActiveBeaconName :: Parser BeaconName
    pActiveBeaconName = flag' ActiveBeaconName
      (  long "active-beacon"
      <> help "Calculate the asset name for the Active beacon."
      )

    pLenderIdName :: Parser BeaconName
    pLenderIdName = LenderIdName <$> pUserCredential "lender"

    pLoanIdName :: Parser BeaconName
    pLoanIdName = LoanIdName <$> pOfferId

    pAssetName :: Parser Command
    pAssetName = 
      BeaconName
        <$> (  pAssetBeaconName 
           <|> pAskBeaconName 
           <|> pOfferBeaconName 
           <|> pLenderIdName 
           <|> pActiveBeaconName
           <|> pLoanIdName
           )
        <*> pOutput

    pPolicyId :: Parser Command
    pPolicyId = 
      BeaconName
        <$> (pNegotiationId <|> pActiveId)
        <*> pOutput

-------------------------------------------------
-- ConvertTime Parser
-------------------------------------------------
pConvertTime :: Parser Command
pConvertTime = ConvertTime <$> (pPOSIXTime <|> pSlot) <*> pNetwork
  where
    pPOSIXTime :: Parser ConvertTime
    pPOSIXTime = POSIXTimeToSlot . POSIXTime <$> option auto
      (  long "posix-time"
      <> metavar "INT"
      <> help "Convert POSIX time (in milliseconds) to slot number."
      )

    pSlot :: Parser ConvertTime
    pSlot = SlotToPOSIXTime . Slot <$> option auto
      (  long "slot"
      <> metavar "INT"
      <> help "Convert slot number to POSIX time."
      )

-------------------------------------------------
-- Query Parser
-------------------------------------------------
parseQuery :: Parser Command
parseQuery = fmap Query . hsubparser $ mconcat
  [ command "personal-address" $
      info pQueryPersonal $ progDesc "Query your personal address." 
  , command "asks" $
      info pQueryAsks $ progDesc "Query open Asks for the protocol." 
  , command "offers" $
      info pQueryOffers $ progDesc "Query open Offers for the protocol." 
  , command "actives" $
      info pQueryActives $ progDesc "Query open Actives for the protocol." 
  , command "current-slot" $
      info pQueryCurrentSlot $ progDesc "Query the current slot number."
  , command "borrower-history" $
      info pQueryBorrowerCreditHistory $ progDesc "Query the borrower's credit history."
  , command "loan-history" $
      info pQueryLoanHistory $ progDesc "Query the loan's event history."
  ]

pQueryPersonal :: Parser Query
pQueryPersonal =
  QueryPersonal
    <$> pNetwork
    <*> pApiService
    <*> pBech32Address
    <*> pKeysOnly
    <*> pFormat
    <*> pOutput
  where
    pKeysOnly :: Parser Bool
    pKeysOnly = flag False True
      (  long "keys"
      <> help "Show only UTxOs with loan key NFTs."
      )

pQueryAsks :: Parser Query
pQueryAsks =
  QueryAsks
    <$> pNetwork
    <*> pApiService
    <*> ((Just <$> pAsset "loan") <|> pure Nothing)
    <*> (Collateral <$> many (pAsset "collateral"))
    <*> ((Just <$> pUserCredential "borrower") <|> pure Nothing)
    <*> pFormat
    <*> pOutput

pQueryOffers :: Parser Query
pQueryOffers =
  QueryOffers
    <$> pNetwork
    <*> pApiService
    <*> ((Just <$> pAsset "loan") <|> pure Nothing)
    <*> ((Just <$> pUserCredential "borrower") <|> pure Nothing)
    <*> ((Just <$> pUserCredential "lender") <|> pure Nothing)
    <*> pFormat
    <*> pOutput

pQueryActives :: Parser Query
pQueryActives =
  QueryActives
    <$> pNetwork
    <*> pApiService
    <*> ((Just <$> pAsset "loan") <|> pure Nothing)
    <*> ((Just <$> pUserCredential "borrower") <|> pure Nothing)
    <*> ((Just <$> pLoanId) <|> pure Nothing)
    <*> pFormat
    <*> pOutput

pQueryCurrentSlot :: Parser Query
pQueryCurrentSlot =
  QueryCurrentSlot
    <$> pNetwork
    <*> pApiService

pQueryBorrowerCreditHistory :: Parser Query
pQueryBorrowerCreditHistory =
  QueryBorrowerCreditHistory
    <$> pNetwork
    <*> pApiService
    <*> pBorrowerId
    <*> pFormat
    <*> pOutput

pQueryLoanHistory :: Parser Query
pQueryLoanHistory =
  QueryLoanHistory
    <$> pNetwork
    <*> pApiService
    <*> pLoanId
    <*> pFormat
    <*> pOutput

-------------------------------------------------
-- Submit Parser
-------------------------------------------------
pSubmitTx :: Parser Command
pSubmitTx = 
  SubmitTx
    <$> pNetwork
    <*> pApiService
    <*> pTxFile

-------------------------------------------------
-- EvaluateTx Parser
-------------------------------------------------
pEvaluateTx :: Parser Command
pEvaluateTx = 
  EvaluateTx 
    <$> pNetwork
    <*> pApiService
    <*> pTxFile

-------------------------------------------------
-- ExportParams Parser
-------------------------------------------------
pExportParams :: Parser Command
pExportParams =
  ExportParams
    <$> pNetwork
    <*> pOutput

-------------------------------------------------
-- Basic Helper Parsers
-------------------------------------------------
pOutputFile :: Parser FilePath
pOutputFile = strOption
  (  long "out-file"
  <> metavar "FILE"
  <> help "Save to file."
  <> completer (bashCompleter "file")
  )

readDuration :: String -> Maybe POSIXTime
readDuration s = case String.words s of
  [n,"days"] -> POSIXTime . (\d -> d * 24 * 3600 * 1000) <$> readMaybe n
  [n,"slots"] -> POSIXTime . (*1000) <$> readMaybe n
  _ -> Nothing

pAsset :: String -> Parser Asset
pAsset prefix = option (eitherReader readAsset)
  (  long (prefix <> "-asset")
  <> metavar "STRING"
  <> help ("The " <> prefix <> " asset (lovelace or policy_id.asset_name).")
  )

pUserCredential :: String -> Parser Credential
pUserCredential s = pStakingPubKeyCredential <|> pStakingScriptCredential
  where
    pStakingScriptCredential :: Parser Credential
    pStakingScriptCredential = ScriptCredential <$> option (eitherReader readScriptHash)
      (  long (s <> "-staking-script-hash")
      <> metavar "STRING"
      <> help ("The hash of the " <> s <> "'s staking script.")
      )

    pStakingPubKeyCredential :: Parser Credential
    pStakingPubKeyCredential = PubKeyCredential <$> option (eitherReader readPubKeyHash)
      (  long (s <> "-staking-pubkey-hash")
      <> metavar "STRING"
      <> help ("The hash of the " <> s <> "'s staking pubkey.")
      )

pPrincipal :: Parser Integer
pPrincipal = option auto
  (  long "principal"
  <> metavar "INT"
  <> help "The size of the loan."
  )

pLoanTerm :: Parser POSIXTime
pLoanTerm = option (maybeReader readDuration)
  (  long "loan-term"
  <> metavar "DURATION"
  <> help "The length of the loan. '# days' or '# slots'"
  )

pBech32Address :: Parser PaymentAddress
pBech32Address = PaymentAddress <$> strOption
    (  long "address"
    <> metavar "BECH32"
    <> help "The target address."
    )

pPaymentAddress :: Parser Address
pPaymentAddress = 
  option (maybeReader $ rightToMaybe . paymentAddressToPlutusAddress . PaymentAddress . toText)
    (  long "payment-address"
    <> metavar "BECH32"
    <> help "The address where loan payments must go."
    )

pClaimPeriod :: Parser POSIXTime
pClaimPeriod = option (maybeReader readDuration)
    (  long "claim-period"
    <> metavar "DURATION"
    <> help "The time needed to claim expired collateral. '# days' or '# slots'"
    )

pOfferExpiration :: Parser (Maybe POSIXTime)
pOfferExpiration = Just . POSIXTime <$> option auto
    (  long "offer-expiration"
    <> metavar "TIME"
    <> help "The time when this offer expires in posix time (in milliseconds)."
    )

pCompoundFrequency :: Parser (Maybe POSIXTime)
pCompoundFrequency = Just <$> option (maybeReader readDuration)
    (  long "compound-frequency"
    <> metavar "DURATION"
    <> help "The length of a compound period. '# days' or '# slots'"
    )

pOfferDeposit :: Parser Integer
pOfferDeposit = option auto
  (  long "offer-deposit"
  <> metavar "INT"
  <> help "The ada amount used for the minUTxOValue of the Offer UTxO."
  )

pIsSwappable :: Parser Bool
pIsSwappable = flag False True
  (  long "collateral-is-swappable"
  <> help "Collateral can be swapped out during payments."
  )

pCollateralization :: Parser [(Asset,Fraction)]
pCollateralization = some ((,) <$> pAsset "collateral" <*> pRelativeRate)
  where 
    pRelativeRate :: Parser Fraction
    pRelativeRate = option (eitherReader readFraction)
      (  long "relative-rate"
      <> metavar "PERCENT"
      <> help "The relative value for this collateral."
      )

pPenalty :: Parser Penalty
pPenalty = pNoPenalty <|> pFixedPenalty <|> pPercentPenalty
  where
    pNoPenalty :: Parser Penalty
    pNoPenalty = flag' NoPenalty
      (  long "no-penalty"
      <> help "No penalty for missing minimum payments."
      )

    pFixedPenalty :: Parser Penalty
    pFixedPenalty = FixedFee <$> option auto
      (  long "fixed-penalty"
      <> metavar "INT"
      <> help "A fixed fee penalty for missing minimum payments."
      )

    pPercentPenalty :: Parser Penalty
    pPercentPenalty = PercentFee <$> option (eitherReader readFraction)
      (  long "percent-penalty"
      <> metavar "PERCENT"
      <> help "A percent fee penalty for missing minimum payments."
      )

pMinPayment :: Parser Integer
pMinPayment = option auto
  (  long "minimum-payment"
  <> metavar "INT"
  <> help "The minimum amount that must be paid each compound period."
  )

pInterest :: Parser Fraction
pInterest = option (eitherReader readFraction)
  (  long "interest"
  <> metavar "PERCENT"
  <> help "The interest rate for the loan."
  )

pNetwork :: Parser Network
pNetwork = pPreProdTestnet <|> pMainnet
  where
    pPreProdTestnet :: Parser Network
    pPreProdTestnet = flag' PreProdTestnet
      (  long "testnet"
      <> help "For the preproduction testnet.")

    pMainnet :: Parser Network
    pMainnet = flag' Mainnet
      (  long "mainnet"
      <> help "For the mainnet.")

pApiService :: Parser ApiService
pApiService = pure Koios
  -- where
  --   pKoios :: Parser Endpoint
  --   pKoios = flag' Koios
  --     (  long "koios"
  --     <> help "Use Koios."
  --     )

pOfferId :: Parser TxOutRef
pOfferId = option (eitherReader readTxOutRef)
  (  long "offer-id"
  <> metavar "STRING"
  <> help "The output reference for the corresponding offer input 'tx_hash#index'."
  )

pStartTime :: Parser POSIXTime
pStartTime = POSIXTime <$> option auto
  (  long "start-time"
  <> metavar "TIME"
  <> help "The start time for the loan in POSIX time (milliseconds)."
  )

pDepositIncrease :: Parser Integer
pDepositIncrease = option auto
  (  long "deposit-increase"
  <> metavar "INT"
  <> help "The ada added for the minUTxOValue increase."
  )

pPaymentAmount :: Parser Integer
pPaymentAmount = option auto
  (  long "payment-amount"
  <> metavar "INT"
  <> help "The amount of the loan asset repaid."
  )

pNumberOfInterestApplications :: Parser Integer
pNumberOfInterestApplications = option auto
  (  long "times-applied"
  <> metavar "INT"
  <> help "The number of times to apply the interest."
  )

pOutput :: Parser Output
pOutput = pStdOut <|> File <$> pOutputFile
  where
    pStdOut :: Parser Output
    pStdOut = flag' Stdout
      (  long "stdout"
      <> help "Display to stdout."
      )

pFormat :: Parser Format
pFormat = pJSON <|> pPretty <|> pPlain
  where
    pJSON :: Parser Format
    pJSON = flag' JSON
      (  long "json"
      <> help "Format as JSON."
      )

    pPretty :: Parser Format
    pPretty = flag' Pretty
      (  long "pretty"
      <> help "Format for pretty-printing."
      )

    pPlain :: Parser Format
    pPlain = flag' Plain
      (  long "plain"
      <> help "Format for pretty-printing without colors."
      )

pLoanId :: Parser LoanId
pLoanId = LoanId <$> option (eitherReader readTokenName)
  (  long "loan-id"
  <> metavar "STRING"
  <> help "The loan id for this loan."
  )

pTotalEpochPayments :: Parser Integer
pTotalEpochPayments = option auto
  (  long "total-epoch-payments"
  <> metavar "INT"
  <> help "The total payments made this compound period so far."
  )

pLoanOutstanding :: Parser Fraction
pLoanOutstanding = option (eitherReader readFraction)
  (  long "outstanding-balance"
  <> metavar "FRACTION"
  <> help "The loan's current outstanding balance."
  )

pLoanExpiration :: Parser POSIXTime
pLoanExpiration = POSIXTime <$> option auto
  (  long "loan-expiration"
  <> metavar "TIME"
  <> help "The expiration time for the loan in POSIX time (milliseconds)."
  )

pClaimExpiration :: Parser POSIXTime
pClaimExpiration = POSIXTime <$> option auto
  (  long "claim-expiration"
  <> metavar "TIME"
  <> help "The expiration time for the lender claim period in POSIX time (milliseconds)."
  )

pLastCompounding :: Parser POSIXTime
pLastCompounding = POSIXTime <$> option auto
  (  long "last-compounding"
  <> metavar "TIME"
  <> help "The last time the interest was applied. In POSIX time (milliseconds)."
  )

pLoanRef :: Parser TxOutRef
pLoanRef = option (eitherReader readTxOutRef)
  (  long "loan-ref"
  <> metavar "STRING"
  <> help "The output reference for the target active loan 'tx_hash#index'."
  )

pBorrowerId :: Parser BorrowerId
pBorrowerId = BorrowerId <$> option (eitherReader readTokenName)
  (  long "borrower-id"
  <> metavar "STRING"
  <> help "The target borrower's id."
  )

pTxFile :: Parser FilePath
pTxFile = strOption
  (  long "tx-file"
  <> metavar "STRING"
  <> help "Transaction file path."
  )
