{-# LANGUAGE OverloadedStrings #-}

module CLI.Parsers
(
  parseCommand
) where

import Options.Applicative
import Data.Text (pack)

import CardanoLoans hiding ((-),(+),(*))
import CLI.Types

-------------------------------------------------
-- Main Parsers
-------------------------------------------------
parseCommand :: Parser Command
parseCommand = hsubparser $ mconcat
  [ command "export-script"
      (info parseExportScript $ progDesc "Export a dApp plutus script.")
  , command "datum"
      (info parseCreateLoanDatum $ progDesc "Create a datum for the dApp.")
  , command "loan-redeemer"
      (info  pCreateLoanRedeemer $ progDesc "Create a redeemer for the loan validator.")
  , command "beacon-redeemer"
      (info parseCreateBeaconRedeemer $ progDesc "Create a redeemer for the beacon policy.")
  , command "convert-address"
      (info pConvertAddress $ progDesc "Convert plutus address <--> Bech32 address.")
  , command "convert-time"
      (info pConvertTime $ progDesc "Convert POSIXTime <--> Slot.")
  , command "query"
      (info parseQuery $ progDesc "Query the dApp's beacon tokens.")
  ]

-------------------------------------------------
-- Scripts Parser
-------------------------------------------------
parseExportScript :: Parser Command
parseExportScript = hsubparser $ mconcat
    [ command "beacon-policy"
        (info pExportPolicy $ progDesc "Export the beacon policy.")
    , command "loan-script"
        (info pExportLoan $ progDesc "Export the loan validator script.")
    ]
  where
    pExportLoan :: Parser Command
    pExportLoan = ExportScript <$> pure LoanScript <*> pOutputFile

    pExportPolicy :: Parser Command
    pExportPolicy = ExportScript <$> pure BeaconPolicy <*> pOutputFile

-------------------------------------------------
-- CreateLoanDatum Parser
-------------------------------------------------
parseCreateLoanDatum :: Parser Command
parseCreateLoanDatum = hsubparser $ mconcat
    [ command "ask-datum"
        (info pAsk $ progDesc "Create the datum for asking for a loan.")
    , command "offer-datum"
        (info pOffer $ progDesc "Create the datum for offering a loan.")
    , command "accept-datum"
        (info pAccept $ progDesc "Create the datum for accepting a loan.")
    , command "collateral-payment-datum"
        (info pCollateralPayment $ progDesc "Create the collateral datum for a loan payment.")
    , command "lender-payment-datum"
        (info pLenderPayment $ progDesc "Create the lender payment datum for a loan payment.")
    , command "rollover-datum"
        (info pRollover $ progDesc "Create the datum for rolling over a loan.")
    , command "update-address-datum"
        (info pUpdate $ progDesc "Create the datum for updating the lender's address.")
    ]
  where
    pAsk :: Parser Command
    pAsk = CreateLoanDatum <$> pAskDatum <*> pOutputFile

    pOffer :: Parser Command
    pOffer = CreateLoanDatum <$> pOfferDatum <*> pOutputFile

    pAccept :: Parser Command
    pAccept = CreateLoanDatum <$> pAcceptDatum <*> pOutputFile
    
    pCollateralPayment :: Parser Command
    pCollateralPayment = CreateLoanDatum <$> pCollateralPaymentDatum <*> pOutputFile

    pLenderPayment :: Parser Command
    pLenderPayment = CreateLoanDatum <$> (LenderDatum <$> pLoanId) <*> pOutputFile

    pRollover :: Parser Command
    pRollover = CreateLoanDatum <$> pRolloverDatum <*> pOutputFile

    pUpdate :: Parser Command
    pUpdate = CreateLoanDatum <$> pUpdateDatum <*> pOutputFile

pAskDatum :: Parser LoanDatum'
pAskDatum =
  fmap CollateralDatum $ AskDatum
    <$> pBeaconPolicy
    <*> (credentialAsToken <$> pBorrowerCredential)
    <*> pLoanAsset
    <*> pPrinciple
    <*> pTerm
    <*> some pCollateralAsset

pOfferDatum :: Parser LoanDatum'
pOfferDatum =
  fmap CollateralDatum $ OfferDatum
    <$> pBeaconPolicy
    <*> (credentialAsToken <$> pLenderCredential)
    <*> pAddress
    <*> pLoanAsset
    <*> pPrinciple
    <*> pCheckpoints
    <*> pTerm
    <*> pInterest
    <*> pCollateralization
    <*> pClaimPeriod

pAcceptDatum :: Parser LoanDatum'
pAcceptDatum = fmap (CollateralDatum . convertToLoanDatum) $
  AcceptDatum'
    <$> pBeaconPolicy
    <*> (credentialAsToken <$> pBorrowerCredential)
    <*> pAddress
    <*> pLoanAsset
    <*> pPrinciple
    <*> pCheckpoints
    <*> pTerm
    <*> pInterest
    <*> pCollateralization
    <*> pClaimPeriod
    <*> pLoanId
    <*> pStart

pCollateralPaymentDatum :: Parser LoanDatum'
pCollateralPaymentDatum = fmap (CollateralDatum . convertToLoanDatum) $
  CollateralPaymentDatum'
    <$> pBeaconPolicy
    <*> (credentialAsToken <$> pBorrowerCredential)
    <*> pAddress
    <*> pLoanAsset
    <*> pPrinciple
    <*> pNextCheckpoints
    <*> pPastCheckpoints
    <*> pTerm
    <*> pInterest
    <*> pCollateralization
    <*> pClaimExpiration
    <*> pLoanExpiration
    <*> pCurrentBalance
    <*> pPaymentAmount
    <*> pLoanId

-- | This will automatically update the information for the user. All fields should be the current
-- datum; it will produce the updated datum from the supplied information.
pRolloverDatum :: Parser LoanDatum'
pRolloverDatum = fmap (CollateralDatum . convertToLoanDatum) $
  RolloverDatum'
    <$> pBeaconPolicy
    <*> (credentialAsToken <$> pBorrowerCredential)
    <*> pAddress
    <*> pLoanAsset
    <*> pPrinciple
    <*> pNextCheckpoints
    <*> pPastCheckpoints
    <*> pTerm
    <*> pInterest
    <*> pCollateralization
    <*> pClaimExpiration
    <*> pLoanExpiration
    <*> pCurrentBalance
    <*> pLoanId

-- | This is used when the only field being updated is the lender's address.
pUpdateDatum :: Parser LoanDatum'
pUpdateDatum =
  fmap CollateralDatum $ ActiveDatum
    <$> pBeaconPolicy
    <*> (credentialAsToken <$> pBorrowerCredential)
    <*> pAddress
    <*> pLoanAsset
    <*> pPrinciple
    <*> pNextCheckpoints
    <*> pPastCheckpoints
    <*> pTerm
    <*> pInterest
    <*> pCollateralization
    <*> pClaimExpiration
    <*> pLoanExpiration
    <*> pCurrentBalance
    <*> pLoanId
    
-------------------------------------------------
-- CreateLoanRedeemer Parsers
-------------------------------------------------
pCreateLoanRedeemer :: Parser Command
pCreateLoanRedeemer = hsubparser $ mconcat
    [ command "close-ask"
        (info pCloseAsk $ progDesc "Close a loan ask.")
    , command "close-offer"
        (info pCloseOffer $ progDesc "Close a loan offer.")
    , command "accept-offer"
        (info pAcceptOffer $ progDesc "Accept a loan offer.")
    , command "make-payment"
        (info pMakePayment $ progDesc "Make a loan payment")
    , command "claim-expired"
        (info pClaimExpired $ progDesc "Spend a UTxO of a finished or expired loan.")
    , command "unlock"
        (info pUnlockLostCollateral $ progDesc "Spend a UTxO of a finished or lost loan.")
    , command "rollover"
        (info pRollover $ progDesc "Rollover a loan.")
    , command "update-address"
        (info pUpdateLenderAddress $ progDesc "Update an active datum's lender address.")
    ]
  where
    pCloseAsk :: Parser Command
    pCloseAsk = CreateLoanRedeemer CloseAsk <$> pOutputFile

    pCloseOffer :: Parser Command
    pCloseOffer = CreateLoanRedeemer CloseOffer <$> pOutputFile

    pAcceptOffer :: Parser Command
    pAcceptOffer = CreateLoanRedeemer AcceptOffer <$> pOutputFile
    
    pMakePayment :: Parser Command
    pMakePayment = CreateLoanRedeemer MakePayment <$> pOutputFile
    
    pClaimExpired :: Parser Command
    pClaimExpired = CreateLoanRedeemer ClaimExpired <$> pOutputFile

    pUnlockLostCollateral :: Parser Command
    pUnlockLostCollateral = CreateLoanRedeemer UnlockLostCollateral <$> pOutputFile
    
    pRollover :: Parser Command
    pRollover = CreateLoanRedeemer Rollover <$> pOutputFile

    pUpdateLenderAddress :: Parser Command
    pUpdateLenderAddress = CreateLoanRedeemer <$> (UpdateLenderAddress <$> pAddress) <*> pOutputFile

-------------------------------------------------
-- CreateBeaconRedeemer Parser
-------------------------------------------------
parseCreateBeaconRedeemer :: Parser Command
parseCreateBeaconRedeemer = hsubparser $ mconcat
    [ command "mint-ask"
        (info pMintAsk $ progDesc "Create the redeemer for minting an Ask beacon.")
    , command "mint-offer"
        (info pMintOffer $ progDesc "Create the redeemer for minting an Offer beacon.")
    , command "mint-active"
        (info pMintActive $ progDesc "Create the redeemer for minting an Active beacon.")
    , command "burn-beacons"
        (info pBurnBeacons $ progDesc "Create the redeemer for burning beacons.")
    ]
  where
    pMintAsk :: Parser Command
    pMintAsk = 
      CreateBeaconRedeemer 
        <$> (MintAskBeacon <$> pBorrowerCredential)
        <*> pOutputFile

    pMintOffer :: Parser Command
    pMintOffer = 
      CreateBeaconRedeemer 
        <$> (MintOfferBeacon <$> pLenderCredential)
        <*> pOutputFile

    pMintActive :: Parser Command
    pMintActive = 
      CreateBeaconRedeemer 
        <$> (MintActiveBeacon <$> pBorrowerCredential <*> pPairings)
        <*> pOutputFile
    
    pBurnBeacons :: Parser Command
    pBurnBeacons = CreateBeaconRedeemer BurnBeacons <$> pOutputFile

-------------------------------------------------
-- ConvertTime Parser
-------------------------------------------------
pConvertTime :: Parser Command
pConvertTime = ConvertTime <$> (pPOSIXTime <|> pSlot)
  where
    pPOSIXTime :: Parser ConvertTime
    pPOSIXTime = POSIXTimeToSlot . POSIXTime <$> option auto
      (  long "posix-time"
      <> metavar "INT"
      <> help "Convert POSIX time to slot number."
      )

    pSlot :: Parser ConvertTime
    pSlot = SlotToPOSIXTime . Slot <$> option auto
      (  long "slot"
      <> metavar "INT"
      <> help "Convert slot number to POSIX time."
      )

-------------------------------------------------
-- ConvertAddress Parser
-------------------------------------------------
pConvertAddress :: Parser Command
pConvertAddress = 
    ConvertAddress <$> (pBech <|> pPlutus) <*> pOutput
  where
    pBech :: Parser ConvertAddress
    pBech = Bech32ToPlutus . pack <$> pBech32Address

    pPlutus :: Parser ConvertAddress
    pPlutus = PlutusToBech32 <$> pAddress

-------------------------------------------------
-- Query Parser
-------------------------------------------------
parseQuery :: Parser Command
parseQuery = fmap Query . hsubparser $ mconcat
    [ command "all-asks"
        (info pAllAsks $ progDesc "Query all open Asks.")
    , command "own-asks"
        (info pOwnAsks $ progDesc "Query all Asks for a specific borrower.")
    , command "all-offers"
        (info pAllOffers $ progDesc "Query all Offers to a specific borrower.")
    , command "own-offers"
        (info pOwnOffers $ progDesc "Query all Offers from a specific lender.")
    , command "borrowers-active-loans"
        (info pBorrowersActiveLoans $ progDesc "Query all the borrower's Active loans.")
    , command "borrowers-finished-loans"
        (info pBorrowersFinishedLoans $ progDesc "Query all finished loan's at the borrower's address.")
    , command "specific-loan"
        (info pSpecificLoan $ progDesc "Query a specific loan by its LoanID.")
    , command "own-key-nfts"
        (info pOwnKeys $ progDesc "Query all Key NFTs at a specific address.")
    , command "borrower-credit-history"
        (info pBorrowersHistory $ progDesc "Query a borrower's credit history.")
    ]
  where
    pAllAsks :: Parser Query
    pAllAsks = 
      QueryAllAsks 
        <$> pNetwork 
        <*> pApiEndpoint
        <*> pOutput
    
    pOwnAsks :: Parser Query
    pOwnAsks =
      QueryOwnAsks
        <$> pNetwork
        <*> pApiEndpoint
        <*> pBech32Address
        <*> pOutput

    pAllOffers :: Parser Query
    pAllOffers =
      QueryAllOffers
        <$> pNetwork
        <*> pApiEndpoint
        <*> pBech32Address
        <*> pOutput
    
    pOwnOffers :: Parser Query
    pOwnOffers =
      QueryOwnOffers
        <$> pNetwork
        <*> pApiEndpoint
        <*> pLenderId
        <*> pOutput
    
    pBorrowersActiveLoans :: Parser Query
    pBorrowersActiveLoans =
      QueryAllBorrowersActiveLoans
        <$> pNetwork
        <*> pApiEndpoint
        <*> pBorrowerId
        <*> pBech32Address
        <*> pOutput

    pBorrowersFinishedLoans :: Parser Query
    pBorrowersFinishedLoans =
      QueryAllBorrowersFinishedLoans
        <$> pNetwork
        <*> pApiEndpoint
        <*> pBorrowerId
        <*> pBech32Address
        <*> pOutput

    pSpecificLoan :: Parser Query
    pSpecificLoan =
      QuerySpecificLoan
        <$> pNetwork
        <*> pApiEndpoint
        <*> (idToString <$> pLoanId)
        <*> pOutput
    
    pOwnKeys :: Parser Query
    pOwnKeys =
      QueryOwnKeys
        <$> pNetwork
        <*> pApiEndpoint
        <*> pBech32Address
        <*> pOutput

    pBorrowersHistory :: Parser Query
    pBorrowersHistory =
      QueryBorrowersHistory
        <$> pNetwork
        <*> pApiEndpoint
        <*> pBorrowerId
        <*> pOutput

-------------------------------------------------
-- Basic Helper Parsers
-------------------------------------------------
pOutputFile :: Parser FilePath
pOutputFile = strOption
  (  long "out-file"
  <> metavar "FILE"
  <> help "The output file."
  <> completer (bashCompleter "file")
  )

pBeaconPolicy :: Parser CurrencySymbol
pBeaconPolicy = option (eitherReader readCurrencySymbol)
  (  long "beacon-policy-id"
  <> metavar "STRING"
  <> help "Policy id for the dApp's beacon policy.")

pBorrowerCredential :: Parser Credential
pBorrowerCredential = pStakingPubKeyCredential <|> pStakingScriptCredential
  where
    pStakingScriptCredential :: Parser Credential
    pStakingScriptCredential = ScriptCredential <$> option (eitherReader readValidatorHash)
      (  long "borrower-staking-script-hash"
      <> metavar "STRING"
      <> help "The hash of the borrower's staking script used in the loan address."
      )

    pStakingPubKeyCredential :: Parser Credential
    pStakingPubKeyCredential = PubKeyCredential <$> option (eitherReader readPubKeyHash)
      (  long "borrower-staking-pubkey-hash"
      <> metavar "STRING"
      <> help "The hash of the staking pubkey used in the loan address."
      )

pLoanAsset :: Parser (CurrencySymbol,TokenName)
pLoanAsset = pLoanAssetLovelace <|> ((,) <$> pLoanAssetCurrencySymbol <*> pLoanAssetTokenName)
  where
    pLoanAssetLovelace :: Parser (CurrencySymbol,TokenName)
    pLoanAssetLovelace = flag' (adaSymbol,adaToken)
      (  long "loan-asset-is-lovelace"
      <> help "The asset loaned is lovelace"
      )

    pLoanAssetCurrencySymbol :: Parser CurrencySymbol
    pLoanAssetCurrencySymbol = option (eitherReader readCurrencySymbol)
      (  long "loan-asset-policy-id" 
      <> metavar "STRING" 
      <> help "The policy id of the loaned asset."
      )

    pLoanAssetTokenName :: Parser TokenName
    pLoanAssetTokenName = option (eitherReader readTokenName)
      (  long "loan-asset-token-name"
      <> metavar "STRING"
      <> help "The token name (in hexidecimal) of the loaned asset."
      )

pCollateralAsset :: Parser (CurrencySymbol,TokenName)
pCollateralAsset = pCollateralAssetLovelace <|> ((,) <$> pCollateralAssetCurrencySymbol <*> pCollateralAssetTokenName)
  where
    pCollateralAssetLovelace :: Parser (CurrencySymbol,TokenName)
    pCollateralAssetLovelace = flag' (adaSymbol,adaToken)
      (  long "collateral-asset-is-lovelace"
      <> help "The collateral asset is lovelace."
      )

    pCollateralAssetCurrencySymbol :: Parser CurrencySymbol
    pCollateralAssetCurrencySymbol = option (eitherReader readCurrencySymbol)
      (  long "collateral-asset-policy-id" 
      <> metavar "STRING" 
      <> help "The policy id of the collateral asset."
      )

    pCollateralAssetTokenName :: Parser TokenName
    pCollateralAssetTokenName = option (eitherReader readTokenName)
      (  long "collateral-asset-token-name"
      <> metavar "STRING"
      <> help "The token name (in hexidecimal) of the collateral asset."
      )

pPrinciple :: Parser Integer
pPrinciple = option auto
  (  long "principle"
  <> metavar "INT"
  <> help "The loan amount in units of the loan asset."
  )

-- | This gets the desired number of slots for the loan to be valid. It is then converted to
-- POSIXTime.
pTerm :: Parser POSIXTime
pTerm = POSIXTime . (* 1000) <$> option auto
  (  long "loan-term"
  <> metavar "INT"
  <> help "The number of slots for the loan to be valid."
  )

pLenderCredential :: Parser Credential
pLenderCredential = pPubKeyCredential <|> pStakingScriptCredential
  where
    pStakingScriptCredential :: Parser Credential
    pStakingScriptCredential = ScriptCredential <$> option (eitherReader readValidatorHash)
      (  long "lender-staking-script-hash"
      <> metavar "STRING"
      <> help "The hash of the lender's staking script to be used as the LenderID."
      )

    pPubKeyCredential :: Parser Credential
    pPubKeyCredential = PubKeyCredential <$> option (eitherReader readPubKeyHash)
      (  long "lender-pubkey-hash"
      <> metavar "STRING"
      <> help "The hash of the lender's pubkey to be used as the LenderID."
      )

pInterest :: Parser PlutusRational
pInterest = unsafeRatio <$> pInterestNum <*> pInterestDen
  where
    pInterestNum :: Parser Integer
    pInterestNum = option auto
      ( long "interest-numerator"
      <> metavar "INT"
      <> help "The numerator of the loan interest rate."
      )

    pInterestDen :: Parser Integer
    pInterestDen = option auto
      ( long "interest-denominator"
      <> metavar "INT"
      <> help "The denominator of the loan interest rate."
      )

pCollateralization :: Parser [((CurrencySymbol,TokenName),PlutusRational)]
pCollateralization = some ((,) <$> pCollateralAsset <*> pRate)
  where
    pRate :: Parser PlutusRational
    pRate = unsafeRatio <$> pRateNum <*> pRateDen
  
    pRateNum :: Parser Integer
    pRateNum = option auto
      ( long "rate-numerator"
      <> metavar "INT"
      <> help "The numerator of the collateral rate."
      )

    pRateDen :: Parser Integer
    pRateDen = option auto
      ( long "rate-denominator"
      <> metavar "INT"
      <> help "The denominator of the collateral rate."
      )

pAddress :: Parser Address
pAddress = 
    Address
      <$> pPaymentCredential
      <*> (pStakingCredential <|> pure Nothing)
  where
    pPaymentScriptCredential :: Parser Credential
    pPaymentScriptCredential = ScriptCredential <$> option (eitherReader readValidatorHash)
      (  long "payment-script-hash"
      <> metavar "STRING"
      <> help "The hash of the payment script used in the address."
      )

    pPaymentPubKeyCredential :: Parser Credential
    pPaymentPubKeyCredential = PubKeyCredential <$> option (eitherReader readPubKeyHash)
      ( long "payment-pubkey-hash"
      <> metavar "STRING"
      <> help "The hash of the payment pubkey used in the address."
      )

    pPaymentCredential :: Parser Credential
    pPaymentCredential = pPaymentPubKeyCredential <|> pPaymentScriptCredential

    pStakingScriptCredential :: Parser StakingCredential
    pStakingScriptCredential = StakingHash . ScriptCredential <$> option (eitherReader readValidatorHash)
      (  long "staking-script-hash"
      <> metavar "STRING"
      <> help "The hash of the staking script used in the address."
      )

    pStakingPubKeyCredential :: Parser StakingCredential
    pStakingPubKeyCredential = StakingHash . PubKeyCredential <$> option (eitherReader readPubKeyHash)
      (  long "staking-pubkey-hash"
      <> metavar "STRING"
      <> help "The hash of the staking pubkey used in the address."
      )

    pStakingCredential :: Parser (Maybe StakingCredential)
    pStakingCredential = Just <$> (pStakingPubKeyCredential <|> pStakingScriptCredential)

-- | This gets the number of slots the lender will be able to claim the collateral for when there
-- is a default. It is then converted to POSIXTime.
pClaimPeriod :: Parser POSIXTime
pClaimPeriod = POSIXTime . (* 1000) <$> option auto
  (  long "claim-period"
  <> metavar "INT"
  <> help "The number of slots for the lender to claim the collateral of a defaulted loan."
  )

-- | The slots where the loan must be rolled over and the interest must accrue.
pCheckpoints :: Parser [POSIXTime]
pCheckpoints = many $ POSIXTime . (* 1000) <$> option auto
  (  long "checkpoint"
  <> metavar "INT"
  <> help "The number of slots until the loan must be rolled over."
  )

pLoanId :: Parser TokenName
pLoanId = option (eitherReader readTokenName)
  (  long "loan-id"
  <> metavar "STRING"
  <> help "The LoanID for this loan."
  )

-- | The slot at which the loan will start. It will be converted to POSIXTime. slotToPOSIXTime
-- is hardcoded for the PreprodTestnet.
pStart :: Parser POSIXTime
pStart = slotToPOSIXTime . Slot <$> option auto
  (  long "starting-slot"
  <> metavar "INT"
  <> help "The slot where the loan starts."
  )

pLoanExpiration :: Parser POSIXTime
pLoanExpiration = slotToPOSIXTime . Slot <$> option auto
  (  long "loan-expiration"
  <> metavar "INT"
  <> help "The slot at which the loan expires."
  )

pClaimExpiration :: Parser POSIXTime
pClaimExpiration = slotToPOSIXTime . Slot <$> option auto
  (  long "claim-expiration"
  <> metavar "INT"
  <> help "The slot at which the lender's claim period expires."
  )

pCurrentBalance :: Parser PlutusRational
pCurrentBalance = unsafeRatio <$> pNum <*> pDen
  where
    pNum :: Parser Integer
    pNum = option auto
      (  long "balance-numerator"
      <> metavar "INT"
      <> help "The numerator for the loan's current balance."
      )
    
    pDen :: Parser Integer
    pDen = option auto
      (  long "balance-denominator"
      <> metavar "INT"
      <> help "The denominator for the loan's current balance."
      )

pPaymentAmount :: Parser Integer
pPaymentAmount = option auto
  (  long "payment-amount"
  <> metavar "INT"
  <> help "The number of units of the loan asset being repaid this tx."
  )

-- | The next times where the loan must be rolled over and the interest must accrue.
pNextCheckpoints :: Parser [POSIXTime]
pNextCheckpoints = many $ slotToPOSIXTime . Slot  <$> option auto
  (  long "next-checkpoint"
  <> metavar "INT"
  <> help "An upcoming time the loan must be rolled over (POSIX time)."
  )

-- | The next times where the loan must be rolled over and the interest must accrue.
pPastCheckpoints :: Parser [POSIXTime]
pPastCheckpoints = many $ slotToPOSIXTime . Slot  <$> option auto
  (  long "past-checkpoint"
  <> metavar "INT"
  <> help "A previous time the loan was rolled over (POSIX time)."
  )

pPairings :: Parser [(TxOutRef,TxOutRef)]
pPairings = some ((,) <$> pInput <*> pInput)
  where
    pInput :: Parser TxOutRef
    pInput = TxOutRef <$> pTxId <*> pOutputIndex

    pTxId :: Parser TxId
    pTxId = option (eitherReader readTxId)
      (  long "tx-hash"
      <> metavar "STRING"
      <> help "The tx hash for this input."
      )

    pOutputIndex :: Parser Integer
    pOutputIndex = option auto
      (  long "output-index"
      <> metavar "INT"
      <> help "The output index for this input."
      )

pBech32Address :: Parser String
pBech32Address = strOption
  (  long "address"
  <> metavar "STRING"
  <> help "Address in bech32 format."
  )

pOutput :: Parser Output
pOutput = pStdOut <|> File <$> pOutputFile
  where
    pStdOut :: Parser Output
    pStdOut = flag' Stdout
      (  long "stdout"
      <> help "Display to stdout."
      )

pNetwork :: Parser Network
pNetwork = pPreProdTestnet
  where
    pPreProdTestnet :: Parser Network
    pPreProdTestnet = flag' PreProdTestnet
      (  long "testnet"
      <> help "Query the preproduction testnet.")

pApiEndpoint :: Parser ApiEndpoint
pApiEndpoint = pKoios <|> pBlockfrost
  where
    pKoios :: Parser ApiEndpoint
    pKoios = flag' Koios
      (  long "koios"
      <> help "Query using Koios."
      )

    pBlockfrost :: Parser ApiEndpoint
    pBlockfrost = Blockfrost <$> strOption
      (  long "blockfrost"
      <> metavar "STRING"
      <> help "Query using Blockfrost with the supplied api key."
      )

pLenderId :: Parser String
pLenderId = strOption
  (  long "lender-id"
  <> metavar "STRING"
  <> help "LenderID for the lender."
  )

pBorrowerId :: Parser String
pBorrowerId = strOption
  (  long "borrower-id"
  <> metavar "STRING"
  <> help "BorrowerID for the borrower."
  )