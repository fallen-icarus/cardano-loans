{-# LANGUAGE OverloadedStrings #-}

module CLI.Parsers
(
  parseCommand
) where

import Options.Applicative

import CardanoLoans hiding ((-),(+),(*))
import CLI.Types

-------------------------------------------------
-- Main Parsers
-------------------------------------------------
parseCommand :: Parser Command
parseCommand = hsubparser $ mconcat
  [ command "export-script"
      (info pExportScript $ progDesc "Export a dApp plutus script.")
  , command "loan-datum"
      (info parseCreateLoanDatum $ progDesc "Create a datum for the loan validator.")
  , command "loan-redeemer"
      (info pCreateLoanRedeemer $ progDesc "Create a redeemer for the loan validator.")
  , command "beacon-redeemer"
      (info parseCreateBeaconRedeemer $ progDesc "Create a redeemer for the beacon policy.")
  , command "query"
      (info parseQueryBeacons $ progDesc "Query the dApp's beacons.")
  ]

-------------------------------------------------
-- ExportScripts Parser
-------------------------------------------------
pExportScript :: Parser Command
pExportScript = 
    ExportScript
      <$> (pLoan <|> pPolicy)
      <*> pOutputFile
  where
    pLoan :: Parser Script
    pLoan = flag' LoanScript
      (  long "loan-script"
      <> help "Export the loan validator script."
      )
    
    pPolicy :: Parser Script
    pPolicy = flag' BeaconPolicy
      (  long "beacon-policy"
      <> help "Export the beacon policy."
      )

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
    , command "payment-datum"
        (info pRepay $ progDesc "Create the datum for making a loan payment.")
    ]
  where
    pAsk :: Parser Command
    pAsk = CreateLoanDatum <$> pAskDatum <*> pOutputFile

    pOffer :: Parser Command
    pOffer = CreateLoanDatum <$> pOfferDatum <*> pOutputFile

    pAccept :: Parser Command
    pAccept = CreateLoanDatum <$> pAcceptDatum <*> pOutputFile
    
    pRepay :: Parser Command
    pRepay = CreateLoanDatum <$> pRepaymentDatum <*> pOutputFile

-------------------------------------------------
-- CreateLoanRedeemer Parsers
-------------------------------------------------
pCreateLoanRedeemer :: Parser Command
pCreateLoanRedeemer = 
    CreateLoanRedeemer
      <$> (pCloseAsk <|> pCloseOffer <|> pAcceptOffer <|> pRepayLoan <|> pClaim)
      <*> pOutputFile
  where
    pCloseAsk :: Parser LoanRedeemer
    pCloseAsk = flag' CloseAsk
      (  long "close-ask"
      <> help "Close a loan ask."
      )

    pCloseOffer :: Parser LoanRedeemer
    pCloseOffer = flag' CloseOffer
      (  long "close-offer"
      <> help "Close a loan offer."
      )

    pAcceptOffer :: Parser LoanRedeemer
    pAcceptOffer = flag' AcceptOffer
      (  long "accept-offer"
      <> help "Accept a loan offer."
      )
    
    pRepayLoan :: Parser LoanRedeemer
    pRepayLoan = flag' RepayLoan
      (  long "repay"
      <> help "Make a loan payment."
      )
    
    pClaim :: Parser LoanRedeemer
    pClaim = flag' Claim
      (  long "claim"
      <> help "Claim a repaid/expired loan."
      )

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
        <$> (MintAskToken <$> pBorrowerId)
        <*> pOutputFile

    pMintOffer :: Parser Command
    pMintOffer = 
      CreateBeaconRedeemer 
        <$> (MintOfferToken <$> pLenderId)
        <*> pOutputFile

    pMintActive :: Parser Command
    pMintActive = 
      CreateBeaconRedeemer 
        <$> (MintActiveToken <$> pBorrowerId <*> pLenderId)
        <*> pOutputFile
    
    pBurnBeacons :: Parser Command
    pBurnBeacons = CreateBeaconRedeemer BurnBeaconToken <$> pOutputFile

-------------------------------------------------
-- QueryBeacons Parser
-------------------------------------------------
parseQueryBeacons :: Parser Command
parseQueryBeacons = fmap QueryBeacons . hsubparser $ mconcat
    [ command "all-asks"
        (info pAllAsks $ progDesc "Query all open Asks.")
    , command "own-asks"
        (info pOwnAsks $ progDesc "Query all the borrower's own Asks.")
    , command "all-offers"
        (info pAllOffers $ progDesc "Query all Offers made to the borrower.")
    , command "own-offers"
        (info pOwnOffers $ progDesc "Query all current Offers made by the lender.")
    , command "borrower-loans"
        (info pAllBorrowerLoans $ progDesc "Query all the borrower's current loans.")
    , command "lender-loans"
        (info pAllLenderLoans $ progDesc "Query all the lender's current loans.")
    , command "borrower-history"
        (info pBorrowerHistory $ progDesc "Query the borrower's credit history.")
    ]
  where
    pAllAsks :: Parser Query
    pAllAsks = QueryAllAsks <$> pNetwork <*> pBeaconPolicy <*> pOutput

    pOwnOffers :: Parser Query
    pOwnOffers = QueryOwnOffers <$> pNetwork <*> pBeaconPolicy <*> pLenderId <*> pOutput

    pOwnAsks :: Parser Query
    pOwnAsks = QueryOwnAsks <$> pNetwork <*> pBeaconPolicy <*> pLoanAddress <*> pOutput

    pAllOffers :: Parser Query
    pAllOffers = QueryAllOffers <$> pNetwork <*> pBeaconPolicy <*> pLoanAddress <*> pOutput

    pAllBorrowerLoans :: Parser Query
    pAllBorrowerLoans = 
      QueryAllBorrowerLoans <$> pNetwork <*> pBeaconPolicy <*> pBorrowerId <*> pLoanAddress <*> pOutput

    pAllLenderLoans :: Parser Query
    pAllLenderLoans = QueryAllLenderLoans <$> pNetwork <*> pBeaconPolicy <*> pLenderId <*> pOutput

    pBorrowerHistory :: Parser Query
    pBorrowerHistory = QueryBorrowerHistory <$> pNetwork <*> pBeaconPolicy <*> pBorrowerId <*> pOutput

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

pAskDatum :: Parser LoanDatum
pAskDatum =
  AskDatum
    <$> pBeaconPolicy
    <*> (pubKeyAsToken <$> pBorrowerId)
    <*> pLoanAsset
    <*> pPrinciple
    <*> pTerm
    <*> some pCollateralAsset

pOfferDatum :: Parser LoanDatum
pOfferDatum =
  OfferDatum
    <$> pBeaconPolicy
    <*> (pubKeyAsToken <$> pLenderId)
    <*> pLoanAsset
    <*> pPrinciple
    <*> pTerm
    <*> pInterest
    <*> pCollateralization

pAcceptDatum :: Parser LoanDatum
pAcceptDatum = fmap convertToLoanDatum $
  AcceptDatum'
    <$> pBeaconPolicy
    <*> (pubKeyAsToken <$> pLenderId)
    <*> (pubKeyAsToken <$> pBorrowerId)
    <*> pLoanAsset
    <*> pPrinciple
    <*> pTerm
    <*> pInterest
    <*> pCollateralization
    <*> pExpiration

pRepaymentDatum :: Parser LoanDatum
pRepaymentDatum = fmap convertToLoanDatum $
  RepaymentDatum'
    <$> pBeaconPolicy
    <*> (pubKeyAsToken <$> pLenderId)
    <*> (pubKeyAsToken <$> pBorrowerId)
    <*> pLoanAsset
    <*> pPrinciple
    <*> pTerm
    <*> pInterest
    <*> pCollateralization
    <*> pExpiration
    <*> pCurrentOutstanding
    <*> pPayment

pBeaconPolicy :: Parser CurrencySymbol
pBeaconPolicy = option (eitherReader readCurrencySymbol)
  (  long "beacon-policy-id"
  <> metavar "STRING"
  <> help "Policy id for the dApp's beacon policy.")

pBorrowerId :: Parser PaymentPubKeyHash
pBorrowerId = PaymentPubKeyHash <$> option (eitherReader readPubKeyHash)
  (  long "borrower-stake-pubkey-hash"
  <> metavar "STRING"
  <> help "The staking pubkey hash for the borrower."
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

pLenderId :: Parser PaymentPubKeyHash
pLenderId = PaymentPubKeyHash <$> option (eitherReader readPubKeyHash)
  (  long "lender-payment-pubkey-hash"
  <> metavar "STRING"
  <> help "The payment pubkey hash for the lender."
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

pExpiration :: Parser POSIXTime
pExpiration = slotToPOSIXTime . Slot <$> option auto
  (  long "expiration"
  <> metavar "INT"
  <> help "The slot at which the loan expires."
  )

pCurrentOutstanding :: Parser PlutusRational
pCurrentOutstanding = unsafeRatio <$> pNum <*> pDen
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

pPayment :: Parser Integer
pPayment = option auto
  (  long "payment-amount"
  <> metavar "INT"
  <> help "The number of units of the loan asset being repaid this tx."
  )

pNetwork :: Parser Network
pNetwork = pPreProdTestnet
  where
    pPreProdTestnet :: Parser Network
    pPreProdTestnet = PreProdTestnet <$> strOption
      (  long "preprod-testnet"
      <> metavar "STRING"
      <> help "Query the preproduction testnet using the Blockfrost Api with the supplied api key.")

pOutput :: Parser Output
pOutput = pStdOut <|> File <$> pOutputFile
  where
    pStdOut :: Parser Output
    pStdOut = flag' Stdout
      (  long "stdout"
      <> help "Display to stdout."
      )

pLoanAddress :: Parser LoanAddress
pLoanAddress = LoanAddress <$> strOption
  (  long "loan-address"
  <> metavar "STRING"
  <> help "Loan address in bech32 format."
  )