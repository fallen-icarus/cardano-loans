{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module CLI.Parsers
(
  parseCommand
) where

import Options.Applicative
import Prelude hiding ((-),(*),(+))

import CardanoLoans
import CLI.Types

-------------------------------------------------
-- Main Parsers
-------------------------------------------------
parseCommand :: Parser Command
parseCommand = hsubparser $ mconcat
  [ command "export-script"
      (info pExportScript $ progDesc "Export a dApp plutus script.")
  , command "borrower"
      (info parseBorrowerCmd $ progDesc "Commands for borrowers.")
  , command "lender"
      (info parseLenderCmd $ progDesc "Commands for lenders.")
  ]

-------------------------------------------------
-- Scripts Parser
-------------------------------------------------
pExportScript :: Parser Command
pExportScript = 
    ExportScript
      <$> (pSpending <|> pPolicy)
      <*> pOutputFile
  where
    pSpending :: Parser Script
    pSpending = flag' Spending
      (  long "loan-script"
      <> help "Export the loan validator script."
      )
    
    pPolicy :: Parser Script
    pPolicy = flag' Policy
      (  long "beacon-policy"
      <> help "Export the beacon policy."
      )

-------------------------------------------------
-- Lender Parser
-------------------------------------------------
parseLenderCmd :: Parser Command
parseLenderCmd = fmap LenderCmd $ hsubparser $ mconcat
  [ command "offer-datum"
      (info pCreateOfferDatum $ 
        progDesc "Create the offer datum for a loan offer.")
  , command "claim-loan"
      (info pCreateClaimRedeemer $ 
        progDesc "Create the redeemer for claiming paid/expired loans.")
  , command "offer-beacon" 
      (info pCreateOfferBeaconRedeemer $ 
        progDesc "Create the redeemer for minting offer beacons and lender IDs.")
  , command "burn-beacons"
      (info pCreateBurnRedeemer $ 
        progDesc "Create the redeemer for burning beacons and IDs.")
  , command "close-offer"
      (info pCreateCloseOfferRedeemer $
        progDesc "Create the redeemer for closing open offers.")
  , command "query-asks"
      (info pQueryAsks $ 
        progDesc "Query all the current open Asks.")
  , command "query-open-offers"
      (info pQueryCurrentOffers $
        progDesc "Query all open offers belonging to the lender.")
  , command "query-active-loans"
      (info pQueryCurrentLoans $
        progDesc "Query all active loans associated to the lender.")
  , command "query-borrower-credit-history"
      (info pQueryBorrowerHistory $
        progDesc "Query the borrower's credit history.")
  ]

pCreateOfferDatum :: Parser LenderCmd
pCreateOfferDatum = CreateOfferDatum <$> pOfferDatum <*> pOutputFile
  where
    pOfferDatum :: Parser LoanDatum
    pOfferDatum =
      OfferDatum (beaconSym,TokenName "Offer")
        <$> ((beaconSym,) . pubKeyAsToken <$> pLenderID)
        <*> pLoanAsset
        <*> pPrinciple
        <*> pTerm
        <*> (pInterestFraction <|> pInterest)
        <*> pBacking
        <*> pCollatRatesFraction

pCreateClaimRedeemer :: Parser LenderCmd
pCreateClaimRedeemer = CreateClaimRedeemer <$> pOutputFile

pCreateOfferBeaconRedeemer :: Parser LenderCmd
pCreateOfferBeaconRedeemer = CreateOfferBeaconRedeemer <$> pLenderID <*> pOutputFile

pCreateBurnRedeemer :: Parser LenderCmd
pCreateBurnRedeemer = CreateBurnBeaconRedeemer <$> pOutputFile

pCreateCloseOfferRedeemer :: Parser LenderCmd
pCreateCloseOfferRedeemer = CreateCloseOfferRedeemer <$> pOutputFile

pQueryAsks :: Parser LenderCmd
pQueryAsks = QueryAllAsks <$> pNetwork <*> pOutput

pQueryCurrentOffers :: Parser LenderCmd
pQueryCurrentOffers = QueryLenderCurrentOffers <$> pLenderID <*> pNetwork <*> pOutput

pQueryCurrentLoans :: Parser LenderCmd
pQueryCurrentLoans = QueryLenderCurrentLoans <$> pLenderID <*> pNetwork <*> pOutput

pQueryBorrowerHistory :: Parser LenderCmd
pQueryBorrowerHistory = QueryBorrowerHistory <$> pBorrowerID <*> pNetwork <*> pOutput

-------------------------------------------------
-- Borrower Parser
-------------------------------------------------
parseBorrowerCmd :: Parser Command
parseBorrowerCmd = fmap BorrowerCmd $ hsubparser $ mconcat
  [ command "ask-datum"
      (info pBorrowerAskDatum $ 
        progDesc "Create the ask datum for a new loan.")
  , command "loan-payment-datum"
      (info pBorrowerPaymentActiveDatum $ 
        progDesc "Create the active datum for a loan payment.")
  , command "accept-offer-datum"
      (info pBorrowerAcceptActiveDatum $ 
        progDesc "Create the active datum for accepting a loan offer.")
  , command "ask-beacon"
      (info pCreateAskBeaconRedeemer $ 
        progDesc "Create the redeemer for minting an Ask beacon.")
  , command "active-beacon"
      (info pCreateActiveBeaconRedeemer $ 
        progDesc "Create the redeemer for minting an Active beacon and borrower ID.")
  , command "close-ask"
      (info pCreateCloseAskRedeemer $ 
        progDesc "Create the redeemer for closing an Ask.")
  , command "accept-offer"
      (info pCreateAcceptOfferRedeemer $
        progDesc "Create the redeemer for accepting loan offers.")
  , command "repay"
      (info pCreateRepayRedeemer $
        progDesc "Create the redeemer for making a loan payment.")
  , command "burn-beacons"
      (info pCreateBorrowerBurnBeaconRedeemer $ 
        progDesc "Create the redeemer for burning beacons and IDs.")
  , command "query-asks"
      (info pQueryBorrowerAsks $ 
        progDesc "Query own Asks.")
  , command "query-offers"
      (info pQueryBorrowerOffers $
        progDesc "Query offers.")
  , command "query-active-loans"
      (info pQueryBorrowerOpenLoans $
        progDesc "Query current loans.")
  ]

pBorrowerAskDatum :: Parser BorrowerCmd
pBorrowerAskDatum = BorrowerAskDatum <$> pAskDatum <*> pOutputFile

pBorrowerPaymentActiveDatum :: Parser BorrowerCmd
pBorrowerPaymentActiveDatum = BorrowerPaymentActiveDatum <$> pActiveDatum <*> pOutputFile
  where
    pActiveDatum :: Parser LoanDatum
    pActiveDatum = 
      ActiveDatum (beaconSym,TokenName "Active")
        <$> ((beaconSym,) . pubKeyAsToken <$> pLenderID)
        <*> ((beaconSym,) . pubKeyAsToken <$> pBorrowerID)
        <*> pLoanAsset
        <*> pPrinciple
        <*> pTerm
        <*> pInterestFraction
        <*> pBacking
        <*> pCollatRatesFraction
        <*> pExpiration
        <*> pNewOutstanding

    pNewOutstanding :: Parser PlutusRational
    pNewOutstanding = (\c p -> c - fromGHC (toRational p)) <$> pCurrentOutstanding <*> pPayment

    pCurrentOutstanding :: Parser PlutusRational
    pCurrentOutstanding = unsafeRatio <$> pNum <*> pDen
    
    pNum :: Parser Integer
    pNum = option auto
      (  long "current-balance-numerator"
      <> metavar "INT"
      <> help "The numerator for the loan's current balance."
      )
    
    pDen :: Parser Integer
    pDen = option auto
      (  long "current-balance-denominator"
      <> metavar "INT"
      <> help "The denominator for the loan's current balance."
      )

    pPayment :: Parser Integer
    pPayment = option auto
      (  long "payment-amount"
      <> metavar "INT"
      <> help "The number of units of the loan asset being repaid this tx."
      )

pBorrowerAcceptActiveDatum :: Parser BorrowerCmd
pBorrowerAcceptActiveDatum = BorrowerAcceptActiveDatum <$> pActiveDatum <*> pOutputFile
  where
    pActiveDatum :: Parser LoanDatum
    pActiveDatum = convert <$> pActiveDatum' <*> pStart

    pActiveDatum' :: Parser ActiveDatum'
    pActiveDatum' =
      ActiveDatum' (beaconSym,TokenName "Active")
        <$> ((beaconSym,) . pubKeyAsToken <$> pLenderID)
        <*> ((beaconSym,) . pubKeyAsToken <$> pBorrowerID)
        <*> pLoanAsset
        <*> pPrinciple
        <*> pTerm
        <*> pInterestFraction
        <*> pBacking
        <*> pCollatRatesFraction

    convert :: ActiveDatum' -> POSIXTime -> LoanDatum
    convert (ActiveDatum' ab lId bId lA p t i b cr) ttl = 
      ActiveDatum ab lId bId lA p t i b cr (ttl+t) (calcOutstanding p i)

    calcOutstanding :: Integer -> PlutusRational -> PlutusRational
    calcOutstanding principle interest =
      fromGHC (toRational principle) * (unsafeRatio 1 1 + interest)

    pStart :: Parser POSIXTime
    pStart = slotToPOSIXTime . Slot <$> option auto
      (  long "loan-start"
      <> metavar "INT"
      <> help "The slot at which the loan will start."
      )

pCreateAskBeaconRedeemer :: Parser BorrowerCmd
pCreateAskBeaconRedeemer = CreateAskBeaconRedeemer <$> pBorrowerID <*> pOutputFile

pCreateActiveBeaconRedeemer :: Parser BorrowerCmd
pCreateActiveBeaconRedeemer = CreateActiveBeaconRedeemer <$> pBorrowerID <*> pLenderID <*> pOutputFile

pCreateCloseAskRedeemer :: Parser BorrowerCmd
pCreateCloseAskRedeemer = CreateCloseAskRedeemer <$> pOutputFile

pCreateBorrowerBurnBeaconRedeemer :: Parser BorrowerCmd
pCreateBorrowerBurnBeaconRedeemer = CreateBorrowerBurnBeaconRedeemer <$> pOutputFile

pCreateRepayRedeemer :: Parser BorrowerCmd
pCreateRepayRedeemer = CreateRepayRedeemer <$> pOutputFile

pQueryBorrowerAsks :: Parser BorrowerCmd
pQueryBorrowerAsks = QueryBorrowerCurrentAsks <$> pBorrowerAddr <*> pNetwork <*> pOutput

pQueryBorrowerOffers :: Parser BorrowerCmd
pQueryBorrowerOffers = QueryBorrowerCurrentOffers <$> pBorrowerAddr <*> pNetwork <*> pOutput

pCreateAcceptOfferRedeemer :: Parser BorrowerCmd
pCreateAcceptOfferRedeemer = CreateAcceptOfferRedeemer <$> pOutputFile

pQueryBorrowerOpenLoans :: Parser BorrowerCmd
pQueryBorrowerOpenLoans = QueryBorrowerCurrentLoans <$> pBorrowerID <*> pNetwork <*> pOutput
      
-------------------------------------------------
-- Basic Helper Parsers
-------------------------------------------------
pAskDatum :: Parser LoanDatum
pAskDatum = 
  AskDatum (beaconSym,TokenName "Ask")
    <$> ((beaconSym,) . pubKeyAsToken <$> pBorrowerID)
    <*> pLoanAsset
    <*> pPrinciple
    <*> pTerm
    <*> some pCollateralAsset

pOutputFile :: Parser FilePath
pOutputFile = strOption
  (  long "out-file"
  <> metavar "FILE"
  <> help "The output file."
  <> completer (bashCompleter "file")
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

pInterest :: Parser PlutusRational
pInterest = fromGHC . (toRational :: Double -> Rational) <$> option auto
  ( long "interest-rate"
  <> metavar "DECIMAL"
  <> help "The interest rate for the loan."
  )

pInterestFraction :: Parser PlutusRational
pInterestFraction = unsafeRatio <$> pNum <*> pDen
  where
    pNum :: Parser Integer
    pNum = option auto
      (  long "loan-interest-numerator"
      <> metavar "INT"
      <> help "The numerator for the loan's interest."
      )
    
    pDen :: Parser Integer
    pDen = option auto
      (  long "loan-interest-denominator"
      <> metavar "INT"
      <> help "The denominator for the loan's interest."
      )

pBacking :: Parser Integer
pBacking = option auto
  (  long "required-backing"
  <> metavar "INT"
  <> help "The number of units of the loan asset that needs to be backed by collateral."
  )
    
pCollatRatesFraction :: Parser [((CurrencySymbol,TokenName),PlutusRational)]
pCollatRatesFraction = some pRate'
  where
    pRate' :: Parser ((CurrencySymbol,TokenName),PlutusRational)
    pRate' = (,) <$> pCollateralAsset <*> pCollateralization
    
    pCollateralization :: Parser PlutusRational
    pCollateralization = unsafeRatio <$> pNum <*> pDen

    pNum :: Parser Integer
    pNum = option auto
      (  long "collateral-rate-numerator"
      <> metavar "INT"
      <> help "The numerator for this collateral's rate."
      )
    
    pDen :: Parser Integer
    pDen = option auto
      (  long "collateral-rate-denominator"
      <> metavar "INT"
      <> help "The denominator for this collateral's rate."
      )

pLenderID :: Parser PaymentPubKeyHash
pLenderID = option (eitherReader readPaymentPubKeyHash)
  (  long "lender-payment-pubkey-hash"
  <> metavar "HASH"
  <> help "The payment pubkey hash for the lender ID."
  )

pBorrowerID :: Parser PaymentPubKeyHash
pBorrowerID = option (eitherReader readPaymentPubKeyHash)
  (  long "borrower-stake-pubkey-hash"
  <> metavar "HASH"
  <> help "The stake pubkey hash for the borrower ID."
  )
      
pExpiration :: Parser POSIXTime
pExpiration = POSIXTime <$> option auto
  (  long "expiration-time"
  <> metavar "INT"
  <> help "The time at which the loan expires (POSIX)."
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
    pPreProdTestnet = PreProdTestnet <$> strOption
      (  long "preprod-testnet"
      <> metavar "STRING"
      <> help "Query the preproduction testnet using the Blockfrost Api with the supplied api key.")

pBorrowerAddr :: Parser String
pBorrowerAddr = strOption
  (  long "borrower-address"
  <> metavar "STRING"
  <> help "The borrower's address."
  )