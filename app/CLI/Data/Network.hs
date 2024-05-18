module CLI.Data.Network where

import Relude 

data Network
  = PreProdTestnet
  | Mainnet
  deriving (Eq)

instance ToText Network where
  toText Mainnet = "mainnet"
  toText PreProdTestnet = "preproduction testnet"
