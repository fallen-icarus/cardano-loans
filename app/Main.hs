module Main where

import Options.Applicative

import CLI.Parsers
import CLI.Run

main :: IO ()
main = do
  let preferences = prefs $ showHelpOnError <> showHelpOnEmpty
      opts = info (parseCommand <**> helper) (fullDesc <> progDesc "A p2p Cardano loans PoC")
  customExecParser preferences opts >>= runCommand