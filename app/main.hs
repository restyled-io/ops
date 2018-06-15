{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Control.Exception (IOException, handle)
import Data.Semigroup ((<>))
import Ops.Commands.Update
import Options.Applicative
import System.Exit (die)

newtype Options
    = Update UpdateOptions

options :: Parser Options
options = subparser
    (  command "update" (Update <$> updateOptions
        `withInfo` "Update Stack parameters")
    )

main :: IO ()
main = handle (\(e :: IOException) -> die $ show e)
    $ execParser (options `withInfo` "Operate Restyled.io") >>= \case
        Update opts -> updateCommand opts

withInfo :: Parser a -> String -> ParserInfo a
cmd `withInfo` desc = info (cmd <**> helper) $ fullDesc <> progDesc desc
