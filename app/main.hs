{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Data.Semigroup ((<>))
import Ops.Commands.Update
import Options.Applicative

newtype Options
    = Update UpdateOptions

options :: Parser Options
options = subparser
    (  command "update" (Update <$> updateOptions
        `withInfo` "Update Stack parameters")
    )

main :: IO ()
main =
    execParser (options `withInfo` "Operate Restyled.io") >>= \case
        Update opts -> updateCommand opts

withInfo :: Parser a -> String -> ParserInfo a
cmd `withInfo` desc = info (cmd <**> helper) $ fullDesc <> progDesc desc
