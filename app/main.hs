{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Control.Exception (IOException, handle)
import Data.Semigroup ((<>))
import Ops.Commands.Deploy
import Ops.Commands.Logs
import Ops.Commands.Template
import Ops.Commands.Update
import Options.Applicative
import System.Exit (die)

data Options
    = Template
    | Deploy DeployOptions
    | Update UpdateOptions
    | Logs

options :: Parser Options
options = subparser
    (  command "template" (pure Template
        `withInfo` "Output the Cloud Formation template")
    <> command "deploy" (Deploy <$> deployOptions
        `withInfo` "Deploy a new Apps image")
    <> command "update" (Update <$> updateOptions
        `withInfo` "Update Stack parameters")
    <> command "logs" (pure Logs
        `withInfo` "Tail CloudWatch logs")
    )

main :: IO ()
main = handle (\(e :: IOException) -> die $ show e)
    $ execParser (options `withInfo` "Operate Restyled.io") >>= \case
        Template -> templateCommand
        Deploy opts -> deployCommand opts
        Update opts -> updateCommand opts
        Logs -> logsCommand

withInfo :: Parser a -> String -> ParserInfo a
cmd `withInfo` desc = info (cmd <**> helper) $ fullDesc <> progDesc desc
