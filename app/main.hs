{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Semigroup ((<>))
import Ops.Commands.Deploy
import Ops.Commands.Template
import Ops.Commands.Update
import Options.Applicative

data Options
    = Template
    | Deploy DeployOptions
    | Update UpdateOptions

options :: Parser Options
options = subparser
    (  command "template" (pure Template
        `withInfo` "Output the Cloud Formation template")
    <> command "deploy" (Deploy <$> deployOptions
        `withInfo` "Deploy a new Apps image")
    <> command "update" (Update <$> updateOptions
        `withInfo` "Update Stack parameters")
    )

main :: IO ()
main = execParser (options `withInfo` "Operate Restyled.io") >>= \case
    Template -> templateCommand
    Deploy opts -> deployCommand opts
    Update opts -> updateCommand opts

withInfo :: Parser a -> String -> ParserInfo a
cmd `withInfo` desc = info (cmd <**> helper) $ fullDesc <> progDesc desc
