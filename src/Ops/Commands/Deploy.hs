{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Ops.Commands.Deploy
    ( DeployOptions(..)
    , deployCommand
    ) where

import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Ops.Commands.Update
import Options.Generic

data DeployOptions = DeployOptions
    { doStackName :: Text
    , doImageName :: Text
    , doImageTag :: Text
    }
    deriving Generic

unPrefixLispCaseModifiers :: String -> Modifiers
unPrefixLispCaseModifiers x = defaultModifiers
    { fieldNameModifier = \y -> fieldNameModifier lispCaseModifiers
        $ fromMaybe y $ stripPrefix x y
    }

instance ParseRecord DeployOptions where
    parseRecord = parseRecordWithModifiers $ unPrefixLispCaseModifiers "do"

deployCommand :: DeployOptions -> IO ()
deployCommand DeployOptions{..} =
    updateCommand UpdateOptions
        { uoStackName = doStackName
        , uoParameters =
            [ ("AppsImageName=", doImageName)
            , ("AppsImageTag=", doImageTag)
            ]
        , uoMessage = "restyled.io updated to " <> doImageTag
        }
