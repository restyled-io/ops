{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Ops.Commands.Deploy
    ( DeployOptions(..)
    , deployOptions
    , deployCommand
    ) where

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Ops.Commands.Update
import Options.Applicative

data DeployOptions = DeployOptions
    { doStackName :: Text
    , doImageName :: Text
    , doImageTag :: Text
    }

deployOptions :: Parser DeployOptions
deployOptions = DeployOptions
    <$> (T.pack <$> strOption
        (  long "stack-name"
        <> value "RestyledProd"
        ))
    <*> (T.pack <$> strOption
        (  long "image-name"
        <> value "restyled/restyled.io"
        ))
    <*> (T.pack <$> strOption
        (  long "image-tag"
        ))

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
