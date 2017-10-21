{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Ops.Commands.Deploy
    ( DeployOptions(..)
    , deployCommand
    ) where

import Control.Lens
import Control.Monad (void)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Ops.AWS
import Ops.CloudFormation.Parameters (cfParameters)
import Ops.Notify
import Options.Generic
import Stratosphere (parameterName, unParameters)
import System.Exit (die)
import qualified Data.Map as M
import qualified Network.AWS.CloudFormation as AWS
import qualified Network.AWS.Waiter as AWS

data DeployOptions = DeployOptions
    { doStackName :: Text
    , doImageName :: Maybe Text
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
deployCommand DeployOptions{..} = do
    updateStack doStackName $ withUsePreviousParameters
        [ ("AppsImageName", doImageName)
        , ("AppsImageTag", Just doImageTag)
        ]

    either die return =<< sendNotification
        (DeploySuccess doStackName doImageName doImageTag)

withUsePreviousParameters :: [(Text, Maybe Text)] -> [(Text, Maybe Text)]
withUsePreviousParameters = M.toList . M.fromList . (knownParameters ++)
  where
    knownParameters = map ((, Nothing) . parameterName) $ unParameters cfParameters

updateStack :: Text -> [(Text, Maybe Text)] -> IO ()
updateStack name params = do
    void $ runAWS
        $ AWS.updateStack name
        & AWS.usUsePreviousTemplate ?~ True
        & AWS.usParameters .~ toParameters params
        & AWS.usCapabilities .~
            [ AWS.CapabilityIAM
            , AWS.CapabilityNamedIAM
            ]

    putStrLn "Stack updated, awaiting..."
    result <- awaitAWS AWS.stackUpdateComplete
        (AWS.describeStacks & AWS.dStackName ?~ name)

    case result of
        AWS.AcceptSuccess -> putStrLn "Success."
        _ -> die "Stack update failed, see AWS console for details."
  where
    toParameters = map (uncurry toParameter)
    toParameter k mv = AWS.parameter & AWS.pParameterKey ?~ k & maybe
        (AWS.pUsePreviousValue ?~ True)
        (AWS.pParameterValue ?~) mv
