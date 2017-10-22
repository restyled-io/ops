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
import Network.AWS.CloudFormation
import Network.AWS.Waiter
import Ops.AWS
import Ops.CloudFormation.Parameters (cfParameters)
import Ops.Notify
import Options.Generic
import Stratosphere (parameterName, unParameters)
import System.Exit (die)
import qualified Data.Map as M

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
deployCommand DeployOptions{..} = do
    deployStack doStackName $ withUsePreviousParameters
        [ ("AppsImageName", Just doImageName)
        , ("AppsImageTag", Just doImageTag)
        ]

    either die return =<< sendNotification
        (DeploySuccess doStackName doImageName doImageTag)

withUsePreviousParameters :: [(Text, Maybe Text)] -> [(Text, Maybe Text)]
withUsePreviousParameters = M.toList . M.fromList . (knownParameters ++)
  where
    knownParameters = map ((, Nothing) . parameterName) $ unParameters cfParameters

deployStack :: Text -> [(Text, Maybe Text)] -> IO ()
deployStack name params = do
    void $ runAWS $ send
        $ updateStack name
        & usUsePreviousTemplate ?~ True
        & usParameters .~ toParameters params
        & usCapabilities .~
            [ CapabilityIAM
            , CapabilityNamedIAM
            ]

    putStrLn "Stack updated, awaiting..."
    result <- runAWS $ await stackUpdateComplete
        (describeStacks & dStackName ?~ name)

    case result of
        AcceptSuccess -> putStrLn "Success."
        _ -> die "Stack update failed, see AWS console for details."
  where
    toParameters = map (uncurry toParameter)
    toParameter k mv = parameter & pParameterKey ?~ k & maybe
        (pUsePreviousValue ?~ True)
        (pParameterValue ?~) mv
