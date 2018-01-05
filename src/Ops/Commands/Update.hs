{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Ops.Commands.Update
    ( UpdateOptions(..)
    , updateCommand
    , updateOptions
    ) where

import Control.Lens hiding (argument)
import Control.Monad (void)
import qualified Data.Map as M
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Network.AWS.CloudFormation
import Network.AWS.Waiter
import Ops.AWS
import Ops.CloudFormation.Parameters (cfParameters)
import Ops.Notify
import Options.Applicative
import Stratosphere (parameterName, unParameters)
import System.Exit (die)

data UpdateOptions = UpdateOptions
    { uoStackName :: Text
    , uoMessage :: Text
    , uoParameters :: [(Text, Text)]
    }

updateOptions :: Parser UpdateOptions
updateOptions = UpdateOptions
    <$> (T.pack <$> strOption
        (  long "stack-name"
        <> value "RestyledProd"
        ))
    <*> (T.pack <$> strOption
        (  long "message"
        <> help "Notification message on success"
        <> value "Stack parameters updated"
        ))
    <*> some (argument (eitherReader readParameterUpdate)
        ( metavar "KEY=VALUE"
        ))

-- | Read KEY=VALUE and validate KEY
--
-- >>> readParameterUpdate "=VALUE"
-- Left "Missing KEY"
--
-- >>> readParameterUpdate "MadeUp=VALUE"
-- Left "Unknown parameter"
--
-- >>> readParameterUpdate "Environment="
-- Right ("Environment","")
--
-- >>> readParameterUpdate "Environment=Some Value = Yup"
-- Right ("Environment","Some Value = Yup")
--
readParameterUpdate :: String -> Either String (Text, Text)
readParameterUpdate x
    | T.null k = Left "Missing KEY"
    | k `notElem` knownParameters = Left "Unknown parameter"
    | otherwise = Right (k, v)
  where
    (k, v) = over _2 (T.drop 1) $ T.break (== '=') $ T.pack x

updateCommand :: UpdateOptions -> IO ()
updateCommand UpdateOptions{..} = do
    deployStack uoStackName $ withUsePreviousParameters uoParameters
    sendNotification uoMessage

withUsePreviousParameters :: [(Text, Text)] -> [(Text, Maybe Text)]
withUsePreviousParameters = M.toList . M.fromList -- uniq by key
    . (existingParameters ++)
    . map (over _2 Just)
  where
    existingParameters = map (, Nothing) knownParameters

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

knownParameters :: [Text]
knownParameters = map parameterName $ unParameters cfParameters
