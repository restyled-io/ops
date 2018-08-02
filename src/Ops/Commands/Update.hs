{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Ops.Commands.Update
    ( UpdateOptions(..)
    , updateCommand
    , updateOptions
    ) where

import Control.Lens hiding (argument)
import Data.ByteString.Lazy (toStrict)
import qualified Data.Map as M
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Ops.AWS
import Ops.CloudFormation.Parameters (cfParameters)
import Ops.CloudFormation.Stack
import Ops.CloudFormation.Template (cfTemplate)
import Options.Applicative
import Stratosphere (encodeTemplate, parameterName, unParameters)

data UpdateOptions = UpdateOptions
    { uoStackName :: Text
    , uoMessage :: Text
    , uoTemplate :: Bool
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
    <*> switch
        (  long "template"
        <> help "Also update the Template"
        )
    <*> many (argument (eitherReader readParameterUpdate)
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
-- >>> readParameterUpdate "Environment"
-- Left "Invalid KEY, use KEY= to unset"
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
    | not ("=" `T.isPrefixOf` v) = Left "Invalid KEY, use KEY= to unset"
    | otherwise = Right (k, T.drop 1 v)
  where
    (k, v) = T.break (== '=') $ T.pack x

updateCommand :: UpdateOptions -> IO (Either Text Text)
updateCommand UpdateOptions {..} = do
    let templateBody = decodeUtf8 $ toStrict $ encodeTemplate cfTemplate
        mTemplate = if uoTemplate then Just templateBody else Nothing

    runAWS
        $ sendStackUpdate uoStackName mTemplate
        $ withUsePreviousParameters uoParameters

    result <- awaitStackUpdate uoStackName
    pure $ if result then Right uoMessage else Left "Stack update failed"

withUsePreviousParameters :: [(Text, Text)] -> [(Text, Maybe Text)]
withUsePreviousParameters = M.toList . M.fromList -- uniq by key
    . (existingParameters ++)
    . map (over _2 Just)
  where
    existingParameters = map (, Nothing) knownParameters

knownParameters :: [Text]
knownParameters = map parameterName $ unParameters cfParameters
