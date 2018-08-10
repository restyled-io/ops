{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Ops.Commands.Update
    ( UpdateOptions(..)
    , updateCommand
    , updateOptions
    ) where

import Control.Lens hiding (argument)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as M
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Network.AWS hiding (runAWS)
import Network.AWS.Data.Text (toText)
import Network.AWS.S3
import Ops.AWS
import Ops.CloudFormation.Parameters (cfParameters)
import Ops.CloudFormation.Stack
import Ops.CloudFormation.Template (cfTemplate)
import Options.Applicative
import Stratosphere (encodeTemplate, parameterName, unParameters)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

data UpdateOptions = UpdateOptions
    { uoStackName :: Text
    , uoMessage :: Text
    , uoTemplate :: Bool
    , uoParameters :: [(Text, Text)]
    }

-- brittany-disable-next-binding
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
    where (k, v) = T.break (== '=') $ T.pack x

updateCommand :: UpdateOptions -> IO (Either Text Text)
updateCommand UpdateOptions {..} = do
    mTemplateUrl <- if uoTemplate
        then Just <$> uploadTemplateToS3
        else pure Nothing

    runAWS $ sendStackUpdate uoStackName mTemplateUrl $ withUsePreviousParameters
        uoParameters

    result <- awaitStackUpdate uoStackName
    pure $ if result then Right uoMessage else Left "Stack update failed"

uploadTemplateToS3 :: IO Text
uploadTemplateToS3 = withSystemTempDirectory "" $ \dir -> do
    let tmp = dir </> "template.json"
    BS.writeFile tmp $ encodeTemplate cfTemplate

    runAWS $ do
        body <- chunkedFile defaultChunkSize tmp
        s3Url <$ send (putObject bucketName objectKey body)
  where
    bucketName = "cf-templates-11c45cq7egw69-us-east-1"
    objectKey = "TODO.json"
    s3Url = "https://s3.amazonaws.com/"
        <> toText bucketName
        <> "/"
        <> toText objectKey

withUsePreviousParameters :: [(Text, Text)] -> [(Text, Maybe Text)]
withUsePreviousParameters =
    M.toList
        . M.fromList -- uniq by key
        . (existingParameters ++)
        . map (over _2 Just)
    where existingParameters = map (, Nothing) knownParameters

knownParameters :: [Text]
knownParameters = map parameterName $ unParameters cfParameters
