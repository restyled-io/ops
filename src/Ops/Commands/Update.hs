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
import Data.List (isSuffixOf)
import qualified Data.Map as M
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
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
    , uoTemplate :: TemplateOption
    , uoParameters :: [(Text, Text)]
    }

data TemplateOption
    = New -- ^ Use the template defined here
    | RollBack ObjectKey -- ^ Use a previously uploaded template in S3
    | None -- ^ Don't update the template

parseTemplateOption :: String -> Either String TemplateOption
parseTemplateOption "new" = Right New
parseTemplateOption "none" = Right None
parseTemplateOption path =
    Right $ RollBack $ ObjectKey $ T.pack $ if ".json" `isSuffixOf` path
        then path
        else path <> ".json"

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
    <*> option (eitherReader parseTemplateOption)
        (  long "template"
        <> help "How to update the Template"
        <> metavar "none|new|{name(.json)}"
        <> value None
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
    mObjectKey <- case uoTemplate of
        New -> Just <$> uploadTemplateToS3
        RollBack objectKey -> pure $ Just objectKey
        None -> pure Nothing

    runAWS
        $ sendStackUpdate uoStackName (cfTemplateUrl <$> mObjectKey)
        $ withUsePreviousParameters uoParameters

    result <- awaitStackUpdate uoStackName
    pure $ if result then Right uoMessage else Left "Stack update failed"

uploadTemplateToS3 :: IO ObjectKey
uploadTemplateToS3 = withSystemTempDirectory "" $ \dir -> do
    let tmp = dir </> "template.json"
    BS.writeFile tmp $ encodeTemplate cfTemplate

    objectKey <-
        ObjectKey
        . T.pack
        . formatTime defaultTimeLocale "%Y%M%d_%s.json"
        <$> getCurrentTime

    runAWS $ do
        body <- chunkedFile defaultChunkSize tmp
        objectKey <$ send (putObject cfBucketName objectKey body)

cfBucketName :: BucketName
cfBucketName = "cf-templates-11c45cq7egw69-us-east-1"

cfTemplateUrl :: ObjectKey -> Text
cfTemplateUrl objectKey =
    "https://s3.amazonaws.com/"
        <> toText cfBucketName
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
