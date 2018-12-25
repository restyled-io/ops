{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hooks.App
    ( appMain
    )
where

import Control.Monad (void)
import Data.Aeson
import Data.Aeson.Casing
import Data.ByteString.Lazy (ByteString)
import Data.Foldable (for_)
import Data.List (find)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import GHC.Generics
import LoadEnv
import Network.HTTP.Types.Status (status200)
import qualified Network.Pushover as Pushover
import Network.Wai.Middleware.RequestLogger
import System.Environment (getEnv, lookupEnv)
import System.Process (callProcess)
import Web.Scotty

data QuayBuild = QuayBuild
    { qbRepository :: Text
    , qbDockerUrl :: Text
    , qbDockerTags :: [Text]
    , qbErrorMessage :: Maybe Text
    }
    deriving Generic

instance FromJSON QuayBuild where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

appMain :: IO ()
appMain = do
    loadEnv
    port <- maybe 3000 read <$> lookupEnv "PORT"
    scotty port $ do
        middleware logStdoutDev

        post "/hooks" $ do
            bs <- body
            whenDecodes bs $ liftAndCatchIO . deployQuayBuild
            status status200

-- | When/if the body JSON-decodes to the type you need, run the action
whenDecodes :: (Applicative f, FromJSON a) => ByteString -> (a -> f ()) -> f ()
whenDecodes bs f = either (const $ pure ()) f $ eitherDecode bs

deployQuayBuild :: QuayBuild -> IO ()
deployQuayBuild build
    | Just err <- qbErrorMessage build
    = pushover $ "Quay build of " <> qbDockerUrl build <> " failed: " <> err
    | otherwise
    = case qbRepository build of
        "restyled-io/restyled.io" -> do
            deployPrefixToProcessType build "v" "web"
            deployPrefixToProcessType build "backend-v" "backend"

        -- TODO: restyled-io/restyler
        -- Update Heroku app RESTYLER_IMAGE=image RESTYLER_TAG=tag

        -- TODO: restyled-io/ops
        -- docker-run-d a script to restart this very service

        _ -> pure ()

deployPrefixToProcessType :: QuayBuild -> Text -> Text -> IO ()
deployPrefixToProcessType QuayBuild {..} prefix processType =
    for_ matchingTag
        $ deployImageToHeroku "restyled-io" processType
        . prependDockerUrl
  where
    matchingTag = find (prefix `T.isPrefixOf`) qbDockerTags
    prependDockerUrl tag = qbDockerUrl <> ":" <> tag

-- brittany-disable-next-binding

deployImageToHeroku :: Text -> Text -> Text -> IO ()
deployImageToHeroku app processType image = do
    email <- getEnv "HEROKU_EMAIL"
    password <- getEnv "HEROKU_API_KEY"

    callProcess "docker"
        [ "login"
        , "--username", email
        , "--password", password
        , unpack herokuRegistry
        ]

    callProcess "docker" ["pull", unpack image]
    callProcess "docker" ["tag", unpack image, unpack herokuImage]
    callProcess "docker" ["push", unpack herokuImage]
    pushover $ image <> " deployed to " <> app <> "/" <> processType
  where
    herokuRegistry = "registry.heroku.com"
    herokuImage = herokuRegistry <> "/" <> app <> "/" <> processType

pushover :: Text -> IO ()
pushover msg = do
    apiKey <- makeToken <$> getEnv "PUSHOVER_API_KEY"
    userKey <- makeToken <$> getEnv "PUSHOVER_USER_KEY"
    void $ Pushover.sendMessage apiKey userKey $ Pushover.text msg
  where
    makeToken :: String -> Pushover.PushoverToken
    makeToken = either (error . show) id . Pushover.makeToken . pack
