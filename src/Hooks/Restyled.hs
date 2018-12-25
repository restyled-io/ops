{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Hooks.Restyled
    ( fromQuayBuild
    , deploy
    )
where

import           Control.Applicative ((<|>))
import           Control.Monad       (guard)
import           Data.Maybe          (listToMaybe)
import           Data.Text           (Text, unpack)
import qualified Data.Text           as T
import           Hooks.Quay
import           System.Environment  (getEnv)
import           System.Process      (callProcess)

data App
    = Restyled Text Text
    | RestyledBackend Text Text
    | Restyler Text Text
    | Ops Text Text

fromQuayBuild :: QuayBuild -> Maybe App
fromQuayBuild QuayBuild {..} = do
    guard $ qbNamespace == "restyled-io"

    case qbName of
        "restyled.io" ->
            (RestyledBackend qbDockerUrl <$> tagPrefixed "backend-v")
                <|> (Restyled qbDockerUrl <$> tagPrefixed "v")
        "restyler" -> Restyler qbDockerUrl <$> tagPrefixed "v"
        "ops" -> Ops qbDockerUrl <$> findTag (== "latest")
        _ -> Nothing
  where
    findTag f = listToMaybe $ filter f qbDockerTags
    tagPrefixed p = findTag (p `T.isPrefixOf`)

deploy :: App -> IO ()
deploy (Restyled image tag) = do
    dockerPull image tag
    dockerLoginHeroku
    dockerTag image tag "registry.heroku.com/restyled-io/web"
    dockerPush "registry.heroku.com/restyled-io/web"

deploy (RestyledBackend image tag) = do
    dockerPull image tag
    dockerLoginHeroku
    dockerTag image tag "registry.heroku.com/restyled-io/backend"
    dockerPush "registry.heroku.com/restyled-io/backend"

deploy (Restyler _image _tag) = undefined
    -- Update Heroku app RESTYLER_IMAGE=image RESTYLER_TAG=tag

deploy (Ops image tag) = do
    callProcess "docker" ["system", "prune", "-a"]
    dockerPull image tag
    callProcess
        "docker"
        [ "run"
        , "-d"
        , "--volume"
        , "/var/run/docker.sock:/var/run/docker.sock"
        , "sh"
        , "-c"
        , unpack restartScript
        ]
  where
    restartScript = T.intercalate
        " && "
        [ "docker stop hooks"
        , "docker rm hooks"
        , "docker run -d --name ops "
        <> "--volume /tmp:/tmp "
        <> "--volume /var/run/docker.sock:/var/run/docker.sock "
        <> image
        <> ":"
        <> tag
        ]

dockerPull :: Text -> Text -> IO ()
dockerPull image tag =
    callProcess "docker" ["pull", unpack image <> ":" <> unpack tag]

dockerTag
    :: Text -- ^ Source image
    -> Text -- ^ Source tag
    -> Text -- ^ Destination tag
    -> IO ()
dockerTag image tag to =
    callProcess "docker" ["tag", unpack image <> ":" <> unpack tag, unpack to]

-- brittany-disable-next-binding

dockerLoginHeroku :: IO ()
dockerLoginHeroku = do
    email <- getEnv "HEROKU_EMAIL"
    password <- getEnv "HEROKU_API_KEY"
    callProcess "docker"
        ["login"
        , "--username" , email
        , "--password" , password
        , "registry.heroku.com"
        ]

dockerPush :: Text -> IO ()
dockerPush image = callProcess "docker" ["push", unpack image]
