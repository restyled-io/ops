{-# LANGUAGE DeriveGeneric #-}

module Hooks.Quay
    ( QuayBuild(..)
    )
where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Text         (Text)
import           GHC.Generics

data QuayBuild = QuayBuild
    { qbNamespace    :: Text
    , qbName         :: Text
    , qbDockerUrl    :: Text
    , qbDockerTags   :: [Text]
    , qbErrorMessage :: Maybe Text
    }
    deriving Generic

instance FromJSON QuayBuild where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase
