{-# LANGUAGE OverloadedStrings #-}

module Hooks.Handler
    ( hooksHandler
    )
where

import           Data.Foldable             (traverse_)
import qualified Hooks.Restyled            as Restyled
import           Network.HTTP.Types.Status (status200)
import           Web.Scotty

hooksHandler :: ScottyM ()
hooksHandler = post "/hooks" $ do
    build <- jsonData
    liftAndCatchIO $ traverse_ Restyled.deploy $ Restyled.fromQuayBuild build
    status status200
