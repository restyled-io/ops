module Hooks.App
    ( appMain
    )
where

import Hooks.Handler
import System.Environment (lookupEnv)
import Web.Scotty

appMain :: IO ()
appMain = do
    port <- maybe 3000 read <$> lookupEnv "PORT"
    scotty port hooksHandler
