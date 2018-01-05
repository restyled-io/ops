{-# LANGUAGE OverloadedStrings #-}
module Ops.Notify
    ( sendNotification
    ) where

import Control.Exception.Safe (throwString)
import Control.Monad ((<=<))
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Network.Pushover
import System.Environment (getEnv)

sendNotification :: Text -> IO ()
sendNotification msg = do
    aKey <- getToken "PUSHOVER_API_KEY"
    uKey <- getToken "PUSHOVER_USER_KEY"
    checkResponse =<< sendMessage aKey uKey (text msg)

getToken :: String -> IO PushoverToken
getToken =
    either (throwString . errorMessage) pure . makeToken . T.pack <=< getEnv

checkResponse :: Response -> IO ()
checkResponse (Response Success _) = pure ()
checkResponse (Response (Failure errs) req) = throwString $ T.unpack $ T.unlines $
    [ "Pushover request errored"
    , "Request: " <> req
    , "Errors: "
    ] ++ errs

