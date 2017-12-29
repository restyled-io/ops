{-# LANGUAGE OverloadedStrings #-}
module Ops.Notify
    ( sendNotification
    ) where

import Control.Monad ((<=<))
import Control.Monad.Except
import Data.Bifunctor (first)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Network.Pushover
import System.Environment (getEnv)
import System.IO.Error (tryIOError)

sendNotification :: Text -> IO (Either String ())
sendNotification msg = runExceptT $ do
    aKey <- getToken "PUSHOVER_API_KEY"
    uKey <- getToken "PUSHOVER_USER_KEY"
    fromResponse <=< tryE $ sendMessage aKey uKey $ text msg

getToken :: String -> ExceptT String IO PushoverToken
getToken = ExceptT . (first errorMessage . makeToken . T.pack <$>) . getEnv

fromResponse :: Response -> ExceptT String IO ()
fromResponse (Response Success _) = pure ()
fromResponse (Response (Failure errs) req) = throwError $ T.unpack $ T.unlines $
    [ "Pushover request errored"
    , "Request: " <> req
    , "Errors: "
    ] ++ errs

tryE :: IO a -> ExceptT String IO a
tryE = ExceptT . (first show <$>) . tryIOError
