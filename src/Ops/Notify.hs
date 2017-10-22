{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Ops.Notify
    ( Notification(..)
    , sendNotification
    ) where

import Control.Monad ((<=<))
import Control.Monad.Except
import Data.Bifunctor (first)
import Data.Monoid ((<>))
import Data.Text (Text)
import Network.Pushover
import System.Environment (getEnv)
import System.IO.Error (tryIOError)
import Text.Shakespeare.Text (st)
import qualified Data.Text as T

data Notification
    -- | Deployed Stack, Image Name, & Tag
    = DeploySuccess Text Text Text

sendNotification :: Notification -> IO (Either String ())
sendNotification n = runExceptT $ do
    aKey <- getToken "PUSHOVER_API_KEY"
    uKey <- getToken "PUSHOVER_USER_KEY"
    fromResponse <=< tryE $ sendMessage aKey uKey $ toMessage n

getToken :: String -> ExceptT String IO PushoverToken
getToken = ExceptT . (first errorMessage . makeToken . T.pack <$>) . getEnv

toMessage :: Notification -> Message
toMessage (DeploySuccess stack name tag) = text [st|
Deployment of #{stack}, image #{name}:#{tag} succeeded
|]

fromResponse :: Response -> ExceptT String IO ()
fromResponse (Response Success _) = pure ()
fromResponse (Response (Failure errs) req) = throwError $ T.unpack $ T.unlines $
    [ "Pushover request errored"
    , "Request: " <> req
    , "Errors: "
    ] ++ errs

tryE :: IO a -> ExceptT String IO a
tryE = ExceptT . (first show <$>) . tryIOError
