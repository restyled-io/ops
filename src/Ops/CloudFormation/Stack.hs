{-# LANGUAGE OverloadedStrings #-}

module Ops.CloudFormation.Stack
    ( sendStackUpdate
    , awaitStackUpdate
    ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable (for_, traverse_)
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Escaped
import qualified Data.Text.IO as T
import Data.Time
import Network.AWS.CloudFormation
import Network.AWS.CloudFormation.Ext
import Network.AWS.Waiter
import Ops.AWS
import Safe (headMay)

sendStackUpdate :: Text -> Maybe Text -> [(Text, Maybe Text)] -> AWS ()
sendStackUpdate name mTemplate params = void
    $ send
    $ updateStack name
    & toTemplateSetter mTemplate
    & usParameters .~ toParameters params
    & usCapabilities .~
        [ CapabilityIAM
        , CapabilityNamedIAM
        ]
  where
    toTemplateSetter (Just body) = usTemplateBody ?~ body
    toTemplateSetter Nothing = usUsePreviousTemplate ?~ True

    toParameters = map (uncurry toParameter)
    toParameter k mv = parameter & pParameterKey ?~ k & maybe
        (pUsePreviousValue ?~ True)
        (pParameterValue ?~) mv

-- | Await an update while printing events
--
-- Returns @'True'@ on success.
--
awaitStackUpdate :: Text -> IO Bool
awaitStackUpdate name = do
    a <- async $ runAWS $ streamStackEvents name
    result <- runAWS $ await stackUpdateComplete
        (describeStacks & dStackName ?~ name)

    cancel a
    pure $ result == AcceptSuccess

streamStackEvents :: Text -> AWS a
streamStackEvents name = go Nothing
  where
    go mLastEvent = do
        resp <- send
            $ DescribeStackEvents'
            $ describeStackEvents
            & dseStackName ?~ name

        let allEvents = resp ^. dserspStackEvents
            mNextLastEvent = headMay allEvents

        -- Ignore the first batch of historical events by only printing if we
        -- were given a last event to pivot by.
        for_ mLastEvent $ \lastEvent ->
            printStackEvents $ reverse $ takeWhile (/= lastEvent) allEvents

        -- Avoid API limits
        liftIO $ threadDelay $ 1 * 1000000
        go mNextLastEvent

printStackEvents :: MonadIO m => [CustomStackEvent] -> m ()
printStackEvents = traverse_ $ liftIO . T.putStrLn . formatStackEvent

formatStackEvent :: CustomStackEvent -> Text
formatStackEvent event = render
    $ lightGray (Plain timestamp)
    <> " " <> statusEscape (Plain status)
    <> " " <> Plain resourceId
    <> " " <> Plain reason
  where
    timestamp = T.pack
        $ formatTime defaultTimeLocale "%F %T %Z"
        $ event ^. cseTimestamp

    status = fromMaybe "UNKNOWN" $ event ^. cseResourceStatus
    resourceId = fromMaybe "" $ event ^. cseLogicalResourceId
    reason = fromMaybe "" $ event ^. cseResourceStatusReason
    statusEscape = case status of
        "CREATE_IN_PROGRESS" -> yellow
        "CREATE_FAILED" -> red
        "CREATE_COMPLETE" -> green
        "ROLLBACK_IN_PROGRESS" -> red
        "ROLLBACK_FAILED" -> red
        "ROLLBACK_COMPLETE" -> green
        "DELETE_IN_PROGRESS" -> yellow
        "DELETE_FAILED" -> red
        "DELETE_COMPLETE" -> green
        "DELETE_SKIPPED" -> yellow
        "UPDATE_IN_PROGRESS" -> yellow
        "UPDATE_FAILED" -> red
        "UPDATE_COMPLETE_CLEANUP_IN_PROGRESS" -> yellow
        "UPDATE_COMPLETE" -> green
        "UPDATE_ROLLBACK_IN_PROGRESS" -> red
        "UPDATE_ROLLBACK_FAILED" -> red
        "UPDATE_ROLLBACK_COMPLETE_CLEANUP_IN_PROGRESS" -> yellow
        "UPDATE_ROLLBACK_COMPLETE" -> green
        "REVIEW_IN_PROGRESS" -> blue
        _ -> blue
