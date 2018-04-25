{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Ops.Commands.Logs
    ( logsCommand
    ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Lens
import Control.Monad (forever, void)
import Control.Monad.IO.Class
import Data.Foldable (for_)
import Data.Maybe (fromMaybe, mapMaybe)
-- TODO: Fix Escaped for Semigroup
import Data.Function (on)
import Data.List (nubBy)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Escaped
import qualified Data.Text.IO as T
import Data.Time
import Data.Time.Clock.POSIX
import Network.AWS.CloudWatchLogs
import Numeric.Natural
import Ops.AWS
import System.Random (randomRIO)

data LogEvent = LogEvent
    { leStream :: Text
    , leTimestamp :: UTCTime
    , leMessage :: Text
    }

logsCommand :: IO ()
logsCommand = do
    -- TODO: CLI options?
    (group, start) <- (,)
        <$> pure "RestyledProdApps"
        <*> getCurrentTime

    allNames <- runAWS $ do
        resp <- send
            $ describeLogStreams group
            & dlssOrderBy ?~ LastEventTime
            & dlssDescending ?~ True

        pure
            $ resp ^. dlsrsLogStreams
            & mapMaybe (^. lsLogStreamName)

    -- We only need the most recent stream for each appName
    let names = nubBy ((==) `on` appName) allNames
    T.putStrLn $ "Streams:\n" <> T.unlines (map ("  - " <>) names)

    chan <- newChan
    for_ names $ void . async . runAWS . streamToChannel start chan group
    forever $ T.putStrLn . formatEvent =<< readChan chan

-- | Fetch events in a LogStream forever, writing each to the channel
streamToChannel :: UTCTime -> Chan LogEvent -> Text -> Text -> AWS a
streamToChannel start chan group name = go (Just start) Nothing
  where
    go mStart mToken = do
        resp <- send
            $ getLogEvents group name
            & gleNextToken .~ mToken
            & gleStartFromHead ?~ True
            & gleStartTime .~ (toMilliseconds <$> mStart)

        for_ (resp ^. glersEvents) $ \event ->
            liftIO $ writeChan chan LogEvent
                { leStream = name
                , leTimestamp =
                    event ^. oleTimestamp
                        & fromMaybe 0
                        & fromMilliseconds
                , leMessage = event ^. oleMessage & fromMaybe ""
                }

        logFetchDelay 1
        go Nothing $ resp ^. glersNextForwardToken

formatEvent :: LogEvent -> Text
formatEvent LogEvent{..} = render
    $ lightGray (Plain $ T.pack $ show leTimestamp)
    <> " " <> magenta (Plain $ appName leStream)
    <> " " <> Plain leMessage

logFetchDelay :: MonadIO m => Int -> m ()
logFetchDelay s = liftIO $ do
    -- offset +/-0.25s to prevent fetching all streams at once
    jitter <- randomRIO (-25000, 25000)
    threadDelay $ (+ jitter) $ (* 1000000) s

appName :: Text -> Text
appName = T.takeWhile (/= '/')

toMilliseconds :: UTCTime -> Natural
toMilliseconds = (* 1000) . round . utcTimeToPOSIXSeconds

fromMilliseconds :: Natural -> UTCTime
fromMilliseconds = posixSecondsToUTCTime . (/ 1000) . realToFrac
