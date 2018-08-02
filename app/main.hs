{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Control.Monad (when)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Ops.Commands.Update
import Ops.Notify
import Options.Applicative
import System.Exit (die)

newtype Command
    = Update UpdateOptions

data Options = Options
    { oNotify :: Bool
    , oCommand :: Command
    }

-- brittany-disable-next-binding
options :: Parser Options
options = Options
    <$> flag True False
        (  long "no-notify"
        <> help "Disable notifications"
        )
    <*> subparser
        (  command "update" (Update <$> updateOptions
            `withInfo` "Update Stack parameters")
        )

main :: IO ()
main = do
    opts <- execParser $ options `withInfo` "Operate Restyled.io"
    result <- runCommand $ oCommand opts
    when (oNotify opts) $ sendNotification $ either id id result

    case result of
        Left err -> die $ T.unpack err
        Right msg -> T.putStrLn msg

runCommand :: Command -> IO (Either Text Text)
runCommand = \case
    Update opts -> updateCommand opts

withInfo :: Parser a -> String -> ParserInfo a
withInfo cmd = info (cmd <**> helper) . (fullDesc <>) . progDesc
