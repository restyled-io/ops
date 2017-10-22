module Ops.AWS
    ( runAWS
    , module Network.AWS
    ) where

import Control.Lens
import Network.AWS hiding (runAWS)
import System.IO (stdout)
import qualified Network.AWS as AWS

runAWS :: AWS a -> IO a
runAWS x = do
    lgr <- newLogger Error stdout
    env <- newEnv Discover
    runResourceT $ AWS.runAWS (env & envLogger .~ lgr) x
