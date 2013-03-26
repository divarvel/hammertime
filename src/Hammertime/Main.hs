
module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (UTCTime(..), getCurrentTime)
import Data.Time.Calendar (addDays)
import Data.Version (showVersion)

import Paths_hammertime

import Hammertime.CLI
import Hammertime.Reports
import Hammertime.Storage
import qualified Hammertime.Storage.File as Store
import qualified Hammertime.Types as Types


processAction :: UTCTime -> Action -> IO ()
processAction now (Start p n ts) = Store.runStorage $ appendEvent $ Types.Start (Types.Activity (T.pack p) (T.pack n) (map T.pack ts)) now
processAction now (Stop) = Store.runStorage $ appendEvent $ Types.Stop now
processAction now (Report s p n t t') = TIO.putStr =<< Store.runStorage ( generateReport t' (timeSpanToRange s now) (fmap T.pack p) (fmap T.pack n) (fmap T.pack t) )
processAction now (Current) = TIO.putStr =<< Store.runStorage ( currentActivity now )
processAction _ (Help) = putStr showHelp
processAction _ (Version) = putStrLn $ "Hammertime v" ++ showVersion version


timeSpanToRange :: Types.TimeSpan -> UTCTime -> Types.TimeRange
timeSpanToRange Types.Day now@(UTCTime day dt) = (UTCTime (addDays (-1) day) dt, now)
timeSpanToRange Types.Week now@(UTCTime day dt) = (UTCTime (addDays (-7) day) dt, now)
timeSpanToRange Types.Month now@(UTCTime day dt) = (UTCTime (addDays (-30) day) dt, now)

main :: IO ()
main = do
    Store.runStorage initStorage
    now <- getCurrentTime
    act <- getAction
    processAction now act
