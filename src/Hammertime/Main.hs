
module Main where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (UTCTime(..), getCurrentTime)
import Data.Time.Calendar (addDays)
import Options.Applicative
import System.Environment.XDG.BaseDir (getUserDataDir, getUserDataFile)

import Hammertime.CLI
import Hammertime.Reports
import Hammertime.Storage
import qualified Hammertime.Storage.File as Store
import qualified Hammertime.Types as Types

eventFile, dataDir :: IO FilePath
eventFile =  getUserDataFile "hammertime" "events"
dataDir = getUserDataDir "hammertime"

processAction :: MonadStorage m => Config ->  UTCTime -> Action -> m ()
processAction cfg now (Start p n ts) = appendEvent cfg $ Types.Start (Types.Activity (T.pack p) (T.pack n) (map T.pack ts)) now
processAction cfg now (Stop) = appendEvent cfg $ Types.Stop now
processAction cfg now (Report s p n t t') = do
    report <- generateReport cfg t' (timeSpanToRange s now) (fmap T.pack p) (fmap T.pack n) (fmap T.pack t)
    liftIO $ TIO.putStr report
processAction cfg now (Current) = do
    current <- currentActivity cfg now
    liftIO $ TIO.putStr current


timeSpanToRange :: Types.TimeSpan -> UTCTime -> Types.TimeRange
timeSpanToRange Types.Day now@(UTCTime day dt) = (UTCTime (addDays (-1) day) dt, now)
timeSpanToRange Types.Week now@(UTCTime day dt) = (UTCTime (addDays (-7) day) dt, now)
timeSpanToRange Types.Month now@(UTCTime day dt) = (UTCTime (addDays (-30) day) dt, now)

main :: IO ()
main = do
    cfg <- liftM2 Store.Config dataDir eventFile
    Store.runStorage $ initStorage cfg
    now <- getCurrentTime
    act <- customExecParser cliParserPrefs cliParserInfo
    Store.runStorage $ processAction cfg now act
