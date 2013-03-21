
module Hammertime.Storage.File (
    loadEvents
  , appendEvent
  ) where


-- import System.IO
import qualified Data.Text as T
--import Data.List
import Data.Maybe (mapMaybe, listToMaybe)
import System.Directory (createDirectoryIfMissing)
import System.Environment.XDG.BaseDir (getUserDataDir, getUserDataFile)

import Hammertime.Types


dataDir, eventFile :: IO FilePath
dataDir = getUserDataDir "hammertime"
eventFile = getUserDataFile "hammertime" "events"

ensureDataDir :: IO ()
ensureDataDir = createDirectoryIfMissing True =<< dataDir

loadEvents :: Maybe TimeRange -> IO [Event]
loadEvents mtr = do
    ensureDataDir
    f <- eventFile
    allEvents <- fmap (readEvents . T.pack) $ readFile f
    return $ filter (maybe (const True) inRange mtr) allEvents
 where
   inRange (b,e) ev = getTime ev >= b && getTime ev <= e
   getTime (Start _ t) = t
   getTime (Stop t) = t

appendEvent :: Event -> IO ()
appendEvent e = do
    f <- eventFile
    appendFile f (show e ++ "\n")

readEvents :: T.Text -> [Event]
readEvents s = mapMaybe readEvent (T.lines s)

readEvent :: T.Text -> Maybe Event
readEvent line = listToMaybe . map fst . take 1 . reads . T.unpack $ line

