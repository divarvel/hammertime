{-#LANGUAGE GeneralizedNewtypeDeriving #-}

module Hammertime.Storage.File  where


-- import System.IO
import qualified Data.Text as T
--import Data.List
import Data.Maybe (mapMaybe, listToMaybe)
import System.Directory (createDirectoryIfMissing)
import System.Environment.XDG.BaseDir (getUserDataDir, getUserDataFile)

import Hammertime.Storage
import Hammertime.Types

newtype FileStorage a = FileStorage { runStorage :: IO a } deriving (Monad)


eventFile, dataDir :: IO FilePath
eventFile =  getUserDataFile "hammertime" "events"
dataDir = getUserDataDir "hammertime"


instance MonadStorage FileStorage where
    initStorage = FileStorage $ do
        dir <- dataDir
        createDirectoryIfMissing True dir
        file <- eventFile
        appendFile file ""

    loadEvents mtr = FileStorage $ do
        f <- eventFile
        allEvents <- fmap (readEvents . T.pack) $ readFile f
        return $ filter (maybe (const True) inRange mtr) allEvents
     where
       inRange (b,e) ev = eventTime ev >= b && eventTime ev <= e

    appendEvent e = FileStorage $ do
        f <- eventFile
        appendFile f (show e ++ "\n")

readEvents :: T.Text -> [Event]
readEvents s = mapMaybe readEvent (T.lines s)

readEvent :: T.Text -> Maybe Event
readEvent line = listToMaybe . map fst . take 1 . reads . T.unpack $ line

