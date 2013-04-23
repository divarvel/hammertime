{-#LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}

module Hammertime.Storage.File  where


-- import System.IO
import qualified Data.Text as T
--import Data.List
import Control.Monad.IO.Class
import Data.Maybe (mapMaybe, listToMaybe)
import System.Directory (createDirectoryIfMissing)

import Hammertime.Storage
import Hammertime.Types

newtype FileStorage a = FileStorage { runStorage :: IO a } deriving (Monad, MonadIO)


instance MonadStorage FileStorage where
    data Config = Config { dir :: FilePath, file :: FilePath }
    initStorage cfg = FileStorage $ do
        createDirectoryIfMissing True (dir cfg)
        appendFile (file cfg) ""

    loadEvents cfg mtr = FileStorage $ do
        allEvents <- fmap (readEvents . T.pack) $ readFile (file cfg)
        return $ filter (maybe (const True) inRange mtr) allEvents
     where
       inRange (b,e) ev = eventTime ev >= b && eventTime ev <= e

    appendEvent cfg e = FileStorage $ do
        appendFile (file cfg) (show e ++ "\n")

readEvents :: T.Text -> [Event]
readEvents s = mapMaybe readEvent (T.lines s)

readEvent :: T.Text -> Maybe Event
readEvent line = listToMaybe . map fst . take 1 . reads . T.unpack $ line

