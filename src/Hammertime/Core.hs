
module Hammertime.Core (
    appendStart
  , appendStop
  , ensureEventFile
  , computeTimes
  , getTotalTime
  , getProjectTime
  , getActivityTime
  , readEvent
  , readFilteredEvents
  , readSavedEvents
  , showSavedEvents
) where

import Control.Monad.Writer
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime, NominalDiffTime)
import Data.Maybe
import System.Directory (createDirectoryIfMissing)
import System.Environment.XDG.BaseDir (getUserDataDir, getUserDataFile)

import Hammertime.Types

dataDir :: IO FilePath
dataDir = getUserDataDir "hammertime"

eventFile :: IO FilePath
eventFile = getUserDataFile "hammertime" "events"

ensureDataDir :: IO ()
ensureDataDir = createDirectoryIfMissing True =<< dataDir

ensureEventFile :: IO ()
ensureEventFile = ensureDataDir >> eventFile >>= (flip appendFile "")

createStart :: Activity -> IO Event
createStart a = fmap (Start a) getCurrentTime

appendStart :: Project -> Name -> [Tag] -> IO ()
appendStart p n ts = createStart (Activity p n ts) >>= appendEvent

appendStop :: IO ()
appendStop = fmap Stop getCurrentTime >>= appendEvent


appendEvent :: Event -> IO ()
appendEvent e = do
    filename <- eventFile
    appendFile filename (show e ++ "\n")

showSavedEvents :: T.Text -> IO ()
showSavedEvents _ = do
    filename <- eventFile
    cs <- readFile filename
    mapM_ print $ readEvents . T.pack $ cs
    return ()

readSavedEvents :: IO [Event]
readSavedEvents = do
    filename <- eventFile
    fmap (readEvents . T.pack) $ readFile filename

computeTimes :: [Event] -> [Span]
computeTimes cs = execWriter $ appendSpan cs Nothing
  where
    appendSpan [] _ = return ()
    appendSpan ((start@(Start _ t)):es) (Just (Start a s)) = tell [Span a s t] >> appendSpan es (Just start)
    appendSpan (Stop t:es) (Just (Start a s)) = tell [Span a s t] >> appendSpan es Nothing
    appendSpan ((start@(Start _ _)):es) _ = appendSpan es (Just start)
    appendSpan (Stop _:es) _ = appendSpan es Nothing

getDiffTime :: Span -> NominalDiffTime
getDiffTime (Span _ b e) = diffUTCTime e b

getTotalTime :: [Span] -> NominalDiffTime
getTotalTime spans = sum . map getDiffTime $ spans

getProjectTime :: Project -> [Span] -> NominalDiffTime
getProjectTime p = getTotalTime . filterByActivity ((==p) . project)

getActivityTime :: Name -> [Span] -> NominalDiffTime
getActivityTime p = getTotalTime . filterByActivity ((p==) . name)

filterByActivity :: (Activity -> Bool) -> [Span] -> [Span]
filterByActivity p = filter p' where
    p' = p . activity

filterByActivityName n = filterByActivity ((==n) . name)
filterByActivityProject p = filterByActivity ((==p) . project)
filterByActivityTag t = filterByActivity ((elem t) . tags)

filterNewEvents :: (UTCTime -> Bool) -> [Event] -> [Event]
filterNewEvents p = filter p' where
    p' = p . getTime
    getTime (Start _ t) = t
    getTime (Stop t) = t

readFilteredEvents :: TimeSpan
                   -> Maybe Project
                   -> Maybe Name
                   -> Maybe Tag
                   -> IO [Span]
readFilteredEvents _ p a t = do
    es <- readSavedEvents
    return $ mainFilter es
    where
        mainFilter = activityFilter . computeTimes . eventFilter
        eventFilter = filterNewEvents (const True) -- Toto only keep new events
        activityFilter = tagFilter . nameFilter . projectFilter
        projectFilter = maybe id filterByActivityProject p
        nameFilter = maybe id filterByActivityName a
        tagFilter = maybe id filterByActivityTag t

readEvents :: T.Text -> [Event]
readEvents s = mapMaybe readEvent (T.lines s)

readEvent :: T.Text -> Maybe Event
readEvent line = listToMaybe . map fst . take 1 . reads . T.unpack $ line
