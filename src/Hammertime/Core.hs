
module Hammertime.Core (
    appendStart
  , appendStop
  , ensureDataDir
  , readEvent
  , showSavedEvents
) where

import Control.Monad.Writer
import System.IO
import qualified Data.Text as T
import Data.Time
import Data.Time.Clock.POSIX
import Data.List
import Data.Maybe
import System.Directory (createDirectoryIfMissing)
import System.Environment.XDG.BaseDir (getUserDataDir, getUserDataFile)

import Hammertime.Types

dataDir = getUserDataDir "hammertime"
eventFile = getUserDataFile "hammertime" "events"

ensureDataDir = createDirectoryIfMissing False =<< dataDir


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
    d <- getCurrentTime
    cs <- readFile filename
    mapM_ print $ readEvents . T.pack $ cs
    return ()

readSavedEvents :: IO [Event]
readSavedEvents = do
    filename <- eventFile
    fmap (readEvents . T.pack) $ readFile filename

removeFirstStop :: [Event] -> [Event]
removeFirstStop (Stop _:t) = t
removeFirstStop events = events

removeLastStart :: [Event] -> [Event]
removeLastStart events = case reverse events of
    (Start _ _:t) -> reverse t
    events -> reverse events

insertStops :: [Event] -> [Event]
insertStops cs = reverse $ foldl step [] cs where
    step acc e@(Start _ time) = case acc of
        (Start _ _:_) -> e: Stop time :acc -- A task was running, stop it now
        _ -> e:acc
    step acc e@(Stop _) = case acc of
        (Stop _:_) -> acc -- Can's stop a task twice, discard the last stop
        _ -> e:acc

computeTimes :: [Event] -> [Span]
computeTimes cs = execWriter $ appendSpan cs Nothing
  where
    appendSpan [] _ = return ()
    appendSpan ((start@(Start _ t)):es) (Just (Start a s)) = tell [Span a s t] >> appendSpan es (Just start)
    appendSpan (Stop t:es) (Just (Start a s)) = tell [Span a s t] >> appendSpan es Nothing
    appendSpan ((start@(Start _ _)):es) Nothing = appendSpan es (Just start)
    appendSpan (Stop _:es) Nothing = appendSpan es Nothing

getDiffTime :: Span -> NominalDiffTime
getDiffTime (Span _ begin end) = diffUTCTime end begin

getTotalTime :: [Span] -> NominalDiffTime
getTotalTime spans = sum . map getDiffTime $ spans

getProjectTime :: Project -> [Span] -> NominalDiffTime
getProjectTime p = getTotalTime . filterByActivity ((==p) . project)

getActivityTime :: Name -> [Span] -> NominalDiffTime
getActivityTime p = getTotalTime . filterByActivity ((p==) . name)

filterByActivity :: (Activity -> Bool) -> [Span] -> [Span]
filterByActivity p = filter p' where
    p' = p . activity

readEvents :: T.Text -> [Event]
readEvents s = mapMaybe readEvent (T.lines s)

readEvent :: T.Text -> Maybe Event
readEvent line = listToMaybe . map fst . take 1 . reads . T.unpack $ line
