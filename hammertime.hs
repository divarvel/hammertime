import System.IO
import Data.Time
import Data.Time.Clock.POSIX
import Data.List
import Data.Maybe
import System.Environment

data Activity = Activity { project :: String
                         , name :: String
                         , tags :: [String]
                         } deriving (Eq, Read, Show)
data Event = Start Activity UTCTime | Stop UTCTime deriving (Eq, Read, Show)
data Span = Span { activity :: Activity
                 , begin :: UTCTime
                 , end :: UTCTime
                 }

filename = "hammertime.txt"

useArgs :: [String] -> IO ()
useArgs ("show":q:_) = showSavedEvents q
useArgs ("start":p:t:ts) = appendStart p t ts
useArgs ("stop":_) = appendStop
useArgs _ = putStrLn "Not enough arguments"

createStart :: Activity -> IO Event
createStart a = fmap (Start a) getCurrentTime

appendStart :: String -> String -> [String] -> IO ()
appendStart p n ts = createStart (Activity p n ts) >>= appendEvent

appendStop :: IO ()
appendStop = fmap Stop getCurrentTime >>= appendEvent


appendEvent :: Event -> IO ()
appendEvent e = do
    appendFile filename $ (show e ++ "\n")

showSavedEvents :: String -> IO ()
showSavedEvents q = do
    d <- getCurrentTime
    cs <- readFile filename
    mapM_ (putStrLn . show) $ readEvents cs
    return ()

readSavedEvents :: IO [Event]
readSavedEvents = fmap readEvents $ readFile filename

removeFirstStop :: [Event] -> [Event]
removeFirstStop ((Stop _):t) = t
removeFirstStop events = events

removeLastStart :: [Event] -> [Event]
removeLastStart events = case reverse events of
    ((Start _ _):t) -> reverse t
    events -> reverse events

insertStops :: [Event] -> [Event]
insertStops cs = reverse $ foldl step [] cs where
    step acc e@(Start _ time) = case acc of
        ((Start _ _):_) -> e:(Stop time):acc -- A task was running, stop it now
        _ -> e:acc
    step acc e@(Stop _) = case acc of
        ((Stop _):_) -> acc -- Can's stop a task twice, discard the last stop
        _ -> e:acc


computeTimes :: [Event] -> [Span]
computeTimes cs = catMaybes . (map getDiff) . buildIntervals $ cs where
    getDiff ((Start a begin), (Stop end)) = Just $ Span a begin end
    getDiff _ = Nothing
    buildIntervals (x:y:t) = (x,y):buildIntervals t
    buildIntervals _ = []

getDiffTime :: Span -> NominalDiffTime
getDiffTime (Span _ begin end) = diffUTCTime end begin

getTotalTime :: [Span] -> NominalDiffTime
getTotalTime spans = sum . map getDiffTime $ spans

getProjectTime p = getTotalTime . filterByActivity ((==p) . project)

getActivityTime p = getTotalTime . filterByActivity ((p==) . name)

filterByActivity :: (Activity -> Bool) -> [Span] -> [Span]
filterByActivity p spans = filter p' $ spans where
    p' = p . activity

readEvents :: String -> [Event]
readEvents s = catMaybes $ map readEvent (lines s)

readEvent line = listToMaybe . (map fst) . (take 1) $ reads line



main = getArgs >>= useArgs
