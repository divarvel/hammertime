
module Hammertime.Core (
    computeTimes
  , getDiffTime
  , getTotalTime
  , getProjectTime
  , getActivityTime
  , makeCurrentSpan
  , readFilteredEvents
) where

import Control.Monad.Writer
import Data.Time.Clock (UTCTime(..), diffUTCTime, NominalDiffTime)

import Hammertime.Types
import Hammertime.Storage


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

filterByActivityName :: Name -> [Span] -> [Span]
filterByActivityName n = filterByActivity ((==n) . name)

filterByActivityProject :: Project -> [Span] -> [Span]
filterByActivityProject p = filterByActivity ((==p) . project)

filterByActivityTag :: Tag -> [Span] -> [Span]
filterByActivityTag t = filterByActivity (elem t . tags)


readFilteredEvents :: MonadStorage m
                   => TimeRange
                   -> Maybe Project
                   -> Maybe Name
                   -> Maybe Tag
                   -> m [Span]
readFilteredEvents tr p a t = do
    es <- loadEvents (Just tr)
    return $ mainFilter es
    where
        mainFilter = activityFilter . computeTimes
        activityFilter = tagFilter . nameFilter . projectFilter
        projectFilter = maybe id filterByActivityProject p
        nameFilter = maybe id filterByActivityName a
        tagFilter = maybe id filterByActivityTag t

makeCurrentSpan :: MonadStorage m
                => UTCTime
                -> m (Maybe Span)
makeCurrentSpan now = do
    lastStart <- readLastStart
    return $ makeSpan lastStart
    where
        makeSpan (Just (Start a b)) = Just $ Span a b now
        makeSpan _ = Nothing
