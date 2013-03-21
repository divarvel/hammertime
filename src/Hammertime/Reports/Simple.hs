{-# LANGUAGE OverloadedStrings #-}

module Hammertime.Reports.Simple (
    report
) where

import Data.Foldable (foldMap)
import Data.Function (on)
import Data.List (groupBy, sortBy)
import Data.Maybe (mapMaybe)
import Data.Monoid (mappend)
import qualified Data.Text as T

import Hammertime.Core (getTotalTime)
import Hammertime.Types

report :: ReportGenerator
report spans =
    let pairs = groupByProject spans
        sorted = sortBy (compare `on` fst) pairs
        projects = foldMap reportProject sorted
    in header `mappend` projects

header :: Report
header = "Hammertime report for \n\n"

reportProject :: (Project, [Span]) -> Report
reportProject (p, spans) =
    let pairs = groupByActivity spans
        sorted = sortBy (compare `on` fst) pairs
        activities = foldMap reportActivity sorted
        totalTime = getTotalTime spans
        displayedTime = T.pack $ show totalTime
        firstLine = p `mappend` ": " `mappend`
                    displayedTime `mappend` "\n"
    in firstLine `mappend` activities `mappend` "\n"

reportActivity :: (Name, [Span]) -> Report
reportActivity (n, spans) =
    let totalTime = getTotalTime spans
        displayedTime = T.pack $ show totalTime
        tab = "    "
    in tab `mappend`
       n `mappend` ": " `mappend`
       displayedTime `mappend` "\n"

groupByProject :: [Span] -> [(Project, [Span])]
groupByProject = makeAssocList (project . activity)

groupByActivity :: [Span] -> [(Name, [Span])]
groupByActivity = makeAssocList (name . activity)

makePair :: (Eq a) => (Span -> a) -> [Span] -> Maybe (a, [Span])
makePair prop spans@(s:_) = Just (prop s,spans)
makePair _ _ = Nothing

makeAssocList :: (Eq a) => (Span -> a) -> [Span] -> [(a, [Span])]
makeAssocList prop = (mapMaybe (makePair prop)) . (groupBy ((==) `on` prop))
