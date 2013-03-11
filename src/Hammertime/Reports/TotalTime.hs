module Hammertime.Reports.TotalTime (
    printTotalTimeReport
) where

import Hammertime.Core (readFilteredEvents, getTotalTime)
import Hammertime.Types

printTotalTimeReport :: Hammertime.Types.TimeSpan
                     -> Maybe Project
                     -> Maybe Name
                     -> Maybe Tag
                     -> IO ()
printTotalTimeReport s p a t = do
    es <- readFilteredEvents s p a t
    print $ getTotalTime es


