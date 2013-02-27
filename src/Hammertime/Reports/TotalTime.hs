
module Hammertime.Reports.TotalTime (
    printTotalTimeReport
) where

import Hammertime.Core

printTotalTimeReport s p a ts = do
    es <- readSavedEvents
    print $ getTotalTime (computeTimes es)


