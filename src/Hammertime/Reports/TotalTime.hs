
module Hammertime.Reports.TotalTime (
    printTotalTimeReport
) where

import Hammertime.Core

printTotalTimeReport s p a t = do
    es <- readFilteredEvents s p a t
    print $ getTotalTime es


