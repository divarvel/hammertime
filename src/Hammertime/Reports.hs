
module Hammertime.Reports (
    printReport
) where

import Hammertime.Types
import Hammertime.Core
import Hammertime.Reports.Simple
import Hammertime.Reports.TotalTime

printReport :: ReportType
            -> TimeSpan
            -> Maybe Project
            -> Maybe Name
            -> Maybe Tag
            -> IO ()
printReport Simple = printSimpleReport
printReport TotalTime = printTotalTimeReport


