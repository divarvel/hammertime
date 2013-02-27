
module Hammertime.Reports (
    printReport
) where

import Hammertime.Types
import Hammertime.Core
import Hammertime.Reports.Simple
import Hammertime.Reports.TotalTime

printReport :: ReportType
            -> TimeSpan
            -> Maybe String
            -> Maybe String
            -> Maybe String
            -> IO ()
printReport Simple = printSimpleReport
printReport TotalTime = printTotalTimeReport


