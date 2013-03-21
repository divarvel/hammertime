
module Hammertime.Reports (
    printReport
) where

import qualified Data.Text as T

import Hammertime.Core (readFilteredEvents)
import Hammertime.Types
import Hammertime.Reports.Simple
import Hammertime.Reports.TotalTime

printReport :: ReportType
            -> TimeSpan
            -> Maybe Project
            -> Maybe Name
            -> Maybe Tag
            -> IO ()
printReport rt s p a ts =
    let g = getReportGenerator rt
    in (generateReport g s p a ts) >>= (putStr . T.unpack)



generateReport :: ReportGenerator
               -> TimeSpan
               -> Maybe Project
               -> Maybe Name
               -> Maybe Tag
               -> IO Report
generateReport generator s p a ts = do
    spans <- readFilteredEvents s p a ts
    return $ generator spans

getReportGenerator :: ReportType
                   -> ReportGenerator
getReportGenerator Simple = simpleReportGen
getReportGenerator TotalTime = totalTimeReportGen
