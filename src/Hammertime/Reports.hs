
module Hammertime.Reports (
    generateReport
) where

import Hammertime.Core (readFilteredEvents)
import Hammertime.Types
import Hammertime.Storage
import qualified Hammertime.Reports.Simple as SR
import qualified Hammertime.Reports.TotalTime as TTR


generateReport :: MonadStorage m
               => ReportType
               -> TimeRange
               -> Maybe Project
               -> Maybe Name
               -> Maybe Tag
               -> m Report
generateReport typ s p a ts = do
    spans <- readFilteredEvents s p a ts
    return $ (getReportGenerator typ) spans

getReportGenerator :: ReportType
                   -> ReportGenerator
getReportGenerator Simple = SR.report
getReportGenerator TotalTime = TTR.report
