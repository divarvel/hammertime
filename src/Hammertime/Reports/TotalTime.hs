{-# LANGUAGE OverloadedStrings #-}

module Hammertime.Reports.TotalTime (
    totalTimeReportGen
) where

import qualified Data.Text as T

import Hammertime.Core (getTotalTime)
import Hammertime.Types

totalTimeReportGen :: ReportGenerator
totalTimeReportGen spans = (T.pack . show) $ getTotalTime spans
