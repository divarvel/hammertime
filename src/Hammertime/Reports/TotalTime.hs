{-# LANGUAGE OverloadedStrings #-}

module Hammertime.Reports.TotalTime (
    report
) where

import qualified Data.Text as T

import Hammertime.Core (getTotalTime)
import Hammertime.Types

report :: ReportGenerator
report spans = (T.pack . show) $ getTotalTime spans
