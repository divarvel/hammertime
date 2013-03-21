{-# LANGUAGE OverloadedStrings #-}

module Hammertime.Reports.Simple (
    simpleReportGen
) where

import Data.Monoid (mappend)

import Hammertime.Types

simpleReportGen :: ReportGenerator
simpleReportGen _ =
    header `mappend` "todo"

header :: Report
header = "Hammertime report for xxxxx"
