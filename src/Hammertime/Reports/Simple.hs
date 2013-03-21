{-# LANGUAGE OverloadedStrings #-}

module Hammertime.Reports.Simple (
    report
) where

import Data.Monoid (mappend)

import Hammertime.Types

report :: ReportGenerator
report _ =
    header `mappend` "todo"

header :: Report
header = "Hammertime report for xxxxx"
