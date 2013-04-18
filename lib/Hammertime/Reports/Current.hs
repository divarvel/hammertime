{-# LANGUAGE OverloadedStrings #-}

module Hammertime.Reports.Current (
    report
) where

import Data.Monoid (mappend)
import qualified Data.Text as T

import Hammertime.Types
import Hammertime.Core

report :: Maybe Span -> Report
report = maybe "No current activity\n" mkReport
    where
        mkReport s =
            (project . activity $ s) `mappend` ": " `mappend`
            (name . activity $ s) `mappend` " " `mappend`
            (T.pack . show . roundedTime $ s) `mappend` "s" `mappend` "\n"
        roundedTime s = round . getDiffTime $ s :: Integer
