
module Hammertime.Types where

import qualified Data.Text as T
import Data.Time             (UTCTime)

--------------------------------------------------------------------------------
-- Core data types

type Project = T.Text
type Name = T.Text
type Tag = T.Text

data Activity = Activity { project :: Project
                         , name :: Name
                         , tags :: [Tag]
                         } deriving (Eq, Read, Show)

data Event = Start Activity UTCTime | Stop UTCTime deriving (Eq, Read, Show)

data Span = Span { activity :: Activity
                 , begin :: UTCTime
                 , end :: UTCTime
                 } deriving (Eq, Read, Show)

type TimeRange = (UTCTime, UTCTime)

--------------------------------------------------------------------------------
-- Reports related data types

type Report = T.Text
type ReportGenerator = [Span] -> Report

data TimeSpan = Month | Week | Day deriving (Bounded, Enum, Show)
data ReportType = Simple | TotalTime deriving (Eq, Read, Show, Bounded, Enum)

