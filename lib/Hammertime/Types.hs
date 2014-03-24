{-| This module declares the core types describing Hammertime data
 -}
module Hammertime.Types where

import qualified Data.Text as T
import Data.Time             (UTCTime)

--------------------------------------------------------------------------------
-- Core data types

type Project = T.Text
type Name = T.Text
type Tag = T.Text

-- | An activity is part of a project and is described by a name and some tags
data Activity = Activity { project :: Project
                         , name :: Name
                         , tags :: [Tag]
                         } deriving (Eq, Read, Show)

-- | An event is what is logged in the data file
data Event = Start { eventActivity :: Activity
                   , eventTime :: UTCTime }
           | Stop  { eventTime :: UTCTime }
           deriving (Eq, Read, Show)

-- | Spans are computed from the data file and group an activity to a time
-- interval
data Span = Span { activity :: Activity
                 , begin :: UTCTime
                 , end :: UTCTime
                 } deriving (Eq, Read, Show)

type TimeRange = (UTCTime, UTCTime)

--------------------------------------------------------------------------------
-- Reports related data types

type Report = T.Text
type ReportGenerator = [Span] -> Report

data TimeSpan = Month | Week | Day deriving (Bounded, Enum, Eq, Show)
data ReportType = Simple | TotalTime deriving (Eq, Read, Show, Bounded, Enum)

