
module Hammertime.Types where

import qualified Data.Text as T
import Data.Time
import Data.Time.Clock.POSIX

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
                 } deriving (Show)
