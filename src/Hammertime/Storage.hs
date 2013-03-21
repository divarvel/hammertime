
module Hammertime.Storage where

import Hammertime.Types
import qualified Hammertime.Storage.File as File

class Monad m => MonadStorage m where
    loadEvents :: Maybe TimeRange -> m [Event]
    appendEvent :: Event -> m ()

instance MonadStorage IO where
    loadEvents = File.loadEvents
    appendEvent = File.appendEvent
