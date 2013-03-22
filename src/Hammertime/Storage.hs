
module Hammertime.Storage where

import Hammertime.Types

class Monad m => MonadStorage m where
    initStorage :: m ()
    loadEvents :: Maybe TimeRange -> m [Event]
    appendEvent :: Event -> m ()

