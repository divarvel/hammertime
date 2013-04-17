
module Hammertime.Storage where

import Hammertime.Types

class Monad m => MonadStorage m where
    initStorage :: m ()
    loadEvents :: Maybe TimeRange -> m [Event]
    appendEvent :: Event -> m ()

readLastStart :: MonadStorage m
              => m (Maybe Event)
readLastStart = do
    es <- loadEvents Nothing
    return $ getLastStart es
    where
        getLastStart [] = Nothing
        getLastStart events = case last events of
            s@(Start _ _) -> Just s
            _ -> Nothing
