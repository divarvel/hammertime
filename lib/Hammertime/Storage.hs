{-# LANGUAGE TypeFamilies #-}

module Hammertime.Storage where

import Control.Monad.IO.Class
import Hammertime.Types

class (Monad m, MonadIO m) => MonadStorage m where
    data Config :: *
    initStorage :: Config -> m ()
    loadEvents :: Config -> Maybe TimeRange -> m [Event]
    appendEvent :: Config -> Event -> m ()

readLastStart :: MonadStorage m
              => Config
              -> m (Maybe Event)
readLastStart cfg = do
    es <- loadEvents cfg Nothing
    return $ getLastStart es
    where
        getLastStart [] = Nothing
        getLastStart events = case last events of
            s@(Start _ _) -> Just s
            _ -> Nothing
