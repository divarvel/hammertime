{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import qualified Data.Text as T
import Data.Version (showVersion)
import System.Console.CmdArgs

import Paths_hammertime
import Hammertime.Core
import Hammertime.Types

data Action = Start_ { project_ :: String
                          , name_ :: String
                          , tags_ :: [String]
                          }
            | Stop_
            | Show_  { query :: String }
            deriving (Show, Data, Typeable)


getAction :: IO Action
getAction = cmdArgs $ modes [showAction &= auto, startAction, stopAction] &=
                      help "Hammertime: a simple time tracker" &=
                      summary ("Hammertime v" ++ showVersion version)
    where
        showAction = Show_ { query = def &= help "A filter query" &= typ "QUERY" } &= help "Show saved events"

        startAction = Start_ { project_ = def &= typ "PROJECT" &= argPos 0
                             , name_ = def &= typ "NAME" &= argPos 1
                             , tags_ = def &= typ "TAG" &= args
                             } &= help "Start a new activity"

        stopAction = Stop_ &= help "Stop current activity"

processAction :: Action -> IO()
processAction (Start_ p n ts) = appendStart (T.pack p) (T.pack n) (map T.pack ts)
processAction (Show_ q) = showSavedEvents $ T.pack q
processAction (Stop_) = appendStop


main = ensureEventFile >> getAction >>= processAction
