
module Hammertime where

import System.Environment
import qualified Data.Text as T

import Hammertime.Core

useArgs :: [String] -> IO ()
useArgs ("show":q:_) = showSavedEvents $ T.pack q
useArgs ("start":p:t:ts) = appendStart (T.pack p) (T.pack t) (map T.pack ts)
useArgs ("stop":_) = appendStop
useArgs _ = putStrLn "Not enough arguments"


main = getArgs >>= useArgs
