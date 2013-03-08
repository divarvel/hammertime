module Hammertime.Reports.Simple (
    printSimpleReport
) where

import Hammertime.Types

printSimpleReport :: TimeSpan
                  -> Maybe Project
                  -> Maybe Name
                  -> Maybe Tag
                  -> IO ()
printSimpleReport _ _ _ _ = do
    printHeader

printHeader :: IO ()
printHeader = do
    putStrLn "Hammertime report for xxxxx"
