
module Hammertime.Reports.Simple (
    printSimpleReport
) where

printSimpleReport s p a ts = do
    printHeader

printHeader = do
    putStrLn "Hammertime report for xxxxx"
