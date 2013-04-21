module Main
    ( main
    ) where

import Test.Framework (defaultMain)

import qualified Hammertime.CLI.Tests
import qualified Hammertime.Core.Tests

main :: IO ()
main = defaultMain
    [ Hammertime.CLI.Tests.tests
    , Hammertime.Core.Tests.tests
    ]
