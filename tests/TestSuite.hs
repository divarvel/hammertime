module Main
    ( main
    ) where

import Test.Tasty (defaultMain, testGroup)

import qualified Hammertime.CLI.Tests
import qualified Hammertime.Core.Tests

main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ Hammertime.CLI.Tests.tests
    , Hammertime.Core.Tests.tests
    ]
