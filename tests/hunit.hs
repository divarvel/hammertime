{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (unless)

import Test.Framework.Providers.HUnit
import Test.Framework
import Test.HUnit hiding (test)
import Hammertime.Types
import Hammertime.Core

main :: IO ()
main = defaultMain tests

tests = [
        testGroup "Parsing events" [
            testCase "Simple start - project" test_parse_simple_start_project,
            testCase "Simple start - name" test_parse_simple_start_name,
            testCase "Simple start - tags" test_parse_simple_start_tags,
            testCase "Simple stop" test_parse_simple_stop
         ]
    ]

s1 = "Start (Activity {project = \"hammertime\", name = \"test\", tags = [\"fu\", \"bar\"]}) 2012-10-31 02:52:15.084919 UTC"
ps1 = readEvent s1

s2 = "Stop 2012-10-31 02:52:32.328453 UTC"
ps2 = readEvent s2

test_parse_simple_start_project =
        p  @=? Just "hammertime"
    where
        extractProject (Start a _) = Just $ project a
        extractProject _ = Nothing
        p = ps1 >>= extractProject

test_parse_simple_start_name =
        p  @=? Just "test"
    where
        extractName (Start a _) = Just $ name a
        extractName _ = Nothing
        p = ps1 >>= extractName

test_parse_simple_start_tags =
        p  @=? Just ["fu", "bar"]
    where
        extractTags (Start a _) = Just $ tags a
        extractTags _ = Nothing
        p = ps1 >>= extractTags

test_parse_simple_stop =
        unless (fmap isStop ps2 == Just True) (assertFailure "can't parse stop")
    where
        isStop (Stop _) = True
        isStop _ = False
