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
            testCase "Simple start" test_parse_simple_start,
            testCase "Simple start - project" test_parse_simple_start_project,
            testCase "Simple start - name" test_parse_simple_start_name,
            testCase "Simple start - tags" test_parse_simple_start_tags,
            testCase "Simple start - time" test_parse_simple_start_time,
            testCase "Simple stop" test_parse_simple_stop,
            testCase "Simple stop - time" test_parse_simple_stop_time
         ],
        testGroup "Computing spans" [
            testCase "Explicit stops" test_compute_explicit_stops,
            testCase "Implicit stops" test_compute_implicit_stops
        ]
    ]

--------------------------------------------------------------------------------
-- Parsing tests

s1 = "Start (Activity {project = \"hammertime\", name = \"test\", tags = [\"fu\", \"bar\"]}) 2012-10-31 02:52:15.084919 UTC"
ps1 = readEvent s1

s2 = "Stop 2012-10-31 02:52:32.328453 UTC"
ps2 = readEvent s2

test_parse_simple_start =
        unless (fmap isStart ps1 == Just True) (assertFailure "can't parse start")
    where
        isStart (Start _ _) = True
        isStart _ = False

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

test_parse_simple_start_time =
        p  @=? (Just $ read "2012-10-31 02:52:15.084919 UTC")
    where
        extractTime (Start _ t) = Just t
        extractTime _ = Nothing
        p = ps1 >>= extractTime

test_parse_simple_stop =
        unless (fmap isStop ps2 == Just True) (assertFailure "can't parse stop")
    where
        isStop (Stop _) = True
        isStop _ = False

test_parse_simple_stop_time =
        p @=? (Just $ read "2012-10-31 02:52:32.328453 UTC")
    where
        extractTime (Stop t) = Just t
        extractTime _ = Nothing
        p = ps2 >>= extractTime


--------------------------------------------------------------------------------
-- Span computing tests

t1 = read "2012-10-31 02:00:00.000000 UTC"
t2 = read "2012-10-31 02:10:00.000000 UTC"
t3 = read "2012-10-31 02:20:00.000000 UTC"
t4 = read "2012-10-31 02:30:00.000000 UTC"
t5 = read "2012-10-31 02:40:00.000000 UTC"

a1 = Activity "Hammertime" "Test" []
a2 = Activity "Hammertime" "Test 2" []
a3 = Activity "Hammertime" "Test 3" []
a4 = Activity "Hammertime" "Test 4" []
start1 = Start a1 t1
stop1 = Stop t2
start2 = Start a2 t2
stop2 = Stop t3
start3 = Start a3 t3
stop3 = Stop t4
start4 = Start a4 t4
stop4 = Stop t5

test_compute_explicit_stops =
        spans @=? [(Span a1 t1 t2), (Span a2 t2 t3), (Span a3 t3 t4), (Span a4 t4 t5)]
    where
        spans = (computeTimes . insertStops) [ start1, stop1
                                           , start2, stop2
                                           , start3, stop3
                                           , start4, stop4
                                           ]
test_compute_implicit_stops =
        spans @=? [(Span a1 t1 t2), (Span a2 t2 t3), (Span a3 t3 t4), (Span a4 t4 t5)]
    where
        spans = (computeTimes . insertStops) [ start1
                                           , start2
                                           , start3
                                           , start4, stop4
                                           ]
