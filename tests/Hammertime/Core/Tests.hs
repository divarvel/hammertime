{-# LANGUAGE OverloadedStrings #-}

module Hammertime.Core.Tests (tests) where

import Control.Monad (unless)

import Test.Framework.Providers.HUnit
import Test.Framework
import Test.HUnit hiding (test, Test)

import Hammertime.Core
import Hammertime.Storage
import Hammertime.Storage.File
import Hammertime.Types

tests :: Test
tests = testGroup "Hammertime.Core.Tests" [
        testGroup "Parsing events" [
            testCase "Simple start" testParseSimpleStart,
            testCase "Simple start - project" testParseSimpleStartProject,
            testCase "Simple start - name" testParseSimpleStartName,
            testCase "Simple start - tags" testParseSimpleStartTags,
            testCase "Simple start - time" testParseSimpleStartTime,
            testCase "Simple stop" testParseSimpleStop,
            testCase "Simple stop - time" testParseSimpleStopTime
         ],
        testGroup "Computing spans" [
            testCase "Explicit stops" testComputeExplicitStops,
            testCase "Implicit stops" testComputeImplicitStops,
            testCase "Multiple stops" testComputeMultipleStops
        ]
    ]

--------------------------------------------------------------------------------
-- Parsing tests

s1 = "Start {eventActivity = Activity {project = \"hammertime\", name = \"test\", tags = [\"fu\",\"bar\"]}, eventTime = 2012-10-31 02:52:15.084919 UTC}"

ps1 = readEvent s1

s2 = "Stop {eventTime = 2012-10-31 02:52:32.328453 UTC}"
ps2 = readEvent s2

testParseSimpleStart =
        unless (fmap isStart ps1 == Just True) (assertFailure "can't parse start")
    where
        isStart (Start _ _) = True
        isStart _ = False

testParseSimpleStartProject =
        p  @=? Just "hammertime"
    where
        extractProject (Start a _) = Just $ project a
        extractProject _ = Nothing
        p = ps1 >>= extractProject

testParseSimpleStartName =
        p  @=? Just "test"
    where
        extractName (Start a _) = Just $ name a
        extractName _ = Nothing
        p = ps1 >>= extractName

testParseSimpleStartTags =
        p  @=? Just ["fu", "bar"]
    where
        extractTags (Start a _) = Just $ tags a
        extractTags _ = Nothing
        p = ps1 >>= extractTags

testParseSimpleStartTime =
        p  @=? (Just $ read "2012-10-31 02:52:15.084919 UTC")
    where
        extractTime (Start _ t) = Just t
        extractTime _ = Nothing
        p = ps1 >>= extractTime

testParseSimpleStop =
        unless (fmap isStop ps2 == Just True) (assertFailure "can't parse stop")
    where
        isStop (Stop _) = True
        isStop _ = False

testParseSimpleStopTime =
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

testComputeExplicitStops =
        spans @=? [Span a1 t1 t2, Span a2 t2 t3, Span a3 t3 t4, Span a4 t4 t5]
    where
        spans = computeTimes [ start1, stop1
                             , start2, stop2
                             , start3, stop3
                             , start4, stop4
                             ]

testComputeImplicitStops =
        spans @=? [Span a1 t1 t2, Span a2 t2 t3, Span a3 t3 t4, Span a4 t4 t5]
    where
        spans = computeTimes [ start1
                             , start2
                             , start3
                             , start4, stop4
                             ]

testComputeMultipleStops =
        spans @=? [ Span a1 t1 t2, Span a2 t2 t3, Span a3 t3 t4, Span a4 t4 t5]
    where
        spans = computeTimes [ start1, stop1, stop1
                             , start2, stop2, stop2
                             , start3, stop3, stop3
                             , start4, stop4, stop4
                             ]
