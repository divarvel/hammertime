module Hammertime.CLI.Tests (tests) where

import Test.Framework.Providers.HUnit
import Test.Framework
import Test.HUnit hiding (test, Test)

import Options.Applicative

import Hammertime.CLI (cliParserInfo, Action(..))
import Hammertime.Types (TimeSpan(..), ReportType(..))

tests :: Test
tests = testGroup "Hammertime.CLI.Tests"
    [
        testGroup "Start"
        [
            testCase "start" testStart,
            testCase "start with tags" testStartWithTags,
            testCase "bogus start" testBogusStart

        ],
        testCase "stop" testStop,
        testGroup "Report"
        [
            testCase " default report" testReport,
            testGroup "report with span"
            [
                testCase "month" $ testReportSpan ("month", Month),
                testCase "week" $ testReportSpan ("week", Week),
                testCase "day" $ testReportSpan ("day", Day)
            ],
            testGroup "report with filter"
            [
                testCase "project" testReportFilterProject,
                testCase "activity" testReportFilterActivity,
                testCase "tag" testReportFilterTag
            ],
            testGroup "report with type"
            [
                testCase "simple" $ testReportType ("simple", Simple),
                testCase "total" $ testReportType ("totaltime", TotalTime)
            ]
        ],
        testCase "current" testCurrent
    ]


runCliParser :: [String] -> Either ParserFailure Action
runCliParser = execParserPure (prefs idm) cliParserInfo


--------------------------------------------------------------------------------
-- Start
--

testStart =
    let result = runCliParser ["start", "project", "activity"]
    in case result of
        Left (ParserFailure m _) -> assertFailure "parse fail: start"
        Right a -> a @=? Start "project" "activity" []

testStartWithTags =
    let result = runCliParser ["start", "project", "activity", "tag1", "tag2", "tag3"]
    in case result of
        Left (ParserFailure _ _) -> assertFailure "parse fail: start with tags"
        Right a -> a @=? Start "project" "activity" ["tag1", "tag2", "tag3"]

testBogusStart =
    let result = runCliParser ["start"]
    in case result of
        Left (ParserFailure m _) -> m "test" >> return ()
        Right a -> assertFailure "bogus start accepted"

--------------------------------------------------------------------------------
-- Stop
--

testStop =
    let result = runCliParser ["stop"]
    in case result of
        Left (ParserFailure _ _) -> assertFailure "parse fail: stop"
        Right a -> a @=? Stop

--------------------------------------------------------------------------------
-- Report
--

testReport =
    let result = runCliParser ["report"]
    in case result of
        Left (ParserFailure m _) -> assertFailure "parse fail: report"
        Right a -> a @=? (Report Day Nothing Nothing Nothing Simple)

testReportSpan (string, span) =
    let result = runCliParser ["report", string]
    in case result of
        Left (ParserFailure _ _) -> assertFailure "parse fail: report"
        Right a -> a @=? (Report span Nothing Nothing Nothing Simple)

testReportFilterProject =
    let result = runCliParser ["report", "day", "--project", "project"]
    in case result of
        Left (ParserFailure _ _) -> assertFailure "parse fail: report"
        Right a -> a @=? (Report Day (Just "project") Nothing Nothing Simple)

testReportFilterActivity =
    let result = runCliParser ["report", "day", "--activity", "activity"]
    in case result of
        Left (ParserFailure _ _) -> assertFailure "parse fail: report"
        Right a -> a @=? (Report Day Nothing (Just "activity") Nothing Simple)

testReportFilterTag =
    let result = runCliParser ["report", "day", "--tag", "tag"]
    in case result of
        Left (ParserFailure _ _) -> assertFailure "parse fail: report"
        Right a -> a @=? (Report Day Nothing Nothing (Just "tag") Simple)

testReportType (string, reportType) =
    let result = runCliParser ["report", "day", "-t", string]
    in case result of
        Left (ParserFailure _ _) -> assertFailure "parse fail: report"
        Right a -> a @=? (Report Day Nothing Nothing Nothing reportType)


--------------------------------------------------------------------------------
-- Current
--

testCurrent =
    let result = runCliParser ["current"]
    in case result of
        Left (ParserFailure _ _) -> assertFailure "parse fail: current"
        Right a -> a @=? Current

