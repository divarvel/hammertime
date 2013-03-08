{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.Char (toLower)
import Data.List (find, intersperse)
import qualified Data.Text as T
import Data.Version (showVersion)
import System.Console.CmdArgs.Explicit

import Paths_hammertime

import Hammertime.Core
import Hammertime.Reports
import qualified Hammertime.Types as Types

data Action = Start { project :: String
                    , name :: String
                    , tags :: [String]
                    }
            | Stop
            | Report  { span_ :: Types.TimeSpan
                      , project_ :: Maybe String
                      , name_ :: Maybe String
                      , tag_ :: Maybe String
                      , type_ :: Types.ReportType
                      }
            | Help
            | Version
            deriving (Show)

defaultReport :: Action
defaultReport = Report Types.Day Nothing Nothing Nothing Types.Simple


parseArgument :: (Bounded a, Enum a, Show a, Read a) => String -> Either String a
parseArgument string = maybe (Left $ "Accepted values: " ++ p values) Right matching
    where
        p = concat . (intersperse " | ") . (map show)
        matching = find ((match string) . show) values
        match s s' = map toLower s == map toLower s'
        values = [minBound..maxBound]

startMode :: Mode Action
startMode =
    let m = mode "start" (Start "" "" []) "Start a new activity" dummyArg  []
    in m { modeArgs = ([
        (flagArg setProject "PROJECT"),
        (flagArg setActivity "ACTIVITY")
    ], Just (flagArg addTag "[TAGS]")) }

stopMode :: Mode Action
stopMode =
    let m = mode "stop" Stop "Stop current activity" dummyArg []
    in m { modeArgs = ([], Nothing) }

reportMode :: Mode Action
reportMode = mode "report" defaultReport  "Generate report for a given time span (default: day)" (flagArg setTimeSpan "month|week|day") [
        flagReq ["project", "p"] setProjectFilter "PROJECT" "Filter by project",
        flagReq ["activity", "a"] setActivityFilter "ACTIVITY" "Filter by activity",
        flagReq ["tags"] setTagsFilter "TAGS" "Filter by tag",
        flagReq ["type", "t"] setReportType "SIMPLE|TOTAL" "Report Type (default: simple)"
    ]

hammertimeModes :: Mode Action
hammertimeModes =
    let m = (modes "hammertime" defaultReport "Lightweight time tracker" [startMode, stopMode, reportMode])
        addHelpTag m' = m' { modeGroupFlags = toGroup [flagHelpSimple $ const Help, flagVersion $ const Version] }
    in addHelpTag m


dummyArg :: Arg a
dummyArg = flagArg (\_ _ -> Left "") ""

setProject :: Update Action
setProject p s = Right $ s { project = p }

setActivity :: Update Action
setActivity a s = Right $ s { name = a }

addTag :: Update Action
addTag t s = Right $ s { tags = (tags s ++ [t]) }

setProjectFilter :: Update Action
setProjectFilter p r = Right $ r { project_ = (Just p) }

setActivityFilter :: Update Action
setActivityFilter a r = Right $ r { name_ = (Just a) }

setTagsFilter :: Update Action
setTagsFilter t r = Right $ r { tag_ = (Just t) }

setReportType :: Update Action
setReportType v r = fmap setType (parseArgument v)
    where
        setType reportType = r { type_ = reportType }

setTimeSpan :: Update Action
setTimeSpan q r = fmap setSpan (parseArgument q)
    where
        setSpan span = r { span_ = span }

getAction :: IO Action
getAction = processArgs hammertimeModes

processAction :: Action -> IO ()
processAction (Start p n ts) = appendStart (T.pack p) (T.pack n) (map T.pack ts)
processAction (Stop) = appendStop
processAction (Report s p n t t') = printReport t' s (fmap T.pack p) (fmap T.pack n) (fmap T.pack t)
processAction (Help) = print $ helpText [] HelpFormatDefault hammertimeModes
processAction (Version) = putStrLn $ "Hammertime v" ++ (showVersion version)

main :: IO ()
main = ensureEventFile >> getAction >>= processAction
