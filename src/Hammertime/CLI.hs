
module Hammertime.CLI (
      Action(..)
    , cliParserInfo
    , cliParserPrefs
    ) where


import Data.Char (toLower)
import Data.List (find, intercalate)
import Options.Applicative
import Options.Applicative.Types

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
            | Current
            deriving (Eq, Show)


parseKeyword :: (Bounded a, Enum a, Show a) => String -> (Either ParseError a)
parseKeyword string = maybe (Left helpText) Right $ matching
    where
        matching = find (match string . show) values
        match s s' = map toLower s == map toLower s'
        values = [minBound..maxBound]
        helpText = ErrorMsg $ "Possible values: " ++ (intercalate "|" . map show $ values)

argumentBuilder :: (Bounded a, Enum a, Show a) => String -> Maybe a
argumentBuilder string = case parseKeyword string of
    Left _ -> Nothing
    Right a -> Just a

cliParserPrefs :: ParserPrefs
cliParserPrefs = prefs showHelpOnError

cliParserInfo :: ParserInfo Action
cliParserInfo = info
    ( helper
  <*> cliParser)
    ( fullDesc
   <> header "Hammertime -- Lightweight time tracker"
    )

cliParser :: Parser Action
cliParser = subparser
     ( command "start"
        (info startParser
              (progDesc "Start a new activity" <> fullDesc))
    <> command "stop"
        (info (pure Stop)
              (progDesc "Stop current activity" <> fullDesc))
    <> command "report"
        (info (helper <*> reportParser)
              (progDesc "Generate report for a given time span (default: day)" <> fullDesc))
    <> command "current"
        (info (pure Current)
              (progDesc "Display current activity" <> fullDesc))
     )

startParser :: Parser Action
startParser =
    Start <$>
        argument str ( metavar "PROJECT" ) <*>
        argument str ( metavar "ACTIVITY" ) <*>
        arguments str (metavar "TAGS")

reportParser :: Parser Action
reportParser =
    Report <$>
        spanParser <*>
        projectFilterParser <*>
        activityFilterParser <*>
        tagFilterParser <*>
        reportTypeParser

spanParser :: Parser Types.TimeSpan
spanParser =
    argument argumentBuilder
        ( metavar "month|week|day"
       <> completeWith ["month","week","day"]
       <> value Types.Day)

mStr :: String -> Either ParseError (Maybe String)
mStr = fmap Just . str

projectFilterParser :: Parser (Maybe String)
projectFilterParser = option
    ( long "project"
   <> short 'p'
   <> reader mStr
   <> value Nothing
   <> metavar "PROJECT"
   <> help "Filter by project")

activityFilterParser :: Parser (Maybe String)
activityFilterParser = option
    ( long "activity"
   <> short 'a'
   <> reader mStr
   <> value Nothing
   <> metavar "ACTIVITY"
   <> help "Filter by activity")

tagFilterParser :: Parser (Maybe String)
tagFilterParser = option
    ( long "tag"
   <> reader mStr
   <> value Nothing
   <> metavar "TAG"
   <> help "Filter by tag")

reportTypeParser :: Parser Types.ReportType
reportTypeParser = option
    ( long "type"
   <> short 't'
   <> reader parseKeyword
   <> value Types.Simple
   <> completeWith ["simple", "totaltime"]
   <> metavar "SIMPLE|TOTALTIME"
   <> help "Report Type (default: simple)")
