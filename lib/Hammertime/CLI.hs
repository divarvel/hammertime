{-| Module dedicated to the parsing of CLI arguments
-}
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

-- | Data type representing the action specified by the user
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


-- | Try to parse a string into a keyword
parseKeyword :: (Bounded a, Enum a, Show a) => String -> ReadM a
parseKeyword string = ReadM $ maybe (Left helpText) Right $ matching
    where
        matching = find (match string . show) values
        match s s' = map toLower s == map toLower s'
        values = [minBound..maxBound]
        helpText = ErrorMsg $ "Possible values: " ++ (intercalate "|" . map show $ values)

-- | Build an argument reading a keyword
argumentBuilder :: (Bounded a, Enum a, Show a) => String -> Maybe a
argumentBuilder string = case runReadM (parseKeyword string) of
    Left _ -> Nothing
    Right a -> Just a

-- | Preferences for the CLI parser
cliParserPrefs :: ParserPrefs
cliParserPrefs = prefs showHelpOnError

-- | Information describing hammertime
cliParserInfo :: ParserInfo Action
cliParserInfo = info
    ( helper
  <*> cliParser)
    ( fullDesc
   <> header "Hammertime -- Lightweight time tracker"
    )

-- | Main parser
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

-- | Parser for the start action
startParser :: Parser Action
startParser =
    Start <$>
        argument str ( metavar "PROJECT" ) <*>
        argument str ( metavar "ACTIVITY" ) <*>
        arguments str (metavar "TAGS")

-- | Parser for the report action
reportParser :: Parser Action
reportParser =
    Report <$>
        spanParser <*>
        projectFilterParser <*>
        activityFilterParser <*>
        tagFilterParser <*>
        reportTypeParser

-- | Parser for a time span
spanParser :: Parser Types.TimeSpan
spanParser =
    argument argumentBuilder
        ( metavar "month|week|day"
       <> completeWith ["month","week","day"]
       <> value Types.Day)

-- | Parser for an optional string
mStr :: String -> ReadM (Maybe String)
mStr = ReadM . fmap Just . str

-- | Parser for an optional filter on the project
projectFilterParser :: Parser (Maybe String)
projectFilterParser = option
    ( long "project"
   <> short 'p'
   <> reader mStr
   <> value Nothing
   <> metavar "PROJECT"
   <> help "Filter by project")

-- | Parser for an optional filter on the activity
activityFilterParser :: Parser (Maybe String)
activityFilterParser = option
    ( long "activity"
   <> short 'a'
   <> reader mStr
   <> value Nothing
   <> metavar "ACTIVITY"
   <> help "Filter by activity")

-- | Parser for an optional filter on the tag
tagFilterParser :: Parser (Maybe String)
tagFilterParser = option
    ( long "tag"
   <> reader mStr
   <> value Nothing
   <> metavar "TAG"
   <> help "Filter by tag")

-- | Parser for the report type
reportTypeParser :: Parser Types.ReportType
reportTypeParser = option
    ( long "type"
   <> short 't'
   <> reader parseKeyword
   <> value Types.Simple
   <> completeWith ["simple", "totaltime"]
   <> metavar "SIMPLE|TOTALTIME"
   <> help "Report Type (default: simple)")
