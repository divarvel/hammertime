
module Hammertime.CLI (
      Action(..)
    , cliParserInfo
    ) where


import Data.Char (toLower)
import Data.List (find, intercalate)
import Options.Applicative

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


defaultReport :: Action
defaultReport = Report Types.Day Nothing Nothing Nothing Types.Simple


parseArgument :: (Bounded a, Enum a, Show a) => String -> Either String a
parseArgument string = maybe (Left $ "Accepted values: " ++ p values) Right matching
    where
        p = intercalate " | " . map show
        matching = find (match string . show) values
        match s s' = map toLower s == map toLower s'
        values = [minBound..maxBound]

cliParserInfo :: ParserInfo Action
cliParserInfo = info  (helper <*> cliParser) fullDesc

cliParser :: Parser Action
cliParser = subparser
     ( command "start"
        (info (pure $ Start "" "" [])
              (progDesc "Start a new activity"))
    <> command "stop"
        (info (pure Stop)
              (progDesc "Stop current activity"))
    <> command "report"
        (info (pure defaultReport)
              (progDesc "Generate report for a given time span (default: day)"))
    <> command "current"
        (info (pure Current)
              (progDesc "Display current activity"))
     )
