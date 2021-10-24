module Teguvot.File where

import Data.ByteString (readFile)
import Data.Char (isLower, isDigit, digitToInt, isLetter)
import Data.Generics.Labels ()
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Void (Void)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Prelude hiding (length, readFile)
import System.Exit (exitFailure)
import Teguvot.Type
import Text.Megaparsec
import Text.Megaparsec.Char hiding (space)

type Parser = Parsec Void Text

data AnalysisItem
  = AnalysisItemWord CorpusWord
  | AnalysisItemCombo Combo
  deriving (Generic, Show)

data CategoryItem = CategoryItem
  { analysis :: Analysis
  , implications :: [Analysis]
  }
  deriving (Generic, Show)

getLocation :: Parser Location
getLocation = do
  sourcePos <- getSourcePos
  (pure . Location . Text.pack . sourcePosPretty)
    sourcePos

syllableParser :: Parser Syllable
syllableParser =
  Syllable <$>
    takeWhile1P (Just "Syllable letter") isLetter

syllablesParser :: Parser [Syllable]
syllablesParser = do
  first <- syllableParser
  rest <- many do
    _ <- char '-'
    syllableParser
  pure (first : rest)

corpusWordParser :: Parser CorpusWord
corpusWordParser = do
  wordNumber <- wordNumberParser
  _ <- char '/'
  syllables <- syllablesParser
  analyses <- analysesParser
  pure CorpusWord { syllables, wordNumber, analyses }

naturalParser :: Parser Natural
naturalParser = do
  digits <- takeWhile1P (Just "WordNumber digit") isDigit
  let step value ch = value * 10 + fromIntegral (digitToInt ch)
  pure (Text.foldl' step 0 digits)

wordNumberParser :: Parser WordNumber
wordNumberParser = WordNumber <$> naturalParser

rangeParser :: Parser Range
rangeParser = do
  start <- wordNumberParser
  _ <- char '-'
  end <- wordNumberParser
  pure Range { start, end }

spaceParser :: Parser Char
spaceParser = char ' '

analysisParser :: Parser Analysis
analysisParser = do
  location <- getLocation
  let isAnalysisChar c
        = isLower c
        || isDigit c
        || c == '+'
        || c == '-'
        || c == '/'
  name <-
    AnalysisName <$>
      takeWhile1P (Just "Analysis char") isAnalysisChar
  pure Analysis {name, location}

analysesParser :: Parser [Analysis]
analysesParser = do
  maybeAnalyses <- optional do
    _ <- spaceParser
    _ <- char ':'
    many do
      _ <- spaceParser
      analysisParser
  pure (fromMaybe [] maybeAnalyses)

comboParser :: Parser Combo
comboParser = do
  _ <- char '+'
  _ <- spaceParser
  range <- rangeParser
  analyses <- analysesParser
  pure Combo { range, analyses }

analysisItemParser :: Parser AnalysisItem
analysisItemParser
  = try (AnalysisItemWord <$> corpusWordParser)
  <|> AnalysisItemCombo <$> comboParser

-- skips empty lines
onePerLineParser :: Parser a -> Parser [a]
onePerLineParser perLineParser =
  some do
    _ <- many eol
    item <- perLineParser
    _ <- eol
    pure item

analysisItemsParser :: Parser [AnalysisItem]
analysisItemsParser =
  onePerLineParser analysisItemParser

categoryItemParser :: Parser CategoryItem
categoryItemParser = do
  analysis <- analysisParser
  implications <- analysesParser
  pure
    CategoryItem
      {analysis, implications}

categoryItemsParser :: Parser [CategoryItem]
categoryItemsParser =
  onePerLineParser categoryItemParser

readFileAsText :: FilePath -> IO Text
readFileAsText filePath = do
  bytes <- readFile filePath
  let text = decodeUtf8With lenientDecode bytes
  pure text

readParseFileOrDie :: Parser a -> FilePath -> IO a
readParseFileOrDie parser filePath = do
  text <- readFileAsText filePath
  let result = parse (parser <* eof) filePath text
  case result of
    Left errorBundle -> do
      putStrLn ("Error in: " <> filePath)
      putStrLn (errorBundlePretty errorBundle)
      exitFailure
    Right value -> pure value

readParseAnalysisFile :: FilePath -> IO [AnalysisItem]
readParseAnalysisFile =
  readParseFileOrDie analysisItemsParser

readParseCategoryFile :: FilePath -> IO [CategoryItem]
readParseCategoryFile =
  readParseFileOrDie categoryItemsParser
