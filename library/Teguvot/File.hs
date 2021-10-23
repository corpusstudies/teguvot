module Teguvot.File where

import Data.ByteString (readFile)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Void (Void)
import Numeric.Natural (Natural)
import Prelude hiding (length, readFile)
import Text.Megaparsec
import Text.Megaparsec.Char hiding (space)
import System.Exit (exitFailure)
import Data.Maybe (fromMaybe)
import Data.Char (isLower, isDigit, digitToInt, isLetter)
import Data.Text qualified as Text
import GHC.Generics (Generic)

type Parser = Parsec Void Text

newtype Analysis = Analysis Text
  deriving newtype Show
newtype Syllable = Syllable Text
  deriving newtype Show
newtype WordNumber = WordNumber Natural
  deriving newtype (Show, Num)

data CorpusWord = CorpusWord
  { syllables :: [Syllable]
  , wordNumber :: WordNumber
  , analyses :: [Analysis]
  }
  deriving (Generic, Show)

data Range = Range
  { start :: WordNumber
  , end :: WordNumber
  }
  deriving (Generic, Show)

data Combo = Combo
  { range :: Range
  , analyses :: [Analysis]
  }
  deriving (Generic, Show)

data Item
  = ItemWord CorpusWord
  | ItemCombo Combo
  deriving (Generic, Show)

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
  let isAnalysisChar c
        = isLower c
        || isDigit c
        || c == '+'
        || c == '-'
        || c == '/'
  Analysis <$>
    takeWhile1P (Just "Analysis char") isAnalysisChar

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

itemParser :: Parser Item
itemParser
  = try (ItemWord <$> corpusWordParser)
  <|> ItemCombo <$> comboParser

itemsParser :: Parser [Item]
itemsParser =
  some do
    _ <- many eol
    item <- itemParser
    _ <- eol
    pure item

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

readParseAnalysisFile :: FilePath -> IO [Item]
readParseAnalysisFile = readParseFileOrDie itemsParser
