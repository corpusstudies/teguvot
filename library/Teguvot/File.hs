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
newtype Form = Form Text
  deriving newtype Show
newtype WordNumber = WordNumber Natural
  deriving newtype (Show, Num)

data CorpusWord = CorpusWord
  { form :: Form
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

formParser :: Parser Form
formParser =
  Form <$>
    takeWhile1P (Just "Form letter") isLetter

corpusWordParser :: Parser CorpusWord
corpusWordParser = do
  wordNumber <- wordNumberParser
  _ <- char '/'
  form <- formParser
  analyses <- analysesParser
  pure CorpusWord { form, wordNumber, analyses }

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

readParseFile :: FilePath -> IO [Item]
readParseFile filePath = do
  bytes <- readFile "data/analysis.txt"
  let text :: Text = decodeUtf8With lenientDecode bytes
  let result = parse (itemsParser <* eof) filePath text
  case result of
    Left errorBundle -> do
      putStrLn ("Error in analysis: " <> filePath)
      putStrLn (errorBundlePretty errorBundle)
      exitFailure
    Right items -> pure items