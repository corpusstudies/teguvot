module Teguvot.Type where

import Data.Text (Text)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Prelude hiding (length, readFile)

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

data AnalysisItem
  = AnalysisItemWord CorpusWord
  | AnalysisItemCombo Combo
  deriving (Generic, Show)

data CategoryItem = CategoryItem
  { analysis :: Analysis
  , implications :: [Analysis]
  }
  deriving (Generic, Show)
