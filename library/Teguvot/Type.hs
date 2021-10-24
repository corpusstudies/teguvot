module Teguvot.Type where

import Data.Text (Text)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Prelude hiding (length, readFile)

newtype AnalysisName = AnalysisName Text
  deriving newtype (Eq, Ord, Show)
newtype Syllable = Syllable Text
  deriving newtype (Eq, Ord, Show)
newtype WordNumber = WordNumber Natural
  deriving newtype (Eq, Ord, Show, Num)
newtype Location = Location Text
  deriving newtype (Eq, Ord, Show)

data Analysis = Analysis
  { name :: AnalysisName
  , location :: Location
  }
  deriving (Generic, Show)

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
