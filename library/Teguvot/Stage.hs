module Teguvot.Stage where

import Control.Lens (TraversableWithIndex (..), FoldableWithIndex (..))
import Data.Bimap (Bimap)
import Data.Bimap qualified as Bimap
import Data.ByteString qualified as ByteString
import Data.Char (ord)
import Data.Foldable qualified as Foldable
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Validation (Validation (..))
import Data.Word (Word8)
import GHC.Generics (Generic)
import Text.Pretty.Simple (pPrintLightBg)

data BimapListError a = BimapListError
  { item :: a
  , index :: Int
  }
  deriving (Generic, Show)

data BimapBuildError a b = BimapBuildError
  { existingL :: Maybe (a, b)
  , existingR :: Maybe (a, b)
  , dup :: (a, b)
  , index :: Int
  }
  deriving (Generic, Show)

build :: forall a b.
  (Ord a, Ord b, Show a, Show b) =>
  [(a, b)] -> Validation [BimapBuildError a b] (Bimap a b)
build pairs = do
  let step index (errs, bimap) (itemL, itemR) =
        case (Bimap.lookup itemL bimap, Bimap.lookupR itemR bimap) of
          (Nothing, Nothing) ->
            ( errs
            , Bimap.insert itemL itemR bimap
            )
          (resultL, resultR) ->
            ( BimapBuildError
                { dup = (itemL, itemR)
                , index
                , existingL = (itemL, ) <$> resultL
                , existingR = (, itemR) <$> resultR
                }
              : errs
            , bimap
            )
      (errors, result) = ifoldl' step ([], Bimap.empty) pairs
  case errors of
    [] -> Success result
    _ : _ -> Failure errors

apply :: (Ord a, Ord b, TraversableWithIndex Int t) =>
  Bimap a b -> t a -> Validation [BimapListError a] (t b)
apply bimap items = do
  let step index item =
        case Bimap.lookup item bimap of
          Nothing -> Failure [BimapListError {item, index}]
          Just result -> Success result
  itraverse step items

getScale :: Scale -> Int -> Int -> Text
getScale (Scale scale) inputMax inputValue =
  let stepValue =
        let (maxDivScale, maxModScale) = inputMax `divMod` scale
        in
          if maxModScale == 0
            then maxDivScale
            else maxDivScale + 1
      go value = 
        if value <= 0
          then ""
          else "*" <> go (value - stepValue)
  in go inputValue

newtype Scale = Scale Int
data Count = Count
  { amount :: Text
  , count :: Int
  }
  deriving (Generic, Show)
data DataCounts a = DataCounts 
  { countTotal :: Int
  , countUnique :: Int
  , valueCount :: [(a, Count)]
  }
  deriving (Generic, Show)

dataCounts :: (Ord a, Show a, Functor t, Foldable t) => Scale -> t a
  -> DataCounts a
dataCounts scale items =
  let counts = (, 1 :: Int) <$> items
      countMap = Map.fromListWith (+) (Foldable.toList counts)
      countAssocs = Map.toList countMap
      maxCount = maximum (snd <$> countAssocs)
      triples =
        (\(x, v) ->
          (x, Count {amount = getScale scale maxCount v, count = v }))
          <$> countAssocs
  
  in DataCounts
    { countTotal = sum (Map.elems countMap)
    , countUnique = Set.size (Map.keysSet countMap)
    , valueCount = triples
    }

sample :: Bimap Word8 ()
sample = Bimap.fromList
  [ ((fromIntegral . ord) '\n', ())
  ]

runStages :: FilePath -> IO ()
runStages filePath = do
  bytes <- ByteString.readFile filePath
  pPrintLightBg $ dataCounts (Scale 10) (ByteString.unpack bytes)
