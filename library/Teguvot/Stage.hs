module Teguvot.Stage where

import Control.Lens (TraversableWithIndex (..), FoldableWithIndex (..))
import Data.Bimap (Bimap)
import Data.Bimap qualified as Bimap
import Data.ByteString qualified as ByteString
import Data.Char (ord)
import Data.Foldable qualified as Foldable
import Data.Map.Strict qualified as Map
import Data.Validation (Validation (..))
import Data.Word (Word8)
import GHC.Generics (Generic)
import Text.Pretty.Simple (pPrintLightBg)

sample :: Bimap Word8 ()
sample = Bimap.fromList
  [ ((fromIntegral . ord) '\n', ())
  ]

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

displayCounts :: (Ord a, Show a, Functor t, Foldable t) => t a -> IO ()
displayCounts items = do
  let counts = (, 1 :: Int) <$> items
  pPrintLightBg $ Map.fromListWith (+) (Foldable.toList counts)

runStages :: FilePath -> IO ()
runStages filePath = do
  bytes <- ByteString.readFile filePath
  displayCounts (ByteString.unpack bytes)
