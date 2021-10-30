module Teguvot.Stage where

import Data.Bimap (Bimap)
import Data.Bimap qualified as Bimap
import Data.Char (ord)
import Data.Either (partitionEithers)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Data.Foldable (foldl')

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

build :: forall a b. (Ord a, Ord b, Show a, Show b) =>
  [(a, b)] ->  Either [BimapBuildError a b] (Bimap a b)
build pairs = do
  let indexedItems = zip [0..] pairs
      step :: ([BimapBuildError a b], Bimap a b)
        -> (Int, (a, b))
        -> ([BimapBuildError a b], Bimap a b)
      step (errs, bimap) (index, (itemL, itemR)) =
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
      (errors, results) = foldl' step ([], Bimap.empty) indexedItems
  case errors of
    [] -> Right results
    _ : _ -> Left errors

overList :: (Ord a, Ord b) =>
  Bimap a b -> [a] -> Either [BimapListError a] [b]
overList bimap items = do
  let indexedItems = zip [0..] items
      step (index, item) =
        case Bimap.lookup item bimap of
          Nothing -> Left BimapListError {item, index}
          Just result -> Right result
      (errors, results) = partitionEithers (step <$> indexedItems)
  case errors of
    [] -> Right results
    _ : _ -> Left errors
