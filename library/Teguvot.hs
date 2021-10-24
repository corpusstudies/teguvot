module Teguvot where

import Teguvot.File
import Teguvot.Type
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import System.Exit (exitFailure)
import Data.Foldable (foldl')
import Text.Pretty.Simple (pPrintLightBg)
import GHC.Generics (Generic)
import Control.Lens ((^.))

data DupCategoryItem = DupCategoryItem
  { name :: AnalysisName
  , dup :: Location
  , existing :: Location
  }
  deriving (Generic, Show)

loadCategoryMap :: FilePath ->
  IO (Map AnalysisName CategoryItem)
loadCategoryMap filePath = do
  categoryItems <-
    readParseCategoryFile filePath

  let
    go (dups, existingMap) newItem
      = let analysisName = newItem ^. #analysis . #name
        in case Map.lookup analysisName existingMap of
          Nothing ->
            ( dups
            , Map.insert
                analysisName
                newItem
                existingMap
            )
          Just existingItem ->
            ( DupCategoryItem 
                { name = analysisName
                , dup =
                    newItem ^. #analysis . #location
                , existing =
                    existingItem ^. #analysis . #location
                }
                : dups
            , existingMap
            )

    dupCategories :: [DupCategoryItem]
    categoryMap :: Map AnalysisName CategoryItem
    (dupCategories, categoryMap) = 
      foldl'
        go
        (mempty, mempty)
        categoryItems
  case dupCategories of
    [] -> pure ()
    _ : _ -> do
      putStrLn "Error: duplicate categories"
      mapM_ pPrintLightBg dupCategories
      exitFailure
  pure categoryMap

main :: IO ()
main = do
  analysisItems <-
    readParseAnalysisFile "data/analysis.txt"
  categoryMap <- loadCategoryMap "data/category.txt"

  putStrLn $
    (show . length) analysisItems
    <> " analysis items; "
    <> (show . length) categoryMap
    <> " category items."
