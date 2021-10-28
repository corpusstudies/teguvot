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
import Data.Maybe (mapMaybe)

data DupCategoryItem = DupCategoryItem
  { name :: AnalysisName
  , dup :: Location
  , existing :: Location
  }
  deriving (Generic, Show)

checkForMissingAnalyses :: Map AnalysisName CategoryItem
  -> [Analysis] -> IO ()
checkForMissingAnalyses categoryMap analyses = do
  let check analysis@Analysis{name} =
        case Map.lookup name categoryMap of
          Nothing -> Just analysis
          Just _ -> Nothing
  let missing = mapMaybe check analyses
  case missing of
    [] -> pure ()
    _ : _ -> do
      putStrLn "Error: missing analyses"
      mapM_ pPrintLightBg missing
      exitFailure

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

  let referencedAnalyses =
        concatMap
          (^. #implications)
          categoryMap
  checkForMissingAnalyses categoryMap referencedAnalyses

  pure categoryMap

getAnalyses :: AnalysisItem -> [Analysis]
getAnalyses = \case
  AnalysisItemWord CorpusWord {analyses} -> analyses
  AnalysisItemCombo Combo {analyses} -> analyses

main :: IO ()
main = do
  textItems <- readParseTextFile "data/text.txt"

  categoryMap <- loadCategoryMap "data/category.txt"

  analysisItems <-
    readParseAnalysisFile "data/analysis.txt"
  checkForMissingAnalyses
    categoryMap
    (concatMap getAnalyses analysisItems)

  putStrLn $ ""
    <> (show . length) textItems
    <> " text items; "
    <> (show . length) analysisItems
    <> " analysis items; "
    <> (show . length) categoryMap
    <> " category items."
