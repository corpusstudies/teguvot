module Teguvot where

import Teguvot.File

main :: IO ()
main = do
  analysisItems <-
    readParseAnalysisFile "data/analysis.txt"
  categoryItems <-
    readParseCategoryFile "data/category.txt"
  putStrLn $
    (show . length) analysisItems
    <> " analysis items; "
    <> (show . length) categoryItems
    <> " category items."
