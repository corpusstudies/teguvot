module Teguvot where

import Teguvot.File

main :: IO ()
main = do
  items <- readParseAnalysisFile "data/analysis.txt"
  print (length items)
