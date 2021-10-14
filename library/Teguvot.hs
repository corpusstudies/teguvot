module Teguvot where

import Teguvot.File

main :: IO ()
main = do
  items <- readParseFile "data/analysis.txt"
  print (length items)
