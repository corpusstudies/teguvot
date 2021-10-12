module Teguvot where

import Data.ByteString (readFile)
import Data.Text (Text, length)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Prelude hiding (length, readFile)

newtype WordNumber = WordNumber Int
newtype Word = Word Text
newtype Category = Category Text
newtype Combo = Combo [WordNumber]

main :: IO ()
main = do
  bytes <- readFile "data/analysis.txt"
  let analysisText :: Text = decodeUtf8With lenientDecode bytes
  print (length analysisText)
