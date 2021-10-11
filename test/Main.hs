module Main where

import Test.Hspec qualified as Hspec

main :: IO ()
main = Hspec.hspec do
  Hspec.it "vacuous" do
    True `Hspec.shouldBe` True
