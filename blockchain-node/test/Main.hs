module Main (main) where

import qualified Test.Hspec

main :: IO ()
main = mapM_ Test.Hspec.hspec specs

specs :: [Test.Hspec.Spec]
specs = []
