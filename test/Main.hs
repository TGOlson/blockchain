module Main where

import qualified Test.Hspec as Hspec

import qualified Data.Blockchain.SolverSpec

main :: IO ()
main = mapM_ Hspec.hspec
    [ Data.Blockchain.SolverSpec.spec ]
