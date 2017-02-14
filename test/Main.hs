module Main where

import qualified Test.Hspec as Hspec

import qualified Data.Blockchain.SolverSpec
import qualified Data.Blockchain.Crypto.ECDSASpec

main :: IO ()
main = mapM_ Hspec.hspec
    [ Data.Blockchain.SolverSpec.spec
    , Data.Blockchain.Crypto.ECDSASpec.spec
    ]
