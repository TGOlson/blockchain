module Main where

import qualified Test.Hspec as Hspec

import qualified Data.BlockchainSpec
import qualified Data.Blockchain.SolverSpec
import qualified Data.Blockchain.Crypto.ECDSASpec
import qualified Data.Blockchain.Crypto.HashTreeSpec

main :: IO ()
main = mapM_ Hspec.hspec
    [ Data.BlockchainSpec.spec
    , Data.Blockchain.SolverSpec.spec
    , Data.Blockchain.Crypto.ECDSASpec.spec
    , Data.Blockchain.Crypto.HashTreeSpec.spec
    ]
