module Main where

import qualified Test.Hspec as Hspec

import qualified Data.Blockchain.Core.BlockchainSpec
import qualified Data.Blockchain.Core.Builder.TransactionSpec
import qualified Data.Blockchain.Core.Crypto.ECDSASpec
import qualified Data.Blockchain.Core.Crypto.HashTreeSpec
import qualified Data.Blockchain.Core.Types.BlockchainConfigSpec
-- import qualified Data.Blockchain.Mining.SolverSpec

main :: IO ()
main = mapM_ Hspec.hspec
    [ Data.Blockchain.Core.BlockchainSpec.spec
    , Data.Blockchain.Core.Builder.TransactionSpec.spec
    , Data.Blockchain.Core.Crypto.ECDSASpec.spec
    , Data.Blockchain.Core.Crypto.HashTreeSpec.spec
    , Data.Blockchain.Core.Types.BlockchainConfigSpec.spec
    -- , Data.Blockchain.Mining.SolverSpec.spec
    ]
