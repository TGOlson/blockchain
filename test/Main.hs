module Main where

import qualified Test.Hspec

import qualified System.Environment

import qualified Data.Blockchain.Core.BlockchainSpec
import qualified Data.Blockchain.Core.Builder.TransactionSpec
import qualified Data.Blockchain.Core.Crypto.ECDSASpec
import qualified Data.Blockchain.Core.Crypto.HashSpec
import qualified Data.Blockchain.Core.Crypto.HashTreeSpec
import qualified Data.Blockchain.Core.Types.BlockchainConfigSpec
import qualified Data.Blockchain.Core.Types.DifficultySpec
import qualified Data.Blockchain.Core.Types.HexSpec
import qualified Data.Blockchain.Core.Types.TransactionSpec
import qualified Data.Blockchain.Mining.BlockSpec
import qualified Integration.Mining
import qualified Integration.Stats
import qualified TestUtil


main :: IO ()
main = System.Environment.getArgs >>= \case
    ["mine", x]   -> Integration.Mining.mineTestChain (read x)
    ["stats", fp] -> readBlockchain fp >>= Integration.Stats.printStats
    _             -> mapM_ Test.Hspec.hspec specs
  where
    readBlockchain = fmap TestUtil.validate' . TestUtil.readJSON

specs :: [Test.Hspec.Spec]
specs = [ Data.Blockchain.Core.BlockchainSpec.spec
        , Data.Blockchain.Core.Builder.TransactionSpec.spec
        , Data.Blockchain.Core.Crypto.ECDSASpec.spec
        , Data.Blockchain.Core.Crypto.HashSpec.spec
        , Data.Blockchain.Core.Crypto.HashTreeSpec.spec
        , Data.Blockchain.Core.Types.BlockchainConfigSpec.spec
        , Data.Blockchain.Core.Types.DifficultySpec.spec
        , Data.Blockchain.Core.Types.HexSpec.spec
        , Data.Blockchain.Core.Types.TransactionSpec.spec
        , Data.Blockchain.Mining.BlockSpec.spec
        ]
