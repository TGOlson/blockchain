module Main (main) where

import qualified Test.Hspec

import qualified System.Environment

import qualified Data.Blockchain.Builder.TransactionSpec
import qualified Data.Blockchain.Crypto.ECDSASpec
import qualified Data.Blockchain.Crypto.HashSpec
import qualified Data.Blockchain.Crypto.HashTreeSpec
import qualified Data.Blockchain.Mining.BlockSpec
import qualified Data.Blockchain.Types.BlockchainConfigSpec
import qualified Data.Blockchain.Types.BlockchainSpec
import qualified Data.Blockchain.Types.DifficultySpec
import qualified Data.Blockchain.Types.HexSpec
import qualified Data.Blockchain.Types.TransactionSpec
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
specs = [ Data.Blockchain.Builder.TransactionSpec.spec
        , Data.Blockchain.Crypto.ECDSASpec.spec
        , Data.Blockchain.Crypto.HashSpec.spec
        , Data.Blockchain.Crypto.HashTreeSpec.spec
        , Data.Blockchain.Types.BlockchainSpec.spec
        , Data.Blockchain.Types.BlockchainConfigSpec.spec
        , Data.Blockchain.Types.DifficultySpec.spec
        , Data.Blockchain.Types.HexSpec.spec
        , Data.Blockchain.Types.TransactionSpec.spec
        , Data.Blockchain.Mining.BlockSpec.spec
        ]
