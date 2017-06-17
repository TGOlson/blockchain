module Main (main) where

import qualified Data.Aeson                      as Aeson
import qualified Data.ByteString.Lazy            as Lazy
import qualified Data.HashMap.Strict             as H
import qualified Data.List                       as List
import           Data.Monoid                     ((<>))

import qualified Data.Blockchain.Core.Blockchain as Blockchain
import qualified Data.Blockchain.Core.Types      as Blockchain
import qualified Data.Blockchain.Test.Mining     as Test
import qualified Data.Blockchain.Test.Stats      as Test

main :: IO ()
main = mineTestChain

mineTestChain :: IO ()
mineTestChain = do
    blockchain <- Test.runTestMiner iterations config (writeJSON "data/test_blockchain.json")

    let stats = Test.computeStats blockchain
        outputs     = H.toList $ Blockchain.addressValues blockchain
        outputsP    = List.intercalate "\n" $ (\(k, v) -> show k <> ": " <> show v) <$> outputs


    putStrLn "Final test chain mining stats"
    putStrLn $ "  Length:              " <> show (Test.numBlocks stats)
    putStrLn $ "  Total time:          " <> show (Test.totalTime stats)
    putStrLn $ "  Time per block time: " <> show (Test.timePerBlock stats)
    putStrLn $ "  Total hashes:        " <> show (Test.totalHashes stats)
    putStrLn $ "  Hashes per block:    " <> show (Test.hashesPerBlock stats)
    putStrLn $ "  Hash rate (hash/s):  " <> show (Test.hashRate stats)
    putStrLn "Current address values"
    putStrLn outputsP

  where
    iterations = 100
    config = Blockchain.defaultConfig
        { Blockchain.initialMiningReward           = 500
        -- , Blockchain.targetSecondsPerBlock         = 60
        , Blockchain.difficultyRecalculationHeight = 10
        , Blockchain.miningRewardHalvingHeight     = 20
        }


writeJSON :: Aeson.ToJSON a => FilePath -> a -> IO ()
writeJSON path = Lazy.writeFile path . Aeson.encode

-- readJSON :: Aeson.FromJSON a => FilePath -> IO a
-- readJSON path =  throwLeft . Aeson.eitherDecode <$> Lazy.readFile path
--
-- throwLeft :: Show a => Either a b -> b
-- throwLeft = either (error . show) id
