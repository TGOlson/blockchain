module Integration.Stats
    ( printStats
    ) where

-- TODO: this module might be useful elsewhere outside of "Test" group

import qualified Data.HashMap.Strict             as H
import qualified Data.List                       as List
import qualified Data.List.NonEmpty              as NonEmpty
import           Data.Monoid                     ((<>))
import           Data.Ratio                      (Ratio)
import qualified Data.Time.Clock                 as Time
import           Data.Word                       (Word)

import qualified Data.Blockchain.Core.Blockchain as B
import qualified Data.Blockchain.Core.Types      as B

data BlockchainStats = BlockchainStats
    { numBlocks   :: Word
    , totalTime   :: Time.NominalDiffTime
    , totalHashes :: Word
    }

printStats :: B.Blockchain B.Validated -> IO ()
printStats blockchain = do
    let stats    = computeStats blockchain
        outputs  = H.toList $ B.addressValues blockchain
        outputsP = List.intercalate "\n" $ (\(k, v) -> show k <> ": " <> show v) <$> outputs

    putStrLn $ List.intercalate "\n"
        [ "Final test chain mining stats"
        , "  Length:             " <> show (numBlocks stats)
        , "  Total time:         " <> show (totalTime stats)
        , "  Time per block:     " <> show (timePerBlock stats)
        , "  Total hashes:       " <> show (totalHashes stats)
        , "  Hashes per block:   " <> show (hashesPerBlock stats)
        , "  Hash rate (hash/s): " <> show (hashRate stats)
        , "Current address values"
        , outputsP
        ]


computeStats :: B.Blockchain B.Validated -> BlockchainStats
computeStats blockchain = BlockchainStats{..}
  where
    numBlocks   = fromIntegral (length mainChain)
    totalTime   = Time.diffUTCTime (B.time lastHeader) (B.time firstHeader)
    totalHashes = fromIntegral $ sum $ (succ . B.nonce) <$> headers -- Add 1 to each nonce to account for first hash
    mainChain   = NonEmpty.toList $ B.longestChain blockchain
    headers     = B.blockHeader <$> mainChain
    firstHeader = head headers
    lastHeader  = last headers

timePerBlock :: BlockchainStats -> Time.NominalDiffTime
timePerBlock BlockchainStats{..} = totalTime / fromIntegral numBlocks

hashesPerBlock :: BlockchainStats -> Word
hashesPerBlock BlockchainStats{..} = totalHashes `div` numBlocks

-- TODO: better return type?
-- TODO: compute hash rate only for last N blocks
hashRate :: BlockchainStats -> Word
hashRate BlockchainStats{..} = round (fromIntegral totalHashes / realToFrac totalTime :: Ratio Integer)
