module Data.Blockchain.Test.Stats
    ( BlockchainStats(..)
    , computeStats
    , timePerBlock
    , hashesPerBlock
    , hashRate
    ) where

-- TODO: this module might be useful elsewhere outside of "Test" group

import qualified Data.List.NonEmpty              as NonEmpty
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
