module Data.Blockchain.Types.Blockheader
    ( Blockheader
    , blockHeaderVersion
    , blockHeaderPrevBlockHash
    , blockHeaderTransactionHashTreeRoot
    , blockHeaderTime
    , blockHeaderDifficulty
    , blockHeaderNonce
    , newBlockheader
    , incNonce
    ) where

import qualified Data.Aeson         as Aeson
import           Data.Aeson         ((.=))
import qualified Data.Time.Clock    as Time
import qualified Data.Time.Calendar as Time


import Data.Blockchain.Crypto.Hash
import Data.Blockchain.Types.Difficulty


-- Field	Purpose	Updated when...	Size (Bytes)
-- Version	Block version number	You upgrade the software and it specifies a new version	4
-- hashPrevBlock	256-bit hash of the previous block header	A new block comes in	32
-- hashMerkleRoot	256-bit hash based on all of the transactions in the block	A transaction is accepted	32
-- Time	Current timestamp as seconds since 1970-01-01T00:00 UTC	Every few seconds	4
-- Bits	Current target in compact format	The difficulty is adjusted	4
-- Nonce	32-bit number (starts at 0)	A hash is tried (increments)	4

-- https://en.bitcoin.it/wiki/Block_hashing_algorithm
data Blockheader = Blockheader
    { blockHeaderVersion                 :: Int
    , blockHeaderPrevBlockHash           :: Hash
    , blockHeaderTransactionHashTreeRoot :: Hash
    , blockHeaderTime                    :: Time.UTCTime
    , blockHeaderDifficulty              :: Difficulty
    , blockHeaderNonce                   :: Int
    }

newBlockheader :: Hash -> Hash -> Difficulty -> Blockheader
newBlockheader
    blockHeaderPrevBlockHash
    blockHeaderTransactionHashTreeRoot
    blockHeaderDifficulty =
      Blockheader{..}
  where
    blockHeaderVersion = 1
    -- Hardcoded, don't care for now...
    blockHeaderTime    = Time.UTCTime (Time.ModifiedJulianDay 0) (Time.secondsToDiffTime 0)
    blockHeaderNonce   = 0

incNonce :: Blockheader -> Blockheader
incNonce header = header { blockHeaderNonce = blockHeaderNonce header + 1 }

instance Hashable Blockheader where
    toHash = hashJSON

instance Aeson.ToJSON Blockheader where
    toJSON Blockheader{..} = Aeson.object
        [ "version"                 .= blockHeaderVersion
        , "prevBlockHash"           .= blockHeaderPrevBlockHash
        , "transactionHashTreeRoot" .= blockHeaderTransactionHashTreeRoot
        , "time"                    .= blockHeaderTime
        , "difficulty"              .= blockHeaderDifficulty
        , "nonce"                   .= blockHeaderNonce
        ]
