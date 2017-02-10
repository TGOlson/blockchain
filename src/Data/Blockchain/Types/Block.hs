module Data.Blockchain.Types.Block
    ( Block
    , BlockHeader
    , blockTransactions
    , newBlock
    , blockDifficulty
    , incBlockNonce
    ) where

import qualified Data.Aeson as Aeson
import           Data.Aeson ((.=))

import Data.Blockchain.Crypto.Hash
import Data.Blockchain.Types.BlockHeader
import Data.Blockchain.Types.Difficulty
import Data.Blockchain.Types.Transaction

-- Field	Description	Size
-- Magic no	value always 0xD9B4BEF9	4 bytes
-- Blocksize	number of bytes following up to end of block	4 bytes
-- BlockHeader	consists of 6 items	80 bytes
-- Transaction counter	positive integer VI = VarInt	1 - 9 bytes
-- transactions	the (non empty) list of transactions	<Transaction counter>-many transactions

-- https://en.bitcoin.it/wiki/Block
data Block = Block
    { blockHeader       :: BlockHeader
    , blockTransactions :: [Transaction]
    }
  deriving (Show)

newBlock :: BlockHash -> Difficulty -> Block
newBlock prevBlockHash difficulty = Block{..}
  where
    blockHeader = newBlockHeader prevBlockHash transactionHashTreeRoot difficulty
    -- TODO: this should necessarily have 1 transaction, the initial
    -- transaction for the "reward" for solving this block
    -- for now leave empty...
    blockTransactions       = [] :: [Transaction]
    transactionHashTreeRoot = hashJSON blockTransactions

blockDifficulty :: Block -> Difficulty
blockDifficulty = blockHeaderDifficulty . blockHeader

incBlockNonce :: Block -> Block
incBlockNonce block = block { blockHeader = incNonce (blockHeader block) }

instance Hashable Block where
    toHash = hashJSON

instance Aeson.ToJSON Block where
    toJSON Block{..} = Aeson.object
        [ "header"       .= blockHeader
        , "transactions" .= blockTransactions
        ]
