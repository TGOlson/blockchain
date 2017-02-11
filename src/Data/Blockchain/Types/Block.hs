module Data.Blockchain.Types.Block
    ( Block
    , BlockHeader
    , blockTransactions
    , makeBlock
    , blockDifficulty
    , incBlockNonce
    ) where

import qualified Data.Aeson      as Aeson
import           Data.Aeson      ((.=))
import qualified Data.Time.Clock as Time

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

-- TODO: should garuantee there at least on transaction in block,
-- the transaction for the "reward" for solving this block
makeBlock :: [Transaction] -> BlockHash -> Time.UTCTime -> Difficulty -> Block
makeBlock blockTransactions prevBlockHash time difficulty = Block{..}
  where
    blockHeader = makeBlockHeader prevBlockHash transactionHashTreeRoot time difficulty
    transactionHashTreeRoot = hashJSON blockTransactions -- Needs to be merkle root

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
