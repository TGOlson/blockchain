module Data.Blockchain.Solver
    ( findNextBlock
    , isValidBlock
    ) where

import qualified Data.Time.Clock as Time

import Data.Blockchain.Crypto.Hash
import Data.Blockchain.Crypto.HashTree
import Data.Blockchain.Types


-- TODO: should this be in IO and find the current time?
-- Note: this also does not modify the coinbase transaction incase
-- of an int overflow on the nonce... TODO
findNextBlock :: Hash BlockHeader -> Time.UTCTime -> Difficulty -> [Transaction] -> Block
findNextBlock prevBlockHeaderHash time difficulty transactions = searchForValidBlockHeader BlockHeader{..}
  where
    version = 1
    nonce   = 0
    transactionHashTreeRoot = hashTreeRoot transactions
    searchForValidBlockHeader header =
        if isValidBlockHeader header
            then Block header transactions
            else searchForValidBlockHeader (incNonce header)

isValidBlock :: Block -> Bool
isValidBlock = isValidBlockHeader . blockHeader

isValidBlockHeader :: BlockHeader -> Bool
isValidBlockHeader blockHeader = rawHash headerHash < rawHash difficultyHash
  where
    headerHash     = hash blockHeader
    difficultyHash = unDifficulty (difficulty blockHeader)
