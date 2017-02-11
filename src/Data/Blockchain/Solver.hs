module Data.Blockchain.Solver
    ( findNextBlock
    ) where

import qualified Data.Time.Clock as Time

import Data.Blockchain.Crypto.Hash
import Data.Blockchain.Types

-- TODO: should this be in IO and find the current time?
-- Also, should it take in a list of transactions, or is creating the coinbase transaction
-- part of the utility of this function?
findNextBlock :: BlockHash -> Time.UTCTime -> Difficulty -> Block
findNextBlock prevBlockHash time difficulty = searchForValidBlock initialBlock
  where
    initialBlock = makeBlock transactions prevBlockHash time difficulty
    transactions = [] :: [Transaction]
    searchForValidBlock block =
        if isValidBlock difficulty block
            then block
            else searchForValidBlock (incBlockNonce block)

isValidBlock :: Difficulty -> Block -> Bool
isValidBlock difficulty block = toHash block < unDifficulty difficulty
