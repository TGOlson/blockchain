module Data.Blockchain.Solver
    ( findNextBlock
    ) where

import Data.Blockchain.Crypto.Hash
import Data.Blockchain.Types

findNextBlock :: BlockHash -> Difficulty -> Block
findNextBlock prevBlockHash difficulty = searchForValidBlock (newBlock prevBlockHash difficulty)
  where
    searchForValidBlock block =
        if isValidBlock difficulty block
            then block
            else searchForValidBlock (incBlockNonce block)

isValidBlock :: Difficulty -> Block -> Bool
isValidBlock difficulty block = toHash block < unDifficulty difficulty
