module Data.Blockchain.Solver
    ( findNextBlock
    ) where

import Data.Blockchain.Crypto.Hash
import Data.Blockchain.Types.Block
import Data.Blockchain.Types.Difficulty

findNextBlock :: Difficulty -> Block -> Block
findNextBlock difficulty prevBlock = searchForValidBlock (newBlock prevBlockHash difficulty)
  where
    prevBlockHash = toHash prevBlock
    searchForValidBlock block =
        if isValidBlock difficulty block
            then block
            else searchForValidBlock (incBlockNonce block)

isValidBlock :: Difficulty -> Block -> Bool
isValidBlock difficulty block = toHash block < unDifficulty difficulty
