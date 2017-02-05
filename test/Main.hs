module Main where

import Test.QuickCheck

import Data.Blockchain.Solver
import Data.Blockchain.Crypto.Hash
import Data.Blockchain.Types.Block
import Data.Blockchain.Types.Blockheader
import Data.Blockchain.Types.Difficulty
import Data.Maybe

main :: IO ()
main = do
    putStrLn "Block difficulty"
    print (unDifficulty $ blockDifficulty block)
    let nextBlock = findNextBlock block
    putStrLn "Next block hash"
    print (toHash nextBlock)
    putStrLn "Next block nonce"
    print (blockHeaderNonce $ blockHeader nextBlock)


block :: Block
block = newBlock prevBlockHash difficulty
  where
    prevBlockHash = hash "seed"
    difficulty    = Difficulty (h "0000ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")
    h = fromMaybe (error "Invalid hash string") . fromByteString



eqInt :: Int -> Property
eqInt i = i === i
