module Data.Blockchain.Crypto.HashTree
    ( HashTree
    , hashTree
    , rootHash
    ) where

import qualified Data.ByteString as BS

import Data.Blockchain.Crypto.Hash

data HashTree = Node ByteStringHash HashTree HashTree | Leaf ByteStringHash deriving Show

-- Note: hash tree constructed with extra leaves at end of tree.
-- This is NOT compatable with the Bitcoin implementation.
--      ┌───┴──┐       ┌────┴───┐         ┌─────┴─────┐
--   ┌──┴──┐   │    ┌──┴──┐     │      ┌──┴──┐     ┌──┴──┐
-- ┌─┴─┐ ┌─┴─┐ │  ┌─┴─┐ ┌─┴─┐ ┌─┴─┐  ┌─┴─┐ ┌─┴─┐ ┌─┴─┐   │
--    (5-leaf)         (6-leaf)             (7-leaf)

rootHash :: HashTree -> ByteStringHash
rootHash (Node h _ _) = h
rootHash (Leaf h)     = h

hashTree :: BS.ByteString -> HashTree
hashTree bytes = buildTree leaves
  where
    buildTree :: [HashTree] -> HashTree
    buildTree [x] = x
    buildTree xs  =
        joinHashTree (buildTree left) (buildTree right)
      where
        (left, right) = splitAt i xs
        i = until (\x -> x * 2 >= length xs) (*2) 1
    leaves :: [HashTree]
    leaves = Leaf . hash <$> blocks
    blocks = segments blockSize bytes
    blockSize = 64


joinHashTree :: HashTree -> HashTree -> HashTree
joinHashTree h1 h2 = Node (joinHash (rootHash h1) (rootHash h2)) h1 h2

segments :: Int -> BS.ByteString -> [BS.ByteString]
segments _ xs | BS.null xs = []
segments size xs           = y : segments size ys
  where
    (y, ys) = BS.splitAt size xs
