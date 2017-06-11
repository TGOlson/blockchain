module Data.Blockchain.Core.Crypto.HashTree
    ( HashTreeRoot
    , unHashTreeRoot
    , hashTreeRoot
    ) where

import qualified Data.Aeson as Aeson

import Data.Blockchain.Core.Crypto.Hash

newtype HashTreeRoot a = HashTreeRoot { unHashTreeRoot :: Hash a }
  deriving (Aeson.FromJSON, Aeson.ToJSON, Eq, Ord, Show)

-- Note: hash tree constructed with extra leaves at end of tree.
-- This is NOT compatible with the Bitcoin implementation.
--      ┌───┴──┐       ┌────┴───┐         ┌─────┴─────┐
--   ┌──┴──┐   │    ┌──┴──┐     │      ┌──┴──┐     ┌──┴──┐
-- ┌─┴─┐ ┌─┴─┐ │  ┌─┴─┐ ┌─┴─┐ ┌─┴─┐  ┌─┴─┐ ┌─┴─┐ ┌─┴─┐   │
--    (5-leaf)         (6-leaf)             (7-leaf)

hashTreeRoot :: forall a. Hashable a => [a] -> HashTreeRoot a
hashTreeRoot = HashTreeRoot . buildHash
  where
    buildHash :: [a] -> Hash a
    buildHash []  = mempty
    buildHash [x] = hash x
    buildHash xs  = mappend (buildHash left) (buildHash right)
      where
        (left, right) = splitAt i xs
        i = until (\x -> x * 2 >= length xs) (*2) 1
