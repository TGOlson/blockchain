module Data.Blockchain.Core.Crypto.HashTree
    ( HashTreeRoot
    , unHashTreeRoot
    , hashTreeRoot
    ) where

import qualified Data.Aeson as Aeson

import Data.Blockchain.Core.Crypto.Hash

data HashTreeRoot a = HashTreeRoot { unHashTreeRoot :: ByteStringHash }
  deriving (Eq, Ord, Show)

instance Aeson.ToJSON (HashTreeRoot a) where
    toJSON = Aeson.toJSON . unHashTreeRoot


-- Note: hash tree constructed with extra leaves at end of tree.
-- This is NOT compatible with the Bitcoin implementation.
--      ┌───┴──┐       ┌────┴───┐         ┌─────┴─────┐
--   ┌──┴──┐   │    ┌──┴──┐     │      ┌──┴──┐     ┌──┴──┐
-- ┌─┴─┐ ┌─┴─┐ │  ┌─┴─┐ ┌─┴─┐ ┌─┴─┐  ┌─┴─┐ ┌─┴─┐ ┌─┴─┐   │
--    (5-leaf)         (6-leaf)             (7-leaf)

hashTreeRoot :: forall a. Hashable a => [a] -> HashTreeRoot [a]
hashTreeRoot = HashTreeRoot . buildHash
  where
    buildHash :: [a] -> ByteStringHash
    buildHash []  = hash ""
    buildHash [x] = toByteStringHash (hash x)
    buildHash xs  =
        joinHash (buildHash left) (buildHash right)
      where
        (left, right) = splitAt i xs
        i = until (\x -> x * 2 >= length xs) (*2) 1
