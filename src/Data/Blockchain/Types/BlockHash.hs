module Data.Blockchain.Types.BlockHash
    ( BlockHash(..)
    ) where

import qualified Data.Aeson as Aeson

import Data.Blockchain.Crypto.Hash

newtype BlockHash = BlockHash { unBlockHash :: Hash }
  deriving (Eq, Ord, Aeson.ToJSON, Show)
