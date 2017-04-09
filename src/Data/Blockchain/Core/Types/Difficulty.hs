module Data.Blockchain.Core.Types.Difficulty
    ( Difficulty(..)
    ) where

import qualified Data.Aeson as Aeson

import Data.Blockchain.Core.Crypto.Hash

newtype Difficulty = Difficulty { unDifficulty :: ByteStringHash }
  deriving (Eq, Ord, Aeson.ToJSON, Show)
