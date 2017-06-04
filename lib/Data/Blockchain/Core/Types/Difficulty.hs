module Data.Blockchain.Core.Types.Difficulty
    ( Difficulty(..)
    , minDifficulty
    , maxDifficulty
    ) where

import qualified Data.Aeson as Aeson
import qualified Data.Word  as Word

newtype Difficulty = Difficulty { unDifficulty :: Word.Word64 }
  deriving (Eq, Num, Ord, Show, Aeson.ToJSON, Aeson.FromJSON)

minDifficulty :: Difficulty
minDifficulty = Difficulty 1

maxDifficulty :: Difficulty
maxDifficulty = Difficulty maxBound -- (2^256 - 1)
