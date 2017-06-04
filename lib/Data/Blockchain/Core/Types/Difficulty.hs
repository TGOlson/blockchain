module Data.Blockchain.Core.Types.Difficulty
    ( Difficulty(..)
    , minDifficulty
    , maxDifficulty
    ) where

import qualified Data.Aeson as Aeson
import qualified Data.Word  as Word

-- TODO: difficulty should never be zero
-- type constructor should enforce that invariant
newtype Difficulty = Difficulty { unDifficulty :: Word.Word64 }
  deriving (Eq, Ord, Aeson.ToJSON, Aeson.FromJSON, Show)

minDifficulty :: Difficulty
minDifficulty = Difficulty 1

maxDifficulty :: Difficulty
maxDifficulty = Difficulty $ (2 :: Word.Word64) ^ (256 :: Word.Word64) - 1
