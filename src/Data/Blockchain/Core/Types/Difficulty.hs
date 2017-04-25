module Data.Blockchain.Core.Types.Difficulty
    ( Difficulty(..)
    , minDifficulty
    , maxDifficulty
    ) where

import qualified Data.Aeson as Aeson

newtype Difficulty = Difficulty { unDifficulty :: Integer }
  deriving (Eq, Ord, Aeson.ToJSON, Show)

minDifficulty :: Difficulty
minDifficulty = Difficulty 1

maxDifficulty :: Difficulty
maxDifficulty = Difficulty $ (2 :: Integer) ^ (256 :: Integer) - 1
