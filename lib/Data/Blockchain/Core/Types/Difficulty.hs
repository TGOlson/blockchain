module Data.Blockchain.Core.Types.Difficulty
    ( Difficulty(..)
    , minDifficulty
    , maxDifficulty
    ) where

import qualified Data.Aeson      as Aeson
import qualified Numeric
import qualified Numeric.Natural as Natural

newtype Difficulty = Difficulty { unDifficulty :: Natural.Natural }
  deriving (Eq, Num, Ord, Show, Aeson.ToJSON, Aeson.FromJSON)

minDifficulty :: Difficulty
minDifficulty = Difficulty 1

-- TODO: should this be in blockchain config?
maxDifficulty :: Difficulty
maxDifficulty = Difficulty $ hexToNatural "00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"
-- maxDifficulty = Difficulty $ hexToNatural "00000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"

-- unsafe
-- TODO: make safe version in util module
hexToNatural :: String -> Natural.Natural
hexToNatural = fst . head . Numeric.readHex
