module Data.Blockchain.Core.Types.Difficulty
    ( Difficulty(..)
    ) where

import qualified Data.Aeson                    as Aeson
import qualified Numeric.Natural               as Natural

import qualified Data.Blockchain.Core.Util.Hex as Hex

newtype Difficulty = Difficulty { unDifficulty :: Natural.Natural }
  deriving (Aeson.FromJSON, Aeson.ToJSON, Enum, Eq, Integral, Num, Real, Ord, Show)

instance Bounded Difficulty where
    minBound = 0
    maxBound = fromIntegral (maxBound :: Hex.Hex256)
