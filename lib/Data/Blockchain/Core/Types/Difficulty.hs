module Data.Blockchain.Core.Types.Difficulty
    ( Difficulty(..)
    , difficulty1Target
    ) where

import qualified Data.Aeson      as Aeson
import qualified Numeric.Natural as Natural

import qualified Data.Blockchain.Core.Util.Hex as Hex

newtype Difficulty = Difficulty { unDifficulty :: Natural.Natural }
  deriving (Enum, Eq, Integral, Num, Real, Ord, Show)

instance Bounded Difficulty where
    minBound = 0
    maxBound = fromIntegral (maxBound :: Hex.Hex256)

instance Aeson.ToJSON Difficulty where
    toJSON = Aeson.toJSON . toInteger

instance Aeson.FromJSON Difficulty where
    parseJSON = fmap fromInteger . Aeson.parseJSON

-- TODO: `targetCycles :: Word -> Difficulty`

-- TODO: from config
difficulty1Target :: Hex.Hex256
difficulty1Target = Hex.hex256LeadingZeros 2
