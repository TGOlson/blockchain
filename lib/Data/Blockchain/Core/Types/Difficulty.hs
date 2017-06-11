module Data.Blockchain.Core.Types.Difficulty
    ( Difficulty(..)
    , difficulty1Target
    ) where

import qualified Data.Aeson     as Aeson
import qualified Data.LargeWord as Word

import qualified Data.Blockchain.Core.Util.Hex as Hex

newtype Difficulty = Difficulty { unDifficulty :: Word.Word256 }
  deriving (Bounded, Enum, Eq, Integral, Num, Real, Ord, Show)

instance Aeson.ToJSON Difficulty where
    toJSON = Aeson.toJSON . toInteger

instance Aeson.FromJSON Difficulty where
    parseJSON = fmap fromInteger . Aeson.parseJSON

-- TODO: `targetCycles :: Word -> Difficulty`

-- TODO: from config
difficulty1Target :: Hex.Hex256
difficulty1Target = Hex.hex256LeadingZeros 2
