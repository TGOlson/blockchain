module Data.Blockchain.Core.Util.Hex
    ( Hex256(..)
    , hex256
    , hex256LeadingZeros
    ) where

import qualified Control.Monad   as Monad
import qualified Data.Aeson      as Aeson
import qualified Data.Maybe      as Maybe
import qualified Data.Text       as Text
import qualified Data.Word       as Word
import qualified Numeric
import qualified Numeric.Natural as Natural

-- Types -----------------------------------------------------------------------------------------------------

newtype Hex256 = Hex256 { unHex256 :: Natural.Natural }
  deriving (Enum, Eq, Integral, Num, Real, Ord)

instance Show Hex256 where
    show = zeroPadded 64 . showHex . unHex256

instance Bounded Hex256 where
    minBound = 0
    maxBound = Maybe.fromMaybe (error "Unexpected parse failure") $ readHexMaybe $ replicate 64 'f'

instance Aeson.ToJSON Hex256 where
    toJSON = Aeson.String . Text.pack . show

instance Aeson.FromJSON Hex256 where
    parseJSON = Aeson.withText "Hex256" parseHex256
      where
        parseHex256 = maybe (fail "Invalid hex 256 string") return . hex256 . Text.unpack

-- Construction helpers --------------------------------------------------------------------------------------

hex256LeadingZeros :: Word.Word -> Hex256
hex256LeadingZeros n = maxBound `div` Hex256 (16 ^ n)

hex256 :: String -> Maybe Hex256
hex256 str = do
    Monad.unless (length str == 64) Nothing
    x <- readHexMaybe str
    return (Hex256 x)

readHexMaybe :: (Num a, Eq a) => String -> Maybe a
readHexMaybe str = case Numeric.readHex str of
    [(x, "")] -> Just x
    _         -> Nothing

-- Utils -----------------------------------------------------------------------------------------------------

showHex :: (Show a, Integral a) => a -> String
showHex x = Numeric.showHex x mempty

zeroPadded :: Int -> String -> String
zeroPadded x str = replicate (x - length str) '0' ++ str
