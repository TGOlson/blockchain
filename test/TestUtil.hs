{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestUtil
    ( module X
    , minHash
    ) where

import Test.QuickCheck       as X
import Test.Hspec            as X
import Test.Hspec.QuickCheck as X

import qualified Data.ByteString.Char8 as BS
import qualified Data.Maybe            as Maybe
import qualified Data.Time.Clock       as Time
import qualified Data.Time.Calendar    as Time

import Data.Blockchain.Crypto.Hash
import Data.Blockchain.Types

-- Used in some tests to filter hashes that are too low.
minHash :: Hash
minHash = unsafefromByteString $ BS.append "0000" $ BS.replicate 60 'f'

unsafefromByteString :: BS.ByteString -> Hash
unsafefromByteString = Maybe.fromMaybe (error "Invalid hash string") . fromByteString

-- Instances -------------------------------------------------------------------------------------------------

-- Blockchain types

instance Arbitrary BlockHeaderHash where
    arbitrary = BlockHeaderHash <$> arbitrary

instance Arbitrary Difficulty where
    arbitrary = Difficulty <$> arbitrary

instance Arbitrary Hash where
    arbitrary = unsafefromByteString . BS.pack <$> vectorOf 64 hexChar
      where
        hexChar = elements $ ['0' .. '9'] ++ ['a' .. 'f']

-- Other Types

instance Arbitrary Time.UTCTime where
    arbitrary = Time.UTCTime <$> dayDen <*> dayTimeGen
      where
        -- The Modified Julian Day is a standard count of days, with zero being the day 1858-11-17.
        dayDen    = Time.ModifiedJulianDay <$> elements [0 .. 60000]
        -- The time from midnight, 0 <= t < 86401s (because of leap-seconds)
        dayTimeGen = Time.secondsToDiffTime <$> elements [0 .. 86400]
