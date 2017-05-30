{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestUtil
    ( module X
    , unsafefromByteString
    ) where

import Test.Hspec            as X
import Test.Hspec.QuickCheck as X
import Test.QuickCheck       as X hiding (generate)

import qualified Crypto.PubKey.ECC.ECDSA    as Crypto
import qualified Crypto.PubKey.ECC.Types    as Crypto
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict   as H
import qualified Data.List.NonEmpty    as NonEmpty
import qualified Data.Maybe            as Maybe
import qualified Data.Time.Clock       as Time
import qualified Data.Time.Calendar    as Time

import Data.Blockchain.Core.Crypto
-- import Data.Blockchain.Crypto.HashTree
import Data.Blockchain.Core.Types

-- Used in some tests to filter generated difficulties that would be too hard to solve.
-- maxDifficulty :: Difficulty
-- maxDifficulty = Difficulty $ unsafefromByteString $ BS.append "0000" $ BS.replicate 60 'f'

unsafefromByteString :: BS.ByteString -> Hash a
unsafefromByteString = Maybe.fromMaybe (error "Invalid hash string") . fromByteString

-- Instances -------------------------------------------------------------------------------------------------

-- Blockchain types

instance Arbitrary BlockchainConfig where
    arbitrary = BlockchainConfig
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> (H.fromList <$> arbitrary)

instance Arbitrary Block where
    arbitrary = variant (442245695 :: Integer) $ Block
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary

instance Arbitrary BlockHeader where
    arbitrary = BlockHeader
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> (hashTreeRoot <$> arbitrary)
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

instance Arbitrary CoinbaseTransaction where
    arbitrary = CoinbaseTransaction <$> arbitrary

instance Arbitrary Transaction where
    arbitrary = resize 5 $ Transaction <$> arbitrary <*> arbitrary

instance Arbitrary TransactionIn where
    arbitrary = resize 5 $ TransactionIn <$> arbitrary <*> arbitrary

instance Arbitrary TransactionOut where
    arbitrary = resize 5 $ TransactionOut <$> arbitrary <*> arbitrary

instance Arbitrary TransactionOutRef where
    arbitrary = TransactionOutRef <$> arbitrary <*> arbitrary

-- TODO: difficulty should never be zero
-- type constructor should enforce that invariant
instance Arbitrary Difficulty where
    arbitrary = Difficulty <$> arbitrary
    -- arbitrary = Difficulty <$> elements [1 .. maxBound]

-- Crypto Types

instance Arbitrary (Hash a) where
    arbitrary = unsafefromByteString . BS.pack <$> vectorOf 64 hexChar
      where
        hexChar = elements $ ['0' .. '9'] ++ ['a' .. 'f']

-- Note: Signature and PublicKey are likely unusable.
instance Arbitrary Signature where
    arbitrary = Signature <$> (Crypto.Signature <$> arbitrary <*> arbitrary)

instance Arbitrary PublicKey where
    arbitrary = PublicKey <$> (Crypto.PublicKey <$> arbitraryCurve <*> arbitraryPoint)
      where
        arbitraryCurve = Crypto.getCurveByName <$> elements [minBound .. maxBound]
        arbitraryPoint = Crypto.Point <$> arbitrary <*> arbitrary

-- Other Types

instance Arbitrary a => Arbitrary (NonEmpty.NonEmpty a) where
    arbitrary = NonEmpty.fromList <$> listOf1 arbitrary

instance Arbitrary BS.ByteString where
    arbitrary = BS.pack <$> arbitrary

instance Arbitrary Time.UTCTime where
    arbitrary = Time.UTCTime <$> dayDen <*> dayTimeGen
      where
        -- The Modified Julian Day is a standard count of days, with zero being the day 1858-11-17.
        dayDen     = Time.ModifiedJulianDay <$> elements [0 .. 60000]
        -- The time from midnight, 0 <= t < 86401s (because of leap-seconds)
        dayTimeGen = Time.secondsToDiffTime <$> elements [0 .. 86400]
