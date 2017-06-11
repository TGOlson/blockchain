{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Blockchain.ArbitraryInstances () where

import Test.QuickCheck

import qualified Crypto.PubKey.ECC.ECDSA as Crypto
import qualified Crypto.PubKey.ECC.Types as Crypto
import qualified Data.ByteString.Char8   as BS
import qualified Data.List.NonEmpty      as NonEmpty
import qualified Data.Time.Calendar      as Time
import qualified Data.Time.Clock         as Time
import qualified Data.LargeWord          as Word

import Data.Blockchain.Core.Crypto
import Data.Blockchain.Core.Types
import Data.Blockchain.Core.Util.Hex

-- Blockchain types

instance Arbitrary BlockchainConfig where
    arbitrary = BlockchainConfig
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

instance Arbitrary Block where
    arbitrary = Block
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

instance Arbitrary Difficulty where
    arbitrary = Difficulty <$> arbitraryWord256

-- Crypto Types

instance Arbitrary (Hash a) where
    arbitrary = unsafeFromByteString . BS.pack <$> vectorOf 64 hexChar
      where
        hexChar = elements $ ['0' .. '9'] ++ ['a' .. 'f']

instance Arbitrary Signature where
    arbitrary = Signature <$> arbitrarySig
      where
        arbitrarySig = Crypto.Signature
            <$> arbitraryPositive
            <*> arbitraryPositive

instance Arbitrary PublicKey where
    arbitrary = PublicKey <$> arbitraryPoint
      where
        arbitraryPoint = Crypto.Point
            <$> arbitraryPositive
            <*> arbitraryPositive

instance Arbitrary PrivateKey where
    arbitrary = PrivateKey <$> arbitraryPositive

-- Other Types

instance Arbitrary Hex256 where
    arbitrary = Hex256 <$> arbitraryWord256

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

-- Utils

arbitraryWord256 :: Gen Word.Word256
arbitraryWord256 = fromInteger <$> arbitrary

arbitraryPositive :: (Num a, Ord a, Arbitrary a) => Gen a
arbitraryPositive = getPositive <$> arbitrary
