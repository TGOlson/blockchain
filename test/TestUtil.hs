{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestUtil
    ( module X
    , propWithSize
    , propNumTests
    , unsafefromByteString
    ) where

import Test.Hspec               as X
import Test.Hspec.QuickCheck    as X
import Test.QuickCheck          as X hiding (Result, labels, reason, theException, generate)
import Test.QuickCheck.Property as X

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

-- Utils -----------------------------------------------------------------------------------------------------

propWithSize :: Testable prop => Int -> String -> prop -> Spec
propWithSize n tag = modifyMaxSize (const n) . prop tag

propNumTests :: Testable prop => Int -> String -> prop -> Spec
propNumTests n tag = modifyMaxSuccess (const n) . prop tag

unsafefromByteString :: BS.ByteString -> Hash a
unsafefromByteString = Maybe.fromMaybe (error "Invalid hash string") . fromByteString

arbitraryPositive :: (Num a, Ord a, Arbitrary a) => Gen a
arbitraryPositive = getPositive <$> arbitrary

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
