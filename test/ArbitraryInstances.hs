{-# OPTIONS_GHC -fno-warn-orphans #-}

module ArbitraryInstances
    ( MediumWord(..)
    ) where

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import qualified Crypto.PubKey.ECC.ECDSA   as Crypto
import qualified Crypto.PubKey.ECC.Types   as Crypto
import qualified Data.ByteString.Char8     as BS
import           Data.Monoid               ((<>))
import qualified Data.Word                 as Word

import           Data.Blockchain

-- Blockchain types

instance Arbitrary Config where
    arbitrary = Config
        <$> arbitrary
        <*> arbitrary
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
    arbitrary = Difficulty <$> arbitrary

-- Crypto Types

instance Arbitrary (Hash a) where
    arbitrary = unsafeFromByteString . BS.pack <$> vectorOf 64 hexChar
      where
        hexChar = elements $ ['0' .. '9'] <> ['a' .. 'f']

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
    arbitrary = Hex256 <$> arbitrary

newtype MediumWord = MediumWord Word.Word deriving (Eq, Show)
instance Arbitrary MediumWord where
    arbitrary = elements $ MediumWord <$> [0..1000]

-- Utils

arbitraryPositive :: (Num a, Ord a, Arbitrary a) => Gen a
arbitraryPositive = getPositive <$> arbitrary
