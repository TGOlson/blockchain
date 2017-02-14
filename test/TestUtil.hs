{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestUtil
    ( module X
    , maxDifficulty
    ) where

import Test.QuickCheck       as X
import Test.Hspec            as X
import Test.Hspec.QuickCheck as X

import qualified Crypto.PubKey.ECC.ECDSA    as Crypto
import qualified Crypto.PubKey.ECC.Generate as Crypto
import qualified Crypto.PubKey.ECC.Types    as Crypto
import qualified Data.ByteString.Char8      as BS
import qualified Data.Maybe                 as Maybe
import qualified Data.Time.Clock            as Time
import qualified Data.Time.Calendar         as Time

import Data.Blockchain.Crypto.ECDSA
import Data.Blockchain.Crypto.Hash
import Data.Blockchain.Types

-- Used in some tests to filter generated difficulties that would be too hard to solve.
maxDifficulty :: Difficulty
maxDifficulty = Difficulty $ unsafefromByteString $ BS.append "0000" $ BS.replicate 60 'f'

unsafefromByteString :: BS.ByteString -> Hash a
unsafefromByteString = Maybe.fromMaybe (error "Invalid hash string") . fromByteString

-- Instances -------------------------------------------------------------------------------------------------

-- Blockchain types

instance Arbitrary Difficulty where
    arbitrary = Difficulty <$> arbitrary

-- Crypto types

instance Arbitrary KeyPair where
    arbitrary = do
        (Positive d) <- arbitrary

        let curve   = Crypto.getCurveByName Crypto.SEC_p256k1
            q       = Crypto.generateQ curve d
            keyPair = Crypto.KeyPair curve q d
            pubKey  = PublicKey $ Crypto.toPublicKey keyPair
            privKey = PrivateKey $ Crypto.toPrivateKey keyPair

        return $ KeyPair pubKey privKey

instance Arbitrary (Hash a) where
    arbitrary = unsafefromByteString . BS.pack <$> vectorOf 64 hexChar
      where
        hexChar = elements $ ['0' .. '9'] ++ ['a' .. 'f']

-- Other Types

instance Arbitrary BS.ByteString where
    arbitrary = BS.pack <$> arbitrary

instance Arbitrary Time.UTCTime where
    arbitrary = Time.UTCTime <$> dayDen <*> dayTimeGen
      where
        -- The Modified Julian Day is a standard count of days, with zero being the day 1858-11-17.
        dayDen    = Time.ModifiedJulianDay <$> elements [0 .. 60000]
        -- The time from midnight, 0 <= t < 86401s (because of leap-seconds)
        dayTimeGen = Time.secondsToDiffTime <$> elements [0 .. 86400]
