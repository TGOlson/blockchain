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

import Data.Blockchain.Crypto.Hash
import Data.Blockchain.Types.Block
import Data.Blockchain.Types.Blockheader
import Data.Blockchain.Types.Difficulty

-- Used in some tests to filter hashes that are too low.
minHash :: Hash
minHash = unsafefromByteString $ BS.append "0000" $ BS.replicate 60 'f'

unsafefromByteString :: BS.ByteString -> Hash
unsafefromByteString = Maybe.fromMaybe (error "Invalid hash string") . fromByteString

-- Instances -------------------------------------------------------------------------------------------------

instance Arbitrary Block where
    arbitrary = newBlock <$> arbitrary <*> arbitrary

instance Arbitrary BlockHash where
    arbitrary = BlockHash <$> arbitrary

instance Arbitrary Difficulty where
    arbitrary = Difficulty <$> arbitrary

instance Arbitrary Hash where
    arbitrary = unsafefromByteString . BS.pack <$> vectorOf 64 hexChar
      where
        hexChar = elements $ ['0' .. '9'] ++ ['a' .. 'f']
