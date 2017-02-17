module Data.Blockchain.Crypto.HashTreeSpec (spec) where

import TestUtil

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8

import Data.Blockchain.Crypto.Hash
import Data.Blockchain.Crypto.HashTree

testData :: BS.ByteString
testData = Char8.replicate numBytes '\000'
  where numBytes = (2 ^ (22 :: Int)) + 32

expectedRootHash :: ByteStringHash
expectedRootHash = unsafefromByteString "e9683665a90bd70aabd7705cba57c2be2a4e913a0ca1a14d765497112c178120"

spec :: Spec
spec =
    describe "HashTree" $
        -- Slow... try to optimize hashing code...
        prop "should create a hash tree for a known hash" $ once $
            rootHash (hashTree testData) === expectedRootHash

        -- TODO: this takes too long... why?
        -- prop "should create a hash tree for a known hash" $
        --     \bs -> (BS.length bs) < 10 ==>
        --         rootHash (hashTree bs) === hash bs
