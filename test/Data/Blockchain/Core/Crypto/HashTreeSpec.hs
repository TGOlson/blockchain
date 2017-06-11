module Data.Blockchain.Core.Crypto.HashTreeSpec (spec) where

import           TestUtil

import qualified Data.ByteString             as BS
import qualified Data.ByteString.Char8       as Char8

import           Data.Blockchain.Core.Crypto

testData :: [BS.ByteString]
testData = replicate (numBytes `div` blockSize) block ++ [partialBlock]
  where
    blockSize    = 64
    numBytes     = (2 ^ (22 :: Int)) + 32
    block        = Char8.replicate blockSize '\000'
    partialBlock = Char8.replicate (numBytes `mod` blockSize) '\000'

expectedRootHash :: Hash BS.ByteString
expectedRootHash = unsafeFromByteString "e9683665a90bd70aabd7705cba57c2be2a4e913a0ca1a14d765497112c178120"

spec :: Spec
spec =
    describe "Data.Blockchain.Core.Crypto.HashTree" $ do
        it "should create a hash tree for a known hash" $ once $
            unHashTreeRoot (hashTreeRoot testData) === expectedRootHash

        prop "should hash a single item correctly" $
            \(bs :: BS.ByteString) ->
                unHashTreeRoot (hashTreeRoot (pure bs)) === hash bs

        prop "should complete for any data" $
            \(bs :: [BS.ByteString]) -> let !_x = unHashTreeRoot (hashTreeRoot bs) in True
