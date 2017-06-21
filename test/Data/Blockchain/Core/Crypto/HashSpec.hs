module Data.Blockchain.Core.Crypto.HashSpec (spec) where

import           TestUtil

import qualified Data.Aeson                  as Aeson
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Char8       as Char8

import           Data.Blockchain.Core.Crypto

spec :: Spec
spec =
    describe "Data.Blockchain.Core.Crypto.Hash" $ do
        prop "should hash to a hex" $
            \(bs :: BS.ByteString) -> hashToHex (hash bs) >= minBound

        prop "should round-trip json serialize" $
            \(bs :: BS.ByteString) -> let h = hash bs in
                Aeson.decode (Aeson.encode h) == Just h

        prop "should round-trip bytestring serialize" $
            \(bs :: BS.ByteString) -> let h = hash bs in
                fromByteString (Char8.pack $ show h) == Just h
