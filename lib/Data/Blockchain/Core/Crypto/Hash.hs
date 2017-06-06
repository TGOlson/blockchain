module Data.Blockchain.Core.Crypto.Hash
    ( Hash
    , rawHash
    , ByteStringHash
    , joinHash
    , hashJSON
    , Hashable(..)
    , fromByteString
    , unsafeFromByteString
    , toByteStringHash
    , hashToNatural
    ) where

import qualified Crypto.Hash             as Crypto
import qualified Data.Aeson              as Aeson
import qualified Data.ByteArray.Encoding as Byte
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as Lazy
import qualified Data.Hashable           as H
import qualified Data.Maybe              as Maybe
import qualified Data.Text               as Text
import qualified Data.Text.Encoding      as Text
import qualified Numeric
import qualified Numeric.Natural         as Natural


data Hash a = Hash { rawHash :: Crypto.Digest Crypto.SHA256 }
  deriving (Eq, Ord)

instance H.Hashable (Hash a) where
    hashWithSalt _ = H.hash . show

type ByteStringHash = Hash BS.ByteString

joinHash :: Hash a -> Hash a -> Hash a
joinHash (Hash h1) (Hash h2) = Hash $ Crypto.hashFinalize $ Crypto.hashUpdates Crypto.hashInit [h1, h2]

instance Show (Hash a) where
    show = show . rawHash

toByteStringHash :: Hash a -> ByteStringHash
toByteStringHash = Hash . rawHash

-- Note: ignore all possible invariants that `Numeric.readHex` would normally need to check
-- we are only converting stringified hashes, which should always be valid hex strings
hashToNatural :: Hash a -> Natural.Natural
hashToNatural = fst . head . Numeric.readHex . show . rawHash

hashJSON :: Aeson.ToJSON a => a -> Hash a
hashJSON = Hash . Crypto.hash . Lazy.toStrict . Aeson.encode

fromByteString :: BS.ByteString -> Maybe (Hash a)
fromByteString bs = case Byte.convertFromBase Byte.Base16 bs of
    Left _    -> Nothing
    Right bs' -> Hash <$> Crypto.digestFromByteString (bs' :: BS.ByteString)

unsafeFromByteString :: BS.ByteString -> Hash a
unsafeFromByteString = Maybe.fromMaybe (error "Invalid hash string") . fromByteString

class Hashable a where
    hash :: a -> Hash a

instance Hashable BS.ByteString where
    hash = Hash . Crypto.hash

instance Aeson.ToJSON (Hash a) where
    toJSON = Aeson.String . Text.pack . show . rawHash

instance Aeson.FromJSON (Hash a) where
    parseJSON = Aeson.withText "Hash" $ \txt -> do
        let bs = Text.encodeUtf8 txt
            h = unsafeFromByteString bs -- TODO: unsafe!

        return h
