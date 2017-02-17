module Data.Blockchain.Crypto.Hash
    ( Hash
    , rawHash
    , ByteStringHash
    , joinHash
    , hash
    , hashJSON
    , Hashable(..)
    , fromByteString
    ) where

import qualified Crypto.Hash             as Crypto
import qualified Data.Aeson              as Aeson
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as Lazy
import qualified Data.Text               as Text
import qualified Data.ByteArray.Encoding as Byte


data Hash a = Hash { rawHash :: Crypto.Digest Crypto.SHA256 }
  deriving (Eq, Ord)

type ByteStringHash = Hash BS.ByteString

joinHash :: Hash a -> Hash a -> Hash a
joinHash (Hash h1) (Hash h2) = Hash $ Crypto.hashFinalize $ Crypto.hashUpdates Crypto.hashInit [h1, h2]

instance Show (Hash a) where
    show = show . rawHash

hash :: BS.ByteString -> Hash a
hash = Hash . Crypto.hash

hashJSON :: Aeson.ToJSON a => a -> Hash a
hashJSON = hash . Lazy.toStrict . Aeson.encode

fromByteString :: BS.ByteString -> Maybe (Hash a)
fromByteString bs = case Byte.convertFromBase Byte.Base16 bs of
    Left _    -> Nothing
    Right bs' -> Hash <$> Crypto.digestFromByteString (bs' :: BS.ByteString)

class Hashable a where
    toHash :: a -> Hash a

instance Aeson.ToJSON (Hash a) where
    toJSON = Aeson.String . Text.pack . show . rawHash
