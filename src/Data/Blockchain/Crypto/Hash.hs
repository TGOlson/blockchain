module Data.Blockchain.Crypto.Hash
    ( Hash
    , rawHash
    , ByteStringHash
    , hash
    , hashJSON
    , Hashable(..)
    , fromByteString
    ) where

import qualified Crypto.Hash           as Crypto
-- Note: using json probably isn't super efficient for all the hashing and data storage we plan on doing
-- consider using a more condensed binary format (cereal?)
import qualified Data.Aeson            as Aeson
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as Lazy
import qualified Data.Text             as Text
import qualified Data.ByteArray.Encoding as Byte


data Hash a = Hash { rawHash :: Crypto.Digest Crypto.SHA256 }
  deriving (Eq, Ord)

type ByteStringHash = Hash BS.ByteString

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
