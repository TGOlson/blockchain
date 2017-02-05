module Data.Blockchain.Crypto.Hash
    ( Hash
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

newtype Hash = Hash { unHash :: Crypto.Digest Crypto.SHA256 }
  deriving (Eq, Ord)

instance Show Hash where
    show = show . unHash

hash :: BS.ByteString -> Hash
hash = Hash . Crypto.hash

hashJSON :: Aeson.ToJSON a => a -> Hash
hashJSON = hash . Lazy.toStrict . Aeson.encode

fromByteString :: BS.ByteString -> Maybe Hash
fromByteString bs = case Byte.convertFromBase Byte.Base16 bs of
    Left _    -> Nothing
    Right bs' -> Hash <$> Crypto.digestFromByteString bs'

class Hashable a where
    toHash :: a -> Hash

instance Aeson.ToJSON Hash where
    toJSON = Aeson.String . Text.pack . show . unHash
