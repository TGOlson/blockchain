module Data.Blockchain.Crypto.Hash
    ( Hash
    , hash
    , hashJSON
    , Hashable(..)
    ) where

import qualified Crypto.Hash           as Crypto
-- Note: using json probably isn't super efficient for all the hashing and data storage we plan on doing
-- consider using a more condensed binary format (cereal?)
import qualified Data.Aeson            as Aeson
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as Lazy
import qualified Data.Text             as Text

newtype Hash = Hash { unHash :: Crypto.Digest Crypto.SHA256 }

hash :: BS.ByteString -> Hash
hash = Hash . Crypto.hash

hashJSON :: Aeson.ToJSON a => a -> Hash
hashJSON = hash . Lazy.toStrict . Aeson.encode

class Hashable a where
    toHash :: a -> Hash

instance Aeson.ToJSON Hash where
    -- This sucks, use a different prelude and drop strings
    toJSON = Aeson.String . Text.pack . show . unHash
