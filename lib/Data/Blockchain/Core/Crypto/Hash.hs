module Data.Blockchain.Core.Crypto.Hash
    ( Hash
    , ToHash(..)
    , hashToHex
    , fromByteString
    , unsafeFromByteString
    ) where

import qualified Crypto.Hash                   as Crypto
import qualified Data.Aeson                    as Aeson
import qualified Data.ByteArray.Encoding       as Byte
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as Lazy
import qualified Data.Hashable                 as H
import qualified Data.Maybe                    as Maybe
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text

import qualified Data.Blockchain.Core.Util.Hex as Hex

-- Types -----------------------------------------------------------------------------------------------------

data Hash a = Hash { unHash :: Crypto.Digest Crypto.SHA256 }
  deriving (Eq, Ord)

instance H.Hashable (Hash a) where
    hashWithSalt _ = H.hash . show

instance Show (Hash a) where
    show = show . unHash

instance Aeson.ToJSON (Hash a) where
    toJSON = Aeson.String . Text.pack . show . unHash

instance Aeson.FromJSON (Hash a) where
    parseJSON = Aeson.withText "Hash" $
        maybe (fail "Invalid bytestring hash") return . fromByteString . Text.encodeUtf8

instance Monoid (Hash a) where
    mempty = Hash $ Crypto.hash (mempty :: BS.ByteString)
    mappend (Hash h1) (Hash h2) = Hash $ Crypto.hashFinalize $ Crypto.hashUpdates Crypto.hashInit [h1, h2]

class ToHash a where
    hash :: a -> Hash a

    default hash :: Aeson.ToJSON a => a -> Hash a
    hash = Hash . Crypto.hash . Lazy.toStrict . Aeson.encode

instance ToHash BS.ByteString where
    hash = Hash . Crypto.hash

-- Utils -----------------------------------------------------------------------------------------------------

-- Note: ignore all possible invariants that `Numeric.readHex` would normally need to check.
-- We are only converting string-ified hashes, which should always be valid hex strings.
hashToHex :: Hash a -> Hex.Hex256
hashToHex = Maybe.fromMaybe (error "Unexpected hex conversion failure") . Hex.hex256 . show . unHash

fromByteString :: BS.ByteString -> Maybe (Hash a)
fromByteString bs = case Byte.convertFromBase Byte.Base16 bs of
    Left _    -> Nothing
    Right bs' -> Hash <$> Crypto.digestFromByteString (bs' :: BS.ByteString)

unsafeFromByteString :: BS.ByteString -> Hash a
unsafeFromByteString = Maybe.fromMaybe (error "Invalid hash string") . fromByteString
