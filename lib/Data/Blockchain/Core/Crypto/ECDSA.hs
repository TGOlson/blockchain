module Data.Blockchain.Core.Crypto.ECDSA
    ( KeyPair(..)
    , Signature(..)
    , PublicKey(..)
    , PrivateKey(..)
    , sign
    , verify
    , generate
    ) where

import qualified Crypto.Hash                as Crypto
import qualified Crypto.PubKey.ECC.ECDSA    as Crypto
import qualified Crypto.PubKey.ECC.Generate as Crypto
import qualified Crypto.PubKey.ECC.Types    as Crypto
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString            as BS
import qualified Data.Hashable              as H
import qualified Data.Text                  as Text

data KeyPair = KeyPair
    { publicKey  :: PublicKey
    , privateKey :: PrivateKey
    }
  deriving (Show)

newtype Signature = Signature { unSignature :: Crypto.Signature }
  deriving (Eq, Show)

instance Aeson.ToJSON Signature where
    toJSON = Aeson.String . Text.pack . show . unSignature

instance Aeson.FromJSON Signature where
    parseJSON = Aeson.withText "Signature" $ \txt -> do
        let str = Text.unpack txt
            sig = read str -- TODO: unsafe!

        return (Signature sig)

newtype PublicKey = PublicKey { unPublicKey :: Crypto.PublicKey }
  deriving (Eq, Show)

instance H.Hashable PublicKey where
    hashWithSalt _ = H.hash . show . unPublicKey

instance Aeson.ToJSON PublicKey where
    toJSON = Aeson.String . Text.pack . show . unPublicKey

instance Aeson.FromJSON PublicKey where
    parseJSON = Aeson.withText "PublicKey" $ \txt -> do
        let str = Text.unpack txt
            pubKey = read str -- TODO: unsafe!

        return (PublicKey pubKey)


newtype PrivateKey = PrivateKey { _unPrivateKey :: Crypto.PrivateKey }

instance Show PrivateKey where
    show = show . Crypto.private_d . _unPrivateKey

hashType :: Crypto.SHA256
hashType = Crypto.SHA256

sign :: PrivateKey -> BS.ByteString -> IO Signature
sign (PrivateKey privKey) = fmap Signature . Crypto.sign privKey hashType

verify :: PublicKey -> Signature -> BS.ByteString -> Bool
verify (PublicKey pubKey) (Signature sig) = Crypto.verify hashType pubKey sig

generate :: IO KeyPair
generate = do
    (pub, priv) <- Crypto.generate curve
    return $ KeyPair (PublicKey pub) (PrivateKey priv)
  where
    curve = Crypto.getCurveByName Crypto.SEC_p256k1
