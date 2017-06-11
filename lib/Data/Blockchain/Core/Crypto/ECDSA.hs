module Data.Blockchain.Core.Crypto.ECDSA
    ( KeyPair(..)
    , Signature(..)
    , PublicKey(..)
    , PrivateKey(..)
    , sign
    , verify
    , generate
    ) where

import qualified Crypto.Hash                   as Crypto
import qualified Crypto.PubKey.ECC.ECDSA       as Crypto
import qualified Crypto.PubKey.ECC.Generate    as Crypto
import qualified Crypto.PubKey.ECC.Types       as Crypto
import qualified Data.Aeson                    as Aeson
import qualified Data.ByteString               as BS
import qualified Data.Hashable                 as H
import qualified Data.Text                     as Text

import           Data.Blockchain.Core.Util.Hex

-- Types -----------------------------------------------------------------------------------------------------

data KeyPair = KeyPair
    { publicKey  :: PublicKey
    , privateKey :: PrivateKey
    }

-- Signature

newtype Signature = Signature { unSignature :: Crypto.Signature }
  deriving (Eq)

instance Show Signature where
    show (Signature (Crypto.Signature r s)) = show (hex256FromInteger r) ++ show (hex256FromInteger s)

instance Aeson.ToJSON Signature where
    toJSON = Aeson.toJSON . show

instance Aeson.FromJSON Signature where
    parseJSON = Aeson.withText "Signature" $ \txt -> do
        (rHex, sHex) <- parseHex256Tuple txt

        let sig = Crypto.Signature (toInteger rHex) (toInteger sHex)

        return (Signature sig)

-- PublicKey

newtype PublicKey = PublicKey { unPublicKey :: Crypto.PublicPoint }
  deriving (Eq)

instance H.Hashable PublicKey where
    hashWithSalt _ = H.hash . show

instance Show PublicKey where
    show = \case PublicKey Crypto.PointO      -> error "Unexpected pattern match - PointO" -- TODO: move invariant to type level?
                 PublicKey (Crypto.Point x y) -> show (hex256FromInteger x) ++ show (hex256FromInteger y)

instance Aeson.ToJSON PublicKey where
    toJSON = Aeson.toJSON . show

instance Aeson.FromJSON PublicKey where
    parseJSON = Aeson.withText "PublicKey" $ \txt -> do
        (xHex, yHex) <- parseHex256Tuple txt

        let point = Crypto.Point (toInteger xHex) (toInteger yHex)

        return (PublicKey point)

-- PrivateKey

newtype PrivateKey = PrivateKey { unPrivateKey :: Crypto.PrivateNumber }
  deriving (Eq)

instance Show PrivateKey where
    show = show . hex256FromInteger . unPrivateKey

instance Aeson.ToJSON PrivateKey where
    toJSON = Aeson.toJSON . show

instance Aeson.FromJSON PrivateKey where
    parseJSON = Aeson.withText "PrivateKey" $ fmap (PrivateKey . toInteger) . parseHex256

-- Core functions --------------------------------------------------------------------------------------------

-- Constants

hashType :: Crypto.SHA256
hashType = Crypto.SHA256

curve :: Crypto.Curve
curve = Crypto.getCurveByName Crypto.SEC_p256k1

sign :: PrivateKey -> BS.ByteString -> IO Signature
sign (PrivateKey number) = fmap Signature . Crypto.sign privKey hashType
  where
    privKey = Crypto.PrivateKey curve number

verify :: PublicKey -> Signature -> BS.ByteString -> Bool
verify (PublicKey point) (Signature sig) = Crypto.verify hashType pubKey sig
  where
    pubKey = Crypto.PublicKey curve point

generate :: IO KeyPair
generate = do
    (pub, priv) <- Crypto.generate curve

    let publicPoint   = Crypto.public_q pub
        privateNumber = Crypto.private_d priv

    return $ KeyPair (PublicKey publicPoint) (PrivateKey privateNumber)

-- Util ------------------------------------------------------------------------------------------------------

parseHex256 :: Monad m => Text.Text -> m Hex256
parseHex256 = maybe (fail "Invalid hex 256 string") return . hex256 . Text.unpack

parseHex256Tuple :: Monad m => Text.Text -> m (Hex256, Hex256)
parseHex256Tuple txt = do
    let (xStr, yStr) = Text.splitAt 64 txt

    x <- parseHex256 xStr
    y <- parseHex256 yStr

    return (x, y)

-- unsafe, but used internally on integers we know will always be positive
hex256FromInteger :: Integer -> Hex256
hex256FromInteger = fromInteger
