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
import qualified Numeric

data KeyPair = KeyPair
    { publicKey  :: PublicKey
    , privateKey :: PrivateKey
    }

newtype Signature = Signature { unSignature :: Crypto.Signature }
  deriving (Eq, Show)

instance Aeson.ToJSON Signature where
    toJSON = Aeson.String . Text.pack . show . unSignature

instance Aeson.FromJSON Signature where
    parseJSON = Aeson.withText "Signature" $ \txt -> do
        let str = Text.unpack txt
            sig = read str -- TODO: unsafe!

        return (Signature sig)

newtype PublicKey = PublicKey { unPublicKey :: Crypto.PublicPoint }
  deriving (Eq)

instance H.Hashable PublicKey where
    hashWithSalt _ = H.hash . show . unPublicKey

instance Show PublicKey where
    show = publicKeyToHex

instance Aeson.ToJSON PublicKey where
    toJSON = Aeson.String . Text.pack . publicKeyToHex

instance Aeson.FromJSON PublicKey where
    parseJSON = Aeson.withText "PublicKey" $ \txt -> do
        let str = Text.unpack txt
            xStr = take 64 str
            yStr = drop 64 str
            x = fst $ head (Numeric.readHex xStr) -- TODO: unsafe!
            y = fst $ head (Numeric.readHex yStr) -- TODO: unsafe!
            point = Crypto.Point x y

        return (PublicKey point)

publicKeyToHex :: PublicKey -> String
publicKeyToHex (PublicKey Crypto.PointO)      = error "Unexpected pattern match - PointO" -- TODO: move invariant to type level?
publicKeyToHex (PublicKey (Crypto.Point x y)) = xHex ++ yHex
  where
      xRawHex = Numeric.showHex x ""
      yRawHex = Numeric.showHex y ""
      xHex    = replicate (64 - length xRawHex) '0' ++ xRawHex
      yHex    = replicate (64 - length yRawHex) '0' ++ yRawHex

newtype PrivateKey = PrivateKey { unPrivateKey :: Crypto.PrivateNumber }
  deriving (Eq, Show)

instance Aeson.ToJSON PrivateKey where
    toJSON (PrivateKey number) = Aeson.String $ Text.pack (Numeric.showHex number mempty)

instance Aeson.FromJSON PrivateKey where
    parseJSON = Aeson.withText "PrivateKey" $ \txt -> do
        let str = Text.unpack txt
            num = fst $ head (Numeric.readHex str) -- TODO: unsafe!

        return (PrivateKey num)

hashType :: Crypto.SHA256
hashType = Crypto.SHA256

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

curve :: Crypto.Curve
curve = Crypto.getCurveByName Crypto.SEC_p256k1
