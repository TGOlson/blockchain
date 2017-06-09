module Data.Blockchain.Core.Crypto.ECDSASpec (spec) where

import TestUtil

import qualified Data.Aeson as Aeson

import Control.DeepSeq
import Data.Blockchain.Core.Crypto.ECDSA

import qualified Data.ByteString as BS


testDeserialize :: (Aeson.FromJSON a, NFData b) => (a -> b) -> BS.ByteString -> Bool
testDeserialize unwrap bs = case Aeson.decodeStrict' json of
    Just [x] -> canEval (unwrap x)
    _        -> True
  where
    json = mconcat [ "[\"", bs, "\"]" ]

canEval :: NFData a => a -> Bool
canEval x = deepseq x True


spec :: Spec
spec =
    describe "Data.Blockchain.Core.Crypto.ECDSA" $ do
        prop "should round-trip json serialize public key" $
            \(publicKey :: PublicKey) -> Aeson.decode (Aeson.encode publicKey) === Just publicKey

        prop "should be safe when deserializing public key" $ testDeserialize unPublicKey

        prop "should round-trip json serialize private key" $
            \(privateKey :: PrivateKey) -> Aeson.decode (Aeson.encode privateKey) === Just privateKey

        prop "should be safe when deserializing private key" $ testDeserialize unPrivateKey

        prop "should round-trip json serialize signature" $
            \(signature :: Signature) -> Aeson.decode (Aeson.encode signature) === Just signature

        prop "should be safe when deserializing signature" $ testDeserialize (show . unSignature)

        it "should sign and verify correctly" $ once $
            \bs -> ioProperty $ do
                (KeyPair publicKey privateKey) <- generate
                sig <- sign privateKey bs
                return $ verify publicKey sig bs
