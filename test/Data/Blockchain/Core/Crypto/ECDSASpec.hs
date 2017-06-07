module Data.Blockchain.Core.Crypto.ECDSASpec (spec) where

import TestUtil

import qualified Data.Aeson as Aeson

import Data.Blockchain.Core.Crypto.ECDSA

spec :: Spec
spec =
    describe "Data.Blockchain.Core.Crypto.ECDSA" $ do
        prop "should round trip public key" $
            \(publicKey :: PublicKey) -> Aeson.decode (Aeson.encode publicKey) === Just publicKey

        prop "should round trip private key" $
            \(privateKey :: PrivateKey) -> Aeson.decode (Aeson.encode privateKey) === Just privateKey

        it "should sign and verify correctly" $ once $
            \bs -> ioProperty $ do
                (KeyPair publicKey privateKey) <- generate
                sig <- sign privateKey bs
                return $ verify publicKey sig bs
