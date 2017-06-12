module Data.Blockchain.Core.Crypto.ECDSASpec (spec) where

import           TestUtil

import           Data.Blockchain.Core.Crypto.ECDSA

spec :: Spec
spec =
    describe "Data.Blockchain.Core.Crypto.ECDSA" $ do
        safeJSONDeserializeSpec (Proxy :: Proxy PublicKey)
        safeJSONDeserializeSpec (Proxy :: Proxy PrivateKey)
        safeJSONDeserializeSpec (Proxy :: Proxy Signature)

        roundTripJSONSpec (Proxy :: Proxy PublicKey)
        roundTripJSONSpec (Proxy :: Proxy PrivateKey)
        roundTripJSONSpec (Proxy :: Proxy Signature)

        it "should sign and verify correctly" $ once $
            \bs -> ioProperty $ do
                (KeyPair publicKey privateKey) <- generate
                sig <- sign privateKey bs
                return $ verify publicKey sig bs
