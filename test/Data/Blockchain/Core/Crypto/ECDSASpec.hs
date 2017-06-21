module Data.Blockchain.Core.Crypto.ECDSASpec (spec) where

import           TestUtil

import           Data.Blockchain.Core.Crypto

spec :: Spec
spec =
    describe "Data.Blockchain.Core.Crypto.ECDSA" $ do
        safeJSONDeserializeSpec (Proxy :: Proxy PublicKey)
        safeJSONDeserializeSpec (Proxy :: Proxy PrivateKey)
        safeJSONDeserializeSpec (Proxy :: Proxy Signature)

        roundTripJSONSpec (Proxy :: Proxy PublicKey)
        roundTripJSONSpec (Proxy :: Proxy PrivateKey)
        roundTripJSONSpec (Proxy :: Proxy Signature)

        propNumTests 5 "should sign and verify correctly" $
            \bs -> ioProperty $ do
                (KeyPair publicKey privateKey) <- generate
                sig <- sign privateKey bs
                return $ verify publicKey sig bs
