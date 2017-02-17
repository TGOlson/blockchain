module Data.Blockchain.Crypto.ECDSASpec (spec) where

import TestUtil

import Data.Blockchain.Crypto.ECDSA

spec :: Spec
spec =
    describe "ECDSA" $
        prop "should sign and verify correctly" $ once $
            \bs -> ioProperty $ do
                (KeyPair publicKey privateKey) <- generate
                sig <- sign privateKey bs
                return $ verify publicKey sig bs
