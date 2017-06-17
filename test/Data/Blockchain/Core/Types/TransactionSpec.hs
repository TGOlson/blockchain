module Data.Blockchain.Core.Types.TransactionSpec (spec) where

import           TestUtil

import           Data.Blockchain.Core.Crypto.ECDSA
import           Data.Blockchain.Core.Types.Transaction

spec :: Spec
spec =
    describe "Data.Blockchain.Core.Types.Transaction" $
        propNumTests 5 "should sign round-trip" $
            \val -> ioProperty $ do
                (KeyPair pub priv) <- generate

                let txOut = TransactionOut val pub
                sig <- signTransaction priv txOut

                return (verifyTransactionSignature sig txOut)
