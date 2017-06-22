module Data.Blockchain.Types.TransactionSpec (spec) where

import           TestUtil

import           Data.Blockchain.Crypto
import           Data.Blockchain.Types

spec :: Spec
spec =
    describe "Data.Blockchain.Types.Transaction" $
        propNumTests 5 "should sign round-trip" $
            \val -> ioProperty $ do
                (KeyPair pub priv) <- generate

                let txOut = TransactionOut val pub
                sig <- signTransaction priv txOut

                return (verifyTransactionSignature sig txOut)
