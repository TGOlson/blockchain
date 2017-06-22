module Data.Blockchain.Builder.TransactionSpec (spec) where

import           TestUtil

import qualified Data.List.NonEmpty as NonEmpty

import           Data.Blockchain

blockchainItems :: IO (Blockchain Validated, KeyPair)
blockchainItems = do
    blockchain <- blockchain1Block
    block      <- block1A
    privateKey <- block1ACoinbasePrivateKey

    return (blockchain, KeyPair (coinbasePublicKey block) privateKey)

coinbasePublicKey :: Block -> PublicKey
coinbasePublicKey = signaturePubKey . NonEmpty.head . coinbaseTransactionOut . coinbaseTransaction

spec :: Spec
spec = describe "Data.Blockchain.Builder.Transaction" $
    describe "createSimpleTransaction" $ do
        propNumTests 5 "should create a valid transaction" $
            \(Small value) (Small fee) targetPublicKey -> value + fee < 100 ==> ioProperty $ do
                (blockchain, keyPair) <- blockchainItems

                (Transaction txIn txOut) <- throwLeft <$>
                    createSimpleTransaction keyPair targetPublicKey value fee blockchain

                return $
                    length txIn == 1 &&
                    txOut == NonEmpty.fromList
                        [ TransactionOut (100 - value - fee) (publicKey keyPair)
                        , TransactionOut value targetPublicKey
                        ]

        propNumTests 5 "should not issue a refund if entire balance is spent" $
            \(Small value) targetPublicKey -> value < 100 ==> ioProperty $ do
                (blockchain, keyPair) <- blockchainItems

                let fee = 100 - value

                (Transaction txIn txOut) <- throwLeft <$>
                    createSimpleTransaction keyPair targetPublicKey value fee blockchain

                return $
                    length txIn == 1 &&
                    txOut       == NonEmpty.fromList [ TransactionOut value targetPublicKey ]

        propNumTests 5 "should reject transactions attempting to spend from empty address" $
            \(Small value) (Small fee) publicKey targetPublicKey -> value + fee < 100 ==> ioProperty $ do
                (blockchain, KeyPair _ privKey) <- blockchainItems

                let keyPair = KeyPair publicKey privKey

                res <- createSimpleTransaction keyPair targetPublicKey value fee blockchain
                return $ res === Left SourceAddressEmpty

        propNumTests 5 "should reject transactions attempting to spend more than is available in the address" $
            \(MediumWord value) (MediumWord fee) targetPublicKey -> value + fee > 100 ==> ioProperty $ do
                  (blockchain, keyPair) <- blockchainItems

                  res <- createSimpleTransaction keyPair targetPublicKey value fee blockchain
                  return $ res === Left SourceAddressInsufficientFunds

        propNumTests 5 "should reject transactions with invalid private key" $
            \(Small value) (Small fee) privateKey targetPublicKey -> value + fee < 100 ==> ioProperty $ do
                  (blockchain, KeyPair pubKey _) <- blockchainItems

                  let keyPair = KeyPair pubKey privateKey

                  res <- createSimpleTransaction keyPair targetPublicKey value fee blockchain
                  return $ res === Left InvalidPrivateKey
