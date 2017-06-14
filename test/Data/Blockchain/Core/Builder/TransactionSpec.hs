module Data.Blockchain.Core.Builder.TransactionSpec (spec) where

import           TestUtil

import qualified Data.List.NonEmpty                       as NonEmpty

import           Data.Blockchain.Core.Blockchain
import           Data.Blockchain.Core.Builder.Transaction
import           Data.Blockchain.Core.Crypto
import           Data.Blockchain.Core.Types

throwLeft :: Show a => Either a b -> b
throwLeft = either (error . show) id

singletonBlockchainItems :: IO (Blockchain Validated, Block, PrivateKey)
singletonBlockchainItems = do
    blockchain <- singletonBlockchain
    block      <- block1A
    privateKey <- block1ACoinbasePrivateKey

    return (blockchain, block, privateKey)

-- TODO: lenses over blockchain?
coinbasePublicKey :: Block -> PublicKey
coinbasePublicKey = signaturePubKey . NonEmpty.head . coinbaseTransactionOut . coinbaseTransaction

spec :: Spec
spec = describe "Data.Blockchain.Core.Builder.Transaction" $
    describe "createSimpleTransaction" $ do
        propNumTests 5 "should create a valid transaction" $
            \(Small value) (Small fee) targetPublicKey -> value + fee < 100 ==> ioProperty $ do
                (blockchain, block, privateKey) <- singletonBlockchainItems

                let keyPair     = KeyPair (coinbasePublicKey block) privateKey
                    blockchain' = throwLeft (addBlock block blockchain)

                (Transaction txIn txOut) <- throwLeft <$>
                    createSimpleTransaction keyPair targetPublicKey value fee blockchain'

                return $
                    length txIn == 1 &&
                    txOut == NonEmpty.fromList
                        [ TransactionOut (100 - value - fee) (coinbasePublicKey block)
                        , TransactionOut value targetPublicKey
                        ]

        propNumTests 5 "should not issue a refund if entire balance is spent" $
            \(Small value) targetPublicKey -> value < 100 ==> ioProperty $ do
                (blockchain, block, privateKey) <- singletonBlockchainItems

                let keyPair     = KeyPair (coinbasePublicKey block) privateKey
                    blockchain' = throwLeft (addBlock block blockchain)
                    fee         = 100 - value

                (Transaction txIn txOut) <- throwLeft <$>
                    createSimpleTransaction keyPair targetPublicKey value fee blockchain'

                return $
                    length txIn == 1 &&
                    txOut       == NonEmpty.fromList [ TransactionOut value targetPublicKey ]

        propNumTests 5 "should reject transactions attempting to spend from empty address" $
            \(Small value) (Small fee) publicKey targetPublicKey -> value + fee < 100 ==> ioProperty $ do
                (blockchain, block, privateKey) <- singletonBlockchainItems

                let keyPair     = KeyPair publicKey privateKey
                    blockchain' = throwLeft (addBlock block blockchain)

                res <- createSimpleTransaction keyPair targetPublicKey value fee blockchain'
                return $ res === Left SourceAddressEmpty

        propNumTests 5 "should reject transactions attempting to spend more than is available in the address" $
            \(MediumWord value) (MediumWord fee) targetPublicKey -> value + fee > 100 ==> ioProperty $ do
                  (blockchain, block, privateKey) <- singletonBlockchainItems

                  let keyPair     = KeyPair (coinbasePublicKey block) privateKey
                      blockchain' = throwLeft (addBlock block blockchain)

                  res <- createSimpleTransaction keyPair targetPublicKey value fee blockchain'
                  return $ res === Left SourceAddressInsufficientFunds

        propNumTests 5 "should reject transactions with invalid private key" $
            \(Small value) (Small fee) privateKey targetPublicKey -> value + fee < 100 ==> ioProperty $ do
                  blockchain <- singletonBlockchain
                  block      <- block1A

                  let keyPair     = KeyPair (coinbasePublicKey block) privateKey
                      blockchain' = throwLeft (addBlock block blockchain)

                  res <- createSimpleTransaction keyPair targetPublicKey value fee blockchain'
                  return $ res === Left InvalidPrivateKey
