module Data.Blockchain.Core.Builder.TransactionSpec (spec) where

import TestUtil

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.List.NonEmpty   as NonEmpty
import qualified Data.Word            as Word

import Data.Blockchain.Core.Blockchain
import Data.Blockchain.Core.Builder.Transaction
import Data.Blockchain.Core.Crypto
import Data.Blockchain.Core.Types


throwLeft :: Show a => Either a b -> b
throwLeft = either (error . show) id

loadUnverifiedTestBlockchain :: IO UnverifiedBlockchain
loadUnverifiedTestBlockchain = throwLeft . Aeson.eitherDecode <$> Lazy.readFile "data/singleton_chain/blockchain.json"

loadVerifiedTestBlockchainWithValidBlock :: IO (Blockchain, Block)
loadVerifiedTestBlockchainWithValidBlock = do
    blockchain <- throwLeft . verifyBlockchain <$> loadUnverifiedTestBlockchain
    -- TODO: hardcoded path breaks "test chain" pattern... fix it
    block <- throwLeft . Aeson.eitherDecode <$> Lazy.readFile "data/singleton_chain/valid_next_block.json"

    return (blockchain, block)


readJSON :: Aeson.FromJSON a => FilePath -> IO a
readJSON path =  throwLeft . Aeson.eitherDecode <$> Lazy.readFile path

newtype MediumWord = MediumWord Word.Word deriving (Eq, Show)
instance Arbitrary MediumWord where
    arbitrary = elements $ MediumWord <$> [0..1000]

spec :: Spec
spec = describe "Data.Blockchain.Core.Builder.Transaction" $
    describe "createSimpleTransaction" $ do
        propNumTests 5 "should create a valid transaction" $
            \(Small value) (Small fee) targetPublicKey -> value + fee < 100 ==> ioProperty $ do
                (blockchain, block) <- loadVerifiedTestBlockchainWithValidBlock
                privateKey <- readJSON "data/singleton_chain/valid_next_block_coinbase_private_key.json"

                let publicKey   = signaturePubKey $ NonEmpty.head $ coinbaseTransactionOut (coinbaseTransaction block)
                    keyPair     = KeyPair publicKey privateKey
                    blockchain' = throwLeft (addBlock block blockchain)

                tx <- throwLeft <$> createSimpleTransaction keyPair targetPublicKey value fee blockchain'

                -- TODO: better tests
                return $ and [ length (transactionIn tx) == 1
                             , length (transactionOut tx) == 2
                             ]

        propNumTests 5 "should not issue a refund if entire balance is spent" $
            \(Small value) targetPublicKey -> value < 100 ==> ioProperty $ do
                (blockchain, block) <- loadVerifiedTestBlockchainWithValidBlock
                privateKey <- readJSON "data/singleton_chain/valid_next_block_coinbase_private_key.json"

                let publicKey   = signaturePubKey $ NonEmpty.head $ coinbaseTransactionOut (coinbaseTransaction block)
                    keyPair     = KeyPair publicKey privateKey
                    blockchain' = throwLeft (addBlock block blockchain)
                    fee         = 100 - value

                tx <- throwLeft <$> createSimpleTransaction keyPair targetPublicKey value fee blockchain'

                -- TODO: better tests
                return $ and [ length (transactionIn tx) == 1
                             , length (transactionOut tx) == 1
                             ]

        propNumTests 5 "should reject transactions attempting to spend from empty address" $
            \(Small value) (Small fee) publicKey targetPublicKey -> value + fee < 100 ==> ioProperty $ do
                (blockchain, block) <- loadVerifiedTestBlockchainWithValidBlock
                privateKey <- readJSON "data/singleton_chain/valid_next_block_coinbase_private_key.json"

                let keyPair     = KeyPair publicKey privateKey
                    blockchain' = throwLeft (addBlock block blockchain)

                createSimpleTransaction keyPair targetPublicKey value fee blockchain' >>=
                    \res -> return (res === Left SourceAddressEmpty)

        propNumTests 5 "should reject transactions attempting to spend more than is available in the address" $
            \(MediumWord value) (MediumWord fee) targetPublicKey -> value + fee > 100 ==> ioProperty $ do
                  (blockchain, block) <- loadVerifiedTestBlockchainWithValidBlock
                  privateKey <- readJSON "data/singleton_chain/valid_next_block_coinbase_private_key.json"

                  let publicKey   = signaturePubKey $ NonEmpty.head $ coinbaseTransactionOut (coinbaseTransaction block)
                      keyPair     = KeyPair publicKey privateKey
                      blockchain' = throwLeft (addBlock block blockchain)

                  createSimpleTransaction keyPair targetPublicKey value fee blockchain' >>=
                      \res -> return (res === Left SourceAddressInsufficientFunds)

        propNumTests 5 "should reject transactions with invalid private key" $
            \(Small value) (Small fee) privateKey targetPublicKey -> value + fee < 100 ==> ioProperty $ do
                  (blockchain, block) <- loadVerifiedTestBlockchainWithValidBlock

                  let publicKey   = signaturePubKey $ NonEmpty.head $ coinbaseTransactionOut (coinbaseTransaction block)
                      keyPair     = KeyPair publicKey privateKey
                      blockchain' = throwLeft (addBlock block blockchain)

                  createSimpleTransaction keyPair targetPublicKey value fee blockchain' >>=
                      \res -> return (res === Left InvalidPrivateKey)
