module Main (main) where

import qualified Data.Aeson                               as Aeson
import qualified Data.ByteString.Lazy                     as Lazy
import qualified Data.List.NonEmpty                       as NonEmpty

import qualified Data.Blockchain.Core.Blockchain          as Blockchain
import qualified Data.Blockchain.Core.Builder.Transaction as Tx
import qualified Data.Blockchain.Core.Crypto              as Crypto
import qualified Data.Blockchain.Core.Types               as Blockchain
import qualified Data.Blockchain.Mining.Block             as Mining

main :: IO ()
main = generateSingletonChain

generateSingletonChain :: IO ()
generateSingletonChain = do
    singleton <- singletonBlockchain

    block1 <- readJSON "data/block_1a.json"
    let blockchain' = throwLeft $ Blockchain.addBlock block1 singleton

    pk <- block1ACoinbasePrivateKey
    let pubKey = coinbasePublicKey block1
        keyPair = Crypto.KeyPair pubKey pk

    (Crypto.KeyPair targetPublicKey privKey) <- Crypto.generate
    tx <- throwLeft <$> Tx.createSimpleTransaction keyPair targetPublicKey 90 0 blockchain'

    block2 <- Mining.mineBlock pubKey (pure tx) blockchain'

    writeJSON "data/block_2a.json" block2
    writeJSON "data/block_2a_coinbase_private_key.json" privKey

coinbasePublicKey :: Blockchain.Block -> Crypto.PublicKey
coinbasePublicKey = Blockchain.signaturePubKey . NonEmpty.head . Blockchain.coinbaseTransactionOut . Blockchain.coinbaseTransaction

block1ACoinbasePrivateKey :: IO Crypto.PrivateKey
block1ACoinbasePrivateKey = readJSON "data/block_1a_coinbase_private_key.json"


singletonBlockchain :: IO (Blockchain.Blockchain Blockchain.Validated)
singletonBlockchain = throwLeft . Blockchain.validate <$> readJSON "data/singleton_blockchain.json"

writeJSON :: Aeson.ToJSON a => FilePath -> a -> IO ()
writeJSON path = Lazy.writeFile path . Aeson.encode

readJSON :: Aeson.FromJSON a => FilePath -> IO a
readJSON path =  throwLeft . Aeson.eitherDecode <$> Lazy.readFile path

throwLeft :: Show a => Either a b -> b
throwLeft = either (error . show) id
