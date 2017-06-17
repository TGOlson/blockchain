module Main (main) where

import           Control.Monad                     (void)
import qualified Data.Aeson                        as Aeson
import qualified Data.ByteString.Lazy              as Lazy
-- import qualified Data.List.NonEmpty                as NonEmpty
import           Data.Monoid                       ((<>))
import qualified Data.Time.Clock                   as Time

import qualified Data.Blockchain.Core.Blockchain   as Blockchain
-- import qualified Data.Blockchain.Core.Builder.Transaction as Tx
import qualified Data.Blockchain.Core.Crypto       as Crypto
import qualified Data.Blockchain.Core.Types        as Blockchain
import qualified Data.Blockchain.Mining.Block      as Mining
import qualified Data.Blockchain.Mining.Blockchain as Mining

main :: IO ()
main = do
    (Crypto.KeyPair pub priv) <- Crypto.generate
    writeJSON "data/test_blockchain_priv_key.json" priv

    putStrLn "Creating blockchain with block 0"
    blockchain <- Mining.createBlockchain config

    void $ runMining pub blockchain
  where
    config = Blockchain.defaultConfig
        { Blockchain.initialMiningReward           = 100000
        , Blockchain.difficultyRecalculationHeight = 10
        , Blockchain.miningRewardHalvingHeight     = 20
        }

runMining :: Crypto.PublicKey -> Blockchain.Blockchain Blockchain.Validated -> IO (Blockchain.Blockchain Blockchain.Validated)
runMining pubKey chain = do
    let chainLength = length $ Blockchain.longestChain chain

    putStrLn $ "Mining block #" <> show chainLength

    start <- Time.getCurrentTime
    block <- throwLeft <$> Mining.mineEmptyBlock pubKey chain
    end   <- Time.getCurrentTime

    let h = Crypto.hash (Blockchain.blockHeader block)

    putStrLn "Found next block"
    putStrLn $ "  Hash: " <> show h
    putStrLn $ "  Elapsed time: " <> show (Time.diffUTCTime end start)

    case Blockchain.addBlock block chain of
        Right chain' -> writeJSON "data/test_blockchain.json" chain' >> runMining pubKey chain'
        Left e       -> error $ "Unexpected error while mining: " <> show e




-- generateSingletonChain :: IO ()
-- generateSingletonChain = do
--     singleton <- singletonBlockchain
--
--     block1 <- readJSON "data/block_1a.json"
--     let blockchain' = throwLeft $ Blockchain.addBlock block1 singleton
--
--     pk <- block1ACoinbasePrivateKey
--     let pubKey = coinbasePublicKey block1
--         keyPair = Crypto.KeyPair pubKey pk
--
--     (Crypto.KeyPair targetPublicKey privKey) <- Crypto.generate
--     tx <- throwLeft <$> Tx.createSimpleTransaction keyPair targetPublicKey 90 0 blockchain'
--
--     block2 <- throwLeft <$> Mining.mineBlock pubKey (pure tx) blockchain'
--
--     writeJSON "data/block_2a.json" block2
--     writeJSON "data/block_2a_coinbase_private_key.json" privKey

-- coinbasePublicKey :: Blockchain.Block -> Crypto.PublicKey
-- coinbasePublicKey = Blockchain.signaturePubKey . NonEmpty.head . Blockchain.coinbaseTransactionOut . Blockchain.coinbaseTransaction
--
-- block1ACoinbasePrivateKey :: IO Crypto.PrivateKey
-- block1ACoinbasePrivateKey = readJSON "data/block_1a_coinbase_private_key.json"
--
--
-- singletonBlockchain :: IO (Blockchain.Blockchain Blockchain.Validated)
-- singletonBlockchain = throwLeft . Blockchain.validate <$> readJSON "data/singleton_blockchain.json"

writeJSON :: Aeson.ToJSON a => FilePath -> a -> IO ()
writeJSON path = Lazy.writeFile path . Aeson.encode

-- readJSON :: Aeson.FromJSON a => FilePath -> IO a
-- readJSON path =  throwLeft . Aeson.eitherDecode <$> Lazy.readFile path

throwLeft :: Show a => Either a b -> b
throwLeft = either (error . show) id
