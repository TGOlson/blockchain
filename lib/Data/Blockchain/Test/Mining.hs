module Data.Blockchain.Test.Mining
    ( runTestMiner
    ) where

import qualified Data.List.NonEmpty                as NonEmpty
import           Data.Monoid                       ((<>))
import qualified Data.Time.Clock                   as Time

import qualified Data.Blockchain.Core.Blockchain   as B
import qualified Data.Blockchain.Core.Crypto       as Crypto
import qualified Data.Blockchain.Core.Types        as B
import qualified Data.Blockchain.Mining.Block      as Mining
import qualified Data.Blockchain.Mining.Blockchain as Mining

runTestMiner
    :: Int -> B.BlockchainConfig -> (B.Blockchain B.Validated -> IO ())
    -> IO (B.Blockchain B.Validated)
runTestMiner iterations config persist = do
    (Crypto.KeyPair pub priv) <- Crypto.generate

    putStrLn "Creating blockchain and mining genesis block"
    blockchain <- Mining.createBlockchain config

    putStrLn "Mining using keypair: "
    putStrLn $ "  PublicKey: "  <> show pub
    putStrLn $ "  PrivateKey: " <> show priv

    loop 0 pub blockchain
  where
    loop i pub chain =
        if i < iterations
            then runMining pub chain >>= \c' -> persist c' >> loop (succ i) pub c'
            else return chain


runMining :: Crypto.PublicKey -> B.Blockchain B.Validated -> IO (B.Blockchain B.Validated)
runMining pubKey chain = do
    let mainChain   = NonEmpty.toList $ B.longestChain chain
        chainLength = length mainChain
        config      = B.blockchainConfig chain

    putStrLn $ "Mining block #" <> show chainLength
    -- TODO: bad interface to find diff. Exported interface should just take blockchain.
    putStrLn $ "Target difficulty: " <> show (B.targetDifficulty config mainChain)

    start <- Time.getCurrentTime
    block <- throwLeft <$> Mining.mineEmptyBlock pubKey chain
    end   <- Time.getCurrentTime

    let h = Crypto.hash (B.blockHeader block)

    putStrLn "Found next block"
    putStrLn $ "  Hash: " <> show h
    putStrLn $ "  Elapsed time: " <> show (Time.diffUTCTime end start)

    either (\e -> error $ "Unexpected error while mining: " <> show e) return $ B.addBlock block chain

throwLeft :: Show a => Either a b -> b
throwLeft = either (error . show) id
