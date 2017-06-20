module Integration.Mining
    ( mineTestChain
    ) where

import qualified Control.Concurrent.Async          as Async
import qualified Control.Concurrent.MVar           as MVar
import           Control.Monad                     (forM, void)
import qualified Data.Aeson                        as Aeson
import qualified Data.ByteString.Lazy              as Lazy
import qualified Data.List.NonEmpty                as NonEmpty
import           Data.Monoid                       ((<>))
import qualified Data.Time.Clock                   as Time

import qualified Data.Blockchain.Core.Blockchain   as B
import qualified Data.Blockchain.Core.Crypto       as Crypto
import qualified Data.Blockchain.Core.Types        as B
import qualified Data.Blockchain.Mining.Block      as Mining
import qualified Data.Blockchain.Mining.Blockchain as Mining

data MinerSpec = MinerSpec
    { mid               :: Int
    , pubKey            :: Crypto.PublicKey
    , receiveBlock      :: IO B.Block
    , sendBlock         :: B.Block -> IO ()
    , persistBlockchain :: B.Blockchain B.Validated -> IO ()
    }


mineTestChain :: Int -> IO ()
mineTestChain miners = runTestMultiMiner miners config persist
  where
    persist i = writeJSON $ "data/test_blockchain_" <> show i <> ".json"
    config = B.defaultConfig
        { B.initialMiningReward           = 500
        , B.difficultyRecalculationHeight = 10
        , B.miningRewardHalvingHeight     = 20
        }


runTestMultiMiner
    :: Int -> B.BlockchainConfig
    -> (Int -> B.Blockchain B.Validated -> IO ()) -> IO ()
runTestMultiMiner numMiners config persist = do
    putStrLn "Creating blockchain and mining genesis block"
    blockchain <- Mining.createBlockchain config

    let idList = [1..numMiners]

    vars <- mapM (const MVar.newEmptyMVar) idList

    let sends    = MVar.putMVar <$> vars
        receives = MVar.takeMVar <$> (tail vars <> pure (head vars))
        specs    = zip3 sends receives idList

    miners <- forM specs $ \(sendBlock, receiveBlock, mid) -> do
        let persistBlockchain = persist mid
        (Crypto.KeyPair pubKey _privKey) <- Crypto.generate
        return MinerSpec{..}

    asyncs <- mapM (Async.async . runMinerFakeNetwork blockchain) miners
    void (Async.waitAny asyncs)


runMinerFakeNetwork :: B.Blockchain B.Validated -> MinerSpec -> IO ()
runMinerFakeNetwork blockchain spec@(MinerSpec mid _pubKey receive send persist) = do
    miningThread <- Async.async $ do
        block <- runMining spec blockchain
        log' "Found block, broadcasting to other miners"
        send block

        case B.addBlock block blockchain of
            Right blockchain' -> return blockchain'
            Left  e           -> error $ "Mined invalid block: " <> show e

    receiveBlockThread <- Async.async $ do
        block <- receive

        case B.addBlock block blockchain of
            Right blockchain' -> do
                log' "Received valid block. Stopping mining on current block and re-broadcasting."
                send block
                return blockchain'
            Left e -> log' ("Received invalid block - unable to insert block into chain. Reason: " <> show e) >> return blockchain

    blockchain' <- either id id <$> Async.waitEitherCancel miningThread receiveBlockThread

    persist blockchain'
    runMinerFakeNetwork blockchain' spec
  where
    log' s = do
        timestamp <- Time.getCurrentTime
        putStrLn $ pad 30 (show timestamp) <> " [miner_" <> show mid <> "] " <> s

runMining :: MinerSpec -> B.Blockchain B.Validated -> IO B.Block
runMining (MinerSpec mid pubKey _ _ _) chain = do
    let mainChain   = NonEmpty.toList $ B.longestChain chain
        chainLength = length mainChain
        config      = B.blockchainConfig chain

    log' $ "Mining block #" <> show chainLength
    -- TODO: bad interface to find diff. Exported interface should just take blockchain.
    log' $ "Target difficulty: " <> show (B.targetDifficulty config mainChain)

    start <- Time.getCurrentTime
    block <- throwLeft <$> Mining.mineEmptyBlock pubKey chain
    end   <- Time.getCurrentTime

    let h = Crypto.hash (B.blockHeader block)

    log' "Found next block"
    log' $ "  Hash: " <> show h
    log' $ "  Elapsed time: " <> show (Time.diffUTCTime end start)

    return block
  where
    log' s = do
        timestamp <- Time.getCurrentTime
        putStrLn $ pad 30 (show timestamp) <> " [miner_" <> show mid <> "] " <> s

throwLeft :: Show a => Either a b -> b
throwLeft = either (error . show) id

pad :: Int -> String -> String
pad x str = str <> replicate (x - length str) ' '


writeJSON :: Aeson.ToJSON a => FilePath -> a -> IO ()
writeJSON path = Lazy.writeFile path . Aeson.encode
