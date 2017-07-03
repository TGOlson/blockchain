module Network.Blockchain.Node.Mining
    ( runMining
    ) where

import qualified Control.Concurrent.Async        as Async
import           Control.Concurrent.STM          (atomically)
import           Control.Concurrent.STM.TVar
import           Data.Blockchain
import           Data.Blockchain.Mining          (mineBlock)
import           Data.Either                     (isRight)
import qualified Data.List.NonEmpty              as NonEmpty
import           Data.Monoid                     ((<>))
import           Data.Time.Clock                 (diffUTCTime, getCurrentTime)
import           Network.Blockchain.Node.Logging (Logger (..))


runMining
    :: Logger
    -> PublicKey
    -> IO (Blockchain Validated)
    -> ((Block, Blockchain Validated) -> IO ())
    -> TVar [Transaction]
    -> Blockchain Validated
    -> IO ()
runMining logger pubKey receiveBlockchain sendBlock transactionPool blockchain = do
    -- Note: additional chain inspection probably adds unnecessary overhead
    -- Add an option to disable this later...
    let mainChain   = NonEmpty.toList $ longestChain blockchain
        chainLength = length mainChain
        config      = blockchainConfig blockchain

    runLogger logger $ "Mining block #" <> show chainLength
    -- TODO: bad interface to find diff. Exported interface should just take blockchain.
    runLogger logger $ "Target difficulty: " <> show (targetDifficulty config mainChain)


    -- TODO: num txs should come from blockchain config
    txs <- pickTransactions numTxs blockchain <$> atomically (readTVar transactionPool)

    receiveAsync <- Async.async receiveBlockchain

    minerAsync   <- Async.async $ do
        start <- getCurrentTime
        block <- mineBlock pubKey txs blockchain >>= \case
            Right block -> return block
            Left e      -> error $ "Unexpected error: mined an invalid block: " <> show e

        end <- getCurrentTime

        let h = hash (blockHeader block)

        runLogger logger "Found next block"
        runLogger logger $ "  Hash: " <> show h
        runLogger logger $ "  Elapsed time: " <> show (diffUTCTime end start)

        return block

    blockchain' <- Async.waitEitherCancel receiveAsync minerAsync >>= \case
        Left blockchain'    -> do
            runLogger logger "Received updated blockchain. Stopping mining on current block and re-broadcasting."
            return blockchain'
        Right block -> case addBlock block blockchain of
            Right blockchain' -> sendBlock (block, blockchain') >> return blockchain'
            Left e            -> error $ "Unexpected error: mined an invalid block: " <> show e

    -- TODO: filter used transactions before mining again
    runMining logger pubKey receiveBlockchain sendBlock transactionPool blockchain'
  where
    numTxs = 10

-- TODO: sort by potential fees
pickTransactions :: Int -> Blockchain Validated -> [Transaction] -> [Transaction]
pickTransactions num blockchain = take num . filter isValidTx
  where
    isValidTx = isRight . validateTransaction blockchain
