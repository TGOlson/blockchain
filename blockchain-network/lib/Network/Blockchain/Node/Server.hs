module Network.Blockchain.Node.Server
    ( ServerConfig(..)
    , server
    ) where

import Control.Concurrent.STM
import Control.Monad.Trans             (liftIO)
import Data.Aeson
import Data.Blockchain
import Data.Monoid                     ((<>))
import Network.Blockchain.API          (NodeAPI)
import Network.HTTP.Client
import Servant

import Network.Blockchain.Node.Logging (Logger (..))

data ServerConfig = ServerConfig
    { blockchainVar         :: TVar (Blockchain Validated)
    , transactionPool       :: TVar [Transaction]
    , logger                :: Logger
    , httpManager           :: Manager
    , receiveBlockchainChan :: TChan (Blockchain Validated)
    }


-- TODO: ReaderT
-- TODO: request logging
server :: ServerConfig -> Server NodeAPI
server config = getBlockchain config
           :<|> insertBlock config
           :<|> addTransaction config
           :<|> getTransactionPool config

getBlockchain :: ServerConfig -> Handler (Blockchain Unvalidated)
getBlockchain = liftIO . fmap unvalidate . readTVarIO . blockchainVar

getTransactionPool :: ServerConfig -> Handler [Transaction]
getTransactionPool = liftIO . readTVarIO . transactionPool

insertBlock :: ServerConfig -> Block -> Handler ()
insertBlock config block = do
    blockchain <- liftIO $ readTVarIO (blockchainVar config)

    case addBlock block blockchain of
        Right blockchain' -> liftIO $ do
            runLogger (logger config) "Received valid block - updating blockchain"
            atomically $ do
                -- TODO: Messy cleanup notify channels
                -- TODO: persist, use sqlite
                writeTChan (receiveBlockchainChan config) blockchain'
                writeTVar (blockchainVar config) blockchain'
        -- TODO: block already exists should be logged and no-op'd
        -- TODO: no parent found should be added to block pool to be tried later
        Left e@NoParentFound -> do
            liftIO $ runLogger (logger config) "NoParentFound DEBUG"
            liftIO $ runLogger (logger config) (show block)
            liftIO $ runLogger (logger config) (show blockchain)
            throwError (addBlockError e)
        Left e             -> throwError (addBlockError e)

addTransaction :: ServerConfig -> Transaction -> Handler ()
addTransaction config transaction = do
    blockchain <- liftIO $ readTVarIO (blockchainVar config)

    -- TODO: make sure we don't already have txin in pool
    case validateTransaction blockchain transaction of
        Right ()                       -> addToTransactionPool
        -- `InvalidTransactionValues` and `TransactionOutRefNotFound` could be issues of timing and/or ordering
        -- Add to transaction pool incase new transactions come in or chain flips to a sub-chain.
        -- If the transactions are indeed invalid they will get cleared out later during pool pruning.
        Left InvalidTransactionValues  -> addToTransactionPool
        Left TransactionOutRefNotFound -> addToTransactionPool
        Left e                         -> throwError (addTransactionError e)
  where
    addToTransactionPool = liftIO $ atomically $ modifyTVar' (transactionPool config) (\txs -> transaction:txs)

addBlockError :: BlockException -> ServantErr
addBlockError e = err400 { errBody = body }
  where
    body = encode $ object [ "error" .= msg ]
    msg  = "Invalid block: " <> show e

addTransactionError :: BlockException -> ServantErr
addTransactionError e = err400 { errBody = body }
  where
    body = encode $ object [ "error" .= msg ]
    msg  = "Invalid transaction: " <> show e
