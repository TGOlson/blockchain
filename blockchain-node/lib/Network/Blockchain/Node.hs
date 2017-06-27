module Network.Blockchain.Node
    ( main
    ) where

import qualified Control.Concurrent.Async    as Async
import           Control.Concurrent.STM      (atomically)
import           Control.Concurrent.STM.TVar
import           Control.Monad
import           Control.Monad.Trans         (liftIO)
import           Data.Aeson
import           Data.Blockchain
import qualified Data.ByteString.Lazy        as Lazy
import           Data.Monoid                 ((<>))
import           Data.Proxy                  (Proxy (..))
import qualified Network.Wai.Handler.Warp    as Warp
import           Servant
import           System.Environment          (getArgs)

type NodeAPI
    =    "blockchain" :> Get  '[JSON] (Blockchain Validated)
    -- TODO: get block by hash
    :<|> "block" :> ReqBody '[JSON] Block :> Post '[JSON] ()
    :<|> "transaction" :> ReqBody '[JSON] Transaction :> Post '[JSON] ()
    :<|> "transaction-pool" :> Get '[JSON] [Transaction]

data Args = Args
    { blockchainPath :: FilePath
    , runAsMiner     :: Bool
    , port           :: Int
    }

data NodeConfig = NodeConfig
    { blockchainVar   :: TVar (Blockchain Validated)
    , transactionPool :: TVar [Transaction]
    , logger          :: Logger
    }

newtype Logger = Logger { runLogger :: String -> IO () }

nodeAPI :: Proxy NodeAPI
nodeAPI = Proxy

 -- TODO: opt-parse or something
parseArgs :: IO Args
parseArgs = getArgs >>= \case
    [blockchainPath] -> return Args{..}
    _                -> error "Invalid input args"
  where
    runAsMiner = True
    port       = 8000

main :: IO ()
main = do
    Args{..} <- parseArgs

    let logger = Logger putStrLn
    runLogger logger $ "Running node using blockchain at path: " <> blockchainPath

    bs <- Lazy.readFile blockchainPath

    blockchainUnvalidated <- either
        (\e -> error $ "Error: unable to decode blockchain: " <> show e)
        return (eitherDecode bs)

    blockchain <- either
        (\e -> error $ "Error: invalid blockchain: " <> show e)
        return (validate blockchainUnvalidated)

    blockchainVar   <- newTVarIO blockchain
    transactionPool <- newTVarIO mempty

    serverAsync <- Async.async $ runServer port NodeConfig{..}
    minerAsync  <- Async.async $ runMining undefined

    void (Async.waitAny [serverAsync, minerAsync])
  where
    runServer p = Warp.run p . serve nodeAPI . server
    runMining _x = undefined
        -- TODO: args
        --  chan of new blocks - on new block, stop mining, integrate block, then get back to mining!
        --  pubkey to mine to
        --  what to do when a block is found
        --    probably an output chan, where other side posts to known connected clients

-- TODO: ReaderT
-- TODO: request logging
server :: NodeConfig -> Server NodeAPI
server config = getBlockchain config
           :<|> insertBlock config
           :<|> addTransaction config
           :<|> getTransactionPool config

getBlockchain :: NodeConfig -> Handler (Blockchain Validated)
getBlockchain = liftIO . readTVarIO . blockchainVar

getTransactionPool :: NodeConfig -> Handler [Transaction]
getTransactionPool = liftIO . readTVarIO . transactionPool

insertBlock :: NodeConfig -> Block -> Handler ()
insertBlock config block = do
    blockchain <- getBlockchain config

    case addBlock block blockchain of
        Right blockchain' -> liftIO $ atomically $ writeTVar (blockchainVar config) blockchain' -- TODO: persist, use sqlite
        Left e            -> throwError (addBlockError e)

addTransaction :: NodeConfig -> Transaction -> Handler ()
addTransaction config transaction = do
    blockchain <- getBlockchain config

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
