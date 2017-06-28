module Network.Blockchain.Node
    ( runNode
    ) where

import qualified Control.Concurrent.Async        as Async
import           Control.Concurrent.STM          (atomically)
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import           Control.Monad
import           Control.Monad.Trans             (liftIO)
import           Data.Aeson
import           Data.Blockchain
import qualified Data.ByteString.Lazy            as Lazy
import           Data.Either.Combinators         (fromRight, mapLeft)
import           Data.Maybe                      (fromMaybe)
import           Data.Monoid                     ((<>))
import           Data.Proxy                      (Proxy (..))
import           Data.Time.Clock                 (getCurrentTime)
import           Network.Blockchain.API          (NodeAPI)
import qualified Network.Wai.Handler.Warp        as Warp
import           Options.Generic
import           Servant
import           Servant.Client

import           Network.Blockchain.Node.Logging
import           Network.Blockchain.Node.Mining
import           Network.Blockchain.Node.Server

data RawArgs w = RawArgs
    { path   :: w ::: FilePath        <?> "Path where the blockchain is stored locally"
    , pubKey :: w ::: Lazy.ByteString <?> "Public key to use when mining- TODO: make optional to disable mining"
    , port   :: w ::: Maybe Warp.Port <?> "Port to run node - defaults to 8000"
    , client :: w ::: [String]        <?> "List of network clients to send and receive updates"
    }
  deriving (Generic)

instance ParseRecord (RawArgs Wrapped)
deriving instance Show (RawArgs Unwrapped)

data ParsedArgs = ParsedArgs
    { blockchainPath :: FilePath
    , miningPubKey   :: PublicKey
    , nodePort       :: Warp.Port
    , networkClients :: [BaseUrl]
    }
  deriving (Show)

parseArgs :: RawArgs Unwrapped -> Either String ParsedArgs
parseArgs RawArgs{..} = do
    let blockchainPath = path
        nodePort       = fromMaybe defaultPort port

    miningPubKey   <- head <$> eitherDecode ("[\"" <> pubKey <> "\"]")
    networkClients <- maybeToEither "Unable to parse client list" $ mapM parseBaseUrl client

    return ParsedArgs{..}
  where
    defaultPort = 8000

runNode :: RawArgs Unwrapped -> IO ()
runNode rawArgs = do
    let logger = stdOutLogger

    runLogger logger $ "Raw input args\n\t" <> show rawArgs
    let !args@ParsedArgs{..} = throwLeftTagged "Error parsing input args\n\t" $ parseArgs rawArgs

    runLogger logger $ "Running node using parsed args\n\t" <> show args

    blockchain      <- throwLeftTagged "Error loading blockchain\n\t" <$> loadBlockchain blockchainPath
    blockchainVar   <- newTVarIO blockchain
    transactionPool <- newTVarIO mempty

    receiveBlockchainChan <- newTChanIO
    sendBlockChan         <- newTChanIO

    let receiveBlockchain = atomically $ readTChan receiveBlockchainChan
        sendBlock         = atomically . writeTChan sendBlockChan

    -- web server thread
    serverAsync <- Async.async $ runServer nodePort ServerConfig{..}

    -- mining thread
    minerAsync <- Async.async $ runMining logger miningPubKey receiveBlockchain sendBlock transactionPool blockchain

    -- miner updates thread
    minerUpdatesAsync <- Async.async $ forever $ do
        -- TODO: broadcast blocks to other clients
        (_block, blockchain') <- atomically $ readTChan sendBlockChan
        atomically $ writeTVar blockchainVar blockchain'

    -- TODO: network updates thread

    void (Async.waitAny [serverAsync, minerAsync, minerUpdatesAsync])


runServer :: Warp.Port -> ServerConfig -> IO ()
runServer p = Warp.run p . serve nodeAPI . server
  where
    nodeAPI = Proxy :: Proxy NodeAPI

loadBlockchain :: FilePath -> IO (Either String (Blockchain Validated))
loadBlockchain = fmap (eitherDecode' >=> mapLeft show . validate) . Lazy.readFile


throwLeft :: Either String b -> b
throwLeft = either error id

throwLeftTagged :: String -> Either String b -> b
throwLeftTagged tag = either (\e -> error $ tag <> e) id

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither x = maybe (Left x) Right
