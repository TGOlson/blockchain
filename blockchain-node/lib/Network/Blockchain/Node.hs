module Network.Blockchain.Node
    ( main
    ) where

import           Control.Concurrent.STM      (atomically)
import           Control.Concurrent.STM.TVar
import           Control.Monad.Trans
import           Data.Aeson
import           Data.Blockchain
import qualified Data.ByteString.Lazy        as Lazy
import           Data.Monoid                 ((<>))
import           Data.Proxy                  (Proxy (..))
import qualified Network.Wai.Handler.Warp    as Warp
import           Servant
import           System.Environment          (getArgs)

type NodeAPI
    =     "blockchain" :> Get '[JSON] (Blockchain Validated)
    :<|>  "block" :> ReqBody '[JSON] Block :> Post '[JSON] ()

data Config = Config
    { blockchainPath :: FilePath
    , blockchainVar  :: TVar (Blockchain Validated)
    , runLog         :: String -> IO ()
    }

nodeAPI :: Proxy NodeAPI
nodeAPI = Proxy

main :: IO ()
main = getArgs >>= \case -- TODO: opt-parse or something
    [blockchainPath] -> do
        let runLog = putStrLn

        runLog $ "Running node using blockchain at path: " <> blockchainPath

        bs <- Lazy.readFile blockchainPath

        blockchainUnvalidated <- either
            (\e -> error $ "Error: unable to decode blockchain: " <> show e)
            return (eitherDecode bs)

        blockchain <- either
            (\e -> error $ "Error: invalid blockchain: " <> show e)
            return (validate blockchainUnvalidated)

        blockchainVar <- newTVarIO blockchain

        runServer Config{..}
    _ -> error "invalid args"
  where
    runServer = Warp.run 8000 . serve nodeAPI . server

-- TODO: ReaderT
-- TODO: request logging
server :: Config -> Server NodeAPI
server config = getBlockchain config
           :<|> insertBlock config

getBlockchain :: Config -> Handler (Blockchain Validated)
getBlockchain = liftIO . readTVarIO . blockchainVar

insertBlock :: Config -> Block -> Handler ()
insertBlock config block = do
    blockchain <- getBlockchain config

    case addBlock block blockchain of
        Right blockchain' -> liftIO $ atomically $ writeTVar (blockchainVar config) blockchain' -- TODO: persist, use sqlite
        Left e            -> throwError (addBlockError e)

addBlockError :: BlockException -> ServantErr
addBlockError e = err400 { errBody = body }
  where
    body = encode $ object ["error" .= msg ]
    msg  = "Invalid block: " <> show e
