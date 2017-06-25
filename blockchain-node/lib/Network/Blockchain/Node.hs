module Network.Blockchain.Node
    ( main
    ) where

import           Control.Concurrent.STM.TVar (TVar)
import qualified Control.Concurrent.STM.TVar as TVar
import           Control.Monad.Trans
import qualified Data.Aeson                  as Aeson
import           Data.Blockchain
import qualified Data.ByteString.Lazy        as Lazy
import           Data.Monoid                 ((<>))
import           Data.Proxy                  (Proxy (..))
import qualified Network.Wai.Handler.Warp    as Warp
import           Servant

type NodeAPI = "blockchain" :> Get '[JSON] (Blockchain Validated)

data Config = Config
    { blockchainPath :: FilePath
    , blockchainVar  :: TVar (Blockchain Validated)
    , runLog         :: String -> IO ()
    }

nodeAPI :: Proxy NodeAPI
nodeAPI = Proxy

main :: IO ()
main = do
    let runLog = putStrLn
    let blockchainPath = "data/singleton_blockchain.json"

    runLog $ "Running node using blockchain at path: " <> blockchainPath

    bs <- Lazy.readFile blockchainPath

    blockchainUnvalidated <- either
        (\e -> error $ "Error: unable to decode blockchain: " <> show e)
        return (Aeson.eitherDecode bs)

    blockchain <- either
        (\e -> error $ "Error: invalid blockchain: " <> show e)
        return (validate blockchainUnvalidated)

    blockchainVar <- TVar.newTVarIO blockchain

    runServer Config{..}
  where
    runServer = Warp.run 8000 . serve nodeAPI . server

-- TODO: ReaderT
server :: Config -> Server NodeAPI
server = getBlockchain

getBlockchain :: Config -> Handler (Blockchain Validated)
getBlockchain = liftIO . TVar.readTVarIO . blockchainVar


throwLeft :: Show a => Either a b -> b
throwLeft = either (error . show) id
