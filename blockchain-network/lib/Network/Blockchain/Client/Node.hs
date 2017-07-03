module Network.Blockchain.Client.Node
    ( getBlockchain
    , sendBlock
    , sendTransaction
    , getTransactionPool
    ) where

import Data.Blockchain
import Network.Blockchain.API (nodeAPI)
import Servant.API
import Servant.Client

getBlockchain      :: ClientM (Blockchain Unvalidated)
sendBlock          :: Block -> ClientM ()
sendTransaction    :: Transaction -> ClientM ()
getTransactionPool :: ClientM [Transaction]
( getBlockchain      :<|>
  sendBlock          :<|>
  sendTransaction    :<|>
  getTransactionPool ) = client nodeAPI
