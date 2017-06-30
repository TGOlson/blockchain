module Network.Blockchain.API
    ( NodeAPI
    , WalletAPI
    , nodeAPI
    , walletAPI
    ) where

import Data.Blockchain
import Data.Proxy      (Proxy (..))
import Servant.API

type NodeAPI
    =    "blockchain" :> Get  '[JSON] (Blockchain Unvalidated)
    :<|> "block" :> ReqBody '[JSON] Block :> Post '[JSON] ()
    :<|> "transaction" :> ReqBody '[JSON] Transaction :> Post '[JSON] ()
    :<|> "transaction-pool" :> Get '[JSON] [Transaction]

type WalletAPI = "TODO" :> Get '[JSON] ()

nodeAPI :: Proxy NodeAPI
nodeAPI = Proxy

walletAPI :: Proxy WalletAPI
walletAPI = Proxy
