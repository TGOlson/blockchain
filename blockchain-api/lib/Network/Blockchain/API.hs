module Network.Blockchain.API
    ( NodeAPI
    , WalletAPI
    ) where

import Data.Blockchain
import Servant.API

type NodeAPI
    =    "blockchain" :> Get  '[JSON] (Blockchain Validated)
    :<|> "block" :> ReqBody '[JSON] Block :> Post '[JSON] ()
    :<|> "transaction" :> ReqBody '[JSON] Transaction :> Post '[JSON] ()
    :<|> "transaction-pool" :> Get '[JSON] [Transaction]

type WalletAPI = "TODO" :> Get '[JSON] ()
