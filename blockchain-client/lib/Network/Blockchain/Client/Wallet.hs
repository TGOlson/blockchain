module Network.Blockchain.Client.Wallet
    ( getTodo
    ) where

import Network.Blockchain.API (walletAPI)
import Servant.Client

getTodo :: ClientM ()
getTodo = client walletAPI
