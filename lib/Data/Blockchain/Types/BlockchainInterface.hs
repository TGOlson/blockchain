module Data.Blockchain.Types.BlockchainInterface
    ( BlockchainInterface(..)
    ) where

import Data.Blockchain.Crypto
import Data.Blockchain.Types.Block
import Data.Blockchain.Types.Transaction

-- Note: this is basically just a "storage" interface
-- All core blockchain logic is encapsulated elsewhere
data BlockchainInterface m = BlockchainInterface
    { persistBlock       :: Block            -> m ()
    , getBlock           :: Hash Block       -> m (Maybe Block)
    , persistTransaction :: Transaction      -> m ()
    , getTransaction     :: Hash Transaction -> m (Maybe Transaction)
    -- maybe not create block here?...
    -- , createBlock    :: Config -> [Transaction]    -> m (Either CreateBlockException Block)
    }
