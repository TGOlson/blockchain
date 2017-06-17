module Data.Blockchain.Core.Types.Block
    ( Block(..)
    , BlockHeader(..)
    ) where

import qualified Data.Aeson                             as Aeson
import qualified Data.Time.Clock                        as Time
import qualified GHC.Generics                           as Generic

import qualified Data.Blockchain.Core.Crypto            as Crypto
import           Data.Blockchain.Core.Types.Difficulty
import           Data.Blockchain.Core.Types.Transaction

data Block = Block
    { blockHeader         :: BlockHeader
    , coinbaseTransaction :: CoinbaseTransaction
    , transactions        :: [Transaction]
    }
  deriving (Generic.Generic, Eq, Show)

instance Aeson.FromJSON Block
instance Aeson.ToJSON Block

data BlockHeader = BlockHeader
    { version                 :: Int
    , prevBlockHeaderHash     :: Crypto.Hash BlockHeader
    , coinbaseTransactionHash :: Crypto.Hash CoinbaseTransaction
    , transactionHashTreeRoot :: Crypto.HashTreeRoot Transaction
    , time                    :: Time.UTCTime
    , difficulty              :: Difficulty
    , nonce                   :: Int
    }
  deriving (Generic.Generic, Eq, Show)

instance Aeson.FromJSON BlockHeader
instance Aeson.ToJSON BlockHeader
instance Crypto.ToHash BlockHeader
