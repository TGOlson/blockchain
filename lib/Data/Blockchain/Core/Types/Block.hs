module Data.Blockchain.Core.Types.Block
    ( Block(..)
    , BlockHeader(..)
    , blockHeaderHashDifficulty
    ) where

import qualified Data.Aeson      as Aeson
import qualified Data.Time.Clock as Time
import qualified GHC.Generics    as Generic

import qualified Data.Blockchain.Core.Crypto            as Crypto
import           Data.Blockchain.Core.Types.Difficulty
import           Data.Blockchain.Core.Types.Transaction

data Block = Block
    { blockHeader         :: BlockHeader
    , coinbaseTransaction :: CoinbaseTransaction
    , transactions        :: [Transaction]
    }
  deriving (Generic.Generic, Eq, Show)

-- TODO: add block header hash for more efficient hash comparison
instance Aeson.ToJSON Block
instance Aeson.FromJSON Block

data BlockHeader = BlockHeader
    { version                 :: Int
    , prevBlockHeaderHash     :: Crypto.Hash BlockHeader
    , coinbaseTransactionHash :: Crypto.Hash CoinbaseTransaction
    , transactionHashTreeRoot :: Crypto.HashTreeRoot [Transaction]
    , time                    :: Time.UTCTime
    , difficulty              :: Difficulty
    , nonce                   :: Int
    }
  deriving (Generic.Generic, Eq, Show)

instance Crypto.Hashable BlockHeader where
    hash = Crypto.hashJSON

instance Aeson.ToJSON BlockHeader
instance Aeson.FromJSON BlockHeader

blockHeaderHashDifficulty :: BlockHeader -> Difficulty
blockHeaderHashDifficulty header = Difficulty $ unDifficulty maxDifficulty `div` headerHash64
  where
    headerHash64 = Crypto.hashToWord64 headerHash
    headerHash   = Crypto.hash header
