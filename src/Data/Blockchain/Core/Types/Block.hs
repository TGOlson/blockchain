module Data.Blockchain.Core.Types.Block
    ( Block(..)
    , BlockHeader(..)
    ) where

import qualified Data.Aeson      as Aeson
import           Data.Aeson      ((.=))
import qualified Data.Time.Clock as Time

import qualified Data.Blockchain.Core.Crypto            as Crypto
import           Data.Blockchain.Core.Types.Difficulty
import           Data.Blockchain.Core.Types.Transaction

data Block = Block
    { blockHeader         :: BlockHeader
    , coinbaseTransaction :: CoinbaseTransaction
    , transactions        :: [Transaction]
    }
  deriving (Eq, Show)

-- TODO: add block header hash for more efficient hash comparison
instance Aeson.ToJSON Block where
    toJSON Block{..} = Aeson.object
        [ "header"       .= blockHeader
        , "coinbase"     .= coinbaseTransaction
        , "transactions" .= transactions
        ]

data BlockHeader = BlockHeader
    { version                 :: Int
    , prevBlockHeaderHash     :: Crypto.Hash BlockHeader
    , coinbaseTransactionHash :: Crypto.Hash CoinbaseTransaction
    , transactionHashTreeRoot :: Crypto.HashTreeRoot [Transaction]
    , time                    :: Time.UTCTime
    , difficulty              :: Difficulty
    , nonce                   :: Int
    }
  deriving (Eq, Show)

instance Crypto.Hashable BlockHeader where
    hash = Crypto.hashJSON

instance Aeson.ToJSON BlockHeader where
    toJSON BlockHeader{..} = Aeson.object
        [ "version"                 .= version
        , "prevBlockHeaderHash"     .= prevBlockHeaderHash
        , "coinbaseTransactionHash" .= coinbaseTransactionHash
        , "transactionHashTreeRoot" .= transactionHashTreeRoot
        , "time"                    .= time
        , "difficulty"              .= difficulty
        , "nonce"                   .= nonce
        ]
