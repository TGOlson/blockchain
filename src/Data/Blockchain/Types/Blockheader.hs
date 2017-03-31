module Data.Blockchain.Types.BlockHeader
    ( BlockHeader(..)
    , incNonce
    ) where

import qualified Data.Aeson         as Aeson
import           Data.Aeson         ((.=))
import qualified Data.Time.Clock    as Time

import Data.Blockchain.Crypto.Hash
import Data.Blockchain.Crypto.HashTree
import Data.Blockchain.Types.Difficulty
import Data.Blockchain.Types.Transaction

data BlockHeader = BlockHeader
    { version                 :: Int
    , prevBlockHeaderHash     :: Hash BlockHeader
    , transactionHashTreeRoot :: HashTreeRoot [Transaction]
    , time                    :: Time.UTCTime
    , difficulty              :: Difficulty
    , nonce                   :: Int
    }
  deriving (Eq, Show)

incNonce :: BlockHeader -> BlockHeader
incNonce header = header { nonce = nonce header + 1 }

instance Hashable BlockHeader where
    hash = hashJSON

instance Aeson.ToJSON BlockHeader where
    toJSON BlockHeader{..} = Aeson.object
        [ "version"                 .= version
        , "prevBlockHeaderHash"     .= prevBlockHeaderHash
        , "transactionHashTreeRoot" .= transactionHashTreeRoot
        , "time"                    .= time
        , "difficulty"              .= difficulty
        , "nonce"                   .= nonce
        ]
