module Data.Blockchain.Types.BlockHeader
    ( BlockHeader(..)
    , BlockHeaderHash(..)
    , incNonce
    ) where

import qualified Data.Aeson         as Aeson
import           Data.Aeson         ((.=))
import qualified Data.Time.Clock    as Time

import Data.Blockchain.Crypto.Hash
import Data.Blockchain.Types.Difficulty

data BlockHeader = BlockHeader
    { version                 :: Int
    , prevBlockHeaderHash     :: BlockHeaderHash
    , transactionHashTreeRoot :: Hash
    , time                    :: Time.UTCTime
    , difficulty              :: Difficulty
    , nonce                   :: Int
    }
  deriving (Show)

incNonce :: BlockHeader -> BlockHeader
incNonce header = header { nonce = nonce header + 1 }

instance Hashable BlockHeader where
    toHash = hashJSON

instance Aeson.ToJSON BlockHeader where
    toJSON BlockHeader{..} = Aeson.object
        [ "version"                 .= version
        , "prevBlockHeaderHash"     .= prevBlockHeaderHash
        , "transactionHashTreeRoot" .= transactionHashTreeRoot
        , "time"                    .= time
        , "difficulty"              .= difficulty
        , "nonce"                   .= nonce
        ]

newtype BlockHeaderHash = BlockHeaderHash { unBlockHash :: Hash }
  deriving (Eq, Ord, Aeson.ToJSON, Show)
