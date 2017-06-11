module Data.Blockchain.Core.Types.UnverifiedBlockchain
    ( UnverifiedBlockchain(..)
    , UnverifiedBlockchainNode(..)
    ) where

import qualified Data.Aeson                                  as Aeson
import qualified GHC.Generics                                as Generic

import           Data.Blockchain.Core.Types.Block
import           Data.Blockchain.Core.Types.BlockchainConfig


data UnverifiedBlockchain = UnverifiedBlockchain
    { config :: BlockchainConfig
    , node   :: UnverifiedBlockchainNode
    }
  deriving (Generic.Generic, Eq, Show)

instance Aeson.FromJSON UnverifiedBlockchain where

data UnverifiedBlockchainNode = UnverifiedBlockchainNode
    { block :: Block
    , nodes :: [UnverifiedBlockchainNode]
    }
  deriving (Generic.Generic, Eq, Show)

instance Aeson.FromJSON UnverifiedBlockchainNode where
