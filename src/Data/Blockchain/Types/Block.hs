module Data.Blockchain.Types.Block
    ( Block(..)
    ) where

import qualified Data.Aeson as Aeson
import           Data.Aeson ((.=))

import Data.Blockchain.Types.BlockHeader
import Data.Blockchain.Types.Transaction

data Block = Block
    { blockHeader  :: BlockHeader
    , transactions :: [Transaction]
    }
  deriving (Eq, Show)

-- TODO: add block header hash for more efficient has comparision
instance Aeson.ToJSON Block where
    toJSON Block{..} = Aeson.object
        [ "header"       .= blockHeader
        , "transactions" .= transactions
        ]
