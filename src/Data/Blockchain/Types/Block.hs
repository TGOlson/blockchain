module Data.Blockchain.Types.Block
    ( Block
    ) where

import qualified Data.Aeson as Aeson
import           Data.Aeson ((.=))

import Data.Blockchain.Crypto.Hash
import Data.Blockchain.Types.Blockheader
import Data.Blockchain.Types.Transaction

-- Field	Description	Size
-- Magic no	value always 0xD9B4BEF9	4 bytes
-- Blocksize	number of bytes following up to end of block	4 bytes
-- Blockheader	consists of 6 items	80 bytes
-- Transaction counter	positive integer VI = VarInt	1 - 9 bytes
-- transactions	the (non empty) list of transactions	<Transaction counter>-many transactions

-- https://en.bitcoin.it/wiki/Block
data Block = Block
    { header           :: Blockheader
    , transactionCount :: Int
    , transactions     :: [Transaction]
    }

instance Hashable Block where
    toHash = hashJSON

instance Aeson.ToJSON Block where
    toJSON Block{..} = Aeson.object
        [ "header"           .= header
        , "transactionCount" .= transactionCount
        , "transactions"     .= transactions
        ]
