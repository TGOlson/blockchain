module Data.Blockchain.Types.Transaction
    ( Transaction
    ) where

import qualified Data.Aeson  as Aeson
import           Data.Aeson  ((.=))

import Data.Blockchain.Crypto.Hash

-- https://en.bitcoin.it/wiki/Transaction
data Transaction = Transaction
    { transactionIn  :: [TransactionIn]
    , transactionOut :: [TransactionOut]
    }
  deriving (Show)

instance Hashable Transaction where
    toHash = hashJSON

instance Aeson.ToJSON Transaction where
    toJSON Transaction{..} = Aeson.object
        [ "transactionIn"  .= transactionIn
        , "transactionOut" .= transactionOut
        ]

data TransactionIn = TransactionIn
    { transactionInPreviousTransactionHash     :: Hash
    , transactionInPreviousTransactionOutIndex :: Int
    , transactionInSignature                   :: String
    }
  deriving (Show)

instance Hashable TransactionIn where
    toHash = hashJSON

instance Aeson.ToJSON TransactionIn where
    toJSON TransactionIn{..} = Aeson.object
        [ "previosTransactionHas"       .= transactionInPreviousTransactionHash
        , "previousTransactionOutIndex" .= transactionInPreviousTransactionOutIndex
        , "signature"                   .= transactionInSignature
        ]

data TransactionOut = TransactionOut
    { transactionOutValue           :: Int
    , transactionOutSignaturePubKey :: String
    }
  deriving (Show)

instance Hashable TransactionOut where
    toHash = hashJSON

instance Aeson.ToJSON TransactionOut where
    toJSON TransactionOut{..} = Aeson.object
        [ "value"           .= transactionOutValue
        , "signaturePubKey" .= transactionOutSignaturePubKey
        ]
