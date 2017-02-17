module Data.Blockchain.Types.Transaction
    ( Transaction
    ) where

import qualified Data.Aeson  as Aeson
import           Data.Aeson  ((.=))

import Data.Blockchain.Crypto.ECDSA
import Data.Blockchain.Crypto.Hash

data Transaction = Transaction
    { transactionIn  :: [TransactionIn]
    , transactionOut :: [TransactionOut]
    }
  deriving (Show)

instance Hashable Transaction where
    hash = hashJSON

instance Aeson.ToJSON Transaction where
    toJSON Transaction{..} = Aeson.object
        [ "transactionIn"  .= transactionIn
        , "transactionOut" .= transactionOut
        ]

data TransactionIn = TransactionIn
    { previousTransactionHash     :: Hash Transaction
    , previousTransactionOutIndex :: Int
    , signature                   :: Signature
    }
  deriving (Show)

instance Hashable TransactionIn where
    hash = hashJSON

instance Aeson.ToJSON TransactionIn where
    toJSON TransactionIn{..} = Aeson.object
        [ "previousTransactionHash"     .= previousTransactionHash
        , "previousTransactionOutIndex" .= previousTransactionOutIndex
        , "signature"                   .= signature
        ]

data TransactionOut = TransactionOut
    { value           :: Int
    , signaturePubKey :: PublicKey
    }
  deriving (Show)

instance Hashable TransactionOut where
    hash = hashJSON

instance Aeson.ToJSON TransactionOut where
    toJSON TransactionOut{..} = Aeson.object
        [ "value"           .= value
        , "signaturePubKey" .= signaturePubKey
        ]
