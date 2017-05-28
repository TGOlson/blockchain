module Data.Blockchain.Core.Types.Transaction
    ( Transaction(..)
    , CoinbaseTransaction(..)
    , TransactionIn(..)
    , TransactionOutRef(..)
    , TransactionOut(..)
    ) where

import qualified Data.Aeson         as Aeson
import           Data.Aeson         ((.=))
import qualified Data.Hashable      as H
import qualified Data.List.NonEmpty as NonEmpty
import qualified GHC.Generics       as Generic


import qualified Data.Blockchain.Core.Crypto as Crypto

data Transaction = Transaction
    { transactionIn  :: NonEmpty.NonEmpty TransactionIn
    , transactionOut :: NonEmpty.NonEmpty TransactionOut
    -- TODO: arbitrary bytes?
    }
  deriving (Eq, Show)

instance Crypto.Hashable Transaction where
    hash = Crypto.hashJSON

instance Aeson.ToJSON Transaction where
    toJSON Transaction{..} = Aeson.object
        [ "transactionIn"  .= transactionIn
        , "transactionOut" .= transactionOut
        ]

newtype CoinbaseTransaction = CoinbaseTransaction
    { coinbaseTransactionOut :: NonEmpty.NonEmpty TransactionOut
    }
  deriving (Eq, Show)

instance Crypto.Hashable CoinbaseTransaction where
    hash = Crypto.hashJSON

instance Aeson.ToJSON CoinbaseTransaction where
    toJSON CoinbaseTransaction{..} = Aeson.object
        [ "coinbaseTransactionOut" .= coinbaseTransactionOut
        ]

data TransactionIn = TransactionIn
    { transactionOutRef :: TransactionOutRef
    , signature         :: Crypto.Signature -- Signature from prev transaction, using pubkey from prev transaction
    }
  deriving (Eq, Show)

instance Crypto.Hashable TransactionIn where
    hash = Crypto.hashJSON

instance Aeson.ToJSON TransactionIn where
    toJSON TransactionIn{..} = Aeson.object
        [ "transactionOutRef" .= transactionOutRef
        , "signature"         .= signature
        ]

-- Pointer to a specific TransactionOut
data TransactionOutRef = TransactionOutRef
    { transactionHash     :: Either (Crypto.Hash CoinbaseTransaction) (Crypto.Hash Transaction)
    , transactionOutIndex :: Int
    }
  deriving (Eq, Show, Generic.Generic)

instance H.Hashable TransactionOutRef

instance Aeson.ToJSON TransactionOutRef where
    toJSON TransactionOutRef{..} = Aeson.object
        [ "transactionHash"     .= transactionHash
        , "transactionOutIndex" .= transactionOutIndex
        ]

data TransactionOut = TransactionOut
    { value           :: Int
    , signaturePubKey :: Crypto.PublicKey -- aka. address of where funds go
    }
  deriving (Eq, Show)

instance Crypto.Hashable TransactionOut where
    hash = Crypto.hashJSON

instance Aeson.ToJSON TransactionOut where
    toJSON TransactionOut{..} = Aeson.object
        [ "value"           .= value
        , "signaturePubKey" .= signaturePubKey
        ]
