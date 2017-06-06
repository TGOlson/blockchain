module Data.Blockchain.Core.Types.Transaction
    ( Transaction(..)
    , CoinbaseTransaction(..)
    , TransactionIn(..)
    , TransactionOutRef(..)
    , TransactionOut(..)
    ) where

import qualified Data.Aeson         as Aeson
import qualified Data.Hashable      as H
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Word          as Word
import qualified GHC.Generics       as Generic


import qualified Data.Blockchain.Core.Crypto as Crypto

data Transaction = Transaction
    { transactionIn  :: NonEmpty.NonEmpty TransactionIn
    , transactionOut :: NonEmpty.NonEmpty TransactionOut
    -- TODO: arbitrary bytes?
    }
  deriving (Generic.Generic, Eq, Show)

instance Crypto.Hashable Transaction where
    hash = Crypto.hashJSON

instance Aeson.ToJSON Transaction
instance Aeson.FromJSON Transaction

newtype CoinbaseTransaction = CoinbaseTransaction
    { coinbaseTransactionOut :: NonEmpty.NonEmpty TransactionOut
    }
  deriving (Generic.Generic, Eq, Show)

instance Crypto.Hashable CoinbaseTransaction where
    hash = Crypto.hashJSON

instance Aeson.ToJSON CoinbaseTransaction
instance Aeson.FromJSON CoinbaseTransaction

data TransactionIn = TransactionIn
    { transactionOutRef :: TransactionOutRef
    , signature         :: Crypto.Signature -- Signature from prev transaction, using pubkey from prev transaction
    }
  deriving (Generic.Generic, Eq, Show)

instance Crypto.Hashable TransactionIn where
    hash = Crypto.hashJSON

instance Aeson.ToJSON TransactionIn
instance Aeson.FromJSON TransactionIn

-- Pointer to a specific TransactionOut
data TransactionOutRef = TransactionOutRef
    { transactionHash     :: Either (Crypto.Hash CoinbaseTransaction) (Crypto.Hash Transaction)
    , transactionOutIndex :: Word.Word
    }
  deriving (Generic.Generic, Eq, Show)

instance H.Hashable TransactionOutRef

instance Aeson.ToJSON TransactionOutRef
instance Aeson.FromJSON TransactionOutRef

data TransactionOut = TransactionOut
    -- > maxBound :: Word
    -- 18,446,744,073,709,551,615
    -- TODO: too low?
    { value           :: Word.Word
    , signaturePubKey :: Crypto.PublicKey -- aka. address of where funds go
    }
  deriving (Generic.Generic, Eq, Show)

instance Crypto.Hashable TransactionOut where
    hash = Crypto.hashJSON

instance Aeson.ToJSON TransactionOut
instance Aeson.FromJSON TransactionOut
