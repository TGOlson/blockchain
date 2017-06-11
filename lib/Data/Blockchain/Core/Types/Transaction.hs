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

instance Aeson.FromJSON Transaction
instance Aeson.ToJSON  Transaction
instance Crypto.ToHash Transaction

newtype CoinbaseTransaction = CoinbaseTransaction
    { coinbaseTransactionOut :: NonEmpty.NonEmpty TransactionOut
    }
  deriving (Generic.Generic, Eq, Show)

instance Aeson.FromJSON CoinbaseTransaction
instance Aeson.ToJSON CoinbaseTransaction
instance Crypto.ToHash CoinbaseTransaction

data TransactionIn = TransactionIn
    { transactionOutRef :: TransactionOutRef
     -- Signature from prev transaction, using pubkey from prev transaction
    , signature         :: Crypto.Signature
    }
  deriving (Generic.Generic, Eq, Show)

instance Aeson.ToJSON TransactionIn
instance Aeson.FromJSON TransactionIn
instance Crypto.ToHash TransactionIn

-- Pointer to a specific TransactionOut
data TransactionOutRef = TransactionOutRef
    { transactionHash     :: Either (Crypto.Hash CoinbaseTransaction) (Crypto.Hash Transaction)
    , transactionOutIndex :: Word.Word
    }
  deriving (Generic.Generic, Eq, Show)

instance H.Hashable TransactionOutRef

instance Aeson.FromJSON TransactionOutRef
instance Aeson.ToJSON TransactionOutRef

data TransactionOut = TransactionOut
    -- > maxBound :: Word
    -- 18,446,744,073,709,551,615
    -- TODO: too low?
    { value           :: Word.Word
    , signaturePubKey :: Crypto.PublicKey -- aka. address of where funds go
    }
  deriving (Generic.Generic, Eq, Show)

instance Aeson.FromJSON TransactionOut
instance Aeson.ToJSON TransactionOut
instance Crypto.ToHash TransactionOut
