module Data.Blockchain.Types.Transaction
    ( Transaction
    ) where

import qualified Data.Aeson as Aeson

import Data.Blockchain.Crypto.Hash

-- https://en.bitcoin.it/wiki/Transaction
data Transaction = Transaction deriving (Show)

instance Hashable Transaction where
    toHash = hashJSON

instance Aeson.ToJSON Transaction where
    toJSON Transaction = Aeson.object []
