module Data.Blockchain.TestData
    ( unvalidatedSingletonBlockchain
    , validatedSingletonBlockchain
    , singletonBlockchainNextBlock
    , singletonBlockchainNextBlockPrivateKey
    ) where

import qualified Data.Aeson                      as Aeson
import qualified Data.ByteString.Lazy            as Lazy

import           Data.Blockchain.Core.Blockchain
import           Data.Blockchain.Core.Crypto
import           Data.Blockchain.Core.Types

-- Singleton Chain

unvalidatedSingletonBlockchain :: IO (Blockchain Unvalidated)
unvalidatedSingletonBlockchain = readJSON "data/singleton_chain/blockchain.json"

validatedSingletonBlockchain :: IO (Blockchain Validated)
validatedSingletonBlockchain = throwLeft . validate <$> unvalidatedSingletonBlockchain

singletonBlockchainNextBlock :: IO Block
singletonBlockchainNextBlock = readJSON "data/singleton_chain/valid_next_block.json"

singletonBlockchainNextBlockPrivateKey :: IO PrivateKey
singletonBlockchainNextBlockPrivateKey = readJSON "data/singleton_chain/valid_next_block_coinbase_private_key.json"

readJSON :: Aeson.FromJSON a => FilePath -> IO a
readJSON path =  throwLeft . Aeson.eitherDecode <$> Lazy.readFile path

throwLeft :: Show a => Either a b -> b
throwLeft = either (error . show) id
