module Data.Blockchain.TestData
    ( singletonBlockchainUnvalidated
    , singletonBlockchain
    , block1A
    , block1ACoinbasePrivateKey
    ) where

import qualified Data.Aeson                      as Aeson
import qualified Data.ByteString.Lazy            as Lazy

import           Data.Blockchain.Core.Blockchain
import           Data.Blockchain.Core.Crypto
import           Data.Blockchain.Core.Types

-- Test Data -------------------------------------------------------------------------------------------------

singletonBlockchainUnvalidated :: IO (Blockchain Unvalidated)
singletonBlockchainUnvalidated = readJSON "data/singleton_blockchain.json"

singletonBlockchain :: IO (Blockchain Validated)
singletonBlockchain = throwLeft . validate <$> singletonBlockchainUnvalidated

block1A :: IO Block
block1A = readJSON "data/block_1a.json"

block1ACoinbasePrivateKey :: IO PrivateKey
block1ACoinbasePrivateKey = readJSON "data/block_1a_coinbase_private_key.json"

-- Utils -----------------------------------------------------------------------------------------------------

readJSON :: Aeson.FromJSON a => FilePath -> IO a
readJSON path =  throwLeft . Aeson.eitherDecode <$> Lazy.readFile path

throwLeft :: Show a => Either a b -> b
throwLeft = either (error . show) id
