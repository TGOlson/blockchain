module TestData
    -- blockchains
    ( singletonBlockchainUnvalidated
    , singletonBlockchain
    , blockchain1Block
    , blockchain2BlockFork
    , blockchain3Block
    -- blocks
    , genesisBlock
    , block1A
    , block1ACoinbasePrivateKey
    , block1B
    , block1BCoinbasePrivateKey
    , block2A
    , block2ACoinbasePrivateKey
    -- utils
    , validate'
    , addBlock'
    , readJSON
    , throwLeft
    ) where

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as Lazy

import           Data.Blockchain

-- Test Data -------------------------------------------------------------------------------------------------

singletonBlockchainUnvalidated :: IO (Blockchain Unvalidated)
singletonBlockchainUnvalidated = readJSON "data/singleton_blockchain.json"

singletonBlockchain :: IO (Blockchain Validated)
singletonBlockchain = validate' <$> singletonBlockchainUnvalidated

blockchain1Block, blockchain2BlockFork, blockchain3Block :: IO (Blockchain Validated)
blockchain1Block     = blockchainWithBlock singletonBlockchain block1A
blockchain2BlockFork = blockchainWithBlock blockchain1Block block1B
blockchain3Block     = blockchainWithBlock blockchain2BlockFork block2A

genesisBlock :: IO Block
genesisBlock = nodeBlock . blockchainNode <$> singletonBlockchain

block1A, block1B, block2A :: IO Block
block1A = readJSON "data/block_1a.json"
block1B = readJSON "data/block_1b.json"
block2A = readJSON "data/block_2a.json"

block1ACoinbasePrivateKey, block1BCoinbasePrivateKey, block2ACoinbasePrivateKey :: IO PrivateKey
block1ACoinbasePrivateKey = readJSON "data/block_1a_coinbase_private_key.json"
block1BCoinbasePrivateKey = readJSON "data/block_1b_coinbase_private_key.json"
block2ACoinbasePrivateKey = readJSON "data/block_2a_coinbase_private_key.json"

-- Utils -----------------------------------------------------------------------------------------------------

blockchainWithBlock :: IO (Blockchain Validated) -> IO Block -> IO (Blockchain Validated)
blockchainWithBlock chain block = do
    c <- chain
    b <- block

    return (addBlock' b c)

validate' :: Blockchain Unvalidated -> Blockchain Validated
validate' = throwLeft . validate

addBlock' :: Block -> Blockchain Validated -> Blockchain Validated
addBlock' block = throwLeft . addBlock block

readJSON :: Aeson.FromJSON a => FilePath -> IO a
readJSON path =  throwLeft . Aeson.eitherDecode <$> Lazy.readFile path

throwLeft :: Show a => Either a b -> b
throwLeft = either (error . show) id
