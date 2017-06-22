module Data.Blockchain.Mining.Blockchain
    ( mineBlockchain
    ) where

import           Data.Monoid                  ((<>))

import qualified Data.Blockchain              as Blockchain
import           Data.Blockchain.Mining.Block
import qualified Data.Blockchain.Types        as Blockchain

-- | Creates a blockchain from the given config.
-- This includes mining a genesis block.
mineBlockchain :: Blockchain.BlockchainConfig -> IO (Blockchain.Blockchain Blockchain.Validated)
mineBlockchain config = either throwValidationError id <$> do
    genesisBlock <- mineGenesisBlock config

    let node  = Blockchain.BlockchainNode genesisBlock mempty
        chain = Blockchain.construct config node

    return (Blockchain.validate chain)
  where
    throwValidationError e = error $ "Unexpected error creating blockchain: " <> show e
