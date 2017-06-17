module Data.Blockchain.Mining.Blockchain
    ( createBlockchain
    ) where

import           Data.Monoid                     ((<>))

import qualified Data.Blockchain.Core.Blockchain as Blockchain
import qualified Data.Blockchain.Core.Types      as Blockchain
import           Data.Blockchain.Mining.Block


createBlockchain :: Blockchain.BlockchainConfig -> IO (Blockchain.Blockchain Blockchain.Validated)
createBlockchain config = either throwValidationError id <$> do
    genesisBlock <- mineGenesisBlock config

    let node  = Blockchain.BlockchainNode genesisBlock mempty
        chain = Blockchain.construct config node

    return (Blockchain.validate chain)
  where
    throwValidationError e = error $ "Unexpected error creating blockchain: " <> show e
