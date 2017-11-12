module Data.Blockchain.Types.DB
    ( DB(..)
    ) where

import Data.Blockchain.Crypto
import Data.Blockchain.Types.Block
import Data.Blockchain.Types.Transaction

-- TODO: how should this be structured?
--
-- | block_header            |
-- |-------------------------|
-- | hash                    |
-- | version                 |
-- | prevBlockHeaderHash     |
-- | coinbaseTransactionHash |
-- | transactionHashTreeRoot |
-- | time                    |
-- | difficulty              |
-- | nonce                   |
--
-- | tx_in            | tx_out       |
-- |------------------|--------------|
-- | tx_hash_root     | tx_hash_root |
-- | sig              | value        |
-- | tx_out_hash_root | pub_key      |
-- | tx_out_index     | index        |
--
-- getTransaction :: HashTreeRoot Transaction -> Transaction

-- Note: this is just a storage interface
-- All core blockchain logic is encapsulated elsewhere
data DB m = DB
    { persistBlock   :: Block                    -> m ()
    , getBlock       :: Hash BlockHeader         -> m (Maybe Block)
    , getTransaction :: HashTreeRoot Transaction -> m (Maybe Transaction)
    -- TODO:
    --   need some utility functions like "get blocks" or "get chains" :: [[Block]]
    --   could also see tracking the longest chain separately
    --
    --   getChainFromBlock :: Hash BlockHeader -> m [BlockHeader]
    --   getLongestChainInfo :: m (Length, Difficulty, BlockHeader)
    --   setLongestChainInfo :: Length -> Difficulty -> BlockHeader -> m ()
    }
