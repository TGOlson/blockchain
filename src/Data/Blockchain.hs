module Data.Blockchain
    ( Blockchain
    , BlockchainConfig(..)
    , UnverifiedBlockchain(..)
    , UnverifiedBlockchainNode(..)
    , BlockchainVerificationException(..)
    , AddBlockException(..)
    , createBlockchain
    , verify
    , addBlock
    , toString
    ) where

import qualified Control.Monad           as M
import qualified Data.Either             as Either (partitionEithers)
import qualified Data.Either.Combinators as Either
import qualified Data.Foldable           as Foldable
import qualified Data.HashMap.Strict     as H
import qualified Data.List               as List
-- import qualified Data.Ord                as Ord

import Data.Blockchain.Crypto.Hash
-- import Data.Blockchain.Crypto.ECDSA
import Data.Blockchain.Types

data Blockchain     = Blockchain BlockchainConfig BlockchainNode
data BlockchainNode = BlockchainNode Block [BlockchainNode]

-- instance ToJSON Blockchain where

-- TODO: could probably tighten this up
data BlockchainConfig = BlockchainConfig
    { initialDifficulty             :: Difficulty
    , targetMillisPerBlock          :: Int
    , difficultyRecalculationHeight :: Int
    , initialMiningReward           :: Int
    -- Defines block heights where reward changes
    -- An empty map means the current reward is always the initial reward
    , miningRewardTransitionMap     :: H.HashMap Int Int
    }

data UnverifiedBlockchain     = UnverifiedBlockchain BlockchainConfig UnverifiedBlockchainNode
data UnverifiedBlockchainNode = UnverifiedBlockchainNode Block [UnverifiedBlockchainNode]

-- TODO: serialization
-- instance FromJSON UnverifiedBlockchain where

data BlockchainVerificationException
    = GenesisBlockException String -- TODO
    | AddBlockVerificationException AddBlockException

data AddBlockException
    = BlockAlreadyExists
    | NoPreviousBlockFound
  deriving (Eq, Show)

-- Note: requires mining a genesis block
createBlockchain :: BlockchainConfig -> Blockchain
createBlockchain = undefined

verify :: UnverifiedBlockchain -> Either BlockchainVerificationException Blockchain
verify (UnverifiedBlockchain config (UnverifiedBlockchainNode genesisBlock nodes)) = do
    verifyGenesisBlock genesisBlock
    Either.mapLeft AddBlockVerificationException $ Foldable.foldrM addBlock blockchainHead blocks
  where
    -- TODO: can probably made generic for any block to config verifications
    verifyGenesisBlock (Block _header _coinbase txs) = do
        M.unless (difficulty (blockHeader genesisBlock) == initialDifficulty config) $ Left (GenesisBlockException "incorrect difficulty")
        M.unless (null txs) $ Left (GenesisBlockException "expected no transactions")

    blockchainHead = Blockchain config (BlockchainNode genesisBlock [])
    blocks = concatMap getBlocks nodes
    getBlocks (UnverifiedBlockchainNode block ns) = block : concatMap getBlocks ns

-- Note: sohuld generally be able to re-use most of the old `addBlock` function below
-- with the addition of verifying the transactions fit into the target chain
addBlock :: Block -> Blockchain -> Either AddBlockException Blockchain
addBlock = undefined

toString :: Blockchain -> String
toString (Blockchain _ node) = List.intercalate "\n" $ toStringLevels 0 node
  where
    toStringLevels :: Int -> BlockchainNode -> [String]
    toStringLevels level (BlockchainNode block blockchains) =
        hashString : concatMap (toStringLevels (level + 1)) blockchains
      where
        spaces = replicate level '\t'
        hashString = spaces ++ show (hash $ blockHeader block)


-- OLD CODE -----------------------
-- TODO: delete

-- Check if the previous block referenced by the block exists and is valid.
-- Check that the timestamp of the block is greater than that of the previous block[2] and less than 2 hours into the future
-- Check that the proof of work on the block is valid.
-- Let S[0] be the state at the end of the previous block.
-- Suppose TX is the block's transaction list with n transactions. For all i in 0...n-1, set S[i+1] = APPLY(S[i],TX[i]) If any application returns an error, exit and return false.
-- Return true, and register S[n] as the state at the end of this block.

-- rules
-- https://en.bitcoin.it/wiki/Protocol_rules#.22block.22_messages
-- block needs to be unique (not already in chain)
-- block needs to reference a valid parent
-- transaction txins need to reference valid txouts from other transactions in same chain
-- transaction txout need to be less the sum of input txouts
-- transaction txin need to have valid signature by input txouts
--
-- TODO: probably needs `prevChain :: [Block]` in order to validate transactions
_addBlockOld :: Block -> Blockchain -> Either AddBlockException Blockchain
_addBlockOld bk (Blockchain config node) = Blockchain config <$> addBlockInternal bk node
  where
    addBlockInternal :: Block -> BlockchainNode -> Either AddBlockException BlockchainNode
    addBlockInternal newBlock (BlockchainNode block blockchains) =
    -- TODO: block headers should contain a hash of themselves,
    -- so that we don't have to hash every single time
    -- Found correct parent node
        if hash (blockHeader block) == prevBlockHeaderHash (blockHeader newBlock)
            then
                -- But first make sure it's not already in the leaves
                if any (\(BlockchainNode blk _) -> blk == newBlock) blockchains
                    then Left BlockAlreadyExists
                    -- TODO: need to verify transactions fit into blockchain
                    else Right (BlockchainNode block $ BlockchainNode newBlock [] : blockchains)
            else
                let eBlockchains = fmap (\bs -> Either.mapLeft (\e -> (e, bs)) (addBlockInternal newBlock bs)) blockchains in
                BlockchainNode block <$> reduceAddBlockResults eBlockchains
      where
        -- Rules:
        --   If all results are `Left NoPreviousBlockFound` the result is `Left NoPreviousBlockFound`.
        --   If any result is `Left BlockAlreadyExists` the result is `Left BlockAlreadyExists`.
        --   If one result is `Right Blockchain` and the rest are `Left NoPreviousBlockFound`
        --      the result is that new block chain and all the previous chains.
        --   If more than one result is `Right Blockchain` it is an unexpected result and the function will error.
        reduceAddBlockResults :: [Either (AddBlockException, BlockchainNode) BlockchainNode] -> Either AddBlockException [BlockchainNode]
        reduceAddBlockResults results = case (blockAlreadyExists, rightResults) of
            (True, [])   -> Left BlockAlreadyExists
            (False, [])  -> Left NoPreviousBlockFound
            (True, [_])  -> Left BlockAlreadyExists
            -- Add new chain to list of old chains
            -- Note: this will cause re-ordering, where newest chain will always appear first
            -- in list of subsequent chains.
            (False, [x]) -> Right (x : oldBlockChains)
            (_, _)       -> error "Unexpected error - block can be inserted into multiple chains"
          where
            (leftResults, rightResults) = Either.partitionEithers results
            (exceptions, oldBlockChains) = unzip leftResults
            -- Note: this ignores invariant where multiple `BlockAlreadyExists` errors are found
            -- However, we do expect our reducing function to monitor for that invariant during original insert.
            blockAlreadyExists = BlockAlreadyExists `elem` exceptions
--
-- flatten :: Blockchain -> [SingleChain]
-- flatten = \case
--     BlockchainNode block []  -> pure $ SingleChain (pure block)
--     BlockchainNode block bcs -> (\(SingleChain bc) -> SingleChain (block : bc)) <$> concatMap flatten bcs
--
-- TODO: need to compare difficulty if two chains have the same length
-- mainChain :: Blockchain -> SingleChain
-- mainChain = List.maximumBy (Ord.comparing (length . unSingleChain)) . flatten
