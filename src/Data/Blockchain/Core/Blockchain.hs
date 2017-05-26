module Data.Blockchain.Core.Blockchain
    ( Blockchain
    , blockchainConfig
    , BlockchainConfig(..)
    , UnverifiedBlockchain(..)
    , UnverifiedBlockchainNode(..)
    , BlockchainVerificationException(..)
    , AddBlockException(..)
    , createBlockchain
    , verifyBlockchain
    , addBlock
    , currentReward
    , currentDifficulty
    , longestChain
    , toString
    ) where

import qualified Control.Monad           as M
import qualified Data.Either             as Either (partitionEithers)
import qualified Data.Either.Combinators as Either
import qualified Data.Foldable           as Foldable
import qualified Data.HashMap.Strict     as H
import qualified Data.List               as List
import qualified Data.List.NonEmpty      as NonEmpty
import qualified Data.Ord                as Ord

import Data.Blockchain.Core.Crypto.Hash
-- import Data.Blockchain.Crypto.ECDSA
import Data.Blockchain.Core.Types

data Blockchain = Blockchain
    { _config :: BlockchainConfig
    , _node   :: BlockchainNode
    }

data BlockchainNode = BlockchainNode
    { _block :: Block
    , _nodes :: [BlockchainNode]
    }

blockchainConfig :: Blockchain -> BlockchainConfig
blockchainConfig (Blockchain config _) = config

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

data UnverifiedBlockchain = UnverifiedBlockchain
    { _uConfig :: BlockchainConfig
    , _uNode   :: UnverifiedBlockchainNode
    }

data UnverifiedBlockchainNode = UnverifiedBlockchainNode Block [UnverifiedBlockchainNode]

-- TODO: serialization
-- instance FromJSON UnverifiedBlockchain where

data BlockchainVerificationException
    = GenesisBlockException String -- TODO
    | AddBlockVerificationException AddBlockException

data AddBlockException
    = BlockAlreadyExists
    | NoPreviousBlockFound
    | BlockCreatedBeforeParent
    | InvalidDifficultyReference
    | InvalidDifficulty
  deriving (Eq, Show)

-- Note: requires mining a genesis block
createBlockchain :: BlockchainConfig -> Blockchain
createBlockchain = undefined

verifyBlockchain :: UnverifiedBlockchain -> Either BlockchainVerificationException Blockchain
verifyBlockchain (UnverifiedBlockchain config (UnverifiedBlockchainNode genesisBlock nodes)) = do
    verifyGenesisBlock genesisBlock
    Either.mapLeft AddBlockVerificationException $ Foldable.foldrM addBlock blockchainHead blocks
  where
    -- TODO: can probably be made generic for any block to config verifications
    verifyGenesisBlock (Block _header _coinbase txs) = do
        verify (difficulty (blockHeader genesisBlock) == initialDifficulty config) (GenesisBlockException "incorrect difficulty")
        verify (null txs) (GenesisBlockException "expected no transactions")

    blockchainHead = Blockchain config (BlockchainNode genesisBlock [])
    blocks = concatMap getBlocks nodes
    getBlocks (UnverifiedBlockchainNode block ns) = block : concatMap getBlocks ns

verify :: Bool -> a -> Either a ()
verify cond = M.unless cond . Left

-- Note: sohuld generally be able to re-use most of the old `addBlock` function below
-- with the addition of verifying the transactions fit into the target chain
--
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
addBlock :: Block -> Blockchain -> Either AddBlockException Blockchain
addBlock block (Blockchain config node) = do
    -- TODO: verify block conforms to config
    Blockchain config <$> addBlockToNode block [] node

addBlockToNode :: Block -> [Block] -> BlockchainNode -> Either AddBlockException BlockchainNode
addBlockToNode newBlock prevBlocks (BlockchainNode block nodes) =
    -- Found correct parent node
    if isParentNode then do
        let blocks         = _block <$> nodes
            newNode        = BlockchainNode newBlock []
            updatedNode    = BlockchainNode block (newNode : nodes)
            config         = undefined -- TODO
            height         = length prevBlocks + 1
            newBlockHeader = blockHeader newBlock

        -- ** Verification **
        -- block is not already in node
        verify (newBlock `notElem` blocks) BlockAlreadyExists

        -- block was not created before parent
        -- The protocol rejects blocks with a timestamp earlier than the median of the timestamps from the previous 11 blocks
        -- TODO: verify (time newBlockHeader > time (blockHeader block)) BlockCreatedBeforeParent

        -- block references expected difficulty
        verify (difficulty newBlockHeader == targetDifficulty config height) InvalidDifficultyReference
        -- block header hashes to expected difficulty
        verify (isValidDifficulty newBlockHeader) InvalidDifficulty

        -- TODO: block created less than X hours, or N blocks intervals, into future

        -- TODO: verify transactions fit into prevBlocks

        return updatedNode
    else
        let eBlockchains = fmap (\bs -> Either.mapLeft (\e -> (e, bs)) (addBlockToNode newBlock (prevBlocks ++ [block]) bs)) nodes in
        BlockchainNode block <$> reduceAddBlockResults eBlockchains
  where
    -- TODO: block headers should contain a hash of themselves,
    -- so that we don't have to hash every single time
    isParentNode = hash (blockHeader block) == prevBlockHeaderHash (blockHeader newBlock)
    -- Rules:
    --   If all results are `Left NoPreviousBlockFound` the result is `Left NoPreviousBlockFound`.
    --   If any result is `Left BlockAlreadyExists` the result is `Left BlockAlreadyExists`.
    --   If one result is `Right Blockchain` and the rest are `Left NoPreviousBlockFound`
    --      the result is that new block chain and all the previous chains.
    --   If more than one result is `Right Blockchain` it is an unexpected result and the function will error.
    reduceAddBlockResults :: [Either (AddBlockException, BlockchainNode) BlockchainNode] -> Either AddBlockException [BlockchainNode]
    reduceAddBlockResults results = case (blockAlreadyExists, rightResults) of
        (True, _)    -> Left BlockAlreadyExists
        (False, [])  -> Left NoPreviousBlockFound

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

isValidDifficulty :: BlockHeader -> Bool
isValidDifficulty header = diff >= headerHashInteger
  where
    diff              = unDifficulty (difficulty header)
    headerHashInteger = hashToInteger (hash header)


-- Config inspection --
currentReward :: Blockchain -> Int
currentReward chain@(Blockchain config _) =
    case currentBounds of
        []     -> initialMiningReward config
        bounds -> fst (minimum bounds)
  where
    currentBounds = filter (\(height, _) -> numBlocks <= height) rewardBounds
    numBlocks     = chainLength chain
    rewardBounds  = H.toList $ miningRewardTransitionMap config

-- TODO: find current difficulty from chain length
currentDifficulty :: Blockchain -> Difficulty
currentDifficulty (Blockchain config _) = initialDifficulty config

targetDifficulty :: BlockchainConfig -> Int -> Difficulty
targetDifficulty _config _height = Difficulty 1 -- TODO:

-- Chain inspection --

chainLength :: Blockchain -> Int
chainLength = length . longestChain

longestChain :: Blockchain -> NonEmpty.NonEmpty Block
longestChain = List.maximumBy lengthOrDifficulty . flatten
  where
    lengthOrDifficulty chain1 chain2 =
        case Ord.comparing length chain1 chain2 of
            EQ -> Ord.comparing chainDifficulty chain1 chain2
            x  -> x
    chainDifficulty = foldr (\block y -> unDifficulty (difficulty (blockHeader block)) + y) 0

flatten :: Blockchain -> NonEmpty.NonEmpty (NonEmpty.NonEmpty Block)
flatten (Blockchain _ node) = flattenInternal node
  where
    flattenInternal :: BlockchainNode -> NonEmpty.NonEmpty (NonEmpty.NonEmpty Block)
    flattenInternal = \case
        BlockchainNode block []  -> pure $ pure block
        BlockchainNode block bcs -> NonEmpty.cons block <$> (NonEmpty.fromList bcs >>= flattenInternal)

toString :: Blockchain -> String
toString (Blockchain _ node) = List.intercalate "\n" $ toStringLevels 0 node
  where
    toStringLevels :: Int -> BlockchainNode -> [String]
    toStringLevels level (BlockchainNode block blockchains) =
        hashString : concatMap (toStringLevels (level + 1)) blockchains
      where
        spaces = replicate level '\t'
        hashString = spaces ++ show (hash $ blockHeader block)
