module Data.Blockchain.Core.Blockchain
    ( Blockchain
    , blockchainConfig
    , UnverifiedBlockchain(..)
    , UnverifiedBlockchainNode(..)
    , BlockchainVerificationException(..)
    , BlockException(..)
    , verifyBlockchain
    , addBlock
    , longestChain
    , toString
    ) where

import qualified Control.Monad           as M
import qualified Data.Aeson              as Aeson
import qualified Data.ByteString.Lazy    as Lazy
import qualified Data.Either             as Either
import qualified Data.Either.Combinators as Either
import qualified Data.Foldable           as Foldable
import qualified Data.HashMap.Strict     as H
import qualified Data.List               as List
import qualified Data.List.NonEmpty      as NonEmpty
import qualified Data.Ord                as Ord

import qualified Data.Blockchain.Core.Crypto as Crypto
import           Data.Blockchain.Core.Types

-- Types ----------------------------------------------------------------------------------------------------

data Blockchain = Blockchain
    { _config :: BlockchainConfig
    , _node   :: BlockchainNode
    }
-- instance ToJSON Blockchain where

data BlockchainNode = BlockchainNode
    { _block :: Block
    , _nodes :: [BlockchainNode]
    }

blockchainConfig :: Blockchain -> BlockchainConfig
blockchainConfig (Blockchain config _) = config

data UnverifiedBlockchain = UnverifiedBlockchain
    { _uConfig :: BlockchainConfig
    , _uNode   :: UnverifiedBlockchainNode
    }
-- instance UnverifiedBlockchain Blockchain where

data UnverifiedBlockchainNode = UnverifiedBlockchainNode Block [UnverifiedBlockchainNode]

-- TODO: serialization
-- instance FromJSON UnverifiedBlockchain where

data BlockchainVerificationException
    = GenesisBlockException String -- TODO
    | AddBlockVerificationException BlockException

data BlockException
    = BlockAlreadyExists
    | NoParentFound
    | TimestampTooOld
    | TimestampTooFarIntoFuture
    | InvalidDifficultyReference
    | InvalidDifficulty
    | InvalidCoinbaseTransactionValue
    | InvalidTransactionValues
    | TransactionOutRefNotFound
    | InvalidTransactionSignature
  deriving (Eq, Show)

-- Construction ---------------------------------------------------------------------------------------------

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

-- ** Notes
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
addBlock :: Block -> Blockchain -> Either BlockException Blockchain
addBlock blk (Blockchain config node) = Blockchain config <$> addBlockToNode blk [] node
  where
    addBlockToNode :: Block -> [Block] -> BlockchainNode -> Either BlockException BlockchainNode
    addBlockToNode newBlock prevBlocks (BlockchainNode block nodes) =
        if isParentNode then do
            let blocks         = _block <$> nodes
                newNode        = BlockchainNode newBlock []
                updatedNode    = BlockchainNode block (newNode : nodes)
                height         = length prevBlocks + 1
                newBlockHeader = blockHeader newBlock

            verify (newBlock `notElem` blocks) BlockAlreadyExists
            verifyBlockDifficulty newBlockHeader config height
            verifyBlockCreationTime newBlockHeader (blockHeader block)
            verifyTransactions newBlock prevBlocks (targetReward config height)

            return updatedNode
        else
            let eBlockchains = fmap (\bs -> Either.mapLeft (\e -> (e, bs)) (addBlockToNode newBlock (prevBlocks ++ [block]) bs)) nodes in
            BlockchainNode block <$> reduceAddBlockResults eBlockchains
      where
        -- TODO: block headers should contain a hash of themselves,
        -- so that we don't have to hash every single time
        isParentNode = Crypto.hash (blockHeader block) == prevBlockHeaderHash (blockHeader newBlock)

    -- Rules:
    --   If all results are `Left NoParentFound` the result is `Left NoParentFound`.
    --   If any result is `Left BlockAlreadyExists` the result is `Left BlockAlreadyExists`.
    --   If one result is `Right Blockchain` and the rest are `Left NoParentFound`
    --      the result is that new block chain and all the previous chains.
    --   If more than one result is `Right Blockchain` it is an unexpected result and the function will error.
    -- TODO: this no longer makes sense... revisit
    reduceAddBlockResults :: [Either (BlockException, BlockchainNode) BlockchainNode] -> Either BlockException [BlockchainNode]
    reduceAddBlockResults results = case (blockAlreadyExists, rightResults) of
        (True, _)    -> Left BlockAlreadyExists
        (False, [])  -> Left NoParentFound

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

-- block references expected difficulty
-- block header hashes to expected difficulty
verifyBlockDifficulty :: BlockHeader -> BlockchainConfig -> Int -> Either BlockException ()
verifyBlockDifficulty header config height = do
    verify (difficulty header == diff) InvalidDifficultyReference
    verify (unDifficulty diff >= headerHashInteger) InvalidDifficulty
  where
    diff = targetDifficulty config height
    headerHashInteger = Crypto.hashToInteger (Crypto.hash header)

-- block was not created before parent
-- TODO: The protocol rejects blocks with a timestamp earlier than the median of the timestamps from the previous 11 blocks
-- TODO: block created less than X hours, or N blocks intervals, into future
verifyBlockCreationTime :: BlockHeader -> BlockHeader -> Either BlockException ()
verifyBlockCreationTime newBlockHeader parentBlockHeader =
    verify (newBlockTimestamp > time parentBlockHeader) TimestampTooOld
    -- verify (newBlockTimestamp < now) TimestampTooFarIntoFuture
  where
    newBlockTimestamp = time newBlockHeader

-- TODO: transactions should be able to reference transactions within the same block
-- this means we should try to apply a transaction, if it fails, try to apply next transaction
-- recurse until stable
-- TODO: until this is implemented it will be possible to "double spend" in the same block... : (
verifyTransactions :: Block -> [Block] -> Int -> Either BlockException ()
verifyTransactions (Block _header coinbaseTx txs) prevBlocks reward = do
    -- ensure coinbase transaction is of correct value
    verify (txOutValue (coinbaseTransactionOut coinbaseTx) == reward) InvalidCoinbaseTransactionValue

    sequence_ (verifyTransaction <$> txs)
  where
    txOutValue :: NonEmpty.NonEmpty TransactionOut -> Int
    txOutValue = sum . fmap value

    unspentTransactions :: H.HashMap TransactionOutRef TransactionOut
    unspentTransactions = unspentTransactionOutputs prevBlocks

    verifyTransaction :: Transaction -> Either BlockException ()
    verifyTransaction (Transaction txIn txOut) = do
        let outputValue = txOutValue txOut

        prevTxOut <- sequence $ flip fmap txIn $ \(TransactionIn ref sig) -> do
            tx <- maybeToEither TransactionOutRefNotFound (H.lookup ref unspentTransactions)
            -- TODO: should keep transaction signing & verification round-tripping in same place
            verify (Crypto.verify (signaturePubKey tx) sig (Lazy.toStrict $ Aeson.encode tx)) InvalidTransactionSignature
            return tx

        verify (txOutValue prevTxOut >= outputValue) InvalidTransactionValues

unspentTransactionOutputs :: [Block] -> H.HashMap TransactionOutRef TransactionOut
unspentTransactionOutputs = undefined -- TODO: !


-- Chain inspection -----------------------------------------------------------------------------------------

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

-- Utils ----------------------------------------------------------------------------------------------------

verify :: Bool -> a -> Either a ()
verify cond = M.unless cond . Left

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither e = maybe (Left e) Right

-- Debugging ------------------------------------------------------------------------------------------------

toString :: Blockchain -> String
toString (Blockchain _ node) = List.intercalate "\n" $ toStringLevels 0 node
  where
    toStringLevels :: Int -> BlockchainNode -> [String]
    toStringLevels level (BlockchainNode block blockchains) =
        hashString : concatMap (toStringLevels (level + 1)) blockchains
      where
        spaces = replicate level '\t'
        hashString = spaces ++ show (Crypto.hash $ blockHeader block)
