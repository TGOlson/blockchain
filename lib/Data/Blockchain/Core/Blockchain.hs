module Data.Blockchain.Core.Blockchain
    -- Types
    ( Validated
    , Unvalidated
    , Blockchain
    , blockchainConfig
    , blockchainNode
    , BlockchainNode(..)
    , ValidationException(..)
    , BlockException(..)
    -- Construction
    , construct
    , validate
    , addBlock
    -- Chain inspection
    , blockHeaderHashDifficulty
    , addressValues
    , unspentTransactionOutputs
    , longestChain
    , flatten
    ) where

import qualified Control.Monad                 as M
import qualified Data.Aeson                    as Aeson
import qualified Data.Aeson.Types              as Aeson
import qualified Data.ByteString.Lazy          as Lazy
import qualified Data.Char                     as Char
import qualified Data.Either                   as Either
import qualified Data.Either.Combinators       as Either
import qualified Data.Foldable                 as Foldable
import qualified Data.HashMap.Strict           as H
import qualified Data.List                     as List
import qualified Data.List.NonEmpty            as NonEmpty
import qualified Data.Ord                      as Ord
import qualified Data.Word                     as Word
import qualified GHC.Generics                  as Generic

import qualified Data.Blockchain.Core.Crypto   as Crypto
import           Data.Blockchain.Core.Types
import qualified Data.Blockchain.Core.Util.Hex as Hex

-- Types ----------------------------------------------------------------------------------------------------

data Validated
data Unvalidated

data Blockchain a = Blockchain
    { _config :: BlockchainConfig
    , _node   :: BlockchainNode
    }
  deriving (Generic.Generic, Eq, Show)

blockchainConfig :: Blockchain a -> BlockchainConfig
blockchainConfig = _config

blockchainNode :: Blockchain a -> BlockchainNode
blockchainNode = _node

instance Aeson.FromJSON (Blockchain Unvalidated) where
    parseJSON = Aeson.genericParseJSON (stripFieldPrefix "_")

instance Aeson.ToJSON (Blockchain a) where
    toEncoding = Aeson.genericToEncoding (stripFieldPrefix "_")

data BlockchainNode = BlockchainNode
    { nodeBlock :: Block
    , nodeNodes :: [BlockchainNode]
    }
  deriving (Generic.Generic, Eq, Show)

instance Aeson.FromJSON BlockchainNode where
    parseJSON = Aeson.genericParseJSON (stripFieldPrefix "node")

instance Aeson.ToJSON BlockchainNode where
    toEncoding = Aeson.genericToEncoding (stripFieldPrefix "node")

data ValidationException
    = GenesisBlockHasTransactions
    | GenesisBlockException BlockException
    | BlockValidationException BlockException
  deriving (Eq, Show)

data BlockException
    = BlockAlreadyExists
    | NoParentFound
    -- timestamps
    | TimestampTooOld
    | TimestampTooFarIntoFuture
    -- difficulty
    | InvalidDifficultyReference
    | InvalidDifficulty
    -- header refs
    | InvalidCoinbaseTransactionHash
    | InvalidTransactionHashTreeRoot
    -- transactions
    | InvalidCoinbaseTransactionValue
    | InvalidTransactionValues
    | TransactionOutRefNotFound
    | InvalidTransactionSignature
  deriving (Eq, Show)


-- Construction ---------------------------------------------------------------------------------------------

construct :: BlockchainConfig -> BlockchainNode -> Blockchain Unvalidated
construct = Blockchain

validate :: Blockchain Unvalidated -> Either ValidationException (Blockchain Validated)
validate (Blockchain config (BlockchainNode genesisBlock nodes)) = do
    let (Block header _coinbase txs) = genesisBlock
        reward                       = initialMiningReward config
        blockchainHead               = Blockchain config (BlockchainNode genesisBlock mempty)
        blocks                       = nodes >>= getBlocks

    verify (null txs) GenesisBlockHasTransactions
    Either.mapLeft BlockValidationException $ validateBlockDifficulty header config mempty
    Either.mapLeft BlockValidationException $ validateTransactions genesisBlock mempty reward
    Either.mapLeft BlockValidationException $ validateBlockHeaderReferences genesisBlock
    Either.mapLeft BlockValidationException $ Foldable.foldrM addBlock blockchainHead blocks
  where
    getBlocks (BlockchainNode block ns) = block : (ns >>= getBlocks)

addBlock :: Block -> Blockchain Validated -> Either BlockException (Blockchain Validated)
addBlock blk (Blockchain config node) = Blockchain config <$> addBlockToNode blk mempty node
  where
    addBlockToNode :: Block -> [Block] -> BlockchainNode -> Either BlockException BlockchainNode
    addBlockToNode newBlock prevBlocks (BlockchainNode block nodes) =
        if isParentNode then do
            let blocks         = nodeBlock <$> nodes
                newNode        = BlockchainNode newBlock mempty
                updatedNode    = BlockchainNode block (newNode : nodes)
                height         = length prevBlocks + 1
                newBlockHeader = blockHeader newBlock

            verify (newBlock `notElem` blocks) BlockAlreadyExists
            validateBlockCreationTime newBlockHeader (blockHeader block)
            validateBlockDifficulty newBlockHeader config prevBlocks
            validateTransactions newBlock prevBlocks (targetReward config $ fromIntegral height)
            validateBlockHeaderReferences newBlock

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
    -- TODO: this no longer makes sense... write tests and revisit
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


-- Internal Validation ---------------------------------------------------------------------------------------

-- block references expected difficulty
-- block header hashes to expected difficulty
validateBlockDifficulty :: BlockHeader -> BlockchainConfig -> [Block] -> Either BlockException ()
validateBlockDifficulty header config blocks = do
    verify (difficulty header == diff) InvalidDifficultyReference
    verify (blockHeaderHashDifficulty (difficulty1Target config) header >= diff) InvalidDifficulty
  where
    diff = targetDifficulty config blocks

-- Exported util
-- TODO: find better place for this function

blockHeaderHashDifficulty :: Hex.Hex256 -> BlockHeader -> Difficulty
blockHeaderHashDifficulty diff1 header = fromIntegral $ diff1 `div` Crypto.hashToHex (Crypto.hash header)


-- block was not created before parent
-- TODO: The protocol rejects blocks with a timestamp earlier than the median of the timestamps from the previous 11 blocks
-- TODO: block created less than X hours, or N blocks intervals, into future
validateBlockCreationTime :: BlockHeader -> BlockHeader -> Either BlockException ()
validateBlockCreationTime newBlockHeader parentBlockHeader =
    verify (newBlockTimestamp > time parentBlockHeader) TimestampTooOld
    -- verify (newBlockTimestamp < now) TimestampTooFarIntoFuture
  where
    newBlockTimestamp = time newBlockHeader

validateBlockHeaderReferences :: Block -> Either BlockException ()
validateBlockHeaderReferences (Block header coinbase txs) = do
    verify (Crypto.hash coinbase == coinbaseTransactionHash header) InvalidCoinbaseTransactionHash
    verify (Crypto.hashTreeRoot txs == transactionHashTreeRoot header) InvalidTransactionHashTreeRoot

-- TODO: transactions should be able to reference transactions within the same block
-- this means we should try to apply a transaction, if it fails, try to apply next transaction
-- recurse until stable
-- TODO: until this is implemented it will be possible to "double spend" in the same block... : (
validateTransactions :: Block -> [Block] -> Word.Word -> Either BlockException ()
validateTransactions (Block _header coinbaseTx txs) prevBlocks reward = do
    -- ensure coinbase transaction is of correct value
    -- TODO: coinbase can be reward + fees
    verify (txOutValue (coinbaseTransactionOut coinbaseTx) == reward) InvalidCoinbaseTransactionValue

    sequence_ (validateTransaction <$> txs)
  where
    txOutValue :: NonEmpty.NonEmpty TransactionOut -> Word.Word
    txOutValue = sum . fmap value

    unspentTransactions :: H.HashMap TransactionOutRef TransactionOut
    unspentTransactions = unspentTransactionOutputsInternal prevBlocks

    validateTransaction :: Transaction -> Either BlockException ()
    validateTransaction (Transaction txIn txOut) = do
        let outputValue = txOutValue txOut

        prevTxOut <- sequence $ flip fmap txIn $ \(TransactionIn ref sig) -> do
            tx <- maybeToEither TransactionOutRefNotFound (H.lookup ref unspentTransactions)
            -- TODO: should keep transaction signing & verification round-tripping in same place
            verify (Crypto.verify (signaturePubKey tx) sig (Lazy.toStrict $ Aeson.encode tx)) InvalidTransactionSignature
            return tx

        verify (txOutValue prevTxOut >= outputValue) InvalidTransactionValues


-- Transaction State -----------------------------------------------------------------------------------------

addressValues :: Blockchain Validated -> H.HashMap Crypto.PublicKey Word.Word
addressValues blockchain = H.fromListWith (+) (toPair <$> unspentTxOuts)
  where
    toPair (TransactionOut value pubKey) = (pubKey, value)
    unspentTxOuts = H.elems $ unspentTransactionOutputsInternal (NonEmpty.toList $ longestChain blockchain)

unspentTransactionOutputs :: Blockchain Validated -> H.HashMap Crypto.PublicKey [(TransactionOutRef, TransactionOut)]
unspentTransactionOutputs blockchain = H.fromListWith (++) (toPair <$> unspentTxOuts)
  where
    toPair (txRef, txOut) = (signaturePubKey txOut, pure (txRef, txOut))
    unspentTxOuts = H.toList $ unspentTransactionOutputsInternal (NonEmpty.toList $ longestChain blockchain)

-- Note: this is required to be an internal method
-- As we assume the list of blocks is a verified sub-chain.
-- TODO: similar issue to "verify transactions", does not recursively apply txouts within a transaction
unspentTransactionOutputsInternal :: [Block] -> H.HashMap TransactionOutRef TransactionOut
unspentTransactionOutputsInternal =
    foldr (\(Block _ coinbase txs) -> addTransactions txs . addCoinbaseTransaction coinbase) mempty
  where
    addCoinbaseTransaction :: CoinbaseTransaction -> H.HashMap TransactionOutRef TransactionOut -> H.HashMap TransactionOutRef TransactionOut
    addCoinbaseTransaction coinbase = H.unionWith onDuplicateTxOutRef coinbaseTxOutRefMap
      where
        coinbaseTxOutRefMap = makeTxOutRefMap (Left $ Crypto.hash coinbase) (coinbaseTransactionOut coinbase)

    addTransactions :: [Transaction] -> H.HashMap TransactionOutRef TransactionOut -> H.HashMap TransactionOutRef TransactionOut
    addTransactions txs hmap = foldr addTransaction hmap txs

    addTransaction :: Transaction -> H.HashMap TransactionOutRef TransactionOut -> H.HashMap TransactionOutRef TransactionOut
    addTransaction tx@(Transaction txIns txOuts) = H.unionWith onDuplicateTxOutRef txOutRefMap . enforceDeleteAll txOutRefsFromTxIns
      where
        txOutRefsFromTxIns = NonEmpty.toList (transactionOutRef <$> txIns)
        txOutRefMap        = makeTxOutRefMap (Right $ Crypto.hash tx) txOuts
        -- Map utils, enforcing expected invariants
        enforceDelete k          = H.alter (maybe (onNotFoundTxOutRef k) (const Nothing)) k
        enforceDeleteAll ks hmap = foldr enforceDelete hmap ks

    makeTxOutRefMap :: Either (Crypto.Hash CoinbaseTransaction) (Crypto.Hash Transaction) -> NonEmpty.NonEmpty TransactionOut -> H.HashMap TransactionOutRef TransactionOut
    makeTxOutRefMap eHash txOuts = H.fromList txOutRefPair
      where
        txOutIndexed = zip (NonEmpty.toList txOuts) [0..]
        txOutRefPair = (\(txOut, i) -> (TransactionOutRef eHash i, txOut)) <$> txOutIndexed

    onDuplicateTxOutRef txOutRef = error ("Unexpected error when computing transaction map - duplicate transaction: " ++ show txOutRef)
    onNotFoundTxOutRef  txOutRef = error ("Unexpected error when computing transaction map - transaction not found: " ++ show txOutRef)


-- Chain inspection -----------------------------------------------------------------------------------------

longestChain :: Blockchain Validated -> NonEmpty.NonEmpty Block
longestChain = List.maximumBy lengthOrDifficulty . flatten
  where
    lengthOrDifficulty chain1 chain2 =
        case Ord.comparing length chain1 chain2 of
            EQ -> Ord.comparing chainDifficulty chain1 chain2
            x  -> x
    chainDifficulty = sum . fmap (difficulty . blockHeader)

flatten :: Blockchain Validated -> NonEmpty.NonEmpty (NonEmpty.NonEmpty Block)
flatten = flattenInternal . blockchainNode
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

stripFieldPrefix :: String -> Aeson.Options
stripFieldPrefix str = Aeson.defaultOptions { Aeson.fieldLabelModifier = stripPrefix }
  where
    stripPrefix x = maybe x lowercase (List.stripPrefix str x)
    lowercase []     = []
    lowercase (x:xs) = Char.toLower x : xs
