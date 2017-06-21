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
    -- Validation
    , validateTransaction
    , validateTransactions
    -- Chain inspection
    , blockHeaderHashDifficulty
    , addressValues
    , unspentTransactionOutputs
    , longestChain
    , flatten
    ) where

-- Haddock TODO: ordering/sections of exports ^^^ similar to comments

import           Control.Monad               (unless)
import qualified Data.Aeson                  as Aeson
import qualified Data.Aeson.Types            as Aeson
import qualified Data.Char                   as Char
import qualified Data.Either                 as Either
import qualified Data.Either.Combinators     as Either
import qualified Data.Foldable               as Foldable
import qualified Data.HashMap.Strict         as H
import qualified Data.List                   as List
import qualified Data.List.NonEmpty          as NonEmpty
import           Data.Monoid                 ((<>))
import qualified Data.Ord                    as Ord
import qualified Data.Word                   as Word
import qualified GHC.Generics                as Generic

import qualified Data.Blockchain.Core.Crypto as Crypto
import           Data.Blockchain.Core.Types

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
    Either.mapLeft BlockValidationException $ validateBlockTransactions genesisBlock mempty reward
    Either.mapLeft BlockValidationException $ validateBlockHeaderReferences genesisBlock
    Either.mapLeft BlockValidationException $ Foldable.foldlM (flip addBlock) blockchainHead blocks
  where
    getBlocks (BlockchainNode block ns) = block : (ns >>= getBlocks)


addBlock :: Block -> Blockchain Validated -> Either BlockException (Blockchain Validated)
addBlock newBlock (Blockchain config node) = Blockchain config <$> addBlockToNode mempty node
  where
    addBlockToNode :: [Block] -> BlockchainNode -> Either BlockException BlockchainNode
    addBlockToNode priorChain (BlockchainNode block nodes) =
        if isParentNode then do
            let siblingBlocks  = nodeBlock <$> nodes
                newNode        = BlockchainNode newBlock mempty
                updatedNode    = BlockchainNode block (newNode : nodes)
                height         = length previousBlocks + 1
                newBlockHeader = blockHeader newBlock

            verify (newBlock `notElem` siblingBlocks) BlockAlreadyExists
            validateBlockCreationTime newBlockHeader (blockHeader block)
            validateBlockDifficulty newBlockHeader config previousBlocks
            validateBlockTransactions newBlock previousBlocks (targetReward config $ fromIntegral height)
            validateBlockHeaderReferences newBlock

            return updatedNode
        else
            let eBlockchains = fmap (\bs -> Either.mapLeft (\e -> (e, bs)) (addBlockToNode previousBlocks bs)) nodes in
            BlockchainNode block <$> reduceAddBlockResults eBlockchains
      where
        previousBlocks = priorChain <> pure block
        isParentNode = Crypto.hash (blockHeader block) == prevBlockHeaderHash (blockHeader newBlock)

    reduceAddBlockResults :: [Either (BlockException, BlockchainNode) BlockchainNode] -> Either BlockException [BlockchainNode]
    reduceAddBlockResults results =
        case (rightResults, specificExceptions) of
            ([x], [])  -> Right (oldBlockChains <> pure x)
            ([],  [])  -> Left NoParentFound
            ([],  [e]) -> Left e
            (_,   _)   -> error "Impossible block insertion error"
      where
        (leftResults, rightResults)     = Either.partitionEithers results
        (allExceptions, oldBlockChains) = unzip leftResults
        specificExceptions              = filter (not . (==) NoParentFound) allExceptions


-- Exported Validators ---------------------------------------------------------------------------------------

-- TODO: transaction-specific exceptions
validateTransaction :: Blockchain Validated -> Transaction -> Either BlockException ()
validateTransaction chain = validateTransactions chain . pure

validateTransactions :: Blockchain Validated -> [Transaction] -> Either BlockException ()
validateTransactions chain = \case
    [] -> return () -- slight optimization - prevents having to calculate unspent transaction outputs
    xs -> let prevBlocks          = NonEmpty.toList (longestChain chain)
              unspentTransactions = unspentTransactionOutputsInternal prevBlocks
          in sequence_ $ validateTransactionInternal unspentTransactions <$> xs

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

blockHeaderHashDifficulty :: Hex256 -> BlockHeader -> Difficulty
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
validateBlockTransactions :: Block -> [Block] -> Word.Word -> Either BlockException ()
validateBlockTransactions (Block _header coinbaseTx txs) prevBlocks reward = do
    -- ensure coinbase transaction is of correct value
    -- TODO: coinbase can be reward + fees
    verify (txOutValue (coinbaseTransactionOut coinbaseTx) == reward) InvalidCoinbaseTransactionValue

    sequence_ (validateTransactionInternal unspentTransactions <$> txs)
  where
    unspentTransactions = unspentTransactionOutputsInternal prevBlocks

txOutValue :: NonEmpty.NonEmpty TransactionOut -> Word.Word
txOutValue = sum . fmap value

validateTransactionInternal :: H.HashMap TransactionOutRef TransactionOut -> Transaction -> Either BlockException ()
validateTransactionInternal unspentTransactions (Transaction txIn txOut) = do
    prevTxOut <- sequence $ flip fmap txIn $ \(TransactionIn ref sig) -> do
        tx <- maybeToEither TransactionOutRefNotFound (H.lookup ref unspentTransactions)
        verify (verifyTransactionSignature sig tx) InvalidTransactionSignature
        return tx

    verify (txOutValue prevTxOut >= txOutValue txOut) InvalidTransactionValues


-- Transaction State -----------------------------------------------------------------------------------------

addressValues :: Blockchain Validated -> H.HashMap Crypto.PublicKey Word.Word
addressValues blockchain = H.fromListWith (+) (toPair <$> unspentTxOuts)
  where
    toPair (TransactionOut value pubKey) = (pubKey, value)
    unspentTxOuts = H.elems $ unspentTransactionOutputsInternal (NonEmpty.toList $ longestChain blockchain)

unspentTransactionOutputs :: Blockchain Validated -> H.HashMap Crypto.PublicKey [(TransactionOutRef, TransactionOut)]
unspentTransactionOutputs blockchain = H.fromListWith (<>) (toPair <$> unspentTxOuts)
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
    addCoinbaseTransaction coinbase = H.unionWith onDuplicate coinbaseTxOutRefMap
      where
        -- TODO: revisit what it means to have duplicate coinbase transaction refs... probably ok?
        onDuplicate (TransactionOut v1 key) (TransactionOut v2 _) = TransactionOut (v1 + v2) key
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

    onDuplicateTxOutRef txOutRef = error ("Unexpected error when computing transaction map - duplicate transaction: " <> show txOutRef)
    onNotFoundTxOutRef  txOutRef = error ("Unexpected error when computing transaction map - transaction not found: " <> show txOutRef)


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
verify cond = unless cond . Left

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither e = maybe (Left e) Right

stripFieldPrefix :: String -> Aeson.Options
stripFieldPrefix str = Aeson.defaultOptions { Aeson.fieldLabelModifier = stripPrefix }
  where
    stripPrefix x = maybe x lowercase (List.stripPrefix str x)
    lowercase = \case []     -> []
                      (x:xs) -> Char.toLower x : xs
