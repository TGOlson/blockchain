module Data.Blockchain
    ( Blockchain
    , SingleChain(..)
    , AddBlockException(..)
    , singleton
    , addBlock
    , mainChain
    , unspentTransactionOutputs
    , toString

    -- Testing utilities
    , BlockchainSpec(..)
    , BlockchainSpecException(..)
    , toSpec
    , fromSpec
    , (~~)
    ) where

import qualified Control.Arrow           as Arrow
import qualified Data.Either             as Either (partitionEithers)
import qualified Data.Either.Combinators as Either
import qualified Data.Foldable           as Foldable
import qualified Data.HashMap.Strict     as H
import qualified Data.List               as List
import qualified Data.Ord                as Ord

import Data.Blockchain.Crypto.Hash
import Data.Blockchain.Crypto.ECDSA
import Data.Blockchain.Types

-- TODO: might need to have a type that represents a sub-node
-- So that we can encode the invariant that a genesis block must exist
data Blockchain = BlockchainNode Block [Blockchain]
  deriving (Eq, Show)

newtype SingleChain = SingleChain { unSingleChain :: [Block] }
  deriving (Eq, Show)

data AddBlockException
    = BlockAlreadyExists
    | NoPreviousBlockFound
  deriving (Eq, Show)

singleton :: Block -> Blockchain
singleton block = BlockchainNode block []


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

-- addBlockInternal :: Block -> Blockchain -> Either AddBlockException (Blockchain, SingleChain)

-- verify :: UnverifiedBlockchain -> Either BlockchainException Blockchain
--

-- TODO: probably needs `prevChain :: [Block]` in order to validate transactions
addBlock :: Block -> Blockchain -> Either AddBlockException Blockchain
addBlock newBlock (BlockchainNode block blockchains) =
    -- TODO: block headers should contain a hash of themselves,
    -- so that we don't have to hash every single time
    -- Found correct parent node
    if hash (blockHeader block) == prevBlockHeaderHash (blockHeader newBlock)
        then
            -- But first make sure it's not already in the leaves
            if any (\(BlockchainNode blk _) -> blk == newBlock) blockchains
                then Left BlockAlreadyExists
                -- TODO: need to verify transactions fit into blockchain
                else Right (BlockchainNode block $ singleton newBlock : blockchains)
        else
            let eBlockchains = fmap (\bs -> Either.mapLeft (\e -> (e, bs)) (addBlock newBlock bs)) blockchains in
            BlockchainNode block <$> reduceAddBlockResults eBlockchains
  where
    -- Rules:
    --   If all results are `Left NoPreviousBlockFound` the result is `Left NoPreviousBlockFound`.
    --   If any result is `Left BlockAlreadyExists` the result is `Left BlockAlreadyExists`.
    --   If one result is `Right Blockchain` and the rest are `Left NoPreviousBlockFound`
    --      the result is that new block chain and all the previous chains.
    --   If more than one result is `Right Blockchain` it is an unexpected result and the function will error.
    reduceAddBlockResults :: [Either (AddBlockException, Blockchain) Blockchain] -> Either AddBlockException [Blockchain]
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

flatten :: Blockchain -> [SingleChain]
flatten = \case
    BlockchainNode block []  -> pure $ SingleChain (pure block)
    BlockchainNode block bcs -> (\(SingleChain bc) -> SingleChain (block : bc)) <$> concatMap flatten bcs

-- TODO: need to compare difficulty if two chains have the same length
mainChain :: Blockchain -> SingleChain
mainChain = List.maximumBy (Ord.comparing (length . unSingleChain)) . flatten

-- TODO: comment
-- an intermediate step may be to create (HashMap TxOut (Maybe TxIn))
-- this would also be useful for signature verification

-- type TransactionMap = H.HashMap (Hash Transaction) (H.HashMap Int TransactionOut)
--
-- data TransactionListException
--     = ReferencedTransactionOutDoesNotExist [TransactionIn]
--     | InvalidSignature [TransactionIn]
--     | InputValuesLessThanOutput [Transaction]
--
-- transactionMap :: [Transaction] -> Either [TransactionListException] TransactionMap
-- transactionMap = undefined
--

-- data TransactionIn = TransactionIn
--     { previousTransactionHash     :: Hash Transaction -- Hash of full Transaction
--     , previousTransactionOutIndex :: Int
--     , signature                   :: Signature -- Signature from prev transaction, using pubkey from prev transaction
--     }
--
-- data TransactionOut = TransactionOut
--     { value           :: Int
--     , signaturePubKey :: PublicKey -- aka. address of where funds go
--     }

-- TODO: tis wrong
-- all txin are swept and then sent to txout
-- with the rest being fees
-- the signature of this function is correct, but the logic is not
-- also, probably needs to return an error type, in the case of a negative value
-- (that is, cumulative txout is more than txin value)
unspentTransactionOutputs :: TransactionMap -> H.HashMap PublicKey Int
unspentTransactionOutputs = H.fromList . fmap (signaturePubKey Arrow.&&& value) . concatMap H.elems . H.elems
  --   toUnspectTxs $ foldr reduceTxs initialTxOutMap txIns
  -- where
  --   reduceTxs (TransactionIn txHash txOutIdx _sig) = H.update (Just . H.delete txOutIdx) txHash
  --   toUnspectTxs :: H.HashMap (Hash Transaction) (H.HashMap Int TransactionOut) -> H.HashMap PublicKey Int
  --   toUnspectTxs =
  --   txs = blocks >>= transactions
  --   txIns = txs >>= transactionIn
  --   -- txOuts   = transactionOut <$> txs
  --   initialTxOutMap :: H.HashMap (Hash Transaction) (H.HashMap Int TransactionOut)
  --   initialTxOutMap = H.fromList $ fmap (hash Arrow.&&& (innerTxOutMap . transactionOut)) txs
  --     where
  --       innerTxOutMap = H.fromList . zip [0..]

toString :: Blockchain -> String
toString = List.intercalate "\n" . toStringLevels 0
  where
    toStringLevels :: Int -> Blockchain -> [String]
    toStringLevels level (BlockchainNode block blockchains) =
        hashString : concatMap (toStringLevels (level + 2)) blockchains
      where
        spaces = replicate level ' '
        hashString = spaces ++ show (hash $ blockHeader block)

-- TestUtils
--
-- Provides a useful api for constructing arbitrary blockchains during testing
-- without exposing the core Blockchain data type

data BlockchainSpec = BlockchainSpec Block [BlockchainSpec]
  deriving (Eq, Show)

newtype BlockchainSpecException = BlockchainSpecException AddBlockException

toSpec :: Blockchain -> BlockchainSpec
toSpec (BlockchainNode block blockchains) = BlockchainSpec block $ toSpec <$> blockchains

fromSpec :: BlockchainSpec -> Either AddBlockException Blockchain
fromSpec (BlockchainSpec block blockchainSpecs) = addSpecs blockchainSpecs (singleton block)
  where
    addSpecs specs chain = Foldable.foldrM reduce chain specs
    reduce (BlockchainSpec blk specs) chain = addBlock blk chain >>= addSpecs specs

(~~) :: Block -> [BlockchainSpec] -> BlockchainSpec
(~~) = BlockchainSpec
