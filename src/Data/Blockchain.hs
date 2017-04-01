module Data.Blockchain
    ( Blockchain
    , singleton
    , addBlock
    , longestChain
    , AddBlockException(..)

    -- Testing utilities
    , BlockchainSpec(..)
    , toSpec
    , fromSpec
    , (~~)
    ) where

import qualified Data.Either             as Either (partitionEithers)
import qualified Data.Either.Combinators as Either
import qualified Data.Foldable           as Foldable
import qualified Data.List               as List
import qualified Data.Ord                as Ord


import Data.Blockchain.Crypto.Hash
import Data.Blockchain.Types

data Blockchain = BlockchainNode Block [Blockchain]
  deriving (Eq, Show)

data AddBlockException
    = BlockAlreadyExists
    | NoPreviousBlockFound
  deriving (Eq, Show)

singleton :: Block -> Blockchain
singleton block = BlockchainNode block []

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
        (_, _)       -> error "Unexpected error - block can be interested into multiple chains"
      where
        (leftResults, rightResults) = Either.partitionEithers results
        (exceptions, oldBlockChains) = unzip leftResults
        -- Note: this ignores invariant where multiple `BlockAlreadyExists` errors are found
        -- However, we do expect our reducing function to monitor for that invariant during original insert.
        blockAlreadyExists = any (== BlockAlreadyExists) exceptions

flatten :: Blockchain -> [[Block]]
flatten = \case
    BlockchainNode block [] -> [[block]]
    BlockchainNode block blockchains  -> (\bc -> block : bc) <$> concatMap flatten blockchains


longestChain :: Blockchain -> [Block]
longestChain = List.maximumBy (Ord.comparing length) . flatten


-- Testing utils
-- Generally only useful for testing purposes

-- TODO: fromSpec, that validates structure!!!

data BlockchainSpec = BlockchainSpec Block [BlockchainSpec]
  deriving (Eq, Show)

toSpec :: Blockchain -> BlockchainSpec
toSpec (BlockchainNode block blockchains) = BlockchainSpec block $ toSpec <$> blockchains

fromSpec :: BlockchainSpec -> Either AddBlockException Blockchain
fromSpec (BlockchainSpec block blockchainSpecs) = addSpecs blockchainSpecs (singleton block)
  where
    addSpecs specs chain = Foldable.foldrM reduce chain specs
    reduce (BlockchainSpec blk specs) chain = addBlock blk chain >>= addSpecs specs

(~~) :: Block -> [BlockchainSpec] -> BlockchainSpec
(~~) = BlockchainSpec
