module Data.Blockchain
    ( Blockchain
    , singleton
    , addBlock
    , longestChain
    , AddBlockException(..)

    -- Testing utilities
    , BlockchainSpec(..)
    , toSpec
    , (~~)
    ) where

import qualified Data.Either as Either
import qualified Data.List   as List
import qualified Data.Ord    as Ord

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
    if block == newBlock then Left BlockAlreadyExists
    else if hash (blockHeader block) == prevBlockHeaderHash (blockHeader newBlock)
        then Right (BlockchainNode block $ singleton newBlock : blockchains)
        else let newBlockchains = fmap (addBlock newBlock) blockchains
            -- can we use: lefts :: [Either a b] -> [a]
            -- here to cleanup?
            -- or: partitionEithers :: [Either a b] -> ([a], [b])
            in if any Either.isRight newBlockchains
                then Right $ BlockchainNode block $ zipWith (\eBc bc -> Either.either (const bc) id eBc) newBlockchains blockchains
                -- this eats other errors, possible to have BlockAlreadyExists take preference?
                else Left NoPreviousBlockFound


flatten :: Blockchain -> [[Block]]
flatten = \case
    BlockchainNode block [] -> [[block]]
    BlockchainNode block blockchains  -> (\bc -> block : bc) <$> concatMap flatten blockchains


longestChain :: Blockchain -> [Block]
longestChain = List.maximumBy (Ord.comparing length) . flatten


-- Testing utils
-- Generally only useful for testing purposes

-- TODO: fromSpec, that validates structure!!!

data BlockchainSpec = BlockchainSpec (Block, [BlockchainSpec])
  deriving (Eq, Show)

toSpec :: Blockchain -> BlockchainSpec
toSpec (BlockchainNode block blockchains) = BlockchainSpec (block, toSpec <$> blockchains)

(~~) :: Block -> [BlockchainSpec] -> BlockchainSpec
(~~) block specs = BlockchainSpec (block, specs)
