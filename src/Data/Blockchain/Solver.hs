module Data.Blockchain.Solver
    ( findNextBlock
    , isValidBlockHeader
    ) where

import qualified Data.Time.Clock as Time

import Data.Blockchain.Crypto.Hash
import Data.Blockchain.Types


-- TODO: should this be in IO and find the current time?
-- Also, should it take in a list of transactions, or is creating the coinbase transaction
-- part of the utility of this function?
findNextBlock :: BlockHeaderHash -> Time.UTCTime -> Difficulty -> Block
findNextBlock prevBlockHeaderHash time difficulty = searchForValidBlockHeader BlockHeader{..}
  where
    version = 1
    nonce   = 0
    transactions = [] :: [Transaction]
    transactionHashTreeRoot = hash "" -- TODO
    searchForValidBlockHeader header =
        if isValidBlockHeader header
            then Block header transactions
            else searchForValidBlockHeader (incNonce header)

isValidBlockHeader :: BlockHeader -> Bool
isValidBlockHeader blockHeader = toHash blockHeader < unDifficulty (difficulty blockHeader)
