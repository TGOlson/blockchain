module Data.Blockchain.Solver
    ( findNextBlock
    ) where

import qualified Data.Time.Clock as Time

import Data.Blockchain.Crypto.Hash
import Data.Blockchain.Types


-- TODO: should this be in IO and find the current time?
-- Note: this also does not modify the coinbase transaction incase
-- of an int overflow on the nonce... TODO
findNextBlock :: Hash BlockHeader -> Time.UTCTime -> Difficulty -> [Transaction] -> Block
findNextBlock = undefined
-- findNextBlock prevBlockHeaderHash time difficulty transactions = findNextBlockInternal startingBlock
  -- where
  --   nonce = 0
  --   startingBlock = makeBlock prevBlockHeaderHash time difficulty nonce transactions
  --   findNextBlockInternal :: Block -> Block
  --   findNextBlockInternal block =
  --       if isValidBlockDifficulty difficulty block
  --           then block
  --           else findNextBlockInternal (incHeaderNonce block)
