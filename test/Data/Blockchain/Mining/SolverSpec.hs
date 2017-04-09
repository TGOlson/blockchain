module Data.Blockchain.Mining.SolverSpec (spec) where

import TestUtil

-- import Data.Blockchain.Crypto.Hash
-- import Data.Blockchain.Solver
-- import Data.Blockchain.Types

spec :: Spec
spec = return ()
-- spec =
--     describe "Solver" $
--         context "newBlock" $
--             prop "should find blocks with a lower hash than the difficulty" $
--                 \blockHash time diff -> diff > maxDifficulty ==>
--                     let block      = findNextBlock blockHash time diff []
--                         headerHash = hash (blockHeader block) in
--                     rawHash headerHash < rawHash (unDifficulty diff)
