module Data.Blockchain.SolverSpec (spec) where

import TestUtil

import Data.Blockchain.Solver
import Data.Blockchain.Crypto.Hash (toHash)
import Data.Blockchain.Types

spec :: Spec
spec =
    describe "Solver" $
        context "newBlock" $
            prop "should find blocks with a lower hash than the difficulty" $
                \blockHash time hash -> hash > minHash ==>
                    toHash (blockHeader $ findNextBlock blockHash time (Difficulty hash)) < hash
