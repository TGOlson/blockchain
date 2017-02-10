module Data.Blockchain.SolverSpec (spec) where

import TestUtil

import Data.Blockchain.Solver
import Data.Blockchain.Crypto.Hash (toHash)
import Data.Blockchain.Types.Difficulty

spec :: Spec
spec =
    describe "Solver" $
        context "newBlock" $
            prop "should find blocks with a lower hash than the difficulty" $
                \blockHash hash -> hash > minHash ==>
                    toHash (findNextBlock blockHash (Difficulty hash)) < hash
