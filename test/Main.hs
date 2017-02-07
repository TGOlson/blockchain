module Main where

import qualified Test.Hspec as Hspec

import qualified Data.Blockchain.SolverSpec

main :: IO ()
main = mapM_ Hspec.hspec
    [ Data.Blockchain.SolverSpec.spec ]
--     putStrLn "Block difficulty"
--     print (unDifficulty $ blockDifficulty block)
--     let nextBlock = findNextBlock block
--     putStrLn "Next block hash"
--     print (toHash nextBlock)
--     putStrLn "Next block nonce"
--     print (blockHeaderNonce $ blockHeader nextBlock)
--
-- -- solverTest ::
-- solverTests :: Spec
-- solverTests = describe "Solver.newBlock" $
--     it "foo" $ property $ 'a' == 'a'
--
-- block :: Block
-- block = newBlock prevBlockHash difficulty
--   where
--     prevBlockHash = hash "seed"
--     difficulty    = Difficulty (h "0000ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")
--     h = fromMaybe (error "Invalid hash string") . fromByteString
--
--
--
-- eqInt :: Int -> Property
-- eqInt i = i === i
