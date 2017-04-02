module Data.BlockchainSpec (spec) where

import TestUtil

import qualified Data.Either.Combinators as Either
import qualified Data.Foldable           as Foldable
import qualified Data.List               as List

import Data.Blockchain
import Data.Blockchain.Crypto.Hash
import Data.Blockchain.Types

-- TODO: good errors when something fails
-- print out graphical chain
assertTransactions :: [Blockchain -> Either AddBlockException Blockchain] -> BlockchainSpec -> BlockchainSpec -> Bool
assertTransactions transactions initialSpec targetSpec =
    assertTransactionsResult transactions initialSpec (fromSpec targetSpec)

assertTransactionsResult
    :: [Blockchain -> Either AddBlockException Blockchain]
    -> BlockchainSpec
    -> Either AddBlockException Blockchain -> Bool
assertTransactionsResult transactions initialSpec result = Either.fromRight False $ do
    chain  <- fromSpec initialSpec
    return $ Foldable.foldrM ($) chain transactions == result

-- Note: this makes the block invalid
-- May need to be changed once blockchain gets more advanced
linkBlockToPrev :: Block -> Block -> Block
linkBlockToPrev prevBlock block = makeBlock prevBlockHeaderHash t diff nonce (transactions block)
  where
    (BlockHeader _ _ _ t diff nonce) = blockHeader block
    prevBlockHeaderHash = hash (blockHeader prevBlock)

data LinkedBlocks = LinkedBlocks Block Block
  deriving (Eq, Show)

data LinkedBlocks3 = LinkedBlocks3 Block Block Block
  deriving (Eq, Show)

instance Arbitrary LinkedBlocks where
    arbitrary = do
        block1 <- arbitrary
        block2 <- linkBlockToPrev block1 <$> arbitrary

        return $ LinkedBlocks block1 block2

instance Arbitrary LinkedBlocks3 where
    arbitrary = do
        block1 <- arbitrary
        block2 <- linkBlockToPrev block1 <$> arbitrary
        block3 <- linkBlockToPrev block2 <$> arbitrary

        return $ LinkedBlocks3 block1 block2 block3

spec :: Spec
spec = do
    describe "addBlock" $ do
        prop "should not add a block that does not reference the previous block" $ once $
            \block1 block2 ->
                assertTransactionsResult [ addBlock block2 ]
                    (block1 ~~ [])
                    $ Left NoPreviousBlockFound

        prop "should not add a block that already exists in a set of leaves" $ once $
            \(LinkedBlocks block1 block2) ->
                assertTransactionsResult [ addBlock block2 ]
                    (block1 ~~ [ block2 ~~ [] ])
                    $ Left BlockAlreadyExists

        prop "should not add a block that has no parent" $ once $
            \(LinkedBlocks block1 block2) block3 ->
                assertTransactionsResult [ addBlock block3 ]
                    (block1 ~~ [ block2 ~~ [] ])
                    $ Left NoPreviousBlockFound

        prop "should add a block that fits at the end of an empty chain" $ once $
            \(LinkedBlocks block1 block2) ->
                assertTransactions [ addBlock block2 ]
                    (block1 ~~ [])
                    (block1 ~~ [ block2 ~~ [] ])

        prop "should add a block that forks the chain" $ once $
            \(LinkedBlocks block1 block2) unlinkedBlock3 ->
                let block3 = linkBlockToPrev block1 unlinkedBlock3 in

                assertTransactions [ addBlock block3 ]
                    (block1 ~~ [ block2 ~~ [] ])
                    (block1 ~~
                        [ block3 ~~ []
                        , block2 ~~ []
                        ])

        prop "should add a block that forks the middle of a chain" $ once $
            \(LinkedBlocks3 block1 block2 block3) unlinkedBlock4 -> do
                let block4 = linkBlockToPrev block1 unlinkedBlock4

                assertTransactions [ addBlock block4 ]
                    (block1 ~~
                        [ block2 ~~ [ block3 ~~ [] ]
                        ])
                    (block1 ~~
                        [ block4 ~~ []
                        , block2 ~~ [ block3 ~~ [] ]
                        ])

    describe "toString" $
        prop "should pretty print the blockchain" $ once $
            \(LinkedBlocks3 block1 block2 block3) unlinkedBlock4 ->
                let block4 = linkBlockToPrev block1 unlinkedBlock4
                    showHash = show . hash . blockHeader
                    eResult = fmap toString $ fromSpec $
                        block1 ~~
                              [ block4 ~~ []
                              , block2 ~~ [ block3 ~~ [] ]
                              ] in

                eResult == (Right $ List.intercalate "\n" [
                        showHash block1,
                        "  " ++ showHash block4,
                        "  " ++ showHash block2,
                        "    " ++ showHash block3
                    ])
