module Data.BlockchainSpec (spec) where

import TestUtil

import qualified Data.Either.Combinators as Either

import Data.Blockchain
import Data.Blockchain.Crypto.Hash
import Data.Blockchain.Types

forceSpec :: Either a Blockchain -> BlockchainSpec
forceSpec = \case Left _   -> error "cannot create spec from left value"
                  Right bc -> toSpec bc

linkBlockToPrev :: Block -> Block -> Block
linkBlockToPrev prevBlock (Block bh transactions) = Block newBlockHeader transactions
  where newBlockHeader = bh { prevBlockHeaderHash = hash (blockHeader prevBlock) }


data LinkedBlocks = LinkedBlocks Block Block
  deriving (Eq, Show)

instance Arbitrary LinkedBlocks where
    arbitrary = do
        block1 <- arbitrary
        block2 <- arbitrary

        return $ LinkedBlocks block1 (linkBlockToPrev block1 block2)

spec :: Spec
spec =
    describe "addBlock" $ do
        prop "should not add a block that does not reference the previous block" $ once $
            \block1 block2 -> addBlock block2 (singleton block1) == Left NoPreviousBlockFound

        prop "should not add a block that already exists in a set of leaves" $ once $
            \(LinkedBlocks block1 block2) ->
                (addBlock block2 (singleton block1) >>= addBlock block2) == Left BlockAlreadyExists

        prop "should not add a block that has no parent" $ once $
            \(LinkedBlocks block1 block2) block3 ->
                (addBlock block2 (singleton block1) >>= addBlock block3) == Left NoPreviousBlockFound

        prop "should add a block that fits at the end of an empty chain" $ once $
            \(LinkedBlocks block1 block2) ->
                forceSpec (addBlock block2 (singleton block1)) ==
                    block1 ~~
                        [ block2 ~~ [] ]

        prop "should add a block that forks the chain" $ once $
            \(LinkedBlocks block1 block2) unlinkedBlock ->
                let block3 = linkBlockToPrev block1 unlinkedBlock in
                forceSpec (addBlock block2 (singleton block1) >>= addBlock block3) ==
                    block1 ~~
                        [ block3 ~~ []
                        , block2 ~~ []
                        ]

        prop "should add a block that forks the middle of a chain" $ once $
            \(LinkedBlocks block1 block2) unlinkedBlock3 unlinkedBlock4 unlinkedBlock5 -> Either.fromRight False $ do
                let block3 = linkBlockToPrev block2 unlinkedBlock3
                    block4 = linkBlockToPrev block3 unlinkedBlock4
                    block5 = linkBlockToPrev block2 unlinkedBlock5

                chain1 <- addBlock block2 (singleton block1)
                chain2 <- addBlock block3 chain1
                chain3 <- addBlock block4 chain2
                chain4 <- addBlock block5 chain3

                return $ toSpec chain4 ==
                    block1 ~~
                        [ block2 ~~
                            [ block5 ~~ []
                            , block3 ~~ [ block4 ~~ [] ]
                            ]
                        ]
