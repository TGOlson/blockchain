module Data.BlockchainSpec (spec) where

import TestUtil

import Data.Blockchain
import Data.Blockchain.Crypto.Hash
import Data.Blockchain.Types

spec :: Spec
spec =
    describe "addBlock" $ do
        prop "should not add a block that does not reference the previous block" $ once $
            \block newBlock -> addBlock newBlock (singleton block) == Left NoPreviousBlockFound
        prop "should add a block that fits at the end of an empty chain" $ once $
            \block bh ->
                let newBlock = Block (bh { prevBlockHeaderHash = hash (blockHeader block) }) [] in -- TODO arbitrary transactions
                    (toSpec <$> addBlock newBlock (singleton block)) == Right (
                        block ~~
                            [ newBlock ~~ [] ]
                    )
        prop "should add a block that forks the chain" $ once $
            \block bh bh2 ->
                let newBlock  = Block (bh { prevBlockHeaderHash = hash (blockHeader block) }) []
                    newBlock2 = Block (bh2 { prevBlockHeaderHash = hash (blockHeader block) }) []
                    eBlockchain = addBlock newBlock (singleton block) in -- TODO arbitrary transactions
                    fmap toSpec (eBlockchain >>= \bc -> addBlock newBlock2 bc) == Right (
                        block ~~
                            [ newBlock2 ~~ []
                            , newBlock  ~~ []
                            ]
                    )
        -- prop "should not a block that already exists" $ once $
        --     \block bh ->
        --         let newBlock  = Block (bh { prevBlockHeaderHash = hash (blockHeader block) }) []
        --             eBlockchain = addBlock newBlock (singleton block) in -- TODO arbitrary transactions
        --             (eBlockchain >>= \bc -> addBlock newBlock bc) == Left BlockAlreadyExists
