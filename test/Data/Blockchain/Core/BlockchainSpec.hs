module Data.Blockchain.Core.BlockchainSpec (spec) where

import           TestUtil

import qualified Control.Arrow                   as Arrow
import qualified Data.Aeson                      as Aeson
import qualified Data.HashMap.Strict             as H
import qualified Data.List.NonEmpty              as NonEmpty

import           Data.Blockchain.Core.Blockchain
import           Data.Blockchain.Core.Types

spec :: Spec
spec = describe "Data.Blockchain.Core.Blockchain" $ do
    it "should serialize and validate round-trip" $ once $ ioProperty $ do
        chain <- blockchain3Block

        let unverifiedBlockchain = throwLeft $ Aeson.eitherDecode (Aeson.encode chain)

        return $ validate unverifiedBlockchain === Right chain

    describe "validate" $ do
        it "should reject a chain with invalid difficulty reference in genesis block" $ once $ ioProperty $ do
            chain <- singletonBlockchainUnvalidated
            let config = (blockchainConfig chain) { initialDifficulty = minBound }
                chain' = construct config (blockchainNode chain)

            return $ validate chain' === Left (BlockValidationException InvalidDifficultyReference)

        it "should reject a chain with invalid genesis block difficulty" $ once $ ioProperty $ do
            chain <- singletonBlockchainUnvalidated

            let (BlockchainNode block nodes) = blockchainNode chain
                blockHeader' = (blockHeader block) { nonce = 1 }
                block'       = block { blockHeader = blockHeader' }
                chain'       = construct (blockchainConfig chain) (BlockchainNode block' nodes)

            return $ validate chain' === Left (BlockValidationException InvalidDifficulty)

        it "should reject a chain with transactions in genesis block" $ once $
            \tx -> ioProperty $ do
                chain <- singletonBlockchainUnvalidated

                let (BlockchainNode block nodes) = blockchainNode chain
                    block' = block { transactions = pure tx }
                    chain' = construct (blockchainConfig chain) (BlockchainNode block' nodes)

                return $ validate chain' === Left GenesisBlockHasTransactions

        it "should reject a chain with invalid coinbase reward in genesis block" $ once $
            \txOut -> ioProperty $ do
                chain <- singletonBlockchainUnvalidated

                let (BlockchainNode block nodes) = blockchainNode chain
                    coinbase = CoinbaseTransaction $ pure $ txOut { value = 999 }
                    block'   = block { coinbaseTransaction = coinbase }
                    chain'   = construct (blockchainConfig chain) (BlockchainNode block' nodes)

                return $ validate chain' === Left (BlockValidationException InvalidCoinbaseTransactionValue)

        it "should reject a chain with invalid coinbase hash in genesis block header" $ once $
            \txOut -> ioProperty $ do
                chain <- singletonBlockchainUnvalidated

                let (BlockchainNode block nodes) = blockchainNode chain
                    coinbase = CoinbaseTransaction $ pure $ txOut { value = 100 }
                    block'   = block { coinbaseTransaction = coinbase }
                    chain'   = construct (blockchainConfig chain) (BlockchainNode block' nodes)

                return $ validate chain' === Left (BlockValidationException InvalidCoinbaseTransactionHash)

        -- TODO: test if possible, hard to do with empty transaction rule & expected header hash
        -- it "should reject a chain with invalid transaction hash in genesis block header" $ once $

    describe "addBlock" $ do
        -- Note: adding valid blocks is already tested by generating by loading test data
        -- it "should add a valid block" ...

        it "should reject a duplicate block" $ once $ ioProperty $ do
            blockchain <- blockchain1Block
            block      <- block1A

            return $ addBlock block blockchain === Left BlockAlreadyExists

        it "should reject a block without a parent" $ once $ ioProperty $ do
            blockchain <- singletonBlockchain
            block      <- block2A

            return $ addBlock block blockchain === Left NoParentFound

        it "should reject a chain with invalid genesis block difficulty" $ once $ ioProperty $ do
            blockchain                  <- blockchain1Block
            (Block header coinbase txs) <- block2A

            let header' = header { nonce = 1 }
                block   = Block header' coinbase txs

            return $ addBlock block blockchain === Left InvalidDifficulty

        it "should reject a chain with invalid coinbase reward in block" $ once $
            \txOut -> ioProperty $ do
                blockchain                   <- blockchain1Block
                (Block header _coinbase txs) <- block2A

                let coinbase = CoinbaseTransaction $ pure $ txOut { value = 999 }
                    block    = Block header coinbase txs

                return $ addBlock block blockchain === Left InvalidCoinbaseTransactionValue

        it "should reject a chain with invalid coinbase hash in block header" $ once $
            \txOut -> ioProperty $ do
                blockchain                   <- blockchain1Block
                (Block header _coinbase txs) <- block2A

                let coinbase = CoinbaseTransaction $ pure $ txOut { value = 100 }
                    block    = Block header coinbase txs

                return $ addBlock block blockchain === Left InvalidCoinbaseTransactionHash

        -- it "should reject a chain with invalid transaction hash in block header" $ once $
        --     \tx -> ioProperty $ do
        --         (blockchain, block) <- loadVerifiedTestBlockchainWithValidBlock
        --         let block' = block { transactions = pure tx }
        --
        --         return $ addBlock block' blockchain === Left InvalidTransactionHashTreeRoot

        -- TODO: transaction testing.........

    describe "addressValues" $
        it "should calculate unspent transaction outputs" $ once $ ioProperty $ do
            blockchain <- blockchain3Block

            return $ showKeys (addressValues blockchain) === H.fromList
                -- genesis coinbase, value can never be spent
                [ ("8330da21047df515590bc752ac27210d3eb11bc1ec1b4dcb8f6c6b0b018fb5d44feb11798ca599b0c1549146508933833c4553d420504bef2c5f96dd42e143df", 100)
                -- mined 1st and 2nd block, spent 90 of 1st block coinbase
                , ("74bbe6d5f70f7d7e433b9b6d5d77a391492cc93161d9f108cef18d64959930cf1d55ef0844c0607f5568d1cd9233995154b5758915eb595e1dee472d13a18517", 110)
                -- received 90 of 1st block coinbase
                , ("c21609f09388037802a7c58e8135e264e8f4bac3354ee300ee73b887a7976a94614e77bcec828579562b9b3c0ac6f0de7323f3f5bcfdb3dfb190cfca1692d735", 90)
                ]

    describe "flatten" $
        it "should flatten the blockchain" $ once $ ioProperty $ do
            blockchain <- blockchain3Block
            b0         <- genesisBlock
            b1a        <- block1A
            b1b        <- block1B
            b2a        <- block2A

            return $ flatten blockchain === NonEmpty.fromList
                    [ NonEmpty.fromList [b0, b1b]
                    , NonEmpty.fromList [b0, b1a, b2a]
                    ]

    describe "longestChain" $
        it "should find the longest chain" $ once $ ioProperty $ do
            blockchain   <- blockchain1Block
            blockchain'  <- blockchain2BlockFork
            blockchain'' <- blockchain3Block
            b0           <- genesisBlock
            b1a          <- block1A
            b2a          <- block2A

            return $ (longestChain blockchain   == NonEmpty.fromList [b0, b1a]) &&
                     (longestChain blockchain'  == NonEmpty.fromList [b0, b1a]) &&
                     (longestChain blockchain'' == NonEmpty.fromList [b0, b1a, b2a])

showKeys :: Show k => H.HashMap k v -> H.HashMap String v
showKeys = H.fromList . fmap (Arrow.first show) . H.toList
