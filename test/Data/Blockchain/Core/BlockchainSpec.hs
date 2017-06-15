module Data.Blockchain.Core.BlockchainSpec (spec) where

import           TestUtil

import qualified Control.Arrow                   as Arrow
import qualified Data.Aeson                      as Aeson
import qualified Data.HashMap.Strict             as H
import qualified Data.List.NonEmpty              as NonEmpty

import           Data.Blockchain.Core.Blockchain
import           Data.Blockchain.Core.Crypto
import           Data.Blockchain.Core.Types

spec :: Spec
spec = describe "Data.Blockchain.Core.Blockchain" $ do
    it "should serialize round-trip" $ once $ ioProperty $ do
        chain <- singletonBlockchain

        let unverifiedBlockchain = throwLeft $ Aeson.eitherDecode (Aeson.encode chain)

        return $ validate unverifiedBlockchain === Right chain

    describe "validate" $ do
        it "should reject a chain with invalid difficulty reference in genesis block" $ once $ ioProperty $ do
            chain <- singletonBlockchainUnvalidated
            let config  = blockchainConfig chain
                config' = config { initialDifficulty = minBound }
                chain'  = construct config' (blockchainNode chain)

            return $ validate chain' === Left (BlockValidationException InvalidDifficultyReference)

        -- Note: this is a known modification that will change block hash to make it invalid
        -- if test data is re-generated, it may cause this test to fail
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
                    txOut'   = txOut { value = 999 }
                    coinbase = CoinbaseTransaction (pure txOut')
                    block'   = block { coinbaseTransaction = coinbase }
                    chain'   = construct (blockchainConfig chain) (BlockchainNode block' nodes)

                return $ validate chain' === Left (BlockValidationException InvalidCoinbaseTransactionValue)

        it "should reject a chain with invalid coinbase hash in genesis block header" $ once $
            \txOut -> ioProperty $ do
                chain <- singletonBlockchainUnvalidated

                let (BlockchainNode block nodes) = blockchainNode chain
                    txOut'   = txOut { value = 100 }
                    coinbase = CoinbaseTransaction (pure txOut')
                    block'   = block { coinbaseTransaction = coinbase }
                    chain'   = construct (blockchainConfig chain) (BlockchainNode block' nodes)

                return $ validate chain' === Left (BlockValidationException InvalidCoinbaseTransactionHash)

        -- TODO: test is possible, hard to do with empty transaction rule & expected header hash
        -- it "should reject a chain with invalid transaction hash in genesis block header" $ once $

    describe "addBlock" $ do
        it "should add a valid block" $ once $ ioProperty $ do
            blockchain <- singletonBlockchain
            block      <- block1A

            let mainChain = longestChain $ throwLeft (addBlock block blockchain)

            return $ length mainChain == 2 && NonEmpty.last mainChain == block

        it "should reject a duplicate block" $ once $ ioProperty $ do
            blockchain <- singletonBlockchain
            block      <- block1A

            let blockchain' = addBlock block $ throwLeft (addBlock block blockchain)

            return $ blockchain' === Left BlockAlreadyExists

        -- TODO: block without parent

        -- Note: this is a known modification that will change block hash to make it invalid
        -- if test data is re-generated, it may cause this test to fail
        it "should reject a chain with invalid genesis block difficulty" $ once $ ioProperty $ do
            blockchain <- singletonBlockchain
            block      <- block1A

            let blockHeader' = (blockHeader block) { nonce = 1 }
                block'       = block { blockHeader = blockHeader' }

            return $ addBlock block' blockchain === Left InvalidDifficulty

        it "should reject a chain with invalid coinbase reward in block" $ once $
            \txOut -> ioProperty $ do
                blockchain <- singletonBlockchain
                block      <- block1A

                let txOut'   = txOut { value = 999 }
                    coinbase = CoinbaseTransaction (pure txOut')
                    block'   = block { coinbaseTransaction = coinbase }

                return $ addBlock block' blockchain === Left InvalidCoinbaseTransactionValue

        it "should reject a chain with invalid coinbase hash in block header" $ once $
            \txOut -> ioProperty $ do
                blockchain <- singletonBlockchain
                block      <- block1A

                let txOut'   = txOut { value = 100 }
                    coinbase = CoinbaseTransaction (pure txOut')
                    block'   = block { coinbaseTransaction = coinbase }

                return $ addBlock block' blockchain === Left InvalidCoinbaseTransactionHash

        -- it "should reject a chain with invalid transaction hash in block header" $ once $
        --     \tx -> ioProperty $ do
        --         (blockchain, block) <- loadVerifiedTestBlockchainWithValidBlock
        --         let block' = block { transactions = pure tx }
        --
        --         return $ addBlock block' blockchain === Left InvalidTransactionHashTreeRoot

        -- TODO: transaction testing.........

    describe "addressValues" $
        it "should calculate unspent transaction outputs" $ once $ ioProperty $ do
            blockchain <- singletonBlockchain
            block      <- block1A

            let blockchain' = throwLeft (addBlock block blockchain)
                unspent     = addressValues blockchain'

            return $ showKeys unspent === H.fromList
                [ ("74bbe6d5f70f7d7e433b9b6d5d77a391492cc93161d9f108cef18d64959930cf1d55ef0844c0607f5568d1cd9233995154b5758915eb595e1dee472d13a18517", 100)
                , ("8330da21047df515590bc752ac27210d3eb11bc1ec1b4dcb8f6c6b0b018fb5d44feb11798ca599b0c1549146508933833c4553d420504bef2c5f96dd42e143df", 100)
                ]

        -- TODO: test w/ more transactions (and non-coinbase txs)


showKeys :: H.HashMap PublicKey v -> H.HashMap String v
showKeys = H.fromList . fmap (Arrow.first show) . H.toList
