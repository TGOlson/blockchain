module Data.Blockchain.Core.BlockchainSpec (spec) where

import TestUtil

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.HashMap.Strict  as H
import qualified Data.List.NonEmpty   as NonEmpty

import Data.Blockchain.Core.Blockchain
import Data.Blockchain.Core.Crypto
import Data.Blockchain.Core.Types


throwLeft :: Show a => Either a b -> b
throwLeft = either (error . show) id


-- TODO: generate all these
data TestBlockchain
   = SingletonChain
   | SmallSingleChainNoTxs   -- TODO!
   | SmallSingleChainWithTxs -- TODO!
   | SmallMutliNodeChain     -- TODO!
--    | ...

testChainFilePath :: TestBlockchain -> FilePath
testChainFilePath = \case SingletonChain          -> "data/singleton_chain/blockchain.json"
                          SmallSingleChainNoTxs   -> error "test blockchain not generated"
                          SmallSingleChainWithTxs -> error "test blockchain not generated"
                          SmallMutliNodeChain     -> error "test blockchain not generated"

loadUnverifiedTestBlockchain :: TestBlockchain -> IO UnverifiedBlockchain
loadUnverifiedTestBlockchain testChain = throwLeft . Aeson.eitherDecode <$> Lazy.readFile (testChainFilePath testChain)

loadVerifiedTestBlockchainWithValidBlock :: TestBlockchain -> IO (Blockchain, Block)
loadVerifiedTestBlockchainWithValidBlock testChain = do
    blockchain <- throwLeft . verifyBlockchain <$> loadUnverifiedTestBlockchain testChain
    -- TODO: hardcoded path breaks "test chain" pattern... fix it
    block <- throwLeft . Aeson.eitherDecode <$> Lazy.readFile "data/singleton_chain/valid_next_block.json"

    return (blockchain, block)

spec :: Spec
spec = describe "Blockchain" $ do
    it "should serialize round-trip" $ once $ ioProperty $ do
        bs <- Lazy.readFile (testChainFilePath SingletonChain) -- TODO: more chains

        let unverifiedBlockchain = throwLeft (Aeson.eitherDecode bs)
            blockchain           = throwLeft (verifyBlockchain unverifiedBlockchain)

        return (Aeson.encode blockchain === bs)

    describe "verifyBlockchain" $ do
        it "should reject a chain with invalid difficulty reference in genesis block" $ once $ ioProperty $ do
            (UnverifiedBlockchain config node) <- loadUnverifiedTestBlockchain SingletonChain
            let config' = config { initialDifficulty = minDifficulty }
                chain   = UnverifiedBlockchain config' node

            return $ verifyBlockchain chain === Left (AddBlockVerificationException InvalidDifficultyReference)

        -- Note: this is a known modification that will change block hash to make it invalid
        -- if test data is re-generated, it may cause this test to fail
        it "should reject a chain with invalid genesis block difficulty" $ once $ ioProperty $ do
            (UnverifiedBlockchain config (UnverifiedBlockchainNode block nodes)) <- loadUnverifiedTestBlockchain SingletonChain
            let blockHeader' = (blockHeader block) { nonce = 1 }
                block'       = block { blockHeader = blockHeader' }
                chain        = UnverifiedBlockchain config (UnverifiedBlockchainNode block' nodes)

            return $ verifyBlockchain chain === Left (AddBlockVerificationException InvalidDifficulty)

        it "should reject a chain with transactions in genesis block" $ once $
            \tx -> ioProperty $ do
                (UnverifiedBlockchain config (UnverifiedBlockchainNode block nodes)) <- loadUnverifiedTestBlockchain SingletonChain
                let block' = block { transactions = pure tx }
                    chain   = UnverifiedBlockchain config (UnverifiedBlockchainNode block' nodes)

                return $ verifyBlockchain chain === Left GenesisBlockHasTransactions

        it "should reject a chain with invalid coinbase reward in genesis block" $ once $
            \txOut -> ioProperty $ do
                (UnverifiedBlockchain config (UnverifiedBlockchainNode block nodes)) <- loadUnverifiedTestBlockchain SingletonChain
                let txOut'   = txOut { value = 999 }
                    coinbase = CoinbaseTransaction (pure txOut')
                    block'   = block { coinbaseTransaction = coinbase }
                    chain    = UnverifiedBlockchain config (UnverifiedBlockchainNode block' nodes)

                return $ verifyBlockchain chain === Left (AddBlockVerificationException InvalidCoinbaseTransactionValue)

        it "should reject a chain with invalid coinbase hash in genesis block header" $ once $
            \txOut -> ioProperty $ do
                (UnverifiedBlockchain config (UnverifiedBlockchainNode block nodes)) <- loadUnverifiedTestBlockchain SingletonChain
                let txOut'   = txOut { value = 100 }
                    coinbase = CoinbaseTransaction (pure txOut')
                    block'   = block { coinbaseTransaction = coinbase }
                    chain    = UnverifiedBlockchain config (UnverifiedBlockchainNode block' nodes)

                return $ verifyBlockchain chain === Left (AddBlockVerificationException InvalidCoinbaseTransactionHash)

        -- TODO: test is possible, hard to do with empty transaction rule & expected header hash
        -- it "should reject a chain with invalid transaction hash in genesis block header" $ once $

    describe "addBlock" $ do
        it "should add a valid block" $ once $ ioProperty $ do
            (blockchain, block) <- loadVerifiedTestBlockchainWithValidBlock SingletonChain
            let mainChain = throwLeft (longestChain <$> addBlock block blockchain)

            return $ length mainChain == 2 && NonEmpty.last mainChain == block

        it "should reject a duplicate block" $ once $ ioProperty $ do
            (blockchain, block) <- loadVerifiedTestBlockchainWithValidBlock SingletonChain
            let blockchain' = throwLeft (addBlock block blockchain)

            return $ addBlock block blockchain' === Left BlockAlreadyExists

        -- TODO: block without parent

        -- Note: this is a known modification that will change block hash to make it invalid
        -- if test data is re-generated, it may cause this test to fail
        it "should reject a chain with invalid genesis block difficulty" $ once $ ioProperty $ do
            (blockchain, block) <- loadVerifiedTestBlockchainWithValidBlock SingletonChain
            let blockHeader' = (blockHeader block) { nonce = 1 }
                block'       = block { blockHeader = blockHeader' }

            return $ addBlock block' blockchain === Left InvalidDifficulty

        it "should reject a chain with invalid coinbase reward in block" $ once $
            \txOut -> ioProperty $ do
                (blockchain, block) <- loadVerifiedTestBlockchainWithValidBlock SingletonChain
                let txOut'   = txOut { value = 999 }
                    coinbase = CoinbaseTransaction (pure txOut')
                    block'   = block { coinbaseTransaction = coinbase }

                return $ addBlock block' blockchain === Left InvalidCoinbaseTransactionValue

        it "should reject a chain with invalid coinbase hash in block header" $ once $
            \txOut -> ioProperty $ do
                (blockchain, block) <- loadVerifiedTestBlockchainWithValidBlock SingletonChain
                let txOut'   = txOut { value = 100 }
                    coinbase = CoinbaseTransaction (pure txOut')
                    block'   = block { coinbaseTransaction = coinbase }

                return $ addBlock block' blockchain === Left InvalidCoinbaseTransactionHash

        -- it "should reject a chain with invalid transaction hash in block header" $ once $
        --     \tx -> ioProperty $ do
        --         (blockchain, block) <- loadVerifiedTestBlockchainWithValidBlock SingletonChain
        --         let block' = block { transactions = pure tx }
        --
        --         return $ addBlock block' blockchain === Left InvalidTransactionHashTreeRoot

        -- TODO: transaction testing.........

    describe "unspentTransactionOutputs" $
        it "should calculate unspent transaction outputs" $ once $ ioProperty $ do
            (blockchain, block) <- loadVerifiedTestBlockchainWithValidBlock SingletonChain
            let blockchain' = throwLeft (addBlock block blockchain)
                unspent     = unspentTransactionOutputs blockchain'

            return $ showKeys unspent === H.fromList
                [ ("6efea3efdc45f20ef3fc1816a965aa2c5a50c5431387fc48f0a4cf3535617ee0ac4bc59a0a77da39b4c19534cf080483888efe7a93604fceb5050712e500e6a9", 100)
                , ("aea900bc02b569fa740039c7a0fd020a31709e32e1d2ed93c1f29d4032af43600c01b333848b4f020c6c4fdd75e0afdef5a84b9617489f9203b451d59515e39f", 100)
                ]

        -- TODO: test w/ more transactions (and non-coinbase txs)


showKeys :: H.HashMap PublicKey v -> H.HashMap String v
showKeys = H.fromList . fmap (\(k, v) -> (show k, v)) . H.toList
