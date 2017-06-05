module Data.Blockchain.Core.BlockchainSpec (spec) where

import TestUtil

import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Aeson           as Aeson

import Data.Blockchain.Core.Blockchain
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

-- loadVerifiedTestBlockchain :: TestBlockchain -> IO Blockchain
-- loadVerifiedTestBlockchain testChain = throwLeft . verifyBlockchain <$> loadUnverifiedTestBlockchain testChain

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
