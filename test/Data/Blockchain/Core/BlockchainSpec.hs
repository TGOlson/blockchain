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

        -- TODO: need a pre-mined block w/ invalid difficulty
        -- it "should reject a chain with invalid genesis block difficulty" $ once $ ioProperty $ do
        --     (UnverifiedBlockchain config node) <- loadUnverifiedTestBlockchain SingletonChain
        --     let config' = config { initialDifficulty = minDifficulty }
        --         chain   = UnverifiedBlockchain config' node
        --
        --     return $ verifyBlockchain chain === Left (AddBlockVerificationException InvalidDifficultyReference)

        it "should reject a chain with transactions in genesis block" $ once $
            \tx -> ioProperty $ do
                (UnverifiedBlockchain config (UnverifiedBlockchainNode block nodes)) <- loadUnverifiedTestBlockchain SingletonChain
                let block' = block { transactions = pure tx }
                    chain   = UnverifiedBlockchain config (UnverifiedBlockchainNode block' nodes)

                return $ verifyBlockchain chain === Left GenesisBlockHasTransactions

        -- it "should reject a chain with invalid coinbase hash in genesis block" $ once $
        --     \tx -> ioProperty $ do
        --         (UnverifiedBlockchain config (UnverifiedBlockchainNode block nodes)) <- loadUnverifiedTestBlockchain SingletonChain
        --         let block' = block { transactions = pure tx }
        --             chain   = UnverifiedBlockchain config (UnverifiedBlockchainNode block' nodes)
        --
        --         return $ verifyBlockchain chain === Left GenesisBlockHasTransactions
