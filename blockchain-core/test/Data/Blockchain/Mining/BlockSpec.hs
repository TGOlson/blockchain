module Data.Blockchain.Mining.BlockSpec (spec) where

import           TestUtil

import           Data.Blockchain
import           Data.Blockchain.Mining

spec :: Spec
spec = describe "Data.Blockchain.Mining.Block" $ do
    context "mineBlock" $ do
        propNumTests 5 "should find a block with a valid difficulty" $
            \pubKey -> ioProperty $ do
                blockchain <- blockchain3Block

                let config      = blockchainConfig blockchain
                    node        = blockchainNode blockchain
                    -- use relatively low diff1 here to allow test to complete in a reasonable amount of time
                    diff1       = hex256LeadingZeros 2
                    config'     = config { difficulty1Target = diff1 }
                    blockchain' = validate' (construct config' node)

                (Block header _ _) <- throwLeft <$> mineBlock pubKey mempty blockchain'

                return (blockHeaderHashDifficulty diff1 header >= initialDifficulty config)

        propNumTests 5 "should fail if passed invalid transactions" $
            \pubKey (NonEmpty txs) -> ioProperty $ do
                blockchain <- blockchain3Block

                res <- mineBlock pubKey txs blockchain

                return (res === Left InvalidTransactionList)

    context "mineGenesisBlock" $
        propNumTests 5 "should find an initial block with a valid difficulty" $
            \config -> ioProperty $ do
                let diff1   = hex256LeadingZeros 2
                    config' = config { difficulty1Target = diff1, initialDifficulty = Difficulty 1 }

                (Block header _ _) <- mineGenesisBlock config'

                return (blockHeaderHashDifficulty diff1 header >= initialDifficulty config')
