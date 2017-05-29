module Data.Blockchain.Core.Types.BlockchainConfigSpec (spec) where

import TestUtil

import qualified Data.HashMap.Strict as H

import Data.Blockchain.Core.Types

config :: BlockchainConfig
config = BlockchainConfig
    { initialDifficulty             = Difficulty 1
    , targetMillisPerBlock          = 10
    , difficultyRecalculationHeight = 10
    , initialMiningReward           = 100
    , miningRewardTransitionMap     = H.fromList [(5, 50), (20, 10)]
    }

spec :: Spec
spec =
    describe "BlockchainConfig" $ do
        it "should produce the correct reward" $
            and [ targetReward config 0  == 100
                , targetReward config 4  == 100
                , targetReward config 5  == 50
                , targetReward config 19 == 50
                , targetReward config 20 == 10
                , targetReward config 21 == 10
                ]

        prop "should always find a valid reward" $
            \conf height ->
                let possibleRewards = initialMiningReward conf : H.elems (miningRewardTransitionMap conf)
                in targetReward conf height `elem` possibleRewards
