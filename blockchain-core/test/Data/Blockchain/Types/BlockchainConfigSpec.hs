module Data.Blockchain.Types.BlockchainConfigSpec (spec) where

import           TestUtil

import           Data.Monoid           ((<>))
import qualified Data.Time.Clock       as Time

import           Data.Blockchain.Types

testConfig :: BlockchainConfig
testConfig = BlockchainConfig
    { initialDifficulty             = Difficulty 1000
    , difficulty1Target             = hex256LeadingZeros 2
    , targetSecondsPerBlock         = 60
    , difficultyRecalculationHeight = 10
    , initialMiningReward           = 100
    , miningRewardHalvingHeight     = 100
    }

spec :: Spec
spec = describe "Data.Blockchain.Types.BlockchainConfig" $ do
    describe "targetReward" $ do
        it "should produce the correct reward" $
            and [ targetReward testConfig 0   == 100
                , targetReward testConfig 99  == 100
                , targetReward testConfig 100 == 50
                , targetReward testConfig 101 == 50
                , targetReward testConfig 300 == 12
                , targetReward testConfig 600 == 1
                , targetReward testConfig 700 == 0
                , targetReward testConfig 800 == 0
                ]

        prop "should always find a valid reward" $
            \conf height ->
                let reward = targetReward conf height
                in  reward >= 0 && reward <= initialMiningReward conf

        prop "should always use initial reward if recalc height it zero" $
            \conf height ->
                let conf' = conf { miningRewardHalvingHeight = 0 }
                in targetReward conf' height == initialMiningReward conf

    describe "targetDifficulty" $ do
        prop "should use initial config when no blocks" $
            \conf -> targetDifficulty conf mempty === initialDifficulty conf

        prop "should correctly increase difficulty" $
            \block -> targetDifficulty testConfig (blocksWithOffset 60 block) === Difficulty 10000

        prop "should correctly decrease difficulty" $
            \block -> targetDifficulty testConfig (blocksWithOffset 6000 block) === Difficulty 100

        propWithSize 20 "should produce the correct difficulty when not recalculating" $
            \(NonEmpty blocks) conf ->
                  -- Pretty complex example, can it be cleaned up?
                  difficultyRecalculationHeight conf /= 0 &&
                  targetSecondsPerBlock conf /= 0 &&
                  length blocks `mod` fromIntegral (difficultyRecalculationHeight conf) /= 0
                  ==> targetDifficulty conf blocks === difficulty (blockHeader $ last blocks)

        prop "should always find a valid difficulty" $
            \conf blocks -> targetDifficulty conf blocks >= minBound

blocksWithOffset :: Time.NominalDiffTime -> Block -> [Block]
blocksWithOffset diffTime (Block header coinbase txs) =
    replicate 9 block <> pure lastBlock
  where
    block      = Block (header { difficulty = initialDifficulty testConfig }) coinbase txs
    endTime    = Time.addUTCTime diffTime $ time header
    lastBlock  = setTime endTime block


setTime :: Time.UTCTime -> Block -> Block
setTime t (Block header tx txs) = Block (header {time = t}) tx txs
