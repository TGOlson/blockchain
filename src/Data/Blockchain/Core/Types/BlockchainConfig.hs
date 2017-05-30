module Data.Blockchain.Core.Types.BlockchainConfig
    ( BlockchainConfig(..)
    , targetReward
    , targetDifficulty
    ) where


import qualified Data.HashMap.Strict as H
import qualified Data.Time.Clock     as Time
import qualified Data.Word           as Word

import Data.Blockchain.Core.Types.Block
import Data.Blockchain.Core.Types.Difficulty

-- import Debug.Trace

data BlockchainConfig = BlockchainConfig
    { initialDifficulty             :: Difficulty
    , targetSecondsPerBlock         :: Word.Word
    , difficultyRecalculationHeight :: Word.Word
    , initialMiningReward           :: Word.Word
    -- Defines block heights where reward changes
    -- An empty map means the current reward is always the initial reward
    , miningRewardTransitionMap     :: H.HashMap Word.Word Word.Word
    }
  deriving (Show)

targetReward :: BlockchainConfig -> Word.Word -> Word.Word
targetReward config height =
    case currentBounds of
        []     -> initialMiningReward config
        bounds -> snd (maximum bounds)
  where
    currentBounds = filter (\(h, _) -> h <= height) rewardBounds
    rewardBounds  = H.toList $ miningRewardTransitionMap config

-- TODO: find current difficulty from chain length
targetDifficulty :: BlockchainConfig -> [Block] -> Difficulty
targetDifficulty config []                                            = initialDifficulty config
targetDifficulty config _ | difficultyRecalculationHeight config == 0 = initialDifficulty config
targetDifficulty config _ | targetSecondsPerBlock config == 0         = initialDifficulty config
targetDifficulty config blocks =
    case length blocks `mod` fromIntegral recalcHeight of
        0 ->
            let !recentBlocks   = debug "blocks" $ take (fromIntegral recalcHeight) (reverse blocks)
                !lastBlock      = debug "lastBlock" $ head recentBlocks -- TODO: unsafe
                !firstBlock     = debug "firstBlock" $ last recentBlocks -- TODO: unsafe
                !diffTime       = debug "diffTime" $ realToFrac $ Time.diffUTCTime (blockTime lastBlock) (blockTime firstBlock)
                !avgSolveTime   = debug "avgSolveTime" $ diffTime / fromIntegral recalcHeight :: Rational
                !ratio          = debug "ratio" $ avgSolveTime / fromIntegral (targetSecondsPerBlock config)
                !lastDifficulty = debug "lastDifficulty" $ difficulty $ blockHeader lastBlock
                !nextDifficulty = debug "nextDifficulty" $ Difficulty (round $ ratio * toRational (unDifficulty lastDifficulty))
            in nextDifficulty

        -- Difficulty {unDifficulty = 15484718690861360} /= Difficulty {unDifficulty = 15484718690861359}
        -- ratio: 1.0 s
        _ -> difficulty $ blockHeader $ last blocks
  where
    recalcHeight = difficultyRecalculationHeight config
    blockTime = time . blockHeader

-- debug :: Show a => String -> a -> a
-- debug tag = trace tag . traceShowId
debug :: String -> a -> a
debug _tag = id

-- convertNum :: (Integral a, Num b) => a -> b
-- convertNum = fromInteger . toInteger fromIn
