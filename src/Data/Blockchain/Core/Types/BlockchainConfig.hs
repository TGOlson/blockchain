module Data.Blockchain.Core.Types.BlockchainConfig
    ( BlockchainConfig(..)
    , targetReward
    , targetDifficulty
    ) where


import qualified Data.HashMap.Strict as H
import qualified Data.Time.Clock     as Time

import Data.Blockchain.Core.Types.Block
import Data.Blockchain.Core.Types.Difficulty


data BlockchainConfig = BlockchainConfig
    { initialDifficulty             :: Difficulty
    , targetMillisPerBlock          :: Int
    , difficultyRecalculationHeight :: Int
    , initialMiningReward           :: Int
    -- Defines block heights where reward changes
    -- An empty map means the current reward is always the initial reward
    , miningRewardTransitionMap     :: H.HashMap Int Int
    }
  deriving (Show)

targetReward :: BlockchainConfig -> Int -> Int
targetReward config height =
    case currentBounds of
        []     -> initialMiningReward config
        bounds -> snd (maximum bounds)
  where
    currentBounds = filter (\(h, _) -> h <= height) rewardBounds
    rewardBounds  = H.toList $ miningRewardTransitionMap config

-- TODO: find current difficulty from chain length
targetDifficulty :: BlockchainConfig -> [Block] -> Difficulty
targetDifficulty config =
    \case []     -> initialDifficulty config
          blocks ->
              case length blocks `mod` recalcHeight of
                  0 ->
                      let recentBlocks   = take recalcHeight (reverse blocks)
                          firstBlock     = head recentBlocks -- TODO: unsafe
                          lastBlock      = last recentBlocks -- TODO: unsafe
                          diffTime       = Time.diffUTCTime (blockTime lastBlock) (blockTime firstBlock)
                          avgSolveTime   = round (realToFrac diffTime :: Double) `div` toInteger recalcHeight
                          ratio          = (fromInteger avgSolveTime :: Double) / fromInteger (toInteger (targetMillisPerBlock config))
                          lastDifficulty = difficulty $ blockHeader lastBlock
                          nextDifficulty = Difficulty (round ratio * unDifficulty lastDifficulty)
                      in nextDifficulty

                  _ -> difficulty $ blockHeader $ last blocks
  where
    recalcHeight = difficultyRecalculationHeight config
    blockTime = time . blockHeader
