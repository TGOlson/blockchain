module Data.Blockchain.Core.Types.BlockchainConfig
    ( BlockchainConfig(..)
    , targetReward
    , targetDifficulty
    ) where


import qualified Data.HashMap.Strict as H

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
              case length blocks `div` difficultyRecalculationHeight config of
                  0 -> undefined
                  _ -> difficulty $ blockHeader $ last blocks
