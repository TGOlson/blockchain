module Data.Blockchain.Core.Types.BlockchainConfig
    ( BlockchainConfig(..)
    , defaultConfig
    , targetReward
    , targetDifficulty
    ) where

import qualified Control.Monad   as Monad
import qualified Data.Aeson      as Aeson
import qualified Data.Time.Clock as Time
import qualified Data.Word       as Word
import qualified GHC.Generics    as Generic

import Data.Blockchain.Core.Types.Block
import Data.Blockchain.Core.Types.Difficulty

data BlockchainConfig = BlockchainConfig
    { initialDifficulty             :: Difficulty
    , targetSecondsPerBlock         :: Word.Word
    , difficultyRecalculationHeight :: Word.Word
    , initialMiningReward           :: Word.Word
    -- Defines num blocks when reward is halved
    -- `0` means reward never changes
    , miningRewardHalvingHeight     :: Word.Word
    }
  deriving (Generic.Generic, Eq, Show)

instance Aeson.ToJSON BlockchainConfig
instance Aeson.FromJSON BlockchainConfig

defaultConfig :: BlockchainConfig
defaultConfig = BlockchainConfig
    { initialDifficulty             = Difficulty 100
    , targetSecondsPerBlock         = 10
    , difficultyRecalculationHeight = 100
    , initialMiningReward           = 100
    , miningRewardHalvingHeight     = 500
    }

targetReward :: BlockchainConfig -> Word.Word -> Word.Word
targetReward config height = either id id $ do
    let initialReward = initialMiningReward config
        halveHeight   = miningRewardHalvingHeight config

    Monad.when (halveHeight == 0) $ Left initialReward

    let numHalves = height `div` halveHeight

    -- 2^64 is greater than maxBound :: Word
    -- And any word halved 64 times will be zero
    Monad.when (numHalves >= 64) $ Left 0

    return $ initialReward `div` (2 ^ numHalves)

-- TODO: array of blocks hold no assurances of expected invariants
-- for example block1 could be created more recently than blockN
-- should create a `SingleChain` wrapper
targetDifficulty :: BlockchainConfig -> [Block] -> Difficulty
targetDifficulty config []                                            = initialDifficulty config
targetDifficulty config _ | difficultyRecalculationHeight config == 0 = initialDifficulty config
targetDifficulty config _ | targetSecondsPerBlock config == 0         = initialDifficulty config
targetDifficulty config blocks =
    case length blocks `mod` fromIntegral recalcHeight of
        0 ->
            let recentBlocks   = take (fromIntegral recalcHeight) (reverse blocks)
                lastBlock      = head recentBlocks
                firstBlock     = last recentBlocks
                -- TODO: get rid of `abs`, move invariant into types
                diffTime       = abs $ Time.diffUTCTime (blockTime lastBlock) (blockTime firstBlock)
                avgSolveTime   = realToFrac diffTime / fromIntegral recalcHeight
                ratio          = avgSolveTime / fromIntegral (targetSecondsPerBlock config)
                lastDifficulty = difficulty (blockHeader lastBlock)
                nextDifficulty = Difficulty $ round $ ratio * toRational (unDifficulty lastDifficulty)
            in nextDifficulty

        _ -> difficulty $ blockHeader $ last blocks
  where
    recalcHeight = difficultyRecalculationHeight config
    blockTime    = time . blockHeader
