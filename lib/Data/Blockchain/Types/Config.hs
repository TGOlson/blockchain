module Data.Blockchain.Types.Config
    ( Config(..)
    , defaultConfig
    , targetReward
    , targetDifficulty
    ) where

import           Control.Monad                    (when)
import qualified Data.Aeson                       as Aeson
import qualified Data.Time.Clock                  as Time
import qualified Data.Word                        as Word
import qualified GHC.Generics                     as Generic

import           Data.Blockchain.Types.Block
import           Data.Blockchain.Types.Difficulty
import           Data.Blockchain.Types.Hex

data Config = Config
    { initialDifficulty             :: Difficulty
    -- Maximum hash - difficulties will be calculated using this value
    , difficulty1Target             :: Hex256
    , targetSecondsPerBlock         :: Word.Word
    , difficultyRecalculationHeight :: Word.Word
    , initialMiningReward           :: Word.Word
    -- Defines num blocks when reward is halved --  `0` means reward never changes
    , miningRewardHalvingHeight     :: Word.Word
    }
  deriving (Generic.Generic, Eq, Show)

instance Aeson.FromJSON Config
instance Aeson.ToJSON Config

-- | A reasonable default config to use for testing. Mines blocks quickly and changes difficulty and rewards frequently.
-- Note: reward will go to zero after 1100 blocks, which will take about 180 minutes of mining.
--
-- @
-- defaultConfig :: Config
-- defaultConfig = Config
--     { initialDifficulty             = Difficulty 1
--     , difficulty1Target             = hex256LeadingZeros 4
--     , targetSecondsPerBlock         = 10
--     , difficultyRecalculationHeight = 50
--     , initialMiningReward           = 1024
--     , miningRewardHalvingHeight     = 100
--     }
-- @
--
defaultConfig :: Config
defaultConfig = Config
    { initialDifficulty             = Difficulty 1
    , difficulty1Target             = hex256LeadingZeros 4
    , targetSecondsPerBlock         = 10
    , difficultyRecalculationHeight = 50
    , initialMiningReward           = 1024
    , miningRewardHalvingHeight     = 100
    }

-- | Calculates the target reward for a blockchain. Uses the longest chain.
targetReward :: Config -> Word.Word -> Word.Word
targetReward config height = either id id $ do
    let initialReward = initialMiningReward config
        halveHeight   = miningRewardHalvingHeight config

    when (halveHeight == 0) $ Left initialReward

    let numHalves = height `div` halveHeight

    -- 2^64 is greater than maxBound :: Word
    -- And any word halved 64 times will be zero
    when (numHalves >= 64) $ Left 0

    return $ initialReward `div` (2 ^ numHalves)

-- TODO: array of blocks hold no assurances of expected invariants
-- for example block1 could be created more recently than blockN
-- should create a `SingleChain` wrapper
-- TODO: take in entire blockchain
-- | Calculates the target difficulty for a blockchain. Uses the longest chain.
targetDifficulty :: Config -> [Block] -> Difficulty
targetDifficulty config []                                            = initialDifficulty config
targetDifficulty config _ | difficultyRecalculationHeight config == 0 = initialDifficulty config
targetDifficulty config _ | difficultyRecalculationHeight config == 1 = initialDifficulty config
targetDifficulty config _ | targetSecondsPerBlock config == 0         = initialDifficulty config
-- targetDifficulty config blocks | length blocks == 1                   = initialDifficulty config -- super messy...
targetDifficulty config blocks =
    case length blocks `mod` fromIntegral recalcHeight of
        -- Note: this includes the genesis block, is that correct?
        0 ->
            let recentBlocks   = take (fromIntegral recalcHeight) (reverse blocks) -- TODO: (drop (length blocks - recalcHeight))
                lastBlock      = head recentBlocks
                firstBlock     = last recentBlocks
                -- TODO: get rid of `abs`, move invariant into types
                diffTime       = abs $ Time.diffUTCTime (blockTime lastBlock) (blockTime firstBlock)
                avgSolveTime   = realToFrac diffTime / fromIntegral recalcHeight
                solveRate      = fromIntegral (targetSecondsPerBlock config) / avgSolveTime
                lastDifficulty = difficulty (blockHeader lastBlock)
                nextDifficulty = Difficulty $ round $ solveRate * toRational lastDifficulty
            in nextDifficulty

        _ -> difficulty $ blockHeader $ last blocks
  where
    recalcHeight = difficultyRecalculationHeight config
    blockTime    = time . blockHeader
