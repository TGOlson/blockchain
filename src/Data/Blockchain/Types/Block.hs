module Data.Blockchain.Types.Block
    ( Block
    , blockHeader
    , transactions
    , makeBlock
    , validateBlock
    , isValidBlockDifficulty
    , incHeaderNonce
    ) where

import qualified Control.Monad      as M
import qualified Data.Aeson         as Aeson
import           Data.Aeson         ((.=))
import qualified Data.Time.Clock    as Time

import Data.Blockchain.Crypto.Hash
import Data.Blockchain.Crypto.HashTree
import Data.Blockchain.Types.BlockHeader
import Data.Blockchain.Types.Difficulty
import Data.Blockchain.Types.Transaction


data Block = Block
    { blockHeader  :: BlockHeader
    , transactions :: [Transaction]
    }
  deriving (Eq, Show)

-- TODO: add block header hash for more efficient hash comparison
instance Aeson.ToJSON Block where
    toJSON Block{..} = Aeson.object
        [ "header"       .= blockHeader
        , "transactions" .= transactions
        ]

data BlockValidityException
    = HeaderDoesNotUseExpectedDifficulty
    | DoesNotMeetDifficulty
    | IncorrectHashTreeRoot

makeBlock :: Hash BlockHeader -> Time.UTCTime -> Difficulty -> Int -> [Transaction] -> Block
makeBlock prevBlockHeaderHash time difficulty nonce transactions = Block header transactions
  where
    version = 0
    transactionHashTreeRoot = hashTreeRoot transactions
    header = BlockHeader{..}

validateBlock :: Difficulty -> Block -> Maybe BlockValidityException
validateBlock diff block@(Block header transactions) = either Just (const Nothing) $ do
    M.unless usesRequiredDifficulty              $ Left HeaderDoesNotUseExpectedDifficulty
    M.unless (isValidBlockDifficulty diff block) $ Left DoesNotMeetDifficulty
    M.unless hasCorrectTransactionHashTreeRoot   $ Left IncorrectHashTreeRoot
  where
    usesRequiredDifficulty = diff == difficulty header
    hasCorrectTransactionHashTreeRoot = hashTreeRoot transactions == transactionHashTreeRoot header

isValidBlockDifficulty :: Difficulty -> Block -> Bool
isValidBlockDifficulty (Difficulty difficultyHash) (Block header _) =
    rawHash (hash header) < rawHash difficultyHash

incHeaderNonce :: Block -> Block
incHeaderNonce (Block header transactions) = Block (incNonce header) transactions
