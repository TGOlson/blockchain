module Data.Blockchain2
    ( findBlock
    , addBlock
    ) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.BlockchainT
import Control.Monad.Trans.Except

import Data.Blockchain.Crypto
import Data.Blockchain.Types           hiding (BlockException (..), addBlock)

data FindBlockException = BlockNotFound

findBlock :: Monad m => Hash BlockHeader -> BlockchainT m (Either FindBlockException Block)
findBlock headerHash = do
    db  <- blockchainDB
    res <- lift (getBlock db headerHash)

    return (maybe (Left BlockNotFound) Right res)

data AddBlockException
    = BlockAlreadyExists
    | NoParentBlockFound
    -- timestamps
    | TimestampTooOld
    --     | TimestampTooFarIntoFuture
    --     -- difficulty
    --     | InvalidDifficultyReference
    --     | InvalidDifficulty
    --     -- header refs
    --     | InvalidCoinbaseTransactionHash
    --     | InvalidTransactionHashTreeRoot
    --     -- transactions
    --     | InvalidCoinbaseTransactionValue
    --     | InvalidTransactionValues
    --     | TransactionOutRefNotFound
    --     | InvalidTransactionSignature


    -- data BlockException
    --     = BlockAlreadyExists
    --     | NoParentFound
    --     -- timestamps
    --     | TimestampTooOld
    --     | TimestampTooFarIntoFuture
    --     -- difficulty
    --     | InvalidDifficultyReference
    --     | InvalidDifficulty
    --     -- header refs
    --     | InvalidCoinbaseTransactionHash
    --     | InvalidTransactionHashTreeRoot
    --     -- transactions
    --     | InvalidCoinbaseTransactionValue
    --     | InvalidTransactionValues
    --     | TransactionOutRefNotFound
    --     | InvalidTransactionSignature

addBlock :: Monad m => Block -> BlockchainT m (Either AddBlockException ())
addBlock block = runExceptT $ do
    validateBlockDoesNotAlreadyExist

    parentBlock <- withExceptT (const NoParentBlockFound) $ ExceptT $ findBlock parentHeaderHash

    -- block was not created before parent
    -- TODO: The protocol rejects blocks with a timestamp earlier than the median of the timestamps from the previous 11 blocks
    -- TODO: block created less than X hours, or N blocks intervals, into future
    unless (time header > time (blockHeader parentBlock)) $ throwE TimestampTooOld
    -- unless (newBlockTimestamp < now) $ throwE TimestampTooFarIntoFuture

    undefined
  where
    header = blockHeader block
    headerHash = hash header
    parentHeaderHash = prevBlockHeaderHash header

    validateBlockDoesNotAlreadyExist = do
        _ <- ExceptT $ either (const $ Right ()) (const $ Left BlockAlreadyExists) <$> findBlock headerHash
        return ()

    -- validateParentBlockExists = do
    --     _ <- withExceptT (const NoParentBlockFound) $ ExceptT $ findBlock parentHeaderHash
    --     return ()

    -- validateBlockCreationTime :: BlockHeader -> BlockHeader -> Either BlockException ()
    -- validateBlockCreationTime newBlockHeader parentBlockHeader =
    --     verify (newBlockTimestamp > time parentBlockHeader) TimestampTooOld
    --     -- verify (newBlockTimestamp < now) TimestampTooFarIntoFuture
    --   where
    --     newBlockTimestamp = time newBlockHeader
