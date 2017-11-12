module Data.Blockchain.Mining.Block
    ( MineBlockException(..)
    , mineBlock
    , mineEmptyBlock
    , mineGenesisBlock
    ) where

import qualified Data.ByteString.Char8  as Char8
import qualified Data.List.NonEmpty     as NonEmpty
import qualified Data.Time.Clock        as Time
import qualified Data.Word              as Word

import qualified Data.Blockchain        as Blockchain
import qualified Data.Blockchain.Crypto as Crypto

data MineBlockException
    = InvalidTransactionList
  deriving (Eq, Show)

-- | Finds the next block of a blockchain. Depending on blockchain configuration, this function may take a long time to complete.
mineBlock
    :: Crypto.PublicKey         -- ^ PublicKey address where coinbase reward will be sent
    -> [Blockchain.Transaction] -- ^ List of transactions to include in transaction
    -> Blockchain.Blockchain Blockchain.Validated -- ^ Validated blockchain
    -> IO (Either MineBlockException Blockchain.Block)
mineBlock pubKey txs blockchain =
    case Blockchain.validateTransactions blockchain txs of
        Left _e  -> return (Left InvalidTransactionList)
        Right () -> Right <$> mineBlockInternal pubKey reward diff1 difficulty prevBlockHeaderHash txs
  where
    diff1               = Blockchain.difficulty1Target config
    reward              = Blockchain.targetReward config (fromIntegral $ length prevBlocks + 1)
    config              = Blockchain.blockchainConfig blockchain
    difficulty          = Blockchain.targetDifficulty config $ NonEmpty.toList prevBlocks -- TODO: next diff?
    prevBlocks          = Blockchain.longestChain blockchain
    prevBlock           = NonEmpty.last prevBlocks
    prevBlockHeaderHash = Crypto.hash (Blockchain.blockHeader prevBlock)

-- | Finds the next block of a blockchain, without including any transactions.
-- Most useful for testing - removes the invariant of an invalid transaction list.
-- Depending on blockchain configuration, this function may take a long time to complete.
mineEmptyBlock
    :: Crypto.PublicKey -- ^ PublicKey address where coinbase reward will be sent
    -> Blockchain.Blockchain Blockchain.Validated -- ^ Validated blockchain
    -> IO (Either MineBlockException Blockchain.Block)
mineEmptyBlock pubKey = mineBlock pubKey mempty

-- | Finds the first block of a blockchain.
-- Depending on blockchain configuration, this function may take a long time to complete.
-- Note: this generates a keypair but throws away the private key. Coinbase reward in genesis block cannot never be spent.
mineGenesisBlock :: Blockchain.Config -> IO Blockchain.Block
mineGenesisBlock config = do
    (Crypto.KeyPair pubKey _privKey) <- Crypto.generate

    mineBlockInternal pubKey reward diff1 difficulty prevBlockHeaderHash mempty
  where
    diff1               = Blockchain.difficulty1Target config
    reward              = Blockchain.initialMiningReward config
    difficulty          = Blockchain.initialDifficulty config
    prevBlockHeaderHash = Crypto.unsafeFromByteString $ Char8.replicate 64 '0'

-- TODO: accept multiple public keys
mineBlockInternal
    :: Crypto.PublicKey -> Word.Word -> Blockchain.Hex256 -> Blockchain.Difficulty
    -> Crypto.Hash Blockchain.BlockHeader -> [Blockchain.Transaction]
    -> IO Blockchain.Block
mineBlockInternal pubKey reward diff1 difficulty prevBlockHeaderHash txs = do
    header <- mineHeader
        prevBlockHeaderHash
        (Crypto.hash coinbaseTx)
        (Crypto.hashTreeRoot txs)
        diff1
        difficulty

    -- TODO: add up fees and include in coinbase reward
    -- fee = address value - total tx value
    return (Blockchain.Block header coinbaseTx txs)
  where
    coinbaseTx = Blockchain.CoinbaseTransaction $ pure (Blockchain.TransactionOut reward pubKey)

-- TODO: could clean up param list by passing config down to this level
mineHeader
    :: Crypto.Hash Blockchain.BlockHeader
    -> Crypto.Hash Blockchain.CoinbaseTransaction
    -> Crypto.HashTreeRoot Blockchain.Transaction
    -> Blockchain.Hex256
    -> Blockchain.Difficulty
    -> IO Blockchain.BlockHeader
mineHeader prevBlockHeaderHash coinbaseTransactionHash transactionHashTreeRoot diff1 difficulty = do
    time <- Time.getCurrentTime

    let version = 0
        nonce   = 0
        initialHeader = Blockchain.BlockHeader{..}

    mineHeaderInternal initialHeader
  where
    -- TODO: get current time in case of int overflow,
    -- or in case current mining operation is getting close to oldest block time
    mineHeaderInternal header =
        if Blockchain.blockHeaderHashDifficulty diff1 header >= difficulty
            then return header
            else mineHeaderInternal (incNonce header)


incNonce :: Blockchain.BlockHeader -> Blockchain.BlockHeader
incNonce header = header { Blockchain.nonce = Blockchain.nonce header + 1 }
