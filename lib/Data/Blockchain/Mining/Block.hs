module Data.Blockchain.Mining.Block
    ( mineBlock
    , mineGenesisBlock
    , createBlockchain
    ) where

import qualified Data.List.NonEmpty              as NonEmpty
import qualified Data.Time.Clock                 as Time
import qualified Data.Word                       as Word

import qualified Data.Blockchain.Core.Blockchain as Blockchain
import qualified Data.Blockchain.Core.Crypto     as Crypto
import qualified Data.Blockchain.Core.Types      as Blockchain
import qualified Data.Blockchain.Core.Util.Hex   as Hex

mineBlock :: Crypto.PublicKey -> [Blockchain.Transaction] -> Blockchain.Blockchain Blockchain.Validated -> IO Blockchain.Block
mineBlock pubKey txs blockchain =
    mineBlockInternal pubKey reward diff1 difficulty prevBlockHeaderHash txs
  where
    diff1               = Blockchain.difficulty1Target config
    reward              = Blockchain.targetReward config 0
    config              = Blockchain.blockchainConfig blockchain
    difficulty          = Blockchain.initialDifficulty config
    prevBlock           = NonEmpty.head (Blockchain.longestChain blockchain)
    prevBlockHeaderHash = Crypto.hash (Blockchain.blockHeader prevBlock)

createBlockchain :: Blockchain.BlockchainConfig -> IO (Blockchain.Blockchain Blockchain.Validated)
createBlockchain config = either throwValidationError id <$> do
    genesisBlock <- mineGenesisBlock config

    let node  = Blockchain.BlockchainNode genesisBlock mempty
        chain = Blockchain.construct config node

    return (Blockchain.validate chain)
  where
    throwValidationError e = error $ "Unexpected error creating blockchain: " ++ show e

mineGenesisBlock :: Blockchain.BlockchainConfig -> IO Blockchain.Block
mineGenesisBlock config = do
    -- Note: ignore private key, coinbase reward in genesis block cannot be spent
    (Crypto.KeyPair pubKey _privKey) <- Crypto.generate

    mineBlockInternal pubKey reward diff1 difficulty prevBlockHeaderHash []
  where
    diff1               = Blockchain.difficulty1Target config
    reward              = Blockchain.initialMiningReward config
    difficulty          = Blockchain.initialDifficulty config
    prevBlockHeaderHash = Crypto.unsafeFromByteString "0000000000000000000000000000000000000000000000000000000000000000"

-- TODO: accept multiple public keys
mineBlockInternal
    :: Crypto.PublicKey -> Word.Word -> Hex.Hex256 -> Blockchain.Difficulty
    -> Crypto.Hash Blockchain.BlockHeader -> [Blockchain.Transaction]
    -> IO Blockchain.Block
mineBlockInternal pubKey reward diff1 difficulty prevBlockHeaderHash txs = do
    header <- mineHeader
        prevBlockHeaderHash
        (Crypto.hash coinbaseTx)
        (Crypto.hashTreeRoot txs)
        diff1
        difficulty

    return (Blockchain.Block header coinbaseTx txs)
  where
    coinbaseTx = Blockchain.CoinbaseTransaction $ pure (Blockchain.TransactionOut reward pubKey)

mineHeader
    :: Crypto.Hash Blockchain.BlockHeader
    -> Crypto.Hash Blockchain.CoinbaseTransaction
    -> Crypto.HashTreeRoot Blockchain.Transaction
    -> Hex.Hex256
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
        -- TODO: need to pass config down to this level
        if Blockchain.blockHeaderHashDifficulty diff1 header >= difficulty
            then return header
            else mineHeaderInternal (incNonce header)

incNonce :: Blockchain.BlockHeader -> Blockchain.BlockHeader
incNonce header = header { Blockchain.nonce = Blockchain.nonce header + 1 }
