module Data.Blockchain.Mining.Block
    ( mineBlock
    , mineGenesisBlock
    ) where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Time.Clock    as Time
import qualified Data.Word          as Word

import qualified Data.Blockchain.Core.Blockchain as Blockchain
import qualified Data.Blockchain.Core.Types      as Blockchain
import qualified Data.Blockchain.Core.Crypto     as Crypto

mineBlock :: Crypto.PublicKey -> [Blockchain.Transaction] -> Blockchain.Blockchain -> IO Blockchain.Block
mineBlock pubKey txs blockchain =
    mineBlockInternal pubKey reward difficulty prevBlockHeaderHash txs
  where
    reward              = Blockchain.targetReward config 0
    config              = Blockchain.blockchainConfig blockchain
    difficulty          = Blockchain.initialDifficulty config
    prevBlock           = NonEmpty.head (Blockchain.longestChain blockchain)
    prevBlockHeaderHash = Crypto.hash (Blockchain.blockHeader prevBlock)

mineGenesisBlock :: Crypto.PublicKey -> Blockchain.BlockchainConfig -> IO Blockchain.Block
mineGenesisBlock pubKey config =
    mineBlockInternal pubKey reward difficulty prevBlockHeaderHash []
  where
    reward              = Blockchain.initialMiningReward config
    difficulty          = Blockchain.initialDifficulty config
    prevBlockHeaderHash = Crypto.unsafeFromByteString "0000000000000000000000000000000000000000000000000000000000000000"

-- TODO: accept multiple public keys
mineBlockInternal
    :: Crypto.PublicKey -> Word.Word -> Blockchain.Difficulty
    -> Crypto.Hash Blockchain.BlockHeader -> [Blockchain.Transaction]
    -> IO Blockchain.Block
mineBlockInternal pubKey reward difficulty prevBlockHeaderHash txs = do
    header <- mineHeader
        prevBlockHeaderHash
        (Crypto.hash coinbaseTx)
        (Crypto.hashTreeRoot txs)
        difficulty

    return (Blockchain.Block header coinbaseTx txs)
  where
    coinbaseTx = Blockchain.CoinbaseTransaction $ pure (Blockchain.TransactionOut reward pubKey)

mineHeader
    :: Crypto.Hash Blockchain.BlockHeader
    -> Crypto.Hash Blockchain.CoinbaseTransaction
    -> Crypto.HashTreeRoot [Blockchain.Transaction]
    -> Blockchain.Difficulty
    -> IO Blockchain.BlockHeader
mineHeader prevBlockHeaderHash coinbaseTransactionHash transactionHashTreeRoot difficulty = do
    time <- Time.getCurrentTime

    let version = 0
        nonce   = 0
        initialHeader = Blockchain.BlockHeader{..}

    mineHeaderInternal initialHeader
  where
    -- TODO: get current time in case of int overflow,
    -- or in case current mining operation is getting close to oldest block time
    mineHeaderInternal header =
        if isValidDifficulty header
            then return header
            else mineHeaderInternal (incNonce header)

isValidDifficulty :: Blockchain.BlockHeader -> Bool
isValidDifficulty header = headerHashWordWord64 < difficulty
  where
    headerHashWordWord64 = Crypto.hashToWord64 (Crypto.hash header)
    difficulty           = Blockchain.unDifficulty (Blockchain.difficulty header)

incNonce :: Blockchain.BlockHeader -> Blockchain.BlockHeader
incNonce header = header { Blockchain.nonce = Blockchain.nonce header + 1 }
