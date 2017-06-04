module Main where

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as Lazy
import qualified System.Environment   as Env

import qualified Data.Blockchain.Core.Blockchain as Blockchain
import qualified Data.Blockchain.Core.Crypto     as Crypto
import qualified Data.Blockchain.Core.Types      as Blockchain
import qualified Data.Blockchain.Mining.Block    as Mining

data Command
    = SingletonChain
  deriving (Read, Show)

main :: IO ()
main = Env.getArgs >>= \case
    [x] -> case read x of SingletonChain -> generateSingletonChain
    xs  -> error ("Invalid arguments: " ++ show xs)

generateSingletonChain :: IO ()
generateSingletonChain = do
    blockchain <- Mining.createBlockchain config -- TODO: diff module
    writeJSON "data/singleton_chain/blockchain.json" blockchain
  where
    config = Blockchain.BlockchainConfig
        { initialDifficulty             = Blockchain.Difficulty 1000
        , targetSecondsPerBlock         = 60
        , difficultyRecalculationHeight = 100
        , initialMiningReward           = 100
        , miningRewardTransitionMap     = mempty
        }

mineBlock :: IO ()
mineBlock = do
    blockchain <- readBlockchain "data/blockchain.json"
    (Crypto.KeyPair pubKey privKey) <- Crypto.generate

    block <- Mining.mineBlock pubKey mempty blockchain
    let blockchain' = either (error . show) id $ Blockchain.addBlock block blockchain

    writeJSON "data/coinbase2privateKey.json" (show privKey)
    writeJSON "data/block2.json" block
    writeJSON "data/blockchain.json" blockchain'

readBlockchain :: FilePath -> IO Blockchain.Blockchain
readBlockchain filePath = do
    rawChain <- Lazy.readFile filePath

    let unverifiedBlockchain = either (error . show) id $ Aeson.eitherDecode rawChain
        blockchain           = either (error . show) id $ Blockchain.verifyBlockchain unverifiedBlockchain

    return blockchain


writeJSON :: Aeson.ToJSON a => FilePath -> a -> IO ()
writeJSON path = Lazy.writeFile path . Aeson.encode
