module Main where

import qualified Data.Aeson                      as Aeson
import qualified Data.ByteString.Lazy            as Lazy

import qualified Data.Blockchain.Core.Blockchain as Blockchain
import qualified Data.Blockchain.Core.Crypto     as Crypto
import qualified Data.Blockchain.Core.Types      as Blockchain
import qualified Data.Blockchain.Mining.Block    as Mining

main :: IO ()
main = generateSingletonChain

generateSingletonChain :: IO ()
generateSingletonChain = do
    blockchain <- Mining.createBlockchain Blockchain.defaultConfig -- TODO: diff module

    (Crypto.KeyPair pubKey privKey) <- Crypto.generate

    block <- Mining.mineBlock pubKey mempty blockchain

    writeJSON "data/singleton_chain/blockchain.json" blockchain
    writeJSON "data/singleton_chain/valid_next_block.json" block
    writeJSON "data/singleton_chain/valid_next_block_coinbase_private_key.json" privKey

readBlockchain :: FilePath -> IO (Blockchain.Blockchain Blockchain.Validated)
readBlockchain filePath = do
    rawChain <- Lazy.readFile filePath

    let unverifiedBlockchain = either (error . show) id $ Aeson.eitherDecode rawChain
        blockchain           = either (error . show) id $ Blockchain.validate unverifiedBlockchain

    return blockchain


writeJSON :: Aeson.ToJSON a => FilePath -> a -> IO ()
writeJSON path = Lazy.writeFile path . Aeson.encode
