module Main (main) where

import Network.Blockchain.Node (runNode)
import Options.Generic         (unwrapRecord)

main :: IO ()
main = unwrapRecord "Blockchain node" >>= runNode
