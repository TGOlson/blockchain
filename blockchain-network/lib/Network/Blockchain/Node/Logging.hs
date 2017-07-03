module Network.Blockchain.Node.Logging
    ( Logger(..)
    , stdOutLogger
    ) where

import Data.Monoid     ((<>))
import Data.Time.Clock (getCurrentTime)

newtype Logger = Logger { runLogger :: String -> IO () }

stdOutLogger :: Logger
stdOutLogger = Logger $ \str -> do
    timestamp <- getCurrentTime
    putStrLn $ pad 30 (show timestamp) <> ": " <> str

pad :: Int -> String -> String
pad x str = str <> replicate (x - length str) ' '
