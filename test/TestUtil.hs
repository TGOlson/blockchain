module TestUtil
    ( module X
    , propWithSize
    , propNumTests
    ) where

import Test.Hspec               as X
import Test.Hspec.QuickCheck    as X
import Test.QuickCheck          as X hiding (Result, labels, reason, theException, generate)
import Test.QuickCheck.Property as X

import Data.Blockchain.ArbitraryInstances ()

propWithSize :: Testable prop => Int -> String -> prop -> Spec
propWithSize n tag = modifyMaxSize (const n) . prop tag

propNumTests :: Testable prop => Int -> String -> prop -> Spec
propNumTests n tag = modifyMaxSuccess (const n) . prop tag
