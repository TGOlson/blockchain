module TestUtil
    ( module X
    , Proxy.Proxy(..)
    , propWithSize
    , propNumTests
    , roundTripJSONSpec
    , safeJSONDeserializeSpec
    ) where

import           Test.Hspec                         as X
import           Test.Hspec.QuickCheck              as X
import           Test.QuickCheck                    as X hiding (Result, generate, labels, reason,
                                                          theException)
import           Test.QuickCheck.Property           as X

import           Control.DeepSeq
import qualified Data.Aeson                         as Aeson
import qualified Data.ByteString                    as BS
import qualified Data.Data                          as Data
import qualified Data.Proxy                         as Proxy

import           Data.Blockchain.ArbitraryInstances ()


propWithSize :: Testable prop => Int -> String -> prop -> Spec
propWithSize n tag = modifyMaxSize (const n) . prop tag

propNumTests :: Testable prop => Int -> String -> prop -> Spec
propNumTests n tag = modifyMaxSuccess (const n) . prop tag

-- Serialization Utils

roundTripJSONSpec :: (Arbitrary a, Aeson.FromJSON a, Aeson.ToJSON a, Data.Typeable a, Show a, Eq a) => Proxy.Proxy a -> Spec
roundTripJSONSpec proxy = prop ("should round-trip json serialize " ++ tag) $ testRoundTripJSON proxy
  where
    tag = show (Data.typeRep proxy)

testRoundTripJSON :: (Aeson.FromJSON a, Aeson.ToJSON a, Show a, Eq a) => Proxy.Proxy a -> a -> Property
testRoundTripJSON _ x = Aeson.decode (Aeson.encode x) === Just x

safeJSONDeserializeSpec :: (Aeson.FromJSON a, Show a, Data.Typeable a) => Proxy.Proxy a -> Spec
safeJSONDeserializeSpec proxy = prop ("should be safe when deserializing " ++ tag) $ testSafeJSONDeserialize proxy
  where
    tag = show (Data.typeRep proxy)

testSafeJSONDeserialize :: forall a. (Aeson.FromJSON a, Show a) => Proxy.Proxy a -> BS.ByteString -> Bool
testSafeJSONDeserialize _ bs = canEval $ show (Aeson.decodeStrict' json :: Maybe [a])
  where
    json = mconcat [ "[\"", bs, "\"]" ]

canEval :: NFData a => a -> Bool
canEval x = deepseq x True
