module Data.Blockchain.Core.Util.HexSpec (spec) where

import           TestUtil

import           Data.Blockchain.Core.Util.Hex

spec :: Spec
spec =
    describe "Data.Blockchain.Core.Util.Hex" $ do
        safeJSONDeserializeSpec (Proxy :: Proxy Hex256)
        roundTripJSONSpec (Proxy :: Proxy Hex256)
