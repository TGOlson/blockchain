module Data.Blockchain.Types.HexSpec (spec) where

import TestUtil

import Data.Blockchain.Types

spec :: Spec
spec =
    describe "Data.Blockchain.Util.Hex" $ do
        safeJSONDeserializeSpec (Proxy :: Proxy Hex256)
        roundTripJSONSpec (Proxy :: Proxy Hex256)
