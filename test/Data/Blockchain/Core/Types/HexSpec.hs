module Data.Blockchain.Core.Types.HexSpec (spec) where

import           TestUtil

import           Data.Blockchain.Core.Types

spec :: Spec
spec =
    describe "Data.Blockchain.Core.Util.Hex" $ do
        safeJSONDeserializeSpec (Proxy :: Proxy Hex256)
        roundTripJSONSpec (Proxy :: Proxy Hex256)
