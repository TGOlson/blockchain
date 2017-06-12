module Data.Blockchain.Core.Types.DifficultySpec (spec) where

import           TestUtil

import qualified Data.Aeson                            as Aeson

import           Data.Blockchain.Core.Types.Difficulty

spec :: Spec
spec =
    describe "Data.Blockchain.Core.Types.Difficulty" $ do
        safeJSONDeserializeSpec (Proxy :: Proxy Difficulty)
        roundTripJSONSpec (Proxy :: Proxy Difficulty)

        it "should handle invalid natural values when deserializing" $ once $
            Aeson.decode "[-1]" === (Nothing :: Maybe [Difficulty])