module Main where

import Test.QuickCheck

main :: IO ()
main = quickCheck eqInt

eqInt :: Int -> Property
eqInt i = i === i
