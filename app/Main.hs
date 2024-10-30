module Main where

import Test.QuickCheck

prop_sumGreater :: Int -> Int -> Bool
prop_sumGreater x y = (x + y) >= max x y

main :: IO ()
main = quickCheck prop_sumGreater
