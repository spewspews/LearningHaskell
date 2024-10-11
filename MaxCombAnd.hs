-- This is a solution to problem 2275. Largest Combination
-- With Bitwise AND Greater Than Zero from LeetCode:
-- https://leetcode.com/problems/largest-combination-with-bitwise-and-greater-than-zero/description/

import Control.Monad (zipWithM_)
import Control.Monad.ST (runST)
import Data.Bits ((.&.))
import Data.List (foldl', sortBy)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

pow :: [a] -> [[a]]
pow [] = [[]]
pow (x : xs) = map (x :) powxs ++ powxs
  where
    powxs = pow xs

maxCombAndSlow :: [Int] -> Int
maxCombAndSlow =
    fst
        . head
        . filter (\(_, v) -> v /= 0)
        . map (\xs -> (length xs, foldl' (.&.) maxBound xs))
        . sortBy (\xs ys -> compare (length ys) (length xs))
        . pow

bitList :: (Integral a) => a -> [a]
bitList = map (`mod` 2) . takeWhile (> 0) . iterate (`div` 2)

maxCombAnd :: [Int] -> Int
maxCombAnd = V.maximum . foldl' addBits (V.replicate 32 0)
  where
    addBits v x = V.unsafeAccum (+) v $ zip [0 .. 31] (bitList x)

maxCombAnd' :: [Int] -> Int
maxCombAnd' xs = V.maximum $ runST $ do
    v <- MV.replicate 32 0
    mapM_ (addBits v) xs
    V.unsafeFreeze v
  where
    addBits v x =
        zipWithM_ (\i b -> MV.unsafeModify v (+ b) i) [0 .. 31] (bitList x)
