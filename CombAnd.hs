import Data.Bits ((.&.))
import Data.List (foldl', sortBy)
import qualified Data.Vector.Unboxed as V

pow :: [a] -> [[a]]
pow [] = [[]]
pow (x : xs) = map (x :) powxs ++ powxs
  where
    powxs = pow xs

maxCombAndSlow :: [Int] -> Int
maxCombAnd a =
  fst $
    head $
      filter (\(_, v) -> v /= 0) $
        map (\xs -> (length xs, foldl' (.&.) maxBound xs)) $
          sortBy (\xs ys -> compare (length ys) (length xs)) $
            pow a

bitList :: (Integral a) => a -> [a]
bitList 0 = []
bitList i = i `mod` 2 : bitList (i `div` 2)

maxCombAnd :: [Int] -> Int
maxCombAndFast xs = V.maximum $ foldl' addBits (V.replicate 32 0) xs
  where
    addBits v x = V.unsafeAccum (+) v $ zip [0 .. 31] $ bitList x
