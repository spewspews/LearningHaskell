import qualified Data.IntMap.Strict as IntMap
import Data.Maybe

twoSum :: Int -> [Int] -> [(Int, Int)]
twoSum k = go mempty . zip [0 ..]
  where
    go seen [] = []
    go seen ((i, x) : ixs) =
      map (,i) (IntMap.findWithDefault [] (k - x) seen)
        ++ go (IntMap.alter (Just [i] <>) x seen) ixs

twoSumMaybe :: Int -> [Int] -> [(Int, Int)]
twoSumMaybe k = fromMaybe [] . go mempty . zip [0 ..]
  where
    go seen [] = Nothing
    go seen ((i, x) : ixs) =
      (map (,i) <$> IntMap.lookup (k - x) seen)
        <> go (IntMap.alter (Just [i] <>) x seen) ixs

twoSumFold :: Int -> [Int] -> [(Int, Int)]
twoSumFold k xs = foldr go (const []) (zip [0 ..] xs) mempty
  where
    go (i, n) cont seen =
      map (,i) (IntMap.findWithDefault [] (k - n) seen)
        ++ cont (IntMap.alter (Just [i] <>) n seen)
