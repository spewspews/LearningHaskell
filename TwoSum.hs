import Data.IntMap.Strict (IntMap, Key, alter, (!?))
import qualified Data.IntMap.Strict as Map
import Data.Maybe

(!) :: IntMap [a] -> Key -> [a]
(!) m k = Map.findWithDefault [] k m

twoSum :: Int -> [Int] -> [(Int, Int)]
twoSum k = go mempty . zip [0 ..]
  where
    go idxs [] = []
    go idxs ((i, x) : ixs) =
      map (,i) (idxs ! (k - x))
        ++ go (alter (Just [i] <>) x idxs) ixs

twoSum' :: Int -> [Int] -> [(Int, Int)]
twoSum' k xs =
  concatMap (\(i, x) -> map (,i) $ takeWhile (< i) (indexes ! (k - x))) indexed
  where
    indexed = zip [0 ..] xs
    indexes = foldr (\(i, x) -> alter (Just [i] <>) x) mempty indexed

twoSum'' :: Int -> [Int] -> [(Int, Int)]
twoSum'' k xs =
  foldr (\(i, x) acc -> map (,i) (takeWhile (< i) (idxs ! (k - x))) ++ acc) [] ixs
  where
    ixs = zip [0 ..] xs
    idxs = foldr (\(i, x) -> alter (Just [i] <>) x) mempty ixs

twoSumMaybe :: Int -> [Int] -> [(Int, Int)]
twoSumMaybe k = fromMaybe [] . go mempty . zip [0 ..]
  where
    go idxs [] = Nothing
    go idxs ((i, x) : ixs) =
      (map (,i) <$> (idxs !? (k - x)))
        <> go (Map.alter (Just [i] <>) x idxs) ixs

twoSumFold :: Int -> [Int] -> [(Int, Int)]
twoSumFold k xs = foldr go (const []) (zip [0 ..] xs) mempty
  where
    go (i, n) cont idxs =
      map (,i) (Map.findWithDefault [] (k - n) idxs)
        ++ cont (Map.alter (Just [i] <>) n idxs)
