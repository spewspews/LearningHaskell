import qualified Data.IntMap.Strict as IntMap

twoSum :: Int -> [Int] -> [(Int, Int)]
twoSum k = go mempty . zip [0 ..]
  where
    go seen [] = []
    go seen ((i, x) : ixs) =
      map (,i) (IntMap.findWithDefault [] (k - x) seen)
        ++ go (IntMap.alter (Just [i] <>) x seen) ixs

twoSumMaybe :: Int -> [Int] -> [(Int, Int)]
twoSumMaybe k = fromJust . go mempty . zip [0 ..]
  where
    go seen [] = Just []
    go seen ((i, x) : ixs) =
      (map (,i) <$> IntMap.lookup (k - x) seen)
        <> go (IntMap.alter (Just [i] <>) x seen) ixs

twoSum' :: Int -> [Int] -> [(Int, Int)]
twoSum' k xs = foldr go (const []) (zip [0 ..] xs) mempty
  where
    go (i, n) cont seen =
      map (,i) (IntMap.findWithDefault [] (k - n) seen)
        ++ cont (IntMap.alter (Just [i] <>) n seen)
