band b c
  | b == c = b
  | otherwise = False

bsignum n
  | n < 0 = -1
  | n == 0 = 0
  | otherwise = 1

btest ('a' : _) = True
btest _ = False

bconst :: a -> (b -> a)
bconst x = \_ -> x

odds :: Int -> [Int]
odds n = map (\x -> x * 2 + 1) [0 .. n - 1]

-- Ex. 1
halve :: [a] -> ([a], [a])
halve xs = (take l xs, drop l xs)
  where
    l = length xs `div` 2

-- Ex. 2
-- a.
third1 :: [a] -> a
third1 xs = head $ tail $ tail xs

-- b.
third2 :: [a] -> a
third2 = (!! 2)

-- c.
third3 :: [a] -> a
third3 (_ : _ : v : _) = v

-- Ex. 3
-- a.
safetail1 :: [a] -> [a]
safetail1 xs = if null xs then xs else tail xs

-- b.
safetail2 :: [a] -> [a]
safetail2 xs
  | null xs = xs
  | otherwise = tail xs

-- c.
safetail3 :: [a] -> [a]
safetail3 [] = []
safetail3 (_ : xs) = xs

-- Ex. 4 OMITTED

-- Ex. 5
myAnd a b =
  if a
    then if b then True else False
    else False

-- Ex. 6
myAnd2 a b = if a then b else False

-- Ex. 7
myMult :: Int -> Int -> Int -> Int
myMult = \x -> \y -> \z -> x * y * z

-- Ex. 8
luhnDouble :: Int -> Int
luhnDouble x
  | d > 9 = d - 9
  | otherwise = d
  where
    d = x + x

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = s `mod` 10 == 0
  where
    a' = luhnDouble a
    c' = luhnDouble c
    s = sum [a', b, c', d]

luhn2 :: Int -> Int -> Int -> Int -> Bool
luhn2 a b c d =
  let a' = luhnDouble a
      c' = luhnDouble c
      s = sum [a', b, c', d]
   in s `mod` 10 == 0
