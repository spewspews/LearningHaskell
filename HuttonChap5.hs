import Data.Char
import Data.List

find :: (Eq a) => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

pairs :: [a] -> [(a, a)]
pairs xs = zip xs $ tail xs

sorted :: (Ord a) => [a] -> Bool
sorted xs = and [x <= y | (x, y) <- pairs xs]

positions :: (Eq a) => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0 ..], x' == x]

-- Caesar Cipher
lowers :: String -> Int
lowers xs = length [x | x <- xs, isLower x]

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x' == x]

let2int :: Char -> Char -> Int
let2int base c = ord c - ord base

int2let :: Char -> Int -> Char
int2let base n = chr $ ord base + n

shift :: Int -> Char -> Char
shift n c
    | isLower c = int2let 'a' $ (let2int 'a' c + n) `mod` 26
    | isUpper c = int2let 'A' $ (let2int 'A' c + n) `mod` 26
    | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [percent (count x lxs) n | x <- ['a' .. 'z']]
  where
    n = lowers lxs
    lxs = [toLower x | x <- xs]

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e) ^ 2) / e | (o, e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: String -> [(String, Float)]
crack xs = [(encode (-n) xs, freq) | (freq, n) <- ns]
  where
    ns = sort $ zip chitab [0 ..]
    chitab = [chisqr (rotate n table') table | n <- [0 .. 25]]
    table' = freqs xs

-- Ex 1.
sumSquares :: Int
sumSquares = sum [n ^ 2 | n <- [1 .. 100]]

-- Ex 2.
grid :: Int -> Int -> [(Int, Int)]
grid a b = [(a', b') | a' <- [0 .. a], b' <- [0 .. b]]

-- Ex 3.
square :: Int -> [(Int, Int)]
square a = [t | t@(x, y) <- grid a a, x /= y]

-- Ex 4.
replicate :: Int -> a -> [a]
replicate n x = [x | _ <- [1 .. n]]

-- Ex 5.
pyths :: Int -> [(Int, Int, Int)]
pyths n =
    [ (a, b, c)
    | a <- lim
    , b <- lim
    , c <- lim
    , a ^ 2 + b ^ 2 == c ^ 2
    ]
  where
    lim = [1 .. n]

-- Ex 6.
factors :: Int -> [Int]
factors n = [f | f <- [1 .. n], n `mod` f == 0]

perfects :: Int -> [Int]
perfects n =
    [ p
    | p <- [1 .. n]
    , (sum $ init $ factors p) == p
    ]

-- Ex 7.
ex7 = [(x, y) | x <- [1, 2], y <- [3, 4]]

sol7 = concat [[(a, b) | b <- [3, 4]] | a <- [1, 2]]

ex7Correct = ex7 == sol7

-- Ex 8.
ex8positions :: (Eq a) => a -> [a] -> [Int]
ex8positions x xs = Main.find x (zip xs [0 ..])

-- Ex 9.
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]

-- Ex 10.
-- Modifications made above.
