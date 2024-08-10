import Data.Char
import Data.List (sort)

{-
twice :: (a -> a) -> a -> a
twice f x = f $ f x

sumsqreven :: [Int] -> Int
sumsqreven = sum . map (^ 2) . filter even

suml :: [Int] -> Int
suml = foldl (+) 0

sumr :: [Int] -> Int
sumr = foldr (+) 0

lengthr :: [a] -> Int
lengthr = foldr (\_ n -> 1 + n) 0

lengthl :: [a] -> Int
lengthl = foldl (\n _ -> n + 1) 0

reverser :: [a] -> [a]
reverser = foldr (\x v -> v ++ [x]) []

reversel :: [a] -> [a]
reversel = foldl (\v x -> x : v) []
-}

type Bit = Int

bin2int :: [Bit] -> Int
bin2int = foldr (\b x -> b + 2 * x) 0

{-
bin2int :: [Bit] -> Int
bin2int = sum . zipWith (*) (iterate (* 2) 1)
-}

int2bin :: Int -> [Bit]
int2bin = map (`mod` 2) . iterate (`div` 2)

{-
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

int2bin' :: Int -> [Bit]
int2bin' n = map (\x -> (n `div` x) `mod` 2) $ iterate (* 2) 1
-}

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concatMap (take 8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 xs = take 8 xs : chop8 (drop 8 xs)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

-- Voting Algorithms

votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count :: (Eq a) => a -> [a] -> Int
count x = length . filter (== x)

rmdups :: (Eq a) => [a] -> [a]
rmdups [] = []
rmdups (x : xs) = x : rmdups (filter (/= x) xs)

result :: (Ord a) => [a] -> [(Int, a)]
result vs = sort [(count v vs, v) | v <- rmdups vs]

winner :: (Ord a) => [a] -> a
winner = snd . last . result

ballots :: [[String]]
ballots =
  [ ["Red", "Green"],
    ["Blue"],
    ["Green", "Red", "Blue"],
    ["Blue", "Green", "Red"],
    ["Green"]
  ]

rmempty :: [[a]] -> [[a]]
rmempty = filter (not . null)

elim :: (Eq a) => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

rank :: (Ord a) => [[a]] -> [a]
rank = map snd . result . map head . rmempty

winner' :: (Ord a) => [[a]] -> a
winner' bs = case rank bs of
  [w] -> w
  c : _ -> winner' $ elim c bs

-- Exercise 1
filterMap :: (a -> Bool) -> (a -> b) -> [a] -> [b]
filterMap p f = map f . filter p

-- Exercise 2.
-- a.
all' :: (a -> Bool) -> [a] -> Bool
all' p = and . map p

-- b.
any' :: (a -> Bool) -> [a] -> Bool
any' p = or . map p

-- c.
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x : xs)
  | p x = x : takeWhile' p xs
  | otherwise = []

-- d.
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p v@(x : xs)
  | p x = dropWhile' p xs
  | otherwise = v

-- Exercise 3.
map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x -> if p x then (x :) else id) []
