import Data.Char
import Data.List (sort)
import Data.Map (Map, alter, findWithDefault, keys)
import Data.Monoid (Sum (Sum), getSum)

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
bin2int = foldr (\b -> (b +) . (2 *)) 0

{-
bin2int :: [Bit] -> Int
bin2int = sum . zipWith (*) (iterate (* 2) 1)
-}

int2bin :: Int -> [Bit]
int2bin = map (`mod` 2) . takeWhile (> 0) . iterate (`div` 2)

{-
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

int2bin' :: Int -> [Bit]
int2bin' n = map (\x -> (n `div` x) `mod` 2) $ iterate (* 2) 1
-}

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

parity :: [Bit] -> [Bit]
parity b = (sum b) `mod` 2 : b

encode :: String -> [Bit]
encode = concatMap (parity . make8 . int2bin . ord)

chop :: Int -> [Bit] -> [[Bit]]
chop _ [] = []
chop c xs = take c xs : chop c (drop c xs)

checkParity :: [Bit] -> [Bit]
checkParity [] = error "No parity bit"
checkParity (b : bs) =
    if sum bs `mod` 2 == b then bs else error "Wrong parity bit"

decode :: [Bit] -> String
decode = map (chr . bin2int . checkParity) . chop 9

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

-- Voting Algorithms

votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

tally :: (Ord a) => [a] -> Map a Int
tally = (getSum <$>) . foldr (alter (Just (Sum 1) <>)) mempty

-- count xs x = length $ filter (== x) xs
count :: (Ord a) => [a] -> a -> Int
count vs v = findWithDefault 0 v $ tally vs

rmdups :: (Eq a) => [a] -> [a]
rmdups [] = []
rmdups (x : xs) = x : rmdups (filter (/= x) xs)

-- result vs = sort [(count vs v, v) | v <- rmdups vs]
result :: (Ord a) => [a] -> [(Int, a)]
result vs = sort $ map (\v -> (findWithDefault 0 v tallied, v)) $ keys tallied
  where
    tallied = tally vs

winner :: (Ord a) => [a] -> a
winner = snd . last . result

ballots :: [[String]]
ballots =
    [ ["Red", "Green"]
    , ["Blue"]
    , ["Green", "Red", "Blue"]
    , ["Blue", "Green", "Red"]
    , ["Green"]
    ]

rmempty :: [[a]] -> [[a]]
rmempty = filter (not . null)

elim :: (Eq a) => a -> [[a]] -> [[a]]
elim x = map $ filter (/= x)

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

-- Exercise 4
dec2int :: [Int] -> Int
dec2int = foldl ((+) . (* 10)) 0

-- Exercise 5
curry' :: ((a, b) -> c) -> a -> b -> c
curry' f a b = f (a, b)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (a, b) = f a b

-- Exercise 6
-- unfold p h t = map h . takeWhile (not . p) . iterate t
unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x
    | p x = []
    | otherwise = h x : unfold p h t (t x)

-- chop8' = map (take 8) . takeWhile (not . null) . iterate (drop 8)
chop8' :: [Bit] -> [[Bit]]
chop8' = unfold null (take 8) (drop 8)

map'' :: (a -> b) -> [a] -> [b]
map'' f = unfold null (f . head) (drop 1)

iterate' :: (a -> a) -> a -> [a]
iterate' = unfold (const False) id

-- Exercise 7 and 8
-- Done above

-- Exercise 9
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g [] = []
altMap f g (x : xs) = f x : altMap g f xs

-- Exercise 10
luhnDouble :: Int -> Int
luhnDouble x
    | d > 9 = d - 9
    | otherwise = d
  where
    d = 2 * x

luhn :: [Int] -> Bool
luhn = (== 0) . (`mod` 10) . sum . altMap luhnDouble id
