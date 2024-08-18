fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n - 1)

product :: (Num a) => [a] -> a
product [] = 1
product (n : ns) = n * Main.product ns

insert :: (Ord a) => a -> [a] -> [a]
insert x [] = [x]
insert x (y : ys)
    | x <= y = x : y : ys
    | otherwise = y : insert x ys

isort :: (Ord a) => [a] -> [a]
isort [] = []
isort (x : xs) = insert x $ isort xs

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n | n > 1 = fib (n - 1) + fib (n - 2)

drop :: Int -> [a] -> [a]
drop 0 xs = xs
drop _ [] = []
drop n (x : xs) = Main.drop (n - 1) xs

init :: [a] -> [a]
init [_] = []
init (x : xs) = x : Main.init xs

-- Ex. 1 Done above

-- Ex. 2
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)

-- Ex. 3
exp :: Int -> Int -> Int
exp a 0 = 1
exp a n = a * Main.exp a (n - 1)

-- exp 2 3 = 2 * exp 2 2
--         = 2 * 2 * exp 2 1
--         = 2 * 2 * 2 * exp 2 0
--         = 2 * 2 * 2 * 1 = 8

-- Ex 4.
euclid :: Int -> Int -> Int
euclid a b
    | a == b = a
    | a < b = euclid (b - a) a
    | a > b = euclid (a - b) b

-- Ex. 5 Skipped.

-- Ex. 6a
and :: [Bool] -> Bool
and [] = True
and (True : xs) = Main.and xs
and (False : _) = False

-- Ex. 6b
concat :: [[a]] -> [a]
concat [] = []
concat (x : xs) = x ++ Main.concat xs

-- Ex. 6c
replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n a = a : Main.replicate (n - 1) a

-- Ex. 6d
(!!) :: [a] -> Int -> a
(!!) (x : xs) 0 = x
(!!) (x : xs) n = xs Main.!! (n - 1)

-- Ex. 6e
elem :: (Eq a) => a -> [a] -> Bool
elem x' [] = False
elem x' (x : xs)
    | x' == x = True
    | otherwise = Main.elem x' xs

-- Ex 7.
merge :: (Ord a) => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge xs@(x : xtail) ys@(y : ytail)
    | x <= y = x : merge xtail ys
    | x > y = y : merge xs ytail

-- Ex 8.
msort :: (Ord a) => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort $ fst half) (msort $ snd half)
  where
    half = (Prelude.take halfl xs, Prelude.drop halfl xs)
    halfl = length xs `div` 2

-- Ex 9.
sum :: (Num a) => [a] -> a
sum [] = 0
sum (x : xs) = x + Main.sum xs

take :: Int -> [a] -> [a]
take 0 _ = []
take n (x : xs) = x : Main.take (n - 1) xs

last :: [a] -> a
last [x] = x
last (_ : xs) = Main.last xs
