import Data.Char

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
