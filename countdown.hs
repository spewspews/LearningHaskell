import Control.Monad (guard, join, (<=<))
import Data.Maybe (mapMaybe)

main :: IO ()
main = print $ head $ solutions'' [1, 3, 7, 10, 25, 50] 765

data Op = Add | Sub | Mul | Div

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

valid :: Op -> Int -> Int -> Bool
valid Sub x y = x > y
valid Div x y = x `mod` y == 0
valid _ _ _ = True

valid' :: Op -> Int -> Int -> Bool
valid' Add x y = x <= y
valid' Sub x y = x > y
valid' Mul x y = x /= 1 && y /= 1 && x <= y
valid' Div x y = y /= 1 && x `mod` y == 0

apply :: Op -> Int -> Int -> Maybe Int
apply op x y = if valid op x y then return (o x y) else Nothing
  where
    o = case op of
        Add -> (+)
        Sub -> (-)
        Mul -> (*)
        Div -> div

apply' :: Op -> Int -> Int -> Maybe Int
apply' op x y = if valid' op x y then Just (o x y) else Nothing
  where
    o = case op of
        Add -> (+)
        Sub -> (-)
        Mul -> (*)
        Div -> div

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
    show (Val n) = show n
    show (App o l r) = brak l ++ show o ++ brak r
      where
        brak (Val n) = show n
        brak e = "(" ++ show e ++ ")"

values :: Expr -> [Int]
values (Val i) = [i]
values (App _ x y) = values x ++ values y

eval :: Expr -> Maybe Int
eval (Val i) = if i > 0 then return i else Nothing
eval (App o x y) = join $ apply o <$> eval x <*> eval y

-- Combinatorial Functions
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x : xs) = xss ++ map (x :) xss
  where
    xss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x l@(y : ys) = (x : l) : map (y :) (interleave x ys)

perms :: [a] -> [[a]]
perms = foldr (concatMap . interleave) [[]]

{-
perms = foldr f [[]]
  where
    f x xss = do
        xs <- xss
        interleave x xs
-}

choices :: [a] -> [[a]]
-- choices = subs >=> perms
-- choices = concatMap perms . subs
-- choices l = subs l >>= perms
-- choices l = perms =<< subs l
choices = perms <=< subs

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n =
    elem (values e) (choices ns) && eval e == Just n

bookExample :: Expr
bookExample = App Mul (App Add (Val 1) (Val 50)) (App Sub (Val 25) (Val 10))

isSolution :: Bool
isSolution = solution bookExample [1, 3, 7, 10, 25, 50] 765

split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x : xs) = ([x], xs) : [(x : ls, rs) | (ls, rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = do
    (ls, rs) <- split ns
    l <- exprs ls
    r <- exprs rs
    combine l r

{-
exprs ns =
    [ e
    | (ls, rs) <- split ns
    , l <- exprs ls
    , r <- exprs rs
    , e <- combine l r
    ]
-}

combine :: Expr -> Expr -> [Expr]
combine l r = map (\o -> App o l r) [Add, Sub, Mul, Div]

solutions :: [Int] -> Int -> [Expr]
solutions l n = concatMap (filter ((== Just n) . eval) . exprs) $ choices l

type Result = (Expr, Int)

results :: [Int] -> [Result]
results [] = []
results [x] = do guard (x > 0); return (Val x, x)
results xs = do
    (ls, rs) <- split xs
    lx <- results ls
    rx <- results rs
    combine' lx rx

combine' :: Result -> Result -> [Result]
combine' (l, x) (r, y) =
    mapMaybe
        (\o -> (App o l r,) <$> apply o x y)
        [Add, Sub, Mul, Div]

solutions' :: [Int] -> Int -> [Expr]
solutions' l n = do
    (e, m) <- results =<< choices l
    guard (m == n)
    return e

-- solutions' l n = map fst $ filter ((== n) . snd) $ concatMap results $ choices l

results' :: [Int] -> [Result]
results' [] = []
results' [x] = do guard (x > 0); return (Val x, x)
results' xs = do
    (ls, rs) <- split xs
    lx <- results ls
    rx <- results rs
    combine'' lx rx

combine'' :: Result -> Result -> [Result]
combine'' (l, x) (r, y) =
    mapMaybe
        (\o -> (App o l r,) <$> apply' o x y)
        [Add, Sub, Mul, Div]

solutions'' :: [Int] -> Int -> [Expr]
solutions'' l n = do
    (e, m) <- results' =<< choices l
    guard (m == n)
    return e
