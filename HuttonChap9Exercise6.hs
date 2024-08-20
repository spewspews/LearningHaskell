import Control.Monad (guard, (>=>))
import Data.List (sortBy)
import Data.Maybe (mapMaybe)

data Op = Add | Sub | Mul | Div | Exp

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Exp = "^"

valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 0 && y /= 1 && x `mod` y == 0
valid Exp _ y = y >= 0

apply :: Op -> Int -> Int -> Maybe Int
apply op x y = if valid op x y then Just (o x y) else Nothing
  where
    o = case op of
        Add -> (+)
        Sub -> (-)
        Mul -> (*)
        Div -> div
        Exp -> (^)

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

choices :: [a] -> [[a]]
choices = subs >=> perms

split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x : xs) = ([x], xs) : [(x : ls, rs) | (ls, rs) <- split xs]

type Result = (Expr, Int)

results :: [Int] -> [Result]
results [] = []
results [x] = do guard (x > 0); return (Val x, x)
results xs = do
    (ls, rs) <- split xs
    lx <- results ls
    rx <- results rs
    combine lx rx

combine :: Result -> Result -> [Result]
combine (l, x) (r, y) =
    mapMaybe
        (\o -> (App o l r,) <$> apply o x y)
        [Add, Sub, Mul, Div, Exp]

allResults :: [Int] -> [Result]
allResults = choices >=> results

solutions :: [Int] -> Int -> [Expr]
solutions l n = do
    (e, m) <- allResults l
    guard (m == n)
    return e

tolerance :: Int -> [Int] -> Int -> [Expr]
tolerance t l n = do
    (e, m) <- allResults l
    guard (abs (m - n) <= t)
    return e

closest :: [Int] -> Int -> [Result]
closest l n =
    sortBy (\(_, x) (_, y) -> compare (abs $ x - n) (abs $ y - n)) $
        allResults l

ordered :: [Int] -> Int -> [Expr]
ordered l n = sortBy (\el er -> compare (length $ values el) (length $ values er)) $ solutions l n
