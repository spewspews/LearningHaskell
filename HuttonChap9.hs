import Control.Monad (join, (>=>))
import Data.List (delete)
import Data.Maybe (mapMaybe)

main :: IO ()
main = do
    print $ length possible
    print $ length successful

data Op = Add | Sub | Mul | Div

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
    show (Val n) = show n
    show (App o l r) = brak l ++ show o ++ brak r
      where
        brak (Val n) = show n
        brak e = "(" ++ show e ++ ")"

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

-- Exercise 1.
-- choices l = [p | s <- subs l, p <- perms s]
choices :: [a] -> [[a]]
choices = subs >=> perms

-- Exercise 2.
isChoice :: (Eq a) => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice (x : xs) ys
    | x `elem` ys = isChoice xs (delete x ys)
    | otherwise = False

-- Exercise 3.
-- No effect, we would be drawing from an empty list
-- meaning that we would not call results on that (empty) side.

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

combine :: Expr -> Expr -> [Expr]
combine l r = map (\o -> App o l r) [Add, Sub, Mul, Div]

eval :: Expr -> Maybe Int
eval (Val i) = Just i
eval (App o x y) = join $ apply o <$> eval x <*> eval y

valid :: Op -> Int -> Int -> Bool
valid Div x y = y /= 0 && x `mod` y == 0
valid _ _ _ = True

apply :: Op -> Int -> Int -> Maybe Int
apply op x y = if valid op x y then return (o x y) else Nothing
  where
    o = case op of
        Add -> (+)
        Sub -> (-)
        Mul -> (*)
        Div -> div

-- Exercise 4.
possible = concatMap exprs $ choices [1, 3, 7, 10, 25, 50]
successful = mapMaybe eval possible
