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

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

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

m2b :: Maybe Bool -> Bool
m2b (Just True) = True
m2b _ = False

eval :: Expr -> Maybe Int
eval (Val i)
  | i > 0 = Just i
  | otherwise = Nothing
eval (App o x y)
  | m2b (valid o <$> evalx <*> evaly) = apply o <$> evalx <*> evaly
  | otherwise = Nothing
  where
    evalx = eval x
    evaly = eval y
