import Control.Monad (join)

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

apply :: Op -> Int -> Int -> Maybe Int
apply op x y = if valid op x y then return (o x y) else Nothing
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
