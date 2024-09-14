import Data.Map.Strict (Map, fromList, (!))

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Show)

occurs :: (Eq a) => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) = x == y || occurs x l || occurs x r

occursS :: (Ord a) => a -> Tree a -> Bool
occursS x (Leaf y) = x == y
occursS x (Node l y r)
    | x == y = True
    | x < y = occursS x l
    | x > y = occursS x r

flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

instance (Eq a) => Eq (Tree a) where
    (==) :: Eq a => Tree a -> Tree a -> Bool
    Leaf x == Leaf y = x == y
    Node l x r == Node l' x' r' = l == l' && x == x' && r == r'
    _ == _ = False

data Prop
    = Const Bool
    | Var Char
    | Not Prop
    | And Prop Prop
    | Or Prop Prop
    | Imply Prop Prop
    | Equiv Prop Prop
    deriving (Show, Ord, Eq, Read)

p1 :: Prop
p1 = And (Var 'A') (Not $ Var 'A')

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

p5 :: Prop
p5 = Or (Var 'A') (Not $ Var 'A')

type Subst = Map Char Bool

evalP :: Subst -> Prop -> Bool
evalP _ (Const b) = b
evalP s (Var x) = s ! x
evalP s (Not p) = not (evalP s p)
evalP s (And p q) = evalP s p && evalP s q
evalP s (Or p q) = evalP s p || evalP s q
evalP s (Imply p q) = evalP s p <= evalP s q
evalP s (Equiv p q) = evalP s p == evalP s q

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Or p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Equiv p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False :) bss ++ map (True :) bss
  where
    bss = bools (n - 1)

rmdups :: (Eq a) => [a] -> [a]
rmdups [] = []
rmdups (x : xs) = x : rmdups (filter (/= x) xs)

substs :: Prop -> [Subst]
substs p = map (fromList . zip vs) $ bools $ length vs
  where
    vs = rmdups $ vars p

isTaut :: Prop -> Bool
isTaut p = all (`evalP` p) $ substs p

-- Abstract Machine

data Expr
    = Val Int
    | Add Expr Expr
    | Mult Expr Expr
    deriving (Ord, Eq, Show, Read)

value :: Expr -> Int
value (Val x) = x
value (Add x y) = value x + value y
value (Mult x y) = value x * value y

type Cont = [Op]

data Op = AEVAL Expr | MEVAL Expr | ADD Int | MULT Int

eval :: Expr -> Cont -> Int
eval (Val n) c = exec c n
eval (Add x y) c = eval x (AEVAL y : c)
eval (Mult x y) c = eval x (MEVAL y : c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (AEVAL y : c) n = eval y (ADD n : c)
exec (MEVAL y : c) n = eval y (MULT n : c)
exec (ADD n : c) m = exec c (n + m)
exec (MULT n : c) m = exec c (n * m)

value' :: Expr -> Int
value' e = eval e []

-- Exercise 1
data Nat = Zero | Succ Nat deriving (Ord, Eq, Show, Read)

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ $ int2nat $ n - 1

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zero n = Zero
mult (Succ m) n = n `add` mult m n

-- Exercise 2
occursS' :: (Ord a) => a -> Tree a -> Bool
occursS' x (Leaf y) = x == y
occursS' x (Node l y r) =
    case compare x y of
        EQ -> True
        LT -> occursS' x l
        GT -> occursS' x r

-- Exercise 3
data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a) deriving (Show)

nLeaves :: Tree' a -> Int
nLeaves (Leaf' _) = 1
nLeaves (Node' x y) = nLeaves x + nLeaves y

balanced :: Tree' a -> Bool
balanced (Leaf' _) = True
balanced (Node' x y) = abs (nLeaves x - nLeaves y) <= 1

-- Exercise 4
split :: [a] -> ([a], [a])
split xs = splitAt (length xs `div` 2) xs

balance :: [a] -> Tree' a
balance [x] = Leaf' x
balance xs = Node' (balance ys) (balance zs)
  where
    (ys, zs) = split xs

-- Exercise 5
folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val x) = f x
folde f g (Add x y) = g (folde f g x) (folde f g y)

-- Exercise 6
eval' :: Expr -> Int
eval' = folde id (+)

size :: Expr -> Int
size = folde (const 1) (+)

-- Exercise 7
{-
instance (Eq a) => Eq (Maybe a) where
  Nothing == Nothing = True
  Just x == Just y = x == y
  _ == _ = False

instance (Eq a) => Eq [a] where
  [] == [] = True
  (x : xs) == (y : ys)
    | x == y = xs == ys
    | otherwise = False
  _ == _ = False
-}

-- Exercise 8 Done above

-- Exercise 9 Done above
