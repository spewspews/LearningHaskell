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
  Leaf x == Leaf y = x == y
  Node l x r == Node l' x' r' = l == l' && x == x' && r == r'
  _ == _ = False

data Prop
  = Const Bool
  | Var Char
  | Not Prop
  | And Prop Prop
  | Imply Prop Prop
  deriving (Show, Ord, Eq, Read)

p1 :: Prop
p1 = And (Var 'A') (Not $ Var 'A')

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

type Subst = Map Char Bool

evalP :: Subst -> Prop -> Bool
evalP _ (Const b) = b
evalP s (Var x) = s ! x
evalP s (Not p) = not (evalP s p)
evalP s (And p q) = evalP s p && evalP s q
evalP s (Imply p q) = evalP s p <= evalP s q

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False :) bss ++ map (True :) bss
  where
    bss = bools (n - 1)

rmdups :: (Eq a) => [a] -> [a]
rmdups [] = []
rmdups (x : xs) = x : rmdups (filter (/= x) xs)

substs :: Prop -> [Subst]
substs p = map fromList $ map (zip vs) $ bools $ length vs
  where
    vs = rmdups $ vars p

isTaut :: Prop -> Bool
isTaut p = all (\s -> evalP s p) $ substs p

-- Abstract Machine

data Expr = Val Int | Add Expr Expr deriving (Ord, Eq, Show, Read)

value :: Expr -> Int
value (Val x) = x
value (Add x y) = value x + value y

type Cont = [Op]

data Op = EVAL Expr | ADD Int

eval :: Expr -> Cont -> Int
eval (Val n) c = exec c n
eval (Add x y) c = eval x (EVAL y : c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVAL y : c) n = eval y (ADD n : c)
exec (ADD n : c) m = exec c (n + m)

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
occursS' x (Node l y r)
  | c == EQ = True
  | c == LT = occursS' x l
  | c == GT = occursS' x r
  where
    c = compare x y

-- Exercise 3
data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a) deriving (Show)

nLeaves :: Tree' a -> Int
nLeaves (Leaf' _) = 1
nLeaves (Node' x y) = nLeaves x + nLeaves y

balanced :: Tree' a -> Bool
balanced (Leaf' _) = True
balanced (Node' x y) = (abs $ nLeaves x - nLeaves y) <= 1

-- Exercise 4
split :: [a] -> ([a], [a])
split xs = (take h xs, drop h xs)
  where
    h = length xs `div` 2

balance :: [a] -> Tree' a
balance [x] = Leaf' x
balance xs
  | null f = balance s
  | otherwise = Node' (balance f) (balance s)
  where
    sp = split xs
    f = fst sp
    s = snd sp