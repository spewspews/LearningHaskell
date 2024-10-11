import Data.Map.Strict (Map, fromList, (!))

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Show)

occurs :: (Eq a) => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) = x == y || occurs x l || occurs x r

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

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x) = s ! x
eval s (Not p) = not (eval s p)
eval s (And p q) = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q

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
isTaut p = all (\s -> eval s p) $ substs p
