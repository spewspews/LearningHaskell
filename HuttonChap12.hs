import Control.Monad (replicateM)
import Data.Char (digitToInt, isDigit)

-- data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show, Functor)

{-
instance Functor Tree where
    -- fmap :: (a -> b) -> Tree a -> Tree b
    fmap g (Leaf x) = Leaf (g x)
    fmap g (Node l r) = Node (fmap g l) (fmap g r)
-}

getChars :: Int -> IO String
getChars 0 = return []
getChars n = (:) <$> getChar <*> getChars (n - 1)

getChars' :: Int -> IO String
getChars' n = replicateM n getChar

pairs :: [a] -> [b] -> [(a, b)]
pairs xs ys = do
    x <- xs
    y <- ys
    return (x, y)

pairs' :: [a] -> [b] -> [(a, b)]
pairs' xs ys = (,) <$> xs <*> ys

newtype Pair a = P (a, a) deriving (Show)

instance Functor Pair where
    fmap f (P (x, y)) = P (f x, f y)

instance Applicative Pair where
    pure x = P (x, x)
    P (f, g) <*> P (x, y) = P (f x, g y)

{-
data Expr = Val Int | Div Expr Expr

eval :: Expr -> Maybe Int
eval (Val n) = Just n
eval (Div x y) = case eval x of
    Nothing -> Nothing
    Just n -> case eval y of
        Nothing -> Nothing
        Just m -> safediv n m

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv n m = Just (n `div` m)
-}

{-
type State = Int

newtype ST a = S {app :: State -> (a, State)}

instance Functor ST where
    -- fmap :: (a -> b) -> ST a -> ST b
    fmap g st = S (\s -> let (x, s') = app st s in (g x, s'))

instance Applicative ST where
    -- pure :: a -> ST a
    pure x = S (x,)

    -- (<*>) :: ST (a -> b) -> ST a -> ST b
    stf <*> stx =
        S
            ( \s ->
                let (f, s') = app stf s
                    (x, s'') = app stx s'
                 in (f x, s'')
            )

-- This version evaluates the "effect" in the opposite order.
newtype ST' a = S' {app' :: State -> (a, State)}

instance Functor ST' where
    -- fmap :: (a -> b) -> ST' a -> ST' b
    fmap g st = S' (\s -> let (x, s') = app' st s in (g x, s'))

instance Applicative ST' where
    -- pure :: a -> ST' a
    pure x = S' (x,)

    -- (<*>) :: ST' (a -> b) -> ST' a -> ST' b
    stf <*> stx =
        S'
            ( \s ->
                let (x, s') = app' stx s
                    (f, s'') = app' stf s'
                 in (f x, s'')
            )

instance Monad ST where
    -- (>>=) :: ST a -> (a -> ST b) -> ST b
    stx >>= f = S (\s -> let (x, s') = app stx s in app (f x) s')
-}

run :: ST a -> State -> a
run m = fst . app m

-- Relabelling trees
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show, Functor)

tree :: Tree Char
tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

rlabel :: Tree a -> Int -> (Tree Int, Int)
rlabel (Leaf _) n = (Leaf n, n + 1)
rlabel (Node l r) n = (Node l' r', n'')
  where
    (l', n') = rlabel l n
    (r', n'') = rlabel r n'

fresh :: ST Int
fresh = S (\n -> (n, n + 1))

alabel :: Tree a -> ST (Tree Int)
alabel (Leaf _) = Leaf <$> fresh
alabel (Node l r) = Node <$> alabel l <*> alabel r

mlabel :: Tree a -> ST (Tree Int)
mlabel (Leaf _) = Leaf <$> fresh
mlabel (Node l r) = do
    l' <- mlabel l
    r' <- mlabel r
    return $ Node l' r'

conv :: Char -> Maybe Int
conv c
    | isDigit c = Just (digitToInt c)
    | otherwise = Nothing

filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
filterM _ [] = return []
filterM p (x : xs) = do
    b <- p x
    ys <- filterM p xs
    return $ if b then x : ys else ys

-- Exercise 1
data Tree' a = Leaf' | Node' (Tree' a) a (Tree' a) deriving (Show)

instance Functor Tree' where
    fmap _ Leaf' = Leaf'
    fmap f (Node' l x r) = Node' (fmap f l) (f x) (fmap f r)

-- Exercise 2
newtype Hom a b = Hom (a -> b)

instance Functor (Hom a) where
    fmap :: (b -> c) -> Hom a b -> Hom a c
    -- fmap :: (b -> c) -> (a -> b) -> (a -> c)
    fmap f (Hom g) = Hom (f . g)

-- Exercise 3
instance Applicative (Hom a) where
    pure :: b -> Hom a b
    -- pure :: b -> a -> b
    pure = Hom . const

    (<*>) :: Hom a (b -> c) -> Hom a b -> Hom a c
    -- <*> :: (a -> b -> c) -> (a -> b) -> a -> c
    Hom f <*> Hom x = Hom $ \y -> f y $ x y

-- Exercise 4
newtype ZipList a = Z [a] deriving (Show)

instance Functor ZipList where
    -- fmap :: (a -> b) -> ZipList a -> ZipList b
    fmap f (Z x) = Z $ map f x

instance Applicative ZipList where
    pure :: a -> ZipList a
    pure = Z . repeat

    (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
    Z fs <*> Z xs = Z $ zipWith ($) fs xs

-- Exercise 5
-- pure id <*> x = x
--   x :: Applicative f => f a
--   id :: a -> a
--   pure id :: Applicative f => f (a -> a)
--   pure id <*> x :: Applicative f => f a
-- pure (g x) = pure g <*> pure x
--   g :: a -> b
--   x :: a
--   pure (g x) = pure g <*> pure x :: Applicative f => f b
-- x <*> pure y = pure (\x -> x y) <*> x
--   x :: Applicative f => f (a -> b)
--   y :: a
--   x <*> pure y :: Applicative f b
-- x <*> (y <*> z) = pure (.) <*> x <*> y <*> z
--   x :: Applicative f => f (b -> c)
--   y :: Applicative f => f (a -> b)
--   z :: Applicative f => f a
--   y <*> z :: Applicative f => f b
--   pure (.) :: Applicative f => f ((b -> c) -> (a -> b) -> a -> c)

-- Exercise 6
instance Monad (Hom a) where
    (>>=) :: Hom a b -> (b -> Hom a c) -> Hom a c
    -- (>>=) :: (a -> b) -> (b -> a -> c) -> a -> c
    Hom x >>= f = Hom $ \y -> let Hom f' = f $ x y in f' y

-- Exercise 7
data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving (Show)

instance Functor Expr where
    fmap :: (a -> b) -> Expr a -> Expr b
    fmap f (Var x) = Var $ f x
    fmap f (Add l r) = Add (fmap f l) (fmap f r)
    fmap _ (Val x) = Val x

instance Applicative Expr where
    -- pure :: a -> Expr a
    pure = Var

    -- <*> :: Expr (a -> b) -> Expr a -> Expr b
    Var f <*> Var x = Var $ f x
    Var f <*> Val i = Val i
    Var f <*> Add l r = Add (f <$> l) (f <$> r)
    Val i <*> _ = Val i
    Add fl fr <*> Var x = Add (fl <*> Var x) (fr <*> Var x)
    Add fl fr <*> Val i = Val i
    Add fl fr <*> Add l r = Add (fl <*> l) (fr <*> r)

instance Monad Expr where
    -- >>= :: Expr a -> (a -> Expr b) -> Expr b
    Var x >>= f = f x
    Val x >>= f = Val x
    Add l r >>= f = do
        l' <- l
        r' <- r
        Add (f l') (f r')

-- Exercise 8
type State = Int

newtype ST a = S {app :: State -> (a, State)}

instance Functor ST where
    -- fmap :: (a -> b) -> ST a -> ST b
    fmap f stx = f <$> stx

instance Applicative ST where
    -- pure :: a -> ST a
    pure x = S (x,)

    -- <*> :: ST (a -> b) -> ST a -> ST b
    stf <*> stx = do
        f <- stf
        f <$> stx

instance Monad ST where
    -- (>>=) :: ST a -> (a -> ST b) -> ST b
    S x >>= f = S $ \s -> let (x', s') = x s in app (f x') s'
