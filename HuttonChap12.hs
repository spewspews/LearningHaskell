import Control.Monad (replicateM)

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show, Functor)

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

instance Monad ST where
    -- (>>=) :: ST a -> (a -> ST b) -> ST b
    stx >>= f = S (\s -> let (x, s') = app stx s in app (f x) s')
