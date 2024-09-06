{-# LANGUAGE LambdaCase #-}

import Control.Applicative
import Data.Char

newtype Parser a = P {parse :: String -> Maybe (a, String)}

item :: Parser Char
item = P $ \case
    [] -> Nothing
    (c : cs) -> Just (c, cs)

instance Functor Parser where
    fmap f p = P $ \s -> do
        (r, s') <- parse p s
        return (f r, s')

instance Applicative Parser where
    pure x = P $ \s -> Just (x, s)
    pf <*> px = P $ \s -> do
        (f, s') <- parse pf s
        parse (f <$> px) s'

three :: Parser String
three = g <$> item <*> item <*> item
  where
    g x y z = x : y : [z]

instance Monad Parser where
    px >>= f = P $ \s -> do
        (x, s') <- parse px s
        parse (f x) s'

threeM :: Parser String
threeM = do
    c0 <- item
    c1 <- item
    c2 <- item
    return $ c0 : c1 : [c2]

threeM' :: Parser String
threeM' = sequence [item, item, item]

instance Alternative Parser where
    empty = P $ const Nothing
    pl <|> pr = P $ \s -> case parse pl s of
        Nothing -> parse pr s
        l -> l

sat :: (Char -> Bool) -> Parser Char
sat p = do
    c <- item
    if p c then return c else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha
