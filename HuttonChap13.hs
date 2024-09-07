{-# LANGUAGE LambdaCase #-}

import Control.Applicative
import Control.Monad (void)
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

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char c = sat (== c)

string :: String -> Parser String
string "" = return ""
string s@(x : xs) = do
    char x
    string xs
    return s

ident :: Parser String
ident = (:) <$> lower <*> many alphanum

nat :: Parser Int
nat = read <$> some digit

space :: Parser ()
space = void $ many $ sat isSpace

int :: Parser Int
int = (\_ n -> -n) <$> char '-' <*> nat <|> nat

token :: Parser a -> Parser a
token p = (\_ p _ -> p) <$> space <*> p <*> space

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol s = token $ string s

nats :: Parser [Int]
{-
nats = do
    symbol "["
    n <- natural
    ns <- many $ (\_ n -> n) <$> symbol "," <*> natural
    symbol "]"
    return $ n : ns
-}
nats =
    (\_ n ns _ -> n : ns)
        <$> symbol "["
        <*> natural
        <*> many ((\_ n -> n) <$> symbol "," <*> natural)
        <*> symbol "]"

expr :: Parser Int
expr = do
    t <- term
    do
        symbol "+"
        e <- expr
        return (t + e)
        <|> return t

term :: Parser Int
term = do
    f <- factor
    do
        symbol "*"
        t <- term
        return $ f * t
        <|> return f

factor :: Parser Int
factor =
    do
        symbol "("
        e <- expr
        symbol ")"
        return e
        <|> natural
