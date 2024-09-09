{-# LANGUAGE LambdaCase #-}

import Control.Applicative
import Control.Monad (void)
import Data.Char
import System.IO (hSetEcho, stdin)

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
int = (\n -> -n) <$> (char '-' *> nat) <|> nat

token :: Parser a -> Parser a
token p = space *> p <* space

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
nats = symbol "[" *> ((:) <$> natural <*> many (symbol "," *> natural)) <* symbol "]"

zero :: Parser Int
zero = P $ \s -> Just (0, s)

one :: Parser Int
one = P $ \s -> Just (1, s)

expr :: Parser Int
{-
expr = (+) <$> term <*> (symbol "+" *> expr <|> zero)
-}
expr = do
    t <- term
    e <- symbol "+" *> expr <|> zero
    return $ t + e

term :: Parser Int
{-
term = (*) <$> factor <*> (term <|> one)
-}
term = do
    f <- factor
    t <- symbol "*" *> term <|> one
    return $ f * t

factor :: Parser Int
factor = symbol "(" *> expr <* symbol ")" <|> natural

{-
eval :: String -> Int
eval s = case parse expr s of
    Just (n, "") -> n
    Just (_, out) -> error $ "Unused input " ++ out
    Nothing -> error "Invalid input"
-}

box :: [String]
box =
    [ "+---------------+"
    , "|               |"
    , "+---+---+---+---+"
    , "| q | c | d | = |"
    , "+---+---+---+---+"
    , "| 1 | 2 | 3 | + |"
    , "+---+---+---+---+"
    , "| 4 | 5 | 6 | - |"
    , "+---+---+---+---+"
    , "| 7 | 8 | 9 | * |"
    , "+---+---+---+---+"
    , "| 0 | ( | ) | / |"
    , "+---+---+---+---+"
    ]

buttons :: String
buttons = standard ++ extra
  where
    standard = "qcd=123+456-789*0()/"
    extra = "QCD \ESC\BS\DEL\n"

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

writeat :: Pos -> String -> IO ()
writeat p xs = do
    goto p
    putStr xs

goto :: Pos -> IO ()
goto (x, y) = putStr $ "\ESC[" ++ show y ++ ";" ++ show x ++ "H"

getCh :: IO Char
getCh = do
    hSetEcho stdin False
    x <- getChar
    hSetEcho stdin True
    return x

showbox :: IO ()
showbox = sequence_ [writeat (1, y) b | (y, b) <- zip [1 ..] box]

display s = do
    writeat (3, 2) $ replicate 13 ' '
    writeat (3, 2) $ reverse $ take 13 $ reverse s

calc :: String -> IO ()
calc s = do
    display s
    c <- getCh
    if c `elem` buttons
        then process c s
        else do
            beep
            calc s

beep :: IO ()
beep = putStr "\BEL"

process :: Char -> String -> IO ()
process c s
    | c `elem` "qQ\ESC" = quit
    | c `elem` "d D\BS\DEL" = delete s
    | c `elem` "=\n" = eval s
    | c `elem` "cC" = clear
    | otherwise = press c s

quit :: IO ()
quit = goto (1, 14)

delete :: String -> IO ()
delete [] = calc []
delete s = calc $ init s

eval :: String -> IO ()
eval s = case parse expr s of
    Just (n, []) -> calc $ show n
    _ -> do
        beep
        calc s

clear :: IO ()
clear = calc []

press :: Char -> String -> IO ()
press c s = calc $ s ++ [c]

run :: IO ()
run = do
    cls
    showbox
    clear

-- Exercise 1
comment :: Parser ()
comment = void $ string "--" *> many (sat (/= '\n')) *> char '\n'
