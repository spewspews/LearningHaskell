\documentclass{article}

\usepackage[letterpaper,margin=1.5in]{geometry}
\usepackage{fontspec}
\usepackage{unicode-math}
\usepackage{fancyvrb}
\usepackage{tikz}

\DefineVerbatimEnvironment{code}{Verbatim}{baselinestretch=.8, samepage=true}

\setmainfont{Garamond Premier Pro}[Contextuals=AlternateOff]
\setmathfont{Garamond Math}[Scale=MatchUppercase]
\setmonofont{Monaspace Argon}[Scale=0.7]

\setlength{\parindent}{0em}

\begin{document}

\section*{\textsc{Chapter 13}}
\vspace{10pt}

This is just the preamble, a few imports:

\begin{code}
{-# LANGUAGE LambdaCase #-}

import Control.Applicative
import Control.Monad (void)
import Data.Char
import System.IO (hSetEcho, stdin)
\end{code}

Here we define what a parser is and implement its Functor, Applicative and Monad classes.

\begin{code}
newtype Parser a = P {parse :: String -> Maybe (a, String)}

item :: Parser Char
item = P $ \case
    [] -> Nothing
    (c : cs) -> Just (c, cs)

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = P $ \s -> do
        (r, s') <- parse p s
        return (f r, s')

instance Applicative Parser where
    pure :: a -> Parser a
    pure x = P $ \s -> Just (x, s)
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    pf <*> px = P $ \s -> do
        (f, s') <- parse pf s
        parse (f <$> px) s'

three :: Parser String
three = g <$> item <*> item <*> item
  where
    g x y z = x : y : [z]

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    px >>= f = P $ \s -> do
        (x, s') <- parse px s
        parse (f x) s'
\end{code}

The same silly parser implemented two ways:

\begin{code}
threeM :: Parser String
threeM = do
    c0 <- item
    c1 <- item
    c2 <- item
    return $ c0 : c1 : [c2]

threeM' :: Parser String
threeM' = sequence [item, item, item]
\end{code}

A parser is also an Alternative Functor:

\begin{code}
instance Alternative Parser where
    empty :: Parser a
    empty = P $ const Nothing
    (<|>) :: Parser a -> Parser a -> Parser a
    pl <|> pr = P $ \s -> case parse pl s of
        Nothing -> parse pr s
        l -> l
\end{code}

We define some basic (atomic) parsers:

\begin{code}
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
\end{code}

And some more advanced parsers:

\begin{code}
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
\end{code}

And we define the parser for expressions now:

\begin{code}
expr :: Parser Int
expr = (+) <$> term <*> (symbol "+" *> expr <|> zero)

term :: Parser Int
term = (*) <$> factor <*> (symbol "*" *> term <|> one)

factor :: Parser Int
factor = symbol "(" *> expr <* symbol ")" <|> natural
\end{code}

Here is some code for displaying the calculator:

\begin{code}
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

display :: [Char] -> IO ()
display s = do
    writeat (3, 2) $ replicate 13 ' '
    writeat (3, 2) $ reverse $ take 13 $ reverse s
\end{code}

And code for controlling and running the calculator:

\begin{code}
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
\end{code}

\subsection*{\textsc{Exercises}}

\textsc{Exercise 1}

Define a parser \verb+comment :: Parser ()+ for ordinary Haskell comments that begin with the symbol \verb+--+ and extend to the end of the current line, which is represented by the control character \verb+'\n'+.

\begin{code}
comment :: Parser ()
comment = void $ string "--" *> many (sat (/= '\n')) *> char '\n'
\end{code}

\textsc{Exercise 2}

Using our second grammar for arithmetic expressions, draw the two possible parse trees for the expression \verb-2+3+4-.

\begin{center}
\begin{tikzpicture}
  [ edge from parent/.style={draw,-latex}
  , every text node part/.style={font=\itshape}
  , level distance=30pt
  ]
\node{expr}
  child{node{expr}
    child{node{expr}
        child{node{term}
        child{node{factor}
            child{node{nat}
                child{node{$2$}}}}}}
    child{node{$+$}}
    child{node{expr}
        child{node{term}
            child{node{factor}
                child{node{nat}
                    child{node{$3$}}}}}}}
  child{node{$+$}}
  child{node{expr}
    child{node{term}
        child{node{factor}
            child{node{nat}
                child{node{$4$}}}}}
  }
;
\end{tikzpicture}
\hspace{15pt}
\begin{tikzpicture}
  [ edge from parent/.style={draw,-latex}
  , every text node part/.style={font=\itshape}
  , level distance=30pt
  ]
\node{expr}
  child{node{expr}
    child{node{term}
        child{node{factor}
            child{node{nat}
                child{node{$2$}}}}}}
  child{node{$+$}}
  child{node{expr}
    child{node{expr}
        child{node{term}
        child{node{factor}
            child{node{nat}
                child{node{$3$}}}}}}
    child{node{$+$}}
    child{node{expr}
        child{node{term}
            child{node{factor}
                child{node{nat}
                    child{node{$4$}}}}}}}
;
\end{tikzpicture}
\end{center}

\textsc{Exercise 3}

Using our third grammar for arithmetic expessions, draw the parse trees for the expressions \verb-2+3-, \verb+2*3*4+ and \verb-(2+3)+4-

\begin{center}
\begin{tikzpicture}
  [ edge from parent/.style={draw,-latex}
  , every text node part/.style={font=\itshape}
  , level distance=30pt
  ]
\node{expr}
  child{node{term}
    child{node{factor}
      child{node{nat}
        child{node{$2$}}}}}
  child{node{$+$}}
  child{node{expr}
    child{node{term}
      child{node{factor}
        child{node{nat}
          child{node{$3$}}}}}}
;
\end{tikzpicture}

\begin{tikzpicture}
  [ edge from parent/.style={draw,-latex}
  , every text node part/.style={font=\itshape}
  , level distance=30pt
  ]
\node{expr}
  child{node{term}
    child{node{factor}
      child{node{nat}
        child{node{$2$}}}}
    child{node{$*$}}
    child{node{term}
      child{node{factor}
        child{node{nat}
          child{node{$3$}}}}
      child{node{$*$}}
      child{node{term}
        child{node{factor}
          child{node{nat}
            child{node{$4$}}}}}}}
;
\end{tikzpicture}

\begin{tikzpicture}
  [ edge from parent/.style={draw,-latex}
  , every text node part/.style={font=\itshape}
  , level distance=30pt
  ]
\node{expr}
  child{node{term}
    child{node{factor}
      child{node{$($}}
      child{node{expr}
        child{node{term}
          child{node{factor}
            child{node{nat}
              child{node{$2$}}}}}
        child{node{$+$}}
        child{node{expr}
          child{node{term}
            child{node{factor}
              child{node{nat}
                child{node{$3$}}}}}}}
      child{node{$)$}}}}
  child{node{$+$}}
  child{node{expr}
    child{node{term}
      child{node{factor}
        child{node{nat}
          child{node{$4$}}}}}}
;
\end{tikzpicture}
\end{center}

\textsc{Exercise 4:}\quad Explain Why the final simplification of the grammar for arithmetic expressions has a dramatic effect on the efficiency of the resulting parser. Hint: begin by considering how an expression comprising a single number would be parsed if this simplification step had not been made.
\vspace{8pt}

Without "left-factoring," the expression parser would be:

\begin{code}
exprUnfactored :: Parser Int
exprUnfactored = (+) <$> term <* symbol "+" <*> expr <|> term
\end{code}

To parse only a natural number, first \verb-term <* symbol "+" <*> expr- will be parsed, so \verb-term- will be parsed completely and then the parser will fail at \verb-symbol "+"-. Then the term will be parsed again and succeed. So the parser will parse the term twice.

\textsc{Exercise 5:} Define a suitable type \verb-Expr- for arithmetic expressions and modify the parser for expressions to have type \verb-expr :: Parser Expr-.

\begin{code}
data Expr = T Term (Maybe Expr) deriving (Show)
data Term = F Factor (Maybe Term) deriving (Show)
data Factor = E Expr | N Int deriving (Show)

nothing :: Parser (Maybe a)
nothing = pure Nothing

expr' :: Parser Expr
expr' = T <$> term' <*> (Just <$> (symbol "+" *> expr') <|> nothing)

term' :: Parser Term
term' = F <$> factor' <*> (Just <$> (symbol "*" *> term') <|> nothing)

factor' :: Parser Factor
factor' = E <$> (symbol "(" *> expr' <* symbol ")") <|> N <$> natural
\end{code}

\end{document}
