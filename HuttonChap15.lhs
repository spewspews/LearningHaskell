\documentclass{article}

\usepackage[paperwidth=5.5in,paperheight=8.5in,margin=0.5in,footskip=.25in]{geometry}
\usepackage{fontspec}
\usepackage{mathtools}
\usepackage{enumitem}
\usepackage{unicode-math}
\usepackage{fancyvrb}
\usepackage{syntax}
\usepackage{tikz}

\DefineVerbatimEnvironment{code}{Verbatim}{baselinestretch=.8, samepage=true}

\setmainfont{Garamond Premier Pro}[Contextuals=AlternateOff]
\setmathfont{Garamond Math}[Scale=MatchUppercase]
\setmonofont{Monaspace Argon}[Scale=0.7]

\setlength{\parindent}{1em}
\setlist{noitemsep}

\begin{document}

\begin{center}
\section*{Chapter 15}
\end{center}

\begin{code}
primes :: [Int]
primes = sieve [2 ..]
  where
    sieve (p : xs) = p : sieve (filter (\x -> x `mod` p /= 0) xs)
\end{code}

\noindent
\textsc{Exercise 1:} Identify the redexes in the following expressions, and determine whether each redex is innermost, outermost, neither, or both:

\begin{Verbatim}[baselinestretch=.8, samepage=true]
    1 + (2*3)
    (1+2) * (2+3)
    fst (1+2, 2+3)
    (\x -> 1 + x) (2*3)
\end{Verbatim}
The expression \texttt{1 + (2*3)} has one redex \texttt{2*3} which is both innermost and outermost.

The expression \texttt{(1+2) * (2+3)} has two redexes, \texttt{1+2} and \texttt{2+3}. The redex \texttt{1+2} is both innermost and outermost.

The expression \texttt{fst (1+2, 2+3)} has three redexes, \texttt{1+2} is  innermost and the whole expression is outermost.

The expression \verb!(\x -> 1 + x) (2*3)! has two redexes, \texttt{2*3} which is innermost and the whole expression which is outermost. The expression \texttt{1 + x} inside the lambda is not a redex since we do not reduce under lambdas.
\vspace{8pt}

\noindent
\textsc{Exercise 2:} Show why outermost evaluation is preferable to innermost for the purposes of evaluating the expression \verb-fst (1+2,2+3)-.

Innermost evaluation results in:
\begin{align*}
\mathtt{fst}\ (1+2, 2+3) &= \mathtt{fst}\ (3, 2+3)\\
&= \mathtt{fst}\ (3, 5)\\
&= 3
\end{align*}

Outermost evaluation results in:
\begin{align*}
\mathtt{fst}\ (1+2,2+3) &= 1+2\\
&= 3
\end{align*}

So outermost evaluation does not evaluate \texttt{2+3} which is discarded anyway by \texttt{fst}.
\vspace{8pt}

\noindent
\textsc{Exercise 3:} Given the definition \verb$mult = \x -> (\y -> x * y)$, show how the evaluation of \texttt{mult 3 4} can be broken down into four separate steps.
\begin{align*}
\texttt{mult}\ 3\ 4 &= (λ x → (λ y → x × y))\ 3\ 4\\
&= (λ y → 3 × y)\ 4\\
&= 3 × 4\\
&= 12
\end{align*}
\noindent
\textsc{Exercise 4:} Using a list comprehension, define an expression \texttt{fibs :: [Integer]} that generates the infinite sequence of Fibonacci numbers
\[ 0, 1, 2, 3, 5, 8, 13, 21, 34, …\]
using the following simple procedure:

\begin{enumerate}
\item the first two numbers are $0$ and $1$;
\item the next is the sum of the previous two;
\item return to the second step.
\end{enumerate}

Hint: make use of the library functions \texttt{zip} and \texttt{tail}. Note that numbers in the Fibonacci sequence quickly become large, hence the use of the type \texttt{Integer} of arbitrary-precision integers above.

\begin{code}
fibs :: [Integer]
fibs = [0, 1] ++ zipWith (+) fibs (tail fibs)
\end{code}

\noindent
\textsc{Exercise 5:} Define appropriate versions of the library functions

\begin{Verbatim}[baselinestretch=.8, samepage=true]
repeat :: a -> [a]
repeat x = xs where xs = x : xs

take :: Int -> [a] -> [a]
take 0 _ = []
take _ [] = []
take n (x : xs) = x : take (n - 1) xs

replicate :: Int -> a -> [a]
replicate n = take n . repeat
\end{Verbatim}
for the following type of binary trees:

\begin{code}
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

repeatT :: a -> Tree a
repeatT x = Node (repeatT x) x (repeatT x)

takeT :: Int -> Tree a -> Tree a
takeT 0 _ = Leaf
takeT _ Leaf = Leaf
takeT n (Node tl x tr) = Node (takeT (n - 1) tl) x (takeT (n - 1) tr)

replicateT :: Int -> a -> Tree a
replicateT n = takeT n . repeatT
\end{code}

\noindent
\textsc{Exercise 6:} \textit{Newton's method} for computing the square root of a (non-negative) floating point number \texttt{n} can be expressed as follows:

\begin{itemize}
\item start with an initial approximation to the result;
\item given the current approximation \texttt{a}, the next approximation is defined by the function \verb$next a = (a + n/a) / 2$;
\item repeat the second step until the two most recent approximations are within some desired distance of one another, at which point the most recent value is returned as the result.
\end{itemize}
Define a function \verb$sqroot :: Double -> Double$ that implements this procedure. Hint: first produce an infinite list of approximations using the library function iterate. For simplicity, take the number \texttt{1.0} as the initial approximation, and \texttt{0.00001} as the distance value.

\begin{code}
approx :: Double -> [Double]
approx n = iterate next 1.0
  where
    next a = (a + n / a) / 2

sqroot :: Double -> Double
sqroot x =
    snd $
        head $
            dropWhile (\(x, y) -> abs (x - y) >= 0.00001) $
                zip a (tail a)
  where
    a = approx x
\end{code}
\end{document}
