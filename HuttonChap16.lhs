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
\setmathfont{Libertinus Math}[Scale=MatchUppercase]
\setmonofont{Monaspace Argon}[Scale=0.7]

\setlength{\parindent}{1em}
\setlist{noitemsep}

\begin{document}

\begin{center}
\section*{Chapter 16}
\end{center}

\begin{code}
data Nat = Zero | Succ Nat deriving Show

add :: Nat -> Nat -> Nat
add Zero m = m
add (Succ n) m = Succ $ add n m
\end{code}

\begin{code}
reverse' :: [a] -> [a] -> [a]
reverse' xs ys = foldl (flip (:)) ys xs
\end{code}

\begin{code}
data Tree = Leaf Int | Node Tree Tree

flatten :: Tree -> [Int]
flatten (Leaf n) = [n]
flatten (Node l r) = flatten l ++ flatten r
\end{code}
We define a relation
\[\texttt{flatten' t ns = flatten t ++ ns}\]
and use this to derive \texttt{flatten'} as follows. Base case:
\begin{align*}
\texttt{flatten' (Leaf n) ns} &= \texttt{flatten (Leaf n) ++ ns}\\
&= \texttt{[n] ++ ns}\\
&= \texttt{n :: ns}
\end{align*}
Induction:
\begin{align*}
\texttt{flatten' (Node l r) ns} &= \texttt{flatten l ++ flatten r ++ ns}\\
&= \texttt{flatten l ++ flatten' r ns}\\
&= \texttt{flatten' l \$ flatten' r ns}
\end{align*}
So we define:
\begin{code}
flatten' :: Tree -> [Int] -> [Int]
flatten' (Leaf n) ns = n : ns
flatten' (Node l r) ns = flatten' l $ flatten' r ns

flatten2 :: Tree -> [Int]
flatten2 t = flatten' t []
\end{code}

\subsection*{Compiler correctness}

\begin{code}
data Expr = Val Int | Add Expr Expr deriving Show

eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y
\end{code}

\begin{code}
type Stack = [Int]
type Code = [Op]
data Op = PUSH Int | ADD deriving Show

exec :: Code -> Stack -> Stack
exec [] s = s
exec (PUSH x : ops) s = exec ops (x : s)
exec (ADD : ops) (x : y : s) = exec ops (y + x : s)
\end{code}

\begin{code}
comp :: Expr -> Code
comp (Val x) = [PUSH x]
comp (Add el er) = comp el ++ comp er ++ [ADD]

e :: Expr
e = Add (Add (Val 2) (Val 3)) (Val 4)
\end{code}
Compiler law version 1:
\[\texttt{exec (comp e) [] = [eval e]}\]
Compiler law version 2:
\[\texttt{exec (comp e) s = eval e : s}\]
\end{document}
