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

\newcommand{\ttx}{\texttt}

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
reverse' [] ys = ys
reverse' (x : xs) ys = reverse' xs (x : ys)
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

\noindent
Compiler law version 1:
\[\texttt{exec (comp e) [] = [eval e]}\]
Compiler law version 2:
\[\texttt{exec (comp e) s = eval e : s}\]
This is proved in the book but the proof needs the distributivity property proved below.

The distributivity property is that \texttt{exec} distributes over lists, that executing two pieces of code appended together is the same as executing the two pieces of code in sequence:
\[\texttt{exec (c ++ d) s = exec d \$ exec c s}\]
Inductive Case:
\begin{align*}
\ttx{exec ((PUSH n : c) ++ d) s} &= \ttx{exec (PUSH n : c ++ d) s} &\textrm{Apply }\ttx{++}\\
&= \ttx{exec (c ++ d) (n : s)} &\textrm{Apply }\ttx{exec}\\
&= \ttx{exec d \$ exec c (n : s)} &\textrm{Apply induction}\\
&= \ttx{exec d \$ exec (PUSH n : c) s} &\textrm{Unapply exec}
\end{align*}
Other inductive case:
\begin{align*}
\ttx{exec ((ADD : c) ++ d) s} &= \ttx{exec (Add : c ++ d) (x : y : s')} &\textrm{Apply }\ttx{++}\\
&= \ttx{exec (c ++ d) (y + x : s')}&\textrm{Apply }\ttx{exec}\\
&= \ttx{exec d \$ exec c (y + x : s')}&\textrm{Apply induction}\\
&=\ttx{exec d \$ exec (ADD : c) s}&\textrm{Unapply exec}
\end{align*}

\noindent
Making append vanish with \ttx{comp}: a new \ttx{comp} that satisfies:
\[\ttx{comp' e c} = \ttx{comp e ++ c}\]
This will allow us to get rid of
\begin{verbatim}
comp (Add el er) = comp el ++ comp er ++ [ADD]
\end{verbatim}
to replace it with
\begin{verbatim}
comp' (Add el er) c = comp' el $ comp' er $ ADD : c
\end{verbatim}
In fact that's the definition:
\begin{code}
comp' :: Expr -> Code -> Code
comp' (Val x) c = PUSH x : c
comp' (Add el er) c = comp' el $ comp' er $ ADD : c
\end{code}
With this new \ttx{comp'} the validity with respect to \ttx{eval} is:
\[\ttx{exec (comp' e c) s} = \ttx{exec c \$ eval e : s}\]
The advantage of this new definition is we do not need the distributivity lemma, but can prove this directly:

Base case:
\begin{align*}
\ttx{exec (comp' (Val x) c) s} &= \ttx{exec (PUSH x : c) s}\\
&=\ttx{exec c \$ x : s}\\
&=\ttx{exec c \$ eval (Val x) : s}
\end{align*}
Inductive case:
\begin{align*}
\ttx{exec (comp' (Add}&\ttx{ el er) c) s}\\
&= \ttx{exec (comp' el \$ comp' er \$ ADD : c) s}\\
&= \ttx{exec (comp' er \$ ADD : c) \$ eval el : s}\\
&= \ttx{exec (ADD : c) \$ eval er : eval el : s}\\
&= \ttx{exec c \$ eval el + eval er : s}\\
&= \ttx{exec c \$ eval (Add el er) : s}
\end{align*}

\subsection*{Exercises}

\textsc{Exercise 1:} Show that \(\ttx{add n \$ Succ m} = \ttx{Succ \$ add n m}\) by induction on \ttx{n}.

\end{document}
