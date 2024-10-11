\documentclass{article}

\usepackage[paperwidth=5.5in,paperheight=8.5in,margin=0.5in,top=.6in,bottom=.6in]{geometry}
\usepackage{fontspec}
\usepackage{unicode-math}
\usepackage{fancyvrb}
\usepackage{syntax}
\usepackage{tikz}

\DefineVerbatimEnvironment{code}{Verbatim}{baselinestretch=.8, samepage=true}

\setmainfont{Garamond Premier Pro}[Contextuals=AlternateOff]
\setmathfont{Garamond Math}[Scale=MatchUppercase]
\setmonofont{Monaspace Argon}[Scale=0.7]

\setlength{\parindent}{0em}

\begin{document}

\begin{center}
\section*{Chapter 14}
\end{center}

\textsc{Exercise 1}: Complete the following instance declaration from \texttt{Data.Monoid} to make a pair type into a monoid provided the two component types are monoids:

\begin{code}
data Pair a b = Pair a b

instance (Semigroup a, Semigroup b) => Semigroup (Pair a b) where
    (<>) :: Pair a b -> Pair a b -> Pair a b
    Pair x1 y1 <> Pair x2 y2 = Pair (x1 <> x2) (y1 <> y2)

instance (Monoid a, Monoid b) => Monoid (Pair a b) where
    mempty :: Pair a b
    mempty = Pair mempty mempty
\end{code}

\textsc{Exercise 2:} In a similar manner, show how a function \texttt{type a -> b} can be made into a monoid provided that the result type \texttt{b} is a monoid.

\begin{code}
newtype Fun a b = Fun (a -> b)

instance (Semigroup b) => Semigroup (Fun a b) where
    (<>) :: Fun a b -> Fun a b -> Fun a b
    Fun f <> Fun g = Fun $ \x -> f x <> g x

instance (Monoid b) => Monoid (Fun a b) where
    mempty :: Fun a b
    mempty = Fun $ const mempty
\end{code}

\end{document}
