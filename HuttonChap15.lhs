\documentclass{article}

\usepackage[paperwidth=5.5in,paperheight=8.5in,margin=0.5in,footskip=.25in]{geometry}
\usepackage{fontspec}
\usepackage{amsmath}
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
\end{code}

\noindent
\textsc{Exercise 1:} Identify the redexes in the following expressions, and determine whether each redex is innermost, outermost, neither, or both:

\begin{Verbatim}
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
\mathtt{fst (1+2,2+3)} &= \mathtt{fst (3, 2+3)}\\
&= \mathtt{fst (3, 5)}\\
&= \mathtt{3}
\end{align*}

Outermost evaluation results in:
\begin{align*}
\mathtt{fst (1+2,2+3)} &= \mathtt{1+2}\\
&= \mathtt{3}
\end{align*}

So outermost evaluation does not evaluate \texttt{2+3} which is discarded anyway by \texttt{fst}.

\noindent
\textsc{Exercise 3:} Given the definition \verb$mult = \x -> (\y -> x * y)$, show how the evaluation of \texttt{mult 3 4} can be broken down into four separate steps.

\begin{center}
\begin{Verbatim}
mult 3 4 = (\x -> (\y -> x * y)) 3 4
         = (\y -> 3 * y) 4
         = 3 * 4
         = 12
\end{Verbatim}
\end{center}

\noindent
\textsc{Exercise 4:} Using a list comprehension, define an expression \texttt{fibs :: [Integer]} that generates the infinite sequence of Fibonacci numbers
\[ 0, 1, 2, 3, 5, 8, 13, 21, 34, \cdots\]

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
\end{document}
