\documentclass[10pt]{article}

\usepackage{neosimsim}
\usepackage{neosimsim-a4paper}
\usepackage{neosimsim-literate-programming}
\usepackage[backend=biber]{biblatex}
\usepackage{cprotect}
\addbibresource{bibliography.bib}

% needed for lhs
\newcommand{\ignore}[1]{}

\begin{document}
This article is inspired by \cite{haskell-for-all_lenses}.

\section{The Problem}
Lenses—Haskell's \verb+Lens+ type—are generalized properties found in
other programming Languages.

\begin{listing}\label{reference-code}
\cprotect\caption{\verb|goRight| of a Circle in C\#}
\begin{verbatim}
class Point {
  public double x { get; set; }
  public double y { get; set; }
}

class Circle {
  public Point  center { get; set; }
  public double radius { get; set; }
}

public void goRight(ref Circe c) {
  c.center.x += 10;
}
\end{verbatim}
\end{listing}

If we want to implement the \verb|goRight| from Listing~\ref{reference-code} in Haskell, we
would have to write something like

\input{Plain.lhs}

This is quite tedious even for this quire simple data structures. Fortunately we know a
better way to declare such accessors and mutators, i.e. record syntax.

\section{Record Syntax}
\input{Records.lhs}

\section{Lenses to the Rescue}
\input{Lenses.lhs}

\section{Conclusion}
\input{Conclusion.lhs}

\printbibliography{}
\end{document}
