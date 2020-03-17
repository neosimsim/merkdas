# Hello World
Now open your editor and try to type the following version of “Hello World”.

```{include=src/Hello.agda}
```

This is not the minimal version of “Hello World”, but this version checks if we

-	can enter special character

-	can compile Agda

-	can use the standard library, e. g. `IO`

-	can run Agda programs

Now the moment of truth

	agda --compile Hello.agda
	./Hello

A few things to notice:

-	Agda import are qualified by default. Using the `open` statement
	we can use the import unqualified.

-	The `using` statement is optional, although in Haskell it turned out
	to be good practice.

-	Type signatures are are defined using a single colon `:` instead
	of `::`. This will bother you a couple of time 😉.

\begin{exercise}
Move the definition of \verb+age+ after \verb+main+.
\end{exercise}

Why: Prevents you from propositions stating theselfe.