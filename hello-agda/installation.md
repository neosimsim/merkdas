# Installation
I feel Agda is quite tricky to setup. Especially compared to Haskell. This is even
more true if you use Stack and all you have to do, is call \verb+wget+ once.

We also need to prepare our editor for unicode input.

## Agda
Basically Agda is a hackage package. So you could install Agda using
cabal or stack.

	cabal v1-install Agda # or
	cabal v2-install Agda # or
	stack install Agda

You can also check your systems package manager.

	pacman -S agda
	apt-get install agda-mode

The package *Agda* installs to binaries

\begin{description}
	\item[agda] The Agda compiler
	\item[agda-mode] An Agda emacs front-end
\end{description}

This however will not be enough. `agda` is an application on Hackage which
has runtime dependencies. It needs certain Haskell libraries, which are nicely
sandbox my Cabal and Stack, so not accessible by our new agda installation.
Therefore I prefer and recommend to install Agda using one of the following
wrapper scripts.

```{include=agda-stack}
```

This script, by the philosophy of Stack, should always work.
Keep this file some where in \verb+$PATH+ as \verb+agda+.

Here is the same for nix:
```{include=agda}
```

Save this as `agda` some where in \$PATH *before* cabal/stack/nix installation
path. For further information, please have a look at the Agda User
Guide \cite{agda-user-guide}.

## Agda Libraries
Agda has a few built-in modules, so you want at least to install
the agda-stdlib \cite{agda-stdlib} to really have fun with Agda.

Like any good programming language, Agda has no real development
infrastructure, e. g.\ no real package/dependency manager. There are two more
or less convenient ways to install libraries for Agda.

```{=latex}
\begin{description}
	\item[Manually] Download the standard library \cite{agda-stdlib}.
		\begin{verbatim}
cd $HOME/src
git clone https://github.com/agda/agda-stdlib.git
		\end{verbatim}

And write the following files

		\begin{verbatim}
cat <<EOF >$HOME/.agda/libraries
$HOME/src/agda/std-lib
EOF
		\end{verbatim}

		\begin{verbatim}
cat <<EOF >$HOME/.agda/defaults
standard-library
EOF
		\end{verbatim}
	\item[agda-pkg] An Agda package manager written in python.
		\begin{verbatim}
pip install agda-pkg
apkg init
apkg install standard-library
		\end{verbatim}

		Keep in mind that it's a Python tool and might break after some update
		due to dynamic dependencies. It's a scripting language after all.
\end{description}
```

\subsection{Editor}
Agda makes heavy use of non-ASCII UTF-8 encoded Unicode, so you \emph{really}
need a proper editor. We'll cover two:

\subsubsection{Emacs}
You can say that Emacs is the official Agda-IDE, and supported by default.
All you have to do is run:
\begin{verbatim}
agda-mode setup
\end{verbatim}

If you used the stack or nix wrapper you should run

\begin{verbatim}
stack install Agda # or
cabal v2-install Agda # or
nix-env -f '<nixpkgs>' -iA haskellPackages.Agda
\end{verbatim}

Look at \cite{emacs-mode}. The “Getting Started” section of \cite{plfa}
has more hints for emacs setup.

\subsubsection{VIM}
Setting up vim is a more manual, so let me ask
\begin{itemize}
	\item who uses vim?
	\item who uses vim without plugin manager?
\end{itemize}

\paragraph{agda-vim}
You should also install derekelkins/agda-vim \cite{agda-vim}, which is to vim what
agda-mode is to emacs.

\paragraph{VIM key maps}
For vim you can find proper vim-scripts on the Agda wiki page \cite{vim-editing}.

Two file are linked

-	agda-utf8.vim

-	unicode-keys.vim

paste the content of these files to $HOME/.vim/ftplugin/agda.vim

Then you have to tell vim how to detect Agda files.

	cat <<EOF >.vim/ftdetect/agda.vim
	autocmd BufNewFile,BufReadPost *.agda set filetype=agda
	autocmd BufNewFile,BufReadPost *.lagda.md set filetype=agda
	autocmd BufNewFile,BufReadPost *.lagda.tex set filetype=agda
	EOF

\subsubsection{Unix Pipes}
A more general way is provided by a command line tool I wrote \cite{scripts-uni}.

	echo "\N" | uni
	ℕ

This can be used from withing vim, e. g.\ in visual line mode and typing

	:'<,'>!uni

and also works on any other propper editor, like sam and acme.

\subsubsection{xmodmap}
If you still use X11 (on Linux) you can also create the file $HOME/.Xmodmap
with the content

```{include=Xmodmap}
```

and call

	xmodmap $HOME/.Xmodmap

Then you can type ℕ by pressing AltGr-n.

If anyone knows a way how todo this in Wayland, please let me know.

This is however a growing list and not recommended for today.
