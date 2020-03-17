\documentclass{article}

\usepackage{neosimsim}
\usepackage{neosimsim-a4paper}
\usepackage[backend=biber]{biblatex}
\addbibresource{bibliography.bib}

% needed for lhs
\lstnewenvironment{code}{}{}
\newcommand{\ignore}[1]{}

\begin{document}
\ignore{
\begin{code}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main
  ( main
  ) where

import           Control.Category
import           Control.Lens        (makeLenses, (+~))
import           Control.Monad.State
import           Prelude             hiding (id, (.))
\end{code}
}

This article is inspired by \cite{haskell-for-all_lenses}.

\section{The Problem}
Lenses—Haskell's \verb+Lens+ type—are generalizes properties found in
other programming Languages.

\begin{lstlisting}[caption={C\# Reference code},label=reference-code]
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
\end{lstlisting}

If we want to implement \verb+goRight+ from Listing \ref{reference-code}, we
would have to write something like

\begin{code}
data Point =
  Point Double Double

getX :: Point -> Double
getX (Point x _) = x

setX :: Double -> Point -> Point
setX x' (Point _ y) = Point x' y

getY :: Point -> Double
getY (Point _ y) = y

setY :: Double -> Point -> Point
setY y' (Point x _) = Point x y'

data Circle =
  Circle Point Double

getCenter :: Circle -> Point
getCenter (Circle p _) = p

setCenter :: Point -> Circle -> Circle
setCenter p' (Circle _ r) = Circle p' r

getRadius :: Circle -> Double
getRadius (Circle _ r) = r

setRadius :: Double -> Circle -> Circle
setRadius r' (Circle p _) = Circle p r'
\end{code}

Is is quite tedious, and fortunately we already know a better way
to declare such accessors and mutators, i. e. record syntax.

\begin{code}
data PointR =
  PointR
    { x :: Double
    , y :: Double
    }
  deriving (Show)

data CircleR =
  CircleR
    { center :: PointR
    , radius :: Double
    }
  deriving (Show)
\end{code}

Using this syntactic sugar, we can modify individual fields using their names:

\begin{code}
set42Radius :: CircleR -> CircleR
set42Radius c = c {radius = 42.0}

get5 :: Double
get5 = radius (CircleR (PointR 3.0 4.0) 5.0)
\end{code}

You can easily see, that record syntax helps us avoiding boilerplate code.

But what if we wanted to implement the Haskell equivalent of the \verb+goRight+
function defined in listing \ref{reference-code} in C\#? Using record syntax,
we would write:

\begin{code}
goRight :: CircleR -> CircleR
goRight c = c {center = (\p -> p {x = x p + 10}) (center c)}
\end{code}
or
\begin{code}
goRight' :: CircleR -> CircleR
goRight' c@(CircleR p@(PointR x _) _) = c { center = p { x = x + 10 }}
\end{code}
or using RecordWildCards
\begin{code}
goRight'' :: CircleR -> CircleR
goRight'' c@(CircleR (p@PointR{..}) _) = c { center = p { x = x  + 10 }}
\end{code}
we could also provide \verb+get+ and \verb+set+ functions
\begin{code}
getXR :: PointR -> Double
getXR PointR{..} = x

setXR :: Double -> PointR -> PointR
setXR x p = p {x = x}

goRight''' :: CircleR -> CircleR
goRight''' c = c {center = setXR (getXR p + 10) p}
  where
    p :: PointR
    p = center c
\end{code}

But again this results in a lot of Java-like boilerplate code.

\section{Lenses}
However, Haskell doesn't require language support for implementing properties!
After all, a property consists of a getter and setter, so we can package them
both up into single data, which we call a \verb+Lens+ instead of a property.

\begin{code}
--               getter     setter
newtype Lens a b = Lens { fromLens :: (a -> b, b -> a -> a) }
\end{code}

So now we can write mote general \verb+get+ and \verb+set+ functions:

\begin{code}
getL :: Lens a b -> a -> b
getL (Lens (g, _)) = g

setL :: Lens a b -> b -> a -> a
setL (Lens (_, s)) = s
-- or setL = snd
\end{code}

Using \verb+Lens+ we can provide a function which allows modification.

\begin{code}
modL :: Lens a b -> (b -> b) -> a -> a
modL l f x = setL l (f $ getL l x) x
\end{code}

Here is the example \verb+Lens+ for the $x$ value of \verb+PointR+

\begin{code}
x' :: Lens PointR Double
x' = Lens (getXR, setXR)
\end{code}

\verb+modL+ might be considered more fundamential than \verb+setL+
\cite{haskell-for-all_lenses}.

Let's play around with it:
\begin{code}
playWithLenses :: PointR -> IO ()
playWithLenses p = do
  putStr "getL:\t"
  print $ getL x' p
  putStr "setL:\t"
  print $ setL x' 3 p
  putStr "modL:\t"
  print $ modL x' (+10) p
\end{code}

We are slightly improving on Java notationally, bu still way behind C\#, However,
we're just getting started. First off, we can take advantage of Haskell's syntactic
support for operators, which are just infix functions:

\begin{code}
(^.) :: a -> Lens a b -> b
(^.) = flip getL

(^=) :: Lens a b -> b -> a -> a
(^=) = setL
\end{code}

\begin{code}
playWithInfixLenses :: PointR -> IO ()
playWithInfixLenses p = do
  putStr "getL:\t"
  print $ p ^. x'
  putStr "setL:\t"
  print $ (x' ^= 3) p
\end{code}

This looks better, but it still resembles a functional style, especially the setter.
What we really want is something stateful, in the spirit of imperative programming.
Fortunately for us, we can use the \verb+State+ monad to implement an imperative
programming style:

\begin{code}
(^:=) :: Lens a b -> b -> State a ()
p ^:= b = do
  a <- get
  let a' = setL p b a
  put a'

access :: Lens a b -> State a b
access p = do
  a <- get
  let b = getL p a
  return b
\end{code}

Now we can write all out favorite imperative functions using the above two
primitives:

\begin{code}
(%=) :: Lens a b -> (b -> b) -> State a ()
p %= f = do
  b <- access p
  p ^:= f b

(+=) :: (Num b) => Lens a b -> b -> State a ()
p += x = p %= (+ x)

(*=) :: (Num b) => Lens a b -> b -> State a ()
p *= x = p %= (* x)
\end{code}

\begin{code}
getYR :: PointR -> Double
getYR PointR{..} = y

setYR :: Double -> PointR -> PointR
setYR y p = p {y = y}

y' :: Lens PointR Double
y' = Lens (getYR, setYR)

goUp :: State PointR ()
goUp = y' += 7.0
\end{code}

\begin{code}
playWithLensesState :: PointR -> IO ()
playWithLensesState p = do
  putStr "getL:\t"
  print $ (execState $ access x') p
  putStr "setL:\t"
  print $ (execState $ x' ^:= 4) p
  putStr "modL:\t"
  print $ (execState $ x' %= (+10)) p
  putStr "modL:\t"
  print $ (execState $ x' += 10) p
  putStr "modL:\t"
  print $ (execState $ x' *= 10) p
  putStr "goUp:\t"
  print $ (execState goUp) p
\end{code}

\section{Categories}
So if all we could do was ape C\# we all would not love Haskell as much
as we do, so it's time to dig a little deeper.

Haskell's great strength stems from the fact that it derives all of its design
patters from mathematics, specifically category theory. Haskell borrows the
\verb+Category+ design pattern from category theory, which is defines as
anything that implements composition ad identity.

\begin{lstlisting}[caption={definition of Category}]
class Category c where
  (.) :: c y z -> c x y -> c x z
  id :: c x x
\end{lstlisting}

Oviously functions form a \verb+Catagory+:
\begin{lstlisting}[caption={(->) instance of Category}]
instance Category (->) where
  (f . g) x = f (g x)
  id x = x
\end{lstlisting}

However, lenses form a \verb+Category+, too!

% Lens a b ^= (a -> b, b -> a -> a)
\begin{code}
instance Category Lens where
  (.) :: Lens b c -> Lens a b -> Lens a c
  (Lens (g1, s1)) . (Lens (g2, s2)) = Lens (g1 . g2, updateA)
    where
      updateA c a = a'
        where
          b = g2 a
          b' = s1 c b
          a' = s2 b' a
  id :: Lens a a
  id = Lens (id, const)
\end{code}

Any \verb+Category+ instance must statisy three laws (knows as the
Category Laws):

\begin{description}
	\item[Left Identity] $id . f = f$
	\item[Right Identity] $f . id = f$
	\item[Associatifity] $(f . g) . h = f . (g . h)$
\end{description}

Now we can define \verb+go+right

\begin{code}
getCenterR :: CircleR -> PointR
getCenterR = center

setCenterR :: PointR -> CircleR -> CircleR
setCenterR p c = c { center = p }

center' :: Lens CircleR PointR
center' = Lens (getCenterR, setCenterR)

goRightLens :: State CircleR ()
goRightLens = (x' . center') += 10
\end{code}

\section{Conclusion}

Now that we have learned about lenses, we still have to admit, there is some
boilerplate code we have to write. Fortunately the package \emph{lens}~\cite{hackage-lens},
found on hackage, provide template haskell function to generate these
boilerplates.

Here is the final version of listing \ref{reference-code} in Haskell using the
\emph{lens} package:
\begin{code}
data PointLens =
  PointLens
    { _xLens :: Double
    , _yLens :: Double
    }
  deriving (Show)

makeLenses ''PointLens

data CircleLens =
  CircleLens
    { _centerLens :: PointLens
    , _radiusLens :: Double
    }
  deriving (Show)

makeLenses ''CircleLens

goRightLens' :: CircleLens -> CircleLens
goRightLens' = (centerLens . xLens) +~ 10

mainLens :: IO ()
mainLens = do
  let center = PointLens {_xLens = 4, _yLens = 5}
  let circle = CircleLens {_centerLens = center, _radiusLens = 6}
  putStr "initial circle:\t"
  print circle
  putStr "goRightLens':\t"
  print $ goRightLens' circle
\end{code}

\ignore{
\begin{code}
main :: IO ()
main = do
  putStrLn "hello lenses"
  let center = PointR {x = 4, y = 5}
  let circle = CircleR {center = center, radius = 6}
  putStr "initial circle:\t"
  print circle
  putStr "goRight:\t"
  print $ goRight circle
  putStr "goRight':\t"
  print $ goRight' circle
  putStr "goRight'':\t"
  print $ goRight'' circle
  putStr "goRight''':\t"
  print $ goRight''' circle
  -- lenses
  putStrLn ""
  playWithLenses center
  putStrLn ""
  putStrLn "with infix"
  playWithInfixLenses center
  putStrLn ""
  putStrLn "with State"
  playWithLensesState center
  putStr "goRightLens:\t"
  print $ execState goRightLens circle
  putStrLn ""
  putStrLn "with lens package"
  mainLens
\end{code}
}

\printbibliography{}
\end{document}
