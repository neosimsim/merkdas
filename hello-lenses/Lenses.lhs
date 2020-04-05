\ignore{
\begin{code}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}

module Main
  ( main
  ) where

import           Control.Category
import           Control.Monad.State (State)
import qualified Control.Monad.State as State
import           Prelude             hiding (id, (.))
\end{code}
}

Again we begin by declaring the data types:
\begin{code}
data Point =
  Point
    { _x :: Double
    , _y :: Double
    }
  deriving (Show)

data Circle =
  Circle
    { _center :: Point
    , _radius :: Double
    }
  deriving (Show)

\end{code}

Note that we decorated the record fields with a leading underscore. This is because, we
will use the record field to define lenses and will use the lenses to access the fields.
Therefore we sacrefice the field name.

Remember that even with record typed we had to deal with a lot of Java-like boilerplate code.
So, it would be nice to have an even more powerful, language feature, thatn C\# \verb|get|
and \verb|set|.

However, Haskell doesn't require language support for implementing properties!
After all, a property consists of a getter and setter, so we can package them
both up into single data, which we call a \verb+Lens+ instead of a property.

\begin{code}
--                                     getter     setter
newtype Lens a b = Lens { fromLens :: (a -> b, b -> a -> a) }
\end{code}

Now we can write more general \verb+get+ and \verb+set+ functions:

\begin{code}
get :: Lens a b -> a -> b
get (Lens (g, _)) = g
-- or get = fst

set :: Lens a b -> b -> a -> a
set (Lens (_, s)) = s
-- or set = snd
\end{code}

Using \verb+Lens+ we can provide a function which allows modification.

\begin{code}
change :: Lens a b -> (b -> b) -> a -> a
change l f x = set l (f $ get l x) x
\end{code}

Here is the example \verb+Lens+ for the $x$ value of \verb+PointR+

\begin{code}
x :: Lens Point Double
x = Lens (_x, \x p -> p {_x = x})
\end{code}

\verb+mod+ might be considered more fundamential than \verb+set+
\cite{haskell-for-all_lenses}.

Let's play around with it:
\begin{code}
playWithLenses :: Point -> IO ()
playWithLenses p = do
  putStr "get:\t"
  print $ get x p
  putStr "set:\t"
  print $ set x 3 p
  putStr "change:\t"
  print $ change x (+10) p
\end{code}

We are slightly improving on Java notationally, bu still way behind C\#, However,
we're just getting started. First off, we can take advantage of Haskell's syntactic
support for operators, which are just infix functions:

\begin{code}
(^.) :: a -> Lens a b -> b
(^.) = flip get

(^=) :: Lens a b -> b -> a -> a
(^=) = set
\end{code}

\begin{code}
playWithInfixLenses :: Point -> IO ()
playWithInfixLenses p = do
  putStr "get:\t"
  print $ p ^. x
  putStr "set:\t"
  print $ (x ^= 3) p
\end{code}

This looks better, but it still resembles a functional style, especially the setter.
What we really want is something stateful, in the spirit of imperative programming.
Fortunately for us, we can use the \verb+State+ monad to implement an imperative
programming style:

\begin{code}
(^:=) :: Lens a b -> b -> State a ()
p ^:= b = do
  a <- State.get
  let a' = set p b a
  State.put a'

access :: Lens a b -> State a b
access p = do
  a <- State.get
  let b = get p a
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
y :: Lens Point Double
y = Lens (_y, \y p -> p{_y = y })

goUp :: State Point ()
goUp = y += 7.0
\end{code}

\begin{code}
playWithLensesState :: Point -> IO ()
playWithLensesState p = do
  putStr "get:\t"
  print $ (State.execState $ access x) p
  putStr "set:\t"
  print $ (State.execState $ x ^:= 4) p
  putStr "mod:\t"
  print $ (State.execState $ x %= (+10)) p
  putStr "mod:\t"
  print $ (State.execState $ x += 10) p
  putStr "mod:\t"
  print $ (State.execState $ x *= 10) p
  putStr "goUp:\t"
  print $ (State.execState goUp) p
\end{code}

\subsection{Categories}
So if all we could do was ape C\# we all would not love Haskell as much
as we do, so it's time to dig a little deeper.

Haskell's great strength stems from the fact that it derives all of its design
patters from mathematics, specifically category theory. Haskell borrows the
\verb+Category+ design pattern from category theory, which is defines as
anything that implements composition ad identity.

\begin{verbatim}
class Category c where
  (.) :: c y z -> c x y -> c x z
  id :: c x x
\end{verbatim}

Oviously functions form a \verb+Catagory+:
\begin{verbatim}
instance Category (->) where
  (f . g) x = f (g x)
  id x = x
\end{verbatim}

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

Now we can define \verb+goRight+

\begin{code}
center :: Lens Circle Point
center = Lens (_center, \p c -> c {_center = p})

goRight :: State Circle ()
goRight = (x . center) += 10
\end{code}


\ignore{
\begin{code}
main :: IO ()
main = do
  let center = Point {_x = 4, _y = 5}
  let circle = Circle {_center = center, _radius = 6}
  putStr "initial circle:\t"
  print circle
  playWithLenses center
  putStrLn ""
  putStrLn "with infix"
  playWithInfixLenses center
  putStrLn ""
  putStrLn "with State"
  playWithLensesState center
  putStr "goRight:\t"
  print $ State.execState goRight circle
\end{code}
}