\ignore{
\begin{code}
{-# LANGUAGE RecordWildCards   #-}

module Main (main) where
\end{code}
}

\begin{code}
data Point =
  Point
    { x :: Double
    , y :: Double
    }
  deriving (Show)

data Circle =
  Circle
    { center :: Point
    , radius :: Double
    }
  deriving (Show)
\end{code}

Using this syntactic sugar, we can modify individual fields using their names:

\begin{code}
set42Radius :: Circle -> Circle
set42Radius c = c {radius = 42.0}

get5 :: Double
get5 = radius (Circle (Point 3.0 4.0) 5.0)
\end{code}

You can easily see, that record syntax helps us avoiding boilerplate code.

But what if we wanted to implement the Haskell equivalent of the \verb+goRight+
function defined in Listing \ref{reference-code} in C\#? Using record syntax,
we would write:

\begin{code}
goRight :: Circle -> Circle
goRight c = c {center = (\p -> p {x = x p + 10}) (center c)}
\end{code}
or

\begin{code}
goRight' :: Circle -> Circle
goRight' c@(Circle p@(Point x _) _) = c { center = p { x = x + 10 }}
\end{code}

or using RecordWildCards
\begin{code}
goRight'' :: Circle -> Circle
goRight'' c@(Circle (p@Point{..}) _) = c { center = p { x = x  + 10 }}
\end{code}

We could also provide \verb+get+ and \verb+set+ functions
\begin{code}
getX :: Point -> Double
getX Point{..} = x

setX :: Double -> Point -> Point
setX x p = p {x = x}
\end{code}

and define a third verion of \verb|goRight| with them
\begin{code}
goRight''' :: Circle -> Circle
goRight''' c = c {center = setX (getX p + 10) p}
  where
    p :: Point
    p = center c
\end{code}

But again this results in a lot of Java-like boilerplate code.

\ignore{
\begin{code}
main :: IO ()
main = do
  let center = Point {x = 4, y = 5}
  let circle = Circle {center = center, radius = 6}
  putStr "initial circle:\t"
  print circle
  putStr "set42Radius:\t"
  print $ set42Radius circle
  putStr "get5:\t"
  print get5
  putStr "goRight:\t"
  print $ goRight circle
  putStr "goRight':\t"
  print $ goRight' circle
  putStr "goRight'':\t"
  print $ goRight'' circle
  putStr "goRight''':\t"
  print $ goRight''' circle
\end{code}
}