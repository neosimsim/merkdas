\ignore{
\begin{code}
module Main
  ( main
  ) where
\end{code}
}

\begin{code}
data Point =
  Point Double Double deriving (Show)

getX :: Point -> Double
getX (Point x _) = x

setX :: Double -> Point -> Point
setX x' (Point _ y) = Point x' y

getY :: Point -> Double
getY (Point _ y) = y

setY :: Double -> Point -> Point
setY y' (Point x _) = Point x y'

data Circle =
  Circle Point Double deriving (Show)

getCenter :: Circle -> Point
getCenter (Circle p _) = p

setCenter :: Point -> Circle -> Circle
setCenter p' (Circle _ r) = Circle p' r

getRadius :: Circle -> Double
getRadius (Circle _ r) = r

setRadius :: Double -> Circle -> Circle
setRadius r' (Circle p _) = Circle p r'

goRight :: Circle -> Circle
goRight circle =
  let
    c = setX (getX (getCenter circle) + 10) (getCenter circle)
  in
    setCenter c circle
\end{code}

Or maybe using pattern matching:
\begin{code}
goRight' :: Circle -> Circle
goRight' (Circle (Point x y) r) = Circle (Point (x + 10) y) r
\end{code}

\ignore{
\begin{code}
goUp :: Circle -> Circle
goUp circle =
  let
    c = setY (getY (getCenter circle) + 10) (getCenter circle)
  in
    setCenter c circle

zoom :: Circle -> Circle
zoom circle = setRadius (getRadius circle * 10) circle
\end{code}
}

\ignore{
\begin{code}
main :: IO ()
main = do
  let center = Point 4 5
  let circle = Circle center 6
  putStr "initial circle:\t"
  print circle
  putStr "goRight:\t"
  print $ goRight circle
  putStr "goRight':\t"
  print $ goRight' circle
  putStr "goUp:\t"
  print $ goUp circle
  putStr "zoom:\t"
  print $ zoom circle
\end{code}
}