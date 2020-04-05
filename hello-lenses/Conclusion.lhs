\ignore{
\begin{code}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE TemplateHaskell   #-}

import           Control.Lens (makeLenses, (+~))
\end{code}
}

Now that we have learned about lenses, we still have to admit, there is some
boilerplate code we have to write. Fortunately the package \emph{lens}~\cite{hackage-lens},
found on hackage, provides template Haskell functions to generate these
boilerplates.

Here is the final version of listing \ref{reference-code} in Haskell using the
\emph{lens} package:
\begin{code}
data Point =
  Point
    { _x :: Double
    , _y :: Double
    }
  deriving (Show)

makeLenses ''Point

data Circle =
  Circle
    { _center :: Point
    , _radius :: Double
    }
  deriving (Show)

makeLenses ''Circle

goRight :: Circle -> Circle
goRight = (center . x) +~ 10

main :: IO ()
main = do
  let center = Point {_x = 4, _y = 5}
  let circle = Circle {_center = center, _radius = 6}
  putStr "initial circle:\t"
  print circle
  putStr "goRight:\t"
  print $ goRight circle
\end{code}
