module Main
  ( main
  ) where

import           Vec

main :: IO ()
main = do
  print $ lengthV (Cons False (Cons False (Cons True Empty)))
  print $
    lengthV'
      (SSuc (SSuc (SSuc SZero)))
      (Cons False (Cons False (Cons True Empty)))
