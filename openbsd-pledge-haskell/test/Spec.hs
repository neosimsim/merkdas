module Main
  ( main
  ) where

import           Control.Monad
import           System.Exit
import           Test.QuickCheck

prop_true :: a -> Bool
prop_true _ = True

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  result <- quickCheckResult (prop_true :: Integer -> Bool)
  unless (isSuccess result) exitFailure
