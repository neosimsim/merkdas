{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE RankNTypes #-}

-- Resources:
-- https://markkarpov.com/post/existential-quantification.html
module Main
  ( main
  ) where

import           Data.Char (toUpper)

main :: IO ()
main = do
  putStrLn "Hallo"
  putStrLn $ myPrettyPrinter stringer 80 True False -- Here I can choose what to print (forall a)
  where
    stringer :: (Show b) => b -> String
    stringer = reverse . show

myPrettyPrinter ::
     forall a. Show a -- 'a' will be fixed when we use 'myPrettyPrinter'
  => (forall b. Show b =>
                  b -> String -- but not 'b'
      )
  -> Int
  -> Bool
  -> a
  -> String
myPrettyPrinter stringer width upperCase =
  take width .
  (if upperCase
     then fmap toUpper
     else id) .
  stringer . length . show
