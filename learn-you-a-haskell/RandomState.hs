-- Copyright © 2018, Alexander Ben Nasrallah <me@abn.sh>
-- Use of this source code is governed by a BSD 3-clause
-- style license that can be found in the LICENSE file.
module Main
  ( main
  ) where

import           Control.Monad.State
import           System.Random

randomSt :: (RandomGen g, Random a) => State g a -- wraps g -> (a, g)
randomSt = state random

threeCoins :: State StdGen (Bool, Bool, Bool)
threeCoins = do
  a <- randomSt
  b <- randomSt
  c <- randomSt
  return (a, b, c)

-- unwrap the do notation in threeCoins
threeCoins' :: State StdGen (Bool, Bool, Bool)
threeCoins' =
  randomSt >>=
  (\a -- bind random number to a and set state to new generator
    ->
     randomSt >>=
     (\b -- bind random number to b and set state to new generator
       ->
        randomSt >>=
        (\c -- bind random number to c and set state to new generator
          ->
           return (a, b, c) -- wrap numbers to minimal context, i.e. stateful function, that keeps the state unchanged.
         )))

-- FYI
-- instance Monad (State s)
-- return x = State $ \s -> (x, s)
-- (State h) >>= f = State $ \s -> let (a, newState) = h s
--                                     (State g) = f a
--                                 in g newState

main = do
  gen <- getStdGen
  print $ runState threeCoins gen
  print $ runState threeCoins' gen
  print $ f gen
  where f = runState threeCoins
