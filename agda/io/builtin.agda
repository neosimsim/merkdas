module builtin where

open import Agda.Builtin.IO
open import Agda.Builtin.Unit
open import Agda.Builtin.String

postulate putStrLn : String -> IO ⊤

{-# COMPILE GHC putStrLn = putStrLn . Data.Text.unpack #-}

main : IO ⊤
main = putStrLn "hallo"
