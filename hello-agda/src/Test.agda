module Test where

open import IO using (run; putStrLn)
import IO.Primitive as Prim using (IO)
open import Data.Nat.Show as Nat using (show)
open import Data.Unit using (⊤)

open import Vector

main : Prim.IO ⊤
main = run (putStrLn (Nat.show (length gravity₂)))
