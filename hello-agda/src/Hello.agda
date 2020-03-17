module Hello where


open import IO using (run; putStrLn)
import IO.Primitive as Prim using (IO)
open import Data.Nat using (ℕ)
import Data.Nat.Show as Nat using (show)
open import Data.Unit using (⊤) -- This is no upper case 't'
open import Data.String using (_++_)

age : ℕ
age = 28

main : Prim.IO ⊤
main = run (putStrLn ("Hello World! I'm " ++ Nat.show age))

