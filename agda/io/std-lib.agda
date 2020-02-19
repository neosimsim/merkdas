module std-lib where

open import IO using (IO; run; putStrLn; _>>_)
open import Data.Unit using (⊤)
open import Codata.Musical.Notation using (♯_)
import Agda.Builtin.IO using (IO)
open import Function using (_$_)

main1 : Agda.Builtin.IO.IO ⊤
main1 = run (♯ putStrLn "hallo" >> ♯ putStrLn "welt")

main2 : Agda.Builtin.IO.IO ⊤
main2 = run (do
  ♯ putStrLn "hallo"
  ♯ putStrLn "welt"
  )

main3 : Agda.Builtin.IO.IO ⊤
main3 = run $ do
  ♯ putStrLn "hallo"
  ♯ putStrLn "welt"

main : Agda.Builtin.IO.IO ⊤
main = main3
